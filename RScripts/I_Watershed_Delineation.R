#######################################################################
#Title: Watershed Delineation
#Coder: Nate Jones (cnjones@umd.edu)
#Date: 4/11/2019
#Description: Delineate watershed for each sampling point
#######################################################################

#######################################################################
#Setup workspace-------------------------------------------------------
#######################################################################
#Clear workspace
remove(list=ls())

#load appropriate packages
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
library(stars)
library(parallel)
library(rslurm)   

#Set data dir
data_dir<-"//storage.research.sesync.org/njones-data/Research Projects/LULC_CBW/Spatial_Data/"
scratch_dir<-"C:\\ScratchWorkspace/"
wbt_dir<-"C:\\WBT/whitebox_tools"

#Download required data
dem<-raster(paste0(data_dir,"NHDPlus02/Elev_Unit_a/elev_cm"))
pnts_CPB<-read_csv(paste0(data_dir,"input_CPB.csv"))
pnts_MBSS<-read_csv(paste0(data_dir, 'input_MBSS.csv'))
HUC08<-st_read(paste0(data_dir,"NHDPlus02/Subbasin.shp"))


#Organzie pnt data
pnts_CPB<- pnts_CPB %>% dplyr::select(CBW_ID, Latitude, Longitude)
pnts_MBSS<- pnts_MBSS %>% dplyr::select(CBW_ID, Latitude83, Longitude83) %>% rename(Latitude = Latitude83, Longitude = Longitude83)
pnts<-rbind(pnts_CPB, pnts_MBSS)
pnts<-st_as_sf(pnts, 
               coords=c("Longitude","Latitude"), 
               crs="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")
pnts<-st_transform(pnts, paste(dem@crs))

#Limit HUC08 to where points occur
HUC08<-st_transform(HUC08, crs=paste(dem@crs))
HUC08<-HUC08[pnts,]

#######################################################################
#Delineate Watershed---------------------------------------------------
#######################################################################
#Create function to delineate watersheds by HUC
fun<-function(n){

  #Define HUC, pnts, and DEM
  HUC<-HUC08[n,]
  pnts<-pnts[HUC,]
  dem<-crop(dem, as.vector(st_bbox(HUC))[c(1, 3, 2, 4)])
  dem<-mask(dem, HUC)

  #Preprocess DEM~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Export DEM and stream layer to local working directory
  writeRaster(dem, 
              paste0(scratch_dir,"dem.tif"), 
              overwrite=T)

  #Gaussian Filter
  system(paste(paste(wbt_dir), 
               "-r=GaussianFilter", 
               paste0("--wd=",scratch_dir),
               "-i='dem.tif'", 
               "-o='dem_filter.tif'",
               "--sigma=3"))
  
  #Fill "single cell" depressions
  system(paste(paste(wbt_dir),
               "-r=FillSingleCellPits",
               paste0("--wd=",scratch_dir),
               "--dem='dem_filter.tif'",
               "-o='dem_breach_minor.tif'"))

  #Breach larger depressions
  system(paste(paste(wbt_dir), 
               "-r=BreachDepressions", 
               paste0("--wd=",scratch_dir),
               "--dem='dem_breach_minor.tif'", 
               "-o='dem_breach_major.tif'"))

  #Define stream network~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create Flow Accumulation Raster
  system(paste(paste(wbt_dir), 
               "-r=D8FlowAccumulation", 
               "--out_type='cells'",
               paste0("--wd=",scratch_dir),
               "--dem='dem_breach_major.tif'", 
               "-o='fac.tif'"))
  
  #Read fac and define values
  fac<-raster(paste0(scratch_dir,"fac.tif"))
  fac<-c(cellStats(fac, min), cellStats(fac, max))
  
  #Define stream network based on fac threshold
  system(paste(paste(wbt_dir), 
               "-r=Reclass", 
               paste0("--wd=",scratch_dir),
               "-i='fac.tif'",
               "-o='flowgrid.tif",
               paste0("--reclass_vals='0;",fac[1],";1000;1;1000;",fac[2]+1)
  ))

  #Create Pour Points~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create UID 
  pnts$id<-seq(0, nrow(pnts)-1)

  #Export pour point to scratch directory 
  st_write(pnts, paste0(scratch_dir,"pnts.shp"), delete_layer = T)

  #Create pour pnt raster
  system(paste(paste(wbt_dir), 
               "-r=VectorPointsToRaster", 
               paste0("--wd=",scratch_dir),
               "-i='pnts.shp'", 
               "--field=UID",
               "-o=pp.tif",
               "--assign=min",
               "--nodata",
               "--base=dem.tif"))
  
  #Jenson Snap Pour point
  system(paste(paste(wbt_dir),
               "-r=JensonSnapPourPoints", 
               paste0("--wd=",scratch_dir),
               "--pour_pts='pp.tif'", 
               "--streams='flowgrid.tif'",
               "-o='pp_snap.tif",
               "--snap_dist=1000"))
  
  #Convert back to point file
  snapgrid<-raster(paste0(scratch_dir,"pp_snap.tif"))
  snapvector<-getValues(snapgrid)
  snapvalues<-na.omit(snapvector)
  snappnts<- tibble(snap_length=which(snapvector %in% snapvalues)) %>% 
    mutate(x = (snap_length %% ncol(snapgrid))*res(snapgrid)[1]+extent(snapgrid)[1]-res(snapgrid)[1]/2, 
           y = extent(snapgrid)[4]-((ceiling(snap_length/ncol(snapgrid)))*res(snapgrid)[2])+res(snapgrid)[2]/2,
           id= snapvalues)
  
  #Create sf file and export to workspace
  snappnts<-st_as_sf(snappnts, 
                     coords=c("x","y"), 
                     crs=paste(dem@crs))
  st_write(snappnts, paste0(scratch_dir,"snap.shp"), delete_layer = T)

  #Delineate wateshed-------------------------------------------------
  #Run flow direction 
  system(paste(paste(wbt_dir), 
               "-r=D8Pointer", 
               paste0("--wd=",scratch_dir),
               "--dem='dem_breach_major.tif'", 
               "-o='fdr.tif'",
               "--out_type=sca"))
  #Delineate watershed
  system(paste(paste(wbt_dir),
               "-r=unnest_basins", 
               paste0("--wd=",scratch_dir),
               "--d8_pntr='fdr.tif'", 
               "--pour_pts='snap.shp'",
               paste0("-o='watershed_", HUC$HUC_8, ".tif")))
}
  
#Execute Function
lapply(seq(1, nrow(HUC08)), fun)

