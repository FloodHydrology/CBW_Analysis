#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: CBW Geospatial Analysis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/4/2019
#Description: Geospatial analysis to characterize reach and watershed characteristics from 
#             CBW macroinvertebrate dataset.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Use WBT flowpath analysis to define reach of interest.
#Use network analysis to find upstream reaches and watersheds
#Estimate NDHplus values
#Estimatate landuse values, dem analysis, etc

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace ===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace
remove(list=ls())

#load appropriate packages
library(parallel)
library(rslurm)   
library(raster)
library(sf)
library(stars)
library(fasterize)
library(whitebox)
library(tidyverse)

#Mater projection
p<-"+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#Set data dir
data_dir<-"//nfs/njones-data/Research Projects/LULC_CBW/Spatial_Data/"

#NHD flowlines
flowlines<-st_read(paste0(data_dir,"NHDPlusMA/NHDPlus02/NHDSnapshot/Hydrography/NHDFlowline.shp")) %>% 
  st_zm() %>% 
  st_transform(., crs=p)

#NHD raster data
fdr<-raster(paste0(data_dir, "NHDPlusMA/NHDPlus02/NHDPlusFdrFac02a/fdr")) 
fac<-raster(paste0(data_dir, "NHDPlusMA/NHDPlus02/NHDPlusFdrFac02a/fac")) 

#gather data pnts
pnts_CPB<-read_csv(paste0(data_dir,"input_CPB.csv")) %>% dplyr::select(CBW_ID, Latitude, Longitude) 
pnts_MBSS<-read_csv(paste0(data_dir, 'input_MBSS.csv'))  %>% dplyr::select(CBW_ID, Latitude83, Longitude83) %>% rename(Latitude = Latitude83, Longitude = Longitude83) %>% mutate( Longitude = -1*Longitude)
pnts<-rbind(pnts_CPB, pnts_MBSS) %>% 
  st_as_sf(., 
           coords=c("Longitude","Latitude"), 
           crs="+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs") %>% 
  st_transform(., crs = p) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Identify reaches===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function -----------------------------------------------------------
fun<-function(n){
  #Organize spatial data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Isolate point of interest
  pnt<-pnts[n,]
  
  #creat buffer around pnt [to clip frd]
  pnt_buffer<-st_buffer(pnt, 250) 
  pnt_buffer_proj<-st_transform(pnt_buffer, crs(fdr))
    
  #Create masked fdr raster
  d8pntr<-fdr %>% 
    #crop fdr to buffer
    crop(., pnt_buffer_proj) %>% 
    mask(., pnt_buffer_proj) %>% 
    #project raster into master projection
    projectRaster(., crs=p)
  
  #Create masked fdr raster
  d8acc<-fac %>% 
    #crop fdr to buffer
    crop(., pnt_buffer_proj) %>% 
    mask(., pnt_buffer_proj) %>% 
    #project raster into master projection
    projectRaster(., crs=p)
    
  #clip nhdflowlines
  flowline<-flowlines %>% st_crop(., pnt_buffer)
  
  #Use WBT to find flowpath to downstream flowline~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create temporary workspace
  scratch_dir<-paste0(tempfile(),"/")
  dir.create(scratch_dir)
  
  #Write data to temp workspace
  st_write(pnt, paste0(scratch_dir, "pnt.shp"), delete_layer = TRUE)
  writeRaster(d8pntr, paste0(scratch_dir, "d8pntr.tif"), overwrite=TRUE)
  
  #Execute WBT flowpaths function
  trace_downslope_flowpaths(seed_pts = paste0(scratch_dir,"pnt.shp"), 
                            d8_pntr  = paste0(scratch_dir,"d8pntr.tif"), 
                            output   = paste0(scratch_dir,"flowpath.tif"))
  
  #Read flowpath into R environmnet
  flowpath<-raster(paste0(scratch_dir,"flowpath.tif")) 
  flowpath<-flowpath*d8acc
  flowpath<-flowpath %>% st_as_stars() %>% st_as_sf(., merge=T)
  
  #Isolate first cell where flowpath intersects a flowline
  pp<-flowpath[flowline,]
  pp<-pp %>% filter(layer == base::max(layer, na.rm=T)) %>% slice(1) 
  
  #Define COMID
  COMID<-flowline[pp,] %>% dplyr::select(COMID) %>% slice(1) %>% st_drop_geometry()
  
  #Convert PP to point and add COMID info
  pp<-pp %>% 
    mutate(COMID = COMID$COMID, 
           CBW_ID = pnt$CBW_ID) %>% 
    dplyr::select(COMID, CBW_ID) %>% 
    st_centroid()
  
  #Export Results~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Remove temp workspace
  #unlink(scratch_dir, recursive = T)
  
  #Export PP to global data space
  pp
}

#2.2 Execute function ----------------------------------------------------------
#apply function (~ minutes on SESYNC server)
t0<-Sys.time()
output<-mclapply(seq(1,10), fun, mc.cores=4)
output<-do.call(rbind, output)
tf<-Sys.time()
tf-t0









