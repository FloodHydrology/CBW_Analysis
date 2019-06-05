#######################################################################
#Title: Process watershed shapes
#Coder: Nate Jones (cnjones@umd.edu)
#Date: 4/12/2019
#Description: Convert unnested basin files into individual files
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
data_dir<-"//nfs/njones-data/Research Projects/LULC_CBW/Spatial_Data/"

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
#Create individual raster and polygong files for each watershed--------
#######################################################################
#Combine pour point files
files<-list.files(paste0(data_dir, "sheds_pp/"))
files<-files[substr(files, nchar(files)-2, nchar(files))=="shp"]
snap_shp<-st_read(paste0(data_dir, "sheds_pp/",files[1]))
for(i in 2:length(files)){snap_shp<-rbind(snap_shp, st_read(paste0(data_dir, "sheds_pp/",files[i])))}

#Locate files with watershed shapes
files<-list.files(paste0(data_dir, "sheds_unnested/"))
index_fun<-function(n){
  temp_watershed<-raster(paste0(data_dir, "sheds_unnested/", files[n]))
  data.frame(wuid = unique(temp_watershed), 
             file = files[n])
}
watershed_index<-mclapply(X = seq(1, length(files)), FUN=index_fun, mc.cores = detectCores())
watershed_index<-do.call(rbind, watershed_index)

#Create function to make individual watershed shapes
attributes_fun<-function(n){
  
  #Download watershed file of interest
  watershed<-raster(paste0(data_dir,"sheds_unnested/",watershed_index$file[n]))
  
  #Identify watershed [use watershed 1 for now]
  w_grd<-watershed %in% watershed_index$wuid[n]
  w_grd[w_grd==0]<-NA
  
  #crop raters to reasonable extent
  w_pnts <- tibble(w_length=which(w_grd@data@values)) %>% 
    mutate(x = (w_length %% ncol(w_grd))*res(w_grd)[1]+extent(w_grd)[1]-res(w_grd)[1]/2, 
           y = extent(w_grd)[4]-((ceiling(w_length/ncol(w_grd)))*res(w_grd)[2])+res(w_grd)[2]/2)
  w_grd<-crop(w_grd, c(min(w_pnts$x,na.rm = T)-res(w_grd)[1]*10,
                       max(w_pnts$x,na.rm = T)+res(w_grd)[1]*10, 
                       min(w_pnts$y,na.rm = F)-res(w_grd)[2]*10, 
                       max(w_pnts$y,na.rm = F)+res(w_grd)[2]*10))
  
  #convert to polygon
  w_shp<- w_grd %>% st_as_stars() %>% st_as_sf(., merge = TRUE) #rasterToPolygons(w_grd, dissolve = T)
  w_shp<-st_as_sf(w_shp)
  w_shp<-st_combine(w_shp)
  st_crs(w_shp)<-st_crs(snap_shp)
  
  #Identify pour point of interest 
  pp_shp<-snap_shp[st_buffer(w_shp,100),]
  w_line<-st_cast(w_shp, "MULTILINESTRING")
  pp_shp$dist<-st_distance(pp_shp, w_line, by_element = T)
  pp_shp<-pp_shp[pp_shp$dist==min(pp_shp$dist, na.rm=T),]
  if(nrow(pp_shp)>1){pp_shp<-pp_shp[1,]}
  ID<-pp_shp$CBW_ID
  
  #Export watershed shapes
  st_write(w_shp, paste0(data_dir, "sheds_poly/watershed_",ID,".shp"), delete_layer = T)
  writeRaster(w_grd, paste0(data_dir, "sheds_raster/watershed_",ID,".tif"), overwrite=TRUE)
}

#Send to Cluster for anlayis
sopts   <- list(partition = "sesync", time = "12:00:00")
params  <- data.frame(n=seq(1,nrow(watershed_index)))
job     <- slurm_apply(attributes_fun, 
                       params,
                       add_objects = c("data_dir", "watershed_index", "snap_shp"),
                       nodes = 24, cpus_per_node=8,
                       pkgs=c('sf','raster','stars','tidyverse'),
                       slurm_options = sopts)
print_job_status(job)
cleanup_files(job)
