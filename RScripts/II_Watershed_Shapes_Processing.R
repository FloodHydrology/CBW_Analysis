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

#######################################################################
#Create individual raster and polygong files for each watershed--------
#######################################################################
#Locate files with watershed shapes
files<-list.files(paste0(data_dir, "sheds_unnested/"))
index_fun<-function(n){
  temp_watershed<-raster(paste0(data_dir, "sheds_unnested/", files[n]))
  data.frame(wuid = unique(temp_watershed), 
             file = files[n])
}
watershed_index<-mclapply(X = seq(1, length(files)), FUN=index_fun, mc.cores = detectCores())
watershed_index<-do.call(rbind, watershed_index)


