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
data_dir<-"//storage.research.sesync.org/njones-data/Research Projects/LULC_CBW/Spatial_Data/"
scratch_dir<-"C:\\ScratchWorkspace/"
wbt_dir<-"C:\\WBT/whitebox_tools"

#Download required data
dem<-raster(paste0(data_dir,"NHDPlus02/Elev_Unit_a/elev_cm"))
pnts_CPB<-read_csv(paste0(data_dir,"input_CPB.csv"))
pnts_MBSS<-read_csv(paste0(data_dir, 'input_MBSS.csv'))
HUC08<-st_read(paste0(data_dir,"NHDPlus02/Subbasin.shp"))

#######################################################################
#Create individual raster and polygong files for each watershed--------
#######################################################################
