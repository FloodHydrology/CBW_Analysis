#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Analyze watershed attributes
#Coder: Nate Jones (cnjones@umd.edu)
#Date: 5/15/2019
#Description: Examine watershed attributes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#load velox package
library(devtools)
install_github('hunzikp/velox')
library(velox)

#Set data dir
data_dir<-"//nfs//njones-data//Research Projects//LULC_CBW//Spatial_Data//"

#Import relevant data
#NLCD Data (from https://www.mrlc.gov/data)
nlcd2001<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2001_Land_Cover_L48_20190424.img"))
nlcd2004<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2004_Land_Cover_L48_20190424.img"))
nlcd2006<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2006_Land_Cover_L48_20190424.img"))
nlcd2008<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2008_Land_Cover_L48_20190424.img"))
nlcd2011<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2011_Land_Cover_L48_20190424.img"))
nlcd2013<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2013_Land_Cover_L48_20190424.img"))
nlcd2016<-raster(paste0(data_dir, "NLCD_Land_Cover_L48//NLCD_2016_Land_Cover_L48_20190424.img"))
nlcd<-stack(nlcd2001, nlcd2004, nlcd2006, nlcd2008, nlcd2011, nlcd2013, nlcd2016)

#NHD DEM Data (from https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data)
dem<-raster(paste0(data_dir, "NHDPlus02//Elev_Unit_a//elev_cm"))

#Create list of catchments for downloads
sheds<-list.files(paste0(data_dir,"sheds_raster/"))
sheds<-substr(sheds, 1, nchar(sheds)-4)

#reproject nlcd
nlcd<-projectRaster(nlcd, crs = dem@crs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Analysis of Shed Attributes------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function to estimate shed attibributes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#fun<-function(n){}

#for testing
n=1

#identify watershe of interest
shed<- st_read(paste0(data_dir, "sheds_poly//", sheds[1],".shp"))
  
#clip rasters and the reproject
#nlcd
shed_nlcd<-st_transform(shed, crs=nlcd@crs)
nlcd<-crop(nlcd, shed_nlcd)  
nlcd<-mask(nlcd, shed_nlcd)  
nlcd<-projectRaster(nlcd, crs=dem@crs)
#dem
dem<-crop(dem, shed)
dem<-mask(dem, shed)

#estimate lulc areas for each image

