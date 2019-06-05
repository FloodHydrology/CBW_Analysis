#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Analyze watershed attributes
#Coder: Nate Jones (cnjones@umd.edu)
#Date: 5/15/2019
#Description: Examine watershed attributes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Analysis of Shed Attributes---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create list of catchments for downloads~~~~~~~~~~~~~~~~~~~~~~~~~~~
sheds<-list.files(paste0(data_dir,"sheds_raster/"))
sheds<-substr(sheds, 1, nchar(sheds)-4)

#2.1 Create function to estimate shed attibributes---------------------
#for testing

