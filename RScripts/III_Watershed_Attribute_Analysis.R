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
library(rslurm)   

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

#PRISM 30-year normals (from http://www.prism.oregonstate.edu/normals/)
precip<-raster(paste0(data_dir, "PRISM_ppt_30yr_normal_800mM2_all_asc//PRISM_ppt_30yr_normal_800mM2_annual_asc.asc"))
temp<-raster(paste0(data_dir, "PRISM_tmean_30yr_normal_800mM2_all_asc//PRISM_tmean_30yr_normal_800mM2_annual_asc.asc"))


#Create list of catchments for downloads
sheds<-list.files(paste0(data_dir,"sheds_raster/"))
sheds<-substr(sheds, 1, nchar(sheds)-4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Analysis of Shed Attributes------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create function to estimate shed attibributes------------------------------
fun<-function(n){
  
  #Organize spatial data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #identify watershed of interest
  id<-substr(sheds[n], 11, nchar(sheds[n]))
  shed<- st_read(paste0(data_dir, "sheds_poly//", sheds[n],".shp"))
  
  #Quit function if shed is <900m^2
  if(as.numeric(st_area(shed))<=900){
    output<- tibble(var = "error", 
                    value = -9999, 
                    year  = -9999, 
                    CBW_ID = id)
  }else{
  
  #clip rasters and the reproject
  #nlcd
  shed_lulc<-st_transform(shed, crs=nlcd@crs)
  lulc<-crop(nlcd, shed_lulc)
  lulc<-mask(lulc, shed_lulc)
  lulc<-projectRaster(lulc, crs=dem@crs)
  #dem
  dem<-crop(dem, shed)
  dem<-mask(dem, shed)
  #climateic data
  shed_prism<-st_transform(shed, crs=precip@crs)
  p<-crop(precip, shed_prism)
  p<-mask(p, shed_prism)
  p<-projectRaster(p, crs=dem@crs)
  t<-crop(temp, shed_prism)
  t<-mask(t, shed_prism)
  t<-projectRaster(t, crs=dem@crs)
  
  #estimate area of nlcd classes~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #tabulate area
  lulc<-freq(lulc)
  
  #Add nlcd info to each collumn
  for(i in 1:length(lulc)){
    #Edit each component in list
    lulc[[i]]<-cbind(
      #Isolate list component
      lulc[[i]],
      #Add year information from component name
      year = substr(names(lulc)[i],6,9)
    )
  }
  
  #collapse list to one tibble
  lulc<-do.call(rbind, lulc)
  lulc<-as_tibble(lulc) %>%
    rename(var = value,
           value = count) %>%
    mutate(var   = paste0("nlcd_",var),
           year  = as.numeric(year),
           value = as.numeric(value)*90)
  
  #Estimate geomoprhic metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  geo<-tibble(
    watershed_area_m2            = st_area(shed),
    watershed_elevation_median_m = cellStats(dem, median)/100,
    watershed_slope_median       = cellStats(terrain(dem, unit='degrees'), median)[1]) %>%
    gather() %>%
    mutate(year = -9999) %>%
    rename(var = key)
  
  #Estimate climatic metrics~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  climate<-tibble(
    temp_c = cellStats(t, median),
    precip_mm = cellStats(p, median)) %>%
    gather() %>%
    mutate(year = -9999) %>%
    rename(var=key)
  
  #Create export
  output<-bind_rows(lulc, geo, climate) %>%
    mutate(CBW_ID = id)
  }
  #Print output
  output
}

#2.2 Send to function to SLURM workflow manager---------------------------------
#Cluster parameters
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-20
n.cpus<-8

#Run functions
sopts   <- list(partition = cluster_name, time = time_limit)
params  <- data.frame(n=seq(1,length(sheds)))
slurm_run <- slurm_apply(fun, 
                         params,
                         add_objects = c(
                           #Directory Locations
                           'data_dir',
                           #Functions
                           "fun", 
                           #Spatial data
                           "dem","nlcd","precip","temp", "sheds"),
                         nodes = n.nodes, cpus_per_node=n.cpus,
                         pkgs=c('tidyverse','raster','sf','rgdal'),
                         slurm_options = sopts)

#Gather results
print_job_status(slurm_run)
results <- get_slurm_out(slurm_run, outtype = "table")
cleanup_files(slurm_run)

#Identify errors to deal with later
errors<-results$CBW_ID[results$var=="error"]
results<-results %>% filter(var != "error", 
                            var != 'nlcd_NA')

#2.3 Create Output Datasets-----------------------------------------------------
#Create long dataset 
output_long<-results

#Create wide datset
output_wide<-results %>% as_tibble() %>%
  mutate(key = if_else(substr(var,1,4)=="nlcd", 
                       paste0(year,"_",var),
                       var)) %>%
  select(key, value, CBW_ID) %>%
  spread(key, value)

#write outputs
write_csv(output_long, paste0(data_dir,"output_long.csv"))
write_csv(output_wide, paste0(data_dir,"output_wide.csv"))