###### Identifying the proportion of suitable area over Protected area for five Bovidae species
# 
# calculating the proportion of suitable area inside protected areas (WDPA) polygons
# extracting values from binary map (1=suitable, 0 = unsuitable) of the best TSS models
# using exactextractr package: https://github.com/isciences/exactextractr 
# using two file types 
# 1) Binary map of suitable areas for each species 
# 2) Protected areas polygons (cleaned the duplicate polygon version)

#install.packages("exactextractr")
# load package --------
library(rgdal)
library(raster)
library(dplyr)
require(tidyverse)
require(xlsx)
library(sf) # Simple Features
library(sp)
library(exactextractr) #extract raster values in polygons

rm(list = ls())

####### calculate binary model proportion with PA ######## 

# set up working directory  
# path <-  "..." this can be the folder path for the dataset.
# path<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation"

path<-"/bovidae_enm/result_sample/binary"
setwd(path)

#import the best model raster (only the best models)
bg <- raster("bg_la_null_bin.tif") #gaur
bj <- raster("bj_la_obr_bin.tif") #banteng
ba <- raster("ba_ssa_obr_bin.tif") #buffalo
cs <- raster("cs_ssa_null_bin.tif") #serow
ng <- raster("ng_la_obr_bin.tif") #goral

best<- list(bg,bj,ba,cs,ng)
best

# LA Protected Area polygon (CRS = WGS84)
path2<-"/bovidae_enm/data_preparation/PA_and_country/wdpa/"

large <- st_read(paste0(path2,"AsiaSelect2_largeacc_wdpar_clean.shp"))

# SSA Protected Areas polygon (CRS = WGS84)
bash<-st_read(paste0(path2,"PA_buffalo.shp"))
cssh<-st_read(paste0(path2,"PA_serow.shp"))

#create PA list
#order of list (gaur LA, banteng LA, buffalo SSA, serow SSA, goral LA)
sh<- list(large,large,bash,cssh,large)
sh

nam<-c("gaur","banteng","buffalo","serow","goral")

mlist <-list()

set.seed(111)

#create & set result directory
result<-"/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/R/suit_prop"
setwd("/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/R/")

result<-"/bovidae_enm/cal_result/suit_prop"
dir.create(result)
setwd(result)

#ref: https://gis.stackexchange.com/questions/368386/r-zonal-statistics-for-classes

# extract suitable and unsuitable value and export as table and shapefile
for (i in 1:length(best)){
  names(best)[i]
  
  #extract suitable values
  sh[[i]]$pa_suitbin <- exact_extract(best[[i]], sh[[i]], function(value, fraction) {
    sum(fraction[value == 1], na.rm = T) / sum(fraction)
  })
  #extract unsuitable values
  sh[[i]]$pa_unsuitbin <- exact_extract(best[[i]], sh[[i]], function(value, fraction) {
    sum(fraction[value == 0], na.rm = T) / sum(fraction)
  })
  
  sh[[i]]$pa_suitsum <- sh[[i]]$pa_unsuitbin+sh[[i]]$pa_suitbin
  
  keep <- c("WDPAID","NAME","DESIG_E","IUCN_CA","GIS_ARE","ISO3","pa_suitbin","pa_unsuitbin","pa_suitsum") # list of want col names
  
  sh[[i]] <- sh[[i]][,(names(sh[[i]]) %in% keep)]
  
  sh[[i]]$model <- nam[[i]]
  
  #Export individual .shp
  st_write(sh[[i]],paste0(nam[i],"_hotspot_bin.shp"),
           driver="ESRI Shapefile")
  #Export individual .xlsx
  st_write(sh[[i]],paste0(nam[i],"_hotspot_bin.xlsx"), driver="XLSX")
   
  mlist[[i]] <- sh[[i]] %>% as.data.frame()
  
}

# bind row and export as .xlsx file
df<-bind_rows(mlist)

#df = subset(df, select = -c(geometry) )
str(df)
View(df)

write.xlsx(df,'hotspot_best_bin.xlsx')
