###### Identifying the proportion of suitable area over Protected area for five Bovidae species

# Calculating the proportion of suitable area inside protected areas (WDPA) polygons
# Extracting values from binary map (1 = suitable, 0 = unsuitable) of the best TSS models
# Use exactextractr package: https://github.com/isciences/exactextractr 
# Use two file types 
# 1) Binary map for each species 
# 2) Protected area polygons (cleaned duplicate polygons)

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
options(scipen = 999)

####### Calculate binary model proportion with PA ######## 

# set up working directory  
# path <-  "..." this can be the folder path for the dataset.

setwd("./GitHub/bovidae_enm/")

path1 <- "./result_sample/binary/"

#import the best model raster (only the best models)
#bg <- raster(paste0(path1,"bg_la_null_bin.tif")) #gaur
#bj <- raster(paste0(path1,"bj_la_obr_bin.tif")) #banteng
#ba <- raster(paste0(path1,"ba_ssa_obr_bin.tif")) #buffalo
#cs <- raster(paste0(path1,"cs_ssa_null_bin.tif")) #serow
#ng <- raster(paste0(path1,"ng_la_obr_bin.tif")) #goral

#> Species specific accessible area (SSA) binary list ----
binlist_ssa <- list.files(path = "./result_sample/binary/", pattern = "_ssa", full.names = T) |> lapply(raster)
binlist_ssa

# SSA Protected Areas polygon (CRS = WGS84)
#create PA list

#order of list ( buffalo, gaur, banteng, serow, goral)

path2 <- "./data_preparation/PA_and_country/wdpa/"

bash <- st_read(paste0(path2, "PA_buffalo.shp"))
bgsh <- st_read(paste0(path2, "PA_gaur.shp"))
bjsh <- st_read(paste0(path2, "PA_banteng.shp"))
cssh <- st_read(paste0(path2, "PA_serow.shp"))
ngsh <- st_read(paste0(path2, "PA_goral.shp"))

sh <- list(bash,bgsh,bjsh,cssh,ngsh)
sh

nam<-c("buffalo", "gaur", "banteng", "serow", "goral")

set.seed(111)

#ref: https://gis.stackexchange.com/questions/368386/r-zonal-statistics-for-classes

# Set homerange for each species
homerange <- list( 55, 60, 45, 6, 1) # home range for each species (buffalo, gaur, banteng, serow, goral)
homerange

# extract suitable and unsuitable value and export as table and shapefile

for (i in 1:length(binlist_ssa)) {
  # Print the name of the current item in binlist_ssa
  names(binlist_ssa)[i]
  
  # Extract suitable values
  sh[[i]]$pa_suitbin <- exact_extract(binlist_ssa[[i]], sh[[i]], function(value, fraction) {
    sum(fraction[value == 1], na.rm = TRUE) / sum(fraction)
  })
  
  # Extract unsuitable values
  sh[[i]]$pa_unsuitbin <- exact_extract(binlist_ssa[[i]], sh[[i]], function(value, fraction) {
    sum(fraction[value == 0], na.rm = TRUE) / sum(fraction)
  })
  
  # Calculate the sum of suitable and unsuitable values
  sh[[i]]$pa_suitsum <- sh[[i]]$pa_unsuitbin + sh[[i]]$pa_suitbin
  
  sh[[i]]$area1 <- sh[[i]]$pa_suitbin*sh[[i]]$GIS_ARE
  sh[[i]]$area0 <- sh[[i]]$pa_unsuitbin*sh[[i]]$GIS_ARE
  
  for (j in 1:nrow(sh[[i]])) {
    # Calculate the home range classification
    
    sh[[i]]$home_range[j] <- ifelse(sh[[i]]$area1[j] < homerange[[i]], 0, 1) 
    
    # Classify suitability, checking for NA values
    if (is.na(sh[[i]]$pa_suitbin[j])) {
      sh[[i]]$class[j] <- "unknown" # Assign a default category for NA values
    } else if (sh[[i]]$pa_suitbin[j] <= 0.2) {
      sh[[i]]$class[j] <- "unsuitable"
    } else if (sh[[i]]$pa_suitbin[j] <= 0.4) {
      sh[[i]]$class[j] <- "low"
    } else if (sh[[i]]$pa_suitbin[j] <= 0.6) {
      sh[[i]]$class[j] <- "medium"
    } else if (sh[[i]]$pa_suitbin[j] <= 0.8) {
      sh[[i]]$class[j] <- "high"
    } else {
      sh[[i]]$class[j] <- "very high"
    }
  }
  
  # Define the columns to keep
  keep <- c("WDPAID", "NAME", "DESIG_E", "IUCN_CA", "GIS_ARE", "ISO3", "pa_suitbin", "pa_unsuitbin", "pa_suitsum", 
            "area1", "area0", "home_range","class")

  # Subset the data frame to keep only the specified columns
  sh[[i]] <- sh[[i]][, (names(sh[[i]]) %in% keep)]
  
  # Add a model column
  sh[[i]]$model <- nam[[i]]
}

# save combine lists ----
# bind row and export as .xlsx file
df <- bind_rows(sh)
df <- st_drop_geometry(df)
# df = subset(df, select = -c(geometry) )

class(df)
str(df)
View(df)

write.xlsx(df, 'hotspot_sm_best_bin.xlsx')

#> Export individual .shp ----
for (i in 1:length(sh)) {
  st_write(sh[[i]],paste0(nam[i],"_hotspot_bin.shp"),
           driver="ESRI Shapefile")
  #Export individual .xlsx
  st_write(sh[[i]],paste0(nam[i],"_hotspot_bin.xlsx"), driver="XLSX")
  
}

# This will be a bit slow or we can just run for the best model.
#> Large accessible area (LA) binary list ----- 
binlist_la <- list.files(path = "./result_sample/binary/", pattern = "_la", full.names = T) |> lapply(raster)
binlist_la

# LA Protected Area polygon (CRS = WGS84)
path2<-"./data_preparation/PA_and_country/wdpa/"

large <- st_read(paste0(path2,"AsiaSelect2_largeacc_wdpar_clean.shp"))
plot(large$geometry)

# Create WDPA list for 5 species
la <- list(large,large,large,large,large)


for (i in 1:length(binlist_la)) {
  # Print the name of the current item in binlist_ssa
  names(binlist_la)[i]
  
  # Extract suitable values
  la[[i]]$pa_suitbin <- exact_extract(binlist_la[[i]], la[[i]], function(value, fraction) {
    sum(fraction[value == 1], na.rm = TRUE) / sum(fraction)
  })
  
  # Extract unsuitable values
  la[[i]]$pa_unsuitbin <- exact_extract(binlist_la[[i]], la[[i]], function(value, fraction) {
    sum(fraction[value == 0], na.rm = TRUE) / sum(fraction)
  })
  
  # Calculate the sum of suitable and unsuitable values
  la[[i]]$pa_suitsum <- la[[i]]$pa_unsuitbin + la[[i]]$pa_suitbin
  
  la[[i]]$area1 <- la[[i]]$pa_suitbin*la[[i]]$GIS_ARE
  la[[i]]$area0 <- la[[i]]$pa_unsuitbin*la[[i]]$GIS_ARE
  
  for (j in 1:nrow(sh[[i]])) {
    # Calculate the home range classification
    
    la[[i]]$home_range[j] <- ifelse(la[[i]]$area1[j] < homerange[[i]], 0, 1) 
    
    # Classify suitability, checking for NA values
    if (is.na(la[[i]]$pa_suitbin[j])) {
      la[[i]]$class[j] <- "unknown" # Assign a default category for NA values
    } else if (la[[i]]$pa_suitbin[j] <= 0.2) {
      la[[i]]$class[j] <- "unsuitable"
    } else if (la[[i]]$pa_suitbin[j] <= 0.4) {
      la[[i]]$class[j] <- "low"
    } else if (la[[i]]$pa_suitbin[j] <= 0.6) {
      la[[i]]$class[j] <- "medium"
    } else if (la[[i]]$pa_suitbin[j] <= 0.8) {
      la[[i]]$class[j] <- "high"
    } else {
      la[[i]]$class[j] <- "very high"
    }
  }
  
  # Define the columns to keep
  keep <- c("WDPAID", "NAME", "DESIG_E", "IUCN_CA", "GIS_ARE", "ISO3", "pa_suitbin", "pa_unsuitbin", "pa_suitsum", 
            "area1", "area0", "home_range","class")
  
  # Subset the data frame to keep only the specified columns
  la[[i]] <- la[[i]][, (names(la[[i]]) %in% keep)]
  
  # Add a model column
  la[[i]]$model <- nam[[i]]
}


# save combine lists ----
# bind row and export as .xlsx file
df2 <- bind_rows(la)
df2 <- st_drop_geometry(df2)

class(df2)
str(df2)
View(df2)

write.xlsx(df2,'hotspot_la_best_bin.xlsx')

#> Export individual .shp ----
for (i in 1:length(la)) {
  st_write(la[[i]],paste0(nam[i],"_hotspot_la_bin.shp"),
           driver="ESRI Shapefile")
  #Export individual .xlsx
  st_write(la[[i]],paste0(nam[i],"_hotspot_la_bin.xlsx"), driver="XLSX")
}

# END #