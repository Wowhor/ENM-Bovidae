########################## Bovidae richness maps ######################
# creating richness map from sum of the best binary models (5 models)
# species richness map will range from 0 - 5; 
# 0 = unsuitable for all 5 species; 
# 5 = suitable for all fve species

# load packages
library(raster)        
library(rgdal)  
library(rasterVis)
library(sf)
library(tmap)
library(tmaptools)
library(grid)

options(digits=7, scipen= 999)
rm(list = ls())
#memory.limit(size = 10000000000000)


## > All the best models from two accessible area types ---------
# Binary raster..select only from best TSS from each small/large accessible area
# path1<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS/"

path1 <-"/bovidae_enm/result_sample/binary"
setwd(path1)
dir()

## > import the best binary models -------------
rbg<-raster("bg_la_null_bin.tif")   #gaur
rbj<-raster("bj_la_obr_bin.tif")    #banteng
rba<-raster("ba_ssa_obr_bin.tif")   #buffalo
rcs<-raster("cs_ssa_null_bin.tif")  #serow  
rng<-raster("ng_la_obr_bin.tif")    #goral

#rasterize species-specific as large accessible areas
rba <- projectRaster(rba,rbg,method = 'ngb')
rcs <- projectRaster(rcs,rbg,method = 'ngb')

#Correcting residual negative values&fill NA with 0
rbg[is.na(rbg[])] <- 0 
rbj[is.na(rbj[])] <- 0 
rba[is.na(rba[])] <- 0 
rcs[is.na(rcs[])] <- 0 
rng[is.na(rng[])] <- 0 

rbg <- as.factor(rbg)
rbj <- as.factor(rbj)
rba <-as.factor(rba)
rcs <-as.factor(rcs)
rng <-as.factor(rng)

#check the plot again to see if the extent is correct
plot(rbg)
plot(rcs)

#crop as large accessible area
#lm<-shapefile("/Users/whorpien/OneDrive - Massey University/GIS data/SpData/acc_ecoregion_la_disv_cropfinalmodel.shp")

lm<-shapefile("/bovidae_enm/data_preparation/acc/acc_la/acc_ecoregion_msdm.shp")
plot(lm)

richness<-raster::stack (rbg,rbj,rba,rcs,rng)
names(richness) <- c("Bos gaurus", 
                     "Bos javanicus", 
                     "Bubalus arnee", 
                     "Capricornis sumatraensis",
                     "Naemorhedus griseus")

# SUM binary maps 
richness_sum <- sum(richness, na.rm = TRUE)
richness_sum <- crop(richness_sum, extent(lm))%>% 
  mask(lm)
richness_sum

values(richness_sum)[values(richness_sum) < 0] = 0
values(richness_sum)[values(richness_sum) > 5] = 5
plot(richness_sum)

#plotting categorical
r<-as.factor(richness_sum)
table(values(r))

# Declaring raster binary levels (using the goral's LA--highest richness ==4)
rat <- levels(r)[[1]]
#rat[["suitability"]] <- c("0","1")
rat[["suitability"]] <- c("0","1","2","3","4") # maximum numbers = 4; minimum = 0
levels(r) <- rat
r

plot(r)
#this one have 5 levels (using goral's SA--highest richness == 5)

#Save as Geotiff
setwd("/bovidae_enm/result_sample/")
raster::writeRaster(x = r, 
                    filename=file.path( "richness_5bov_mix_besttss.tif"), 
                    bylayer = TRUE,
                    overwrite = TRUE)

## > plotting & checking the richness map ------------
# plotting Bovidae richness map figure
mypalhot <- viridisLite::turbo(6)

## export the richness map figure
setwd("/bovidae_enm/fig/")
png(filename = "richness_5bov_mix_besttss.png",
    res=600, units = "cm", width= 25, height=22)

levelplot(r, cuts=5, col.regions= mypalhot, type='continuous')

dev.off()
