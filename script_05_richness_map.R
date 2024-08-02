########################## Bovidae richness maps ######################
# Create richness map from sum of the best binary models (5 models)
# Species richness map range from 0 - 5; 
# 0 = unsuitable for all 5 species; 
# 5 = suitable for all five species (with goral SSA)

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

# Entire species range --------
# > All the best models from two accessible area types ---------
# Binary raster..select only from best TSS from each small/large accessible area

path1 <-"/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/GitHub/bovidae_enm/"
setwd(path1)
dir()

## > import the best binary models -------------
rbg <- raster("./result_sample/binary/bg_la_null_bin.tif")   #gaur
rbj <- raster("./result_sample/binary/bj_la_obr_bin.tif")    #banteng
rba <- raster("./result_sample/binary/ba_ssa_obr_bin.tif")   #buffalo
rcs <- raster("./result_sample/binary/cs_ssa_null_bin.tif")  #serow  
rng <- raster("./result_sample/binary/ng_ssa_obr_bin.tif")    #goral

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

#rasterize species-specific as large accessible areas
rba <- projectRaster(rba, rbg, method = 'ngb')
rcs <- projectRaster(rcs, rbg, method = 'ngb')
rng <- projectRaster(rng, rbg, method = 'ngb')

#check the plot again to see if the extent is correct
plot(rbg)
plot(rbj)
plot(rba)
plot(rcs)
plot(rng)

#crop as large accessible area

lm <- shapefile("./data_preparation/acc/acc_la/acc_ecoregion_msdm.shp")
plot(lm)

richness<-raster::stack (rbg, rbj, rba, rcs, rng)
names(richness) <- c("Bos gaurus", 
                     "Bos javanicus", 
                     "Bubalus arnee", 
                     "Capricornis sumatraensis",
                     "Naemorhedus griseus")

# SUM binary maps 
richness_sum <- sum(richness, na.rm = TRUE)
richness_sum <- crop(richness_sum, extent(lm)) %>% 
  mask(lm)
richness_sum

values(richness_sum)[values(richness_sum) < 0] = 0
values(richness_sum)[values(richness_sum) > 5] = 5
plot(richness_sum)

#plotting categorical
r <- as.factor(richness_sum)
table(values(r))

# Declaring raster binary levels (using the goral's LA--highest richness ==4)
rat <- levels(r)[[1]]
#rat[["suitability"]] <- c("0","1")
rat[["suitability"]] <- c("0","1","2","3","4","5") # maximum numbers = 4; minimum = 0
levels(r) <- rat
r

plot(r)

# This can skip
## export the richness map: GeoTif & figure ###
# Save as Geotiff
raster::writeRaster(x = r, 
                    filename=file.path( "./result_sample/richness_5bov_best.tif"), 
                    bylayer = TRUE,
                    overwrite = TRUE)

## > plotting & checking the richness map ------------
# plotting Bovidae richness map figure
mypalhot <- viridisLite::turbo(6)

png(filename = "./result_sample/richness_5bov_mix_besttss.png",
    res=600, units = "cm", width = 25, height = 22)

levelplot(r, cuts = 5, col.regions = mypalhot, type = 'continuous')

dev.off()

# Thailand species's potential distribution -----------
# Thailand boundary
tha <- shapefile("./data_preparation/adm_border/gadm36_THA_0.shp")
plot(tha)

#> import the best binary models -----
#rbg <- raster("./result_sample/binary/bg_la_null_bin.tif")  #gaur
#rbj <- raster("./result_sample/binary/bj_la_obr_bin.tif")   #banteng
#rba <- raster("./result_sample/binary/ba_ssa_obr_bin.tif")  #buffalo
#rcs <- raster("./result_sample/binary/cs_ssa_null_bin.tif") #serow  
#rng <- raster("./result_sample/binary/ng_ssa_obr_bin.tif")  #goral; use ng ssa it predict suitable area in Thailand

plot(rng)

# Add NA to fix extent of a goral map 
# Correcting empty values for chinese goral area filling with zero
rng <- projectRaster(rng,rbg,method = 'ngb')
rng[is.na(rng[])] <- 0

rs <- list(rbg, rbj, rba, rcs, rng)
cr <- list()

for ( i in 1:length(rs)) {
  
  names(rs)[i]
  
  cr[[i]] <- rs[[i]] %>% 
    raster::crop(tha) %>% 
    raster::mask(tha)
}

cr

# Dimension 
dim(cr[[1]]) == dim(cr[[2]])
dim(cr[[1]]) == dim(cr[[3]])
dim(cr[[1]]) == dim(cr[[4]])
dim(cr[[1]]) == dim(cr[[5]])

cr[[4]] <- projectRaster(cr[[4]],cr[[1]],method = 'ngb')

tlist <- raster::stack(cr)

plot(tlist)

names(tlist) <- c("Gaur",
                  "Banteng",           
                  "Wild water buffalo", 
                  "Mainland serow",    
                  "Chinese goral")

# create list
tlist_3sp <- stack(tlist[[1]], tlist[[2]], tlist[[3]])
tlist_3sp

plot(tlist_3sp)

# levelplot
library(rasterVis)
cols <- c("lightgray", "gray26")
levelplot(tlist_3sp, col.regions= cols,layout=c(3, 2),colorkey=FALSE)

#> richness map, Thai ------------ 
richness_sum <- sum(tlist_3sp, na.rm = TRUE)
richness_sum <- crop(richness_sum, extent(tha))%>% 
  mask(tha)
richness_sum
plot(richness_sum)

#Save as Geotiff with Thailand extent
#setwd("/bovidae_enm/result_sample/")
raster::writeRaster(x = richness_sum, 
                    filename=file.path( "./result_sample/richness_3bov_best_tha.tif"), 
                    bylayer = TRUE,
                    overwrite = TRUE)
# END #