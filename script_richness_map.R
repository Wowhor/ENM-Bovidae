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

lm<-shapefile("/bovidae_enm/data_preparation/acc/accmsdm/acc_ecoregion_msdm.shp")
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
  mask(lmac)
richness_sum

values(richness_sum)[values(richness_sum) < 0] = 0
values(richness_sum)[values(richness_sum) > 5] = 5
plot(richness_sum)

#plotting categorical
r<-as.factor(richness_sum)
table(values(r))
r
# Declaring raster binary levels (using the goral's LA--highest richness ==4)
rat <- levels(r)[[1]]
#rat[["suitability"]] <- c("0","1")
rat[["suitability"]] <- c("0","1","2","3","4")
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
# Bovidae richness map figure
mypalhot <- viridisLite::turbo(6)

## export the richness map figure
setwd("/bovidae_enm/fig/")
png(filename = "richness_5bov_mix_besttss.png",
    res=600, units = "cm", width= 25, height=22)

levelplot(r, cuts=5, col.regions= mypalhot, type='continuous')

dev.off()

########################### Thailand richness with all the best --------
# Cropping process can be skipped and use cropped .tif file (richness_5bov_mix_besttss_tha.tif) in result_sample
###########################
# crop as Thailand extent
# import Thailand polygon
#tha<-shapefile("/Users/whorpien/OneDrive - Massey University/GIS data/AdmWorld/gadm36_THA_shp/gadm36_THA_0.shp")

tha<-shapefile("bovidae_enm/data_preparation/adm_border/gadm36_THA_0.shp")
plot(tha)

rn <- crop(r, extent(tha))%>% 
  mask(tha)

#Save as Geotiff with Thailand extent
setwd("/bovidae_enm/result_sample/")
raster::writeRaster(x = rn, 
                    filename=file.path( "richness_5bov_mix_besttss_tha.tif"), 
                    bylayer = TRUE,
                    overwrite = TRUE)
setwd("/Users/whorpien/OneDrive - Massey University/R/RichnessMap/")
###########################

# import shapefiles:
tha<-shapefile("bovidae_enm/data_preparation/adm_border/gadm36_THA_0.shp")
sacc<-shapefile("/bovidae_enm/data_preparation/adm_border/acc_ecoregion_ssa_disv_cropfinalmodel.shp")
accm<-shapefile("/bovidae_enm/data_preparation/acc/accmsdm/acc_ecoregion_msdm.shp")
asia<-shapefile("/bovidae_enm/data_preparation/adm_border/asiamap3.shp")
world<-shapefile("/bovidae_enm/data_preparation/adm_border/World_Countries__Generalized_.shp")
pa<- shapefile("/bovidae_enm/data_preparation/PA_and_country/AsiaSelect2_largeacc_wdpar_clean.shp")

# focus on PA which species number = 4
pa_path<-"/bovidae_enm/data_preparation/PA_and_country/"

dpky<-st_read(paste0(pa_path,"dpky.shp"))
west<-st_read(paste0(pa_path,"western.shp"))
east<-st_read(paste0(pa_path,"eastern.shp"))
pk<-st_read(paste0(pa_path,"PhukhewNumNao.shp"))

#fix invalid polygon error
tmap_options(check.and.fix = TRUE)


# import Thailand richness map:
rn<-raster("/bovidae_enm/result_sample/richness_5bov_mix_besttss_tha.tif")

rn<-raster("richness_5bov_mix_besttss_tha.tif")

#color
#vv <- max(na.omit(values(rnc)))+1
#cc <- viridisLite::mako (vv,direction =1) 

#***color = mako with red
#cc<-c("#0B0405FF","#395D9CFF", "#3E9BFEFF","#60CEACFF", "#DEF5E5FF","#c03100")

#Spectral with 0 = grey (This one)
cc<-c("grey90","#3288BD","#99D594","#FEE08B","#FC8D59","#D53E4F")

# Creating bbox for framing interesting PA: b1,2,3,4,5,6

#Western
b1 = st_bbox(c(xmin = 98, xmax = 99.55,
               ymin = 14.1, ymax = 16.65),
             crs = st_crs(4326)) %>% 
  st_as_sfc()

#Phu Khieo-Nam Nao
b2 = st_bbox(c(xmin = 101.2, xmax = 102.05,
               ymin = 15.82, ymax = 17.13),
             crs = st_crs(4326)) %>% 
  st_as_sfc()



#Khoa Yai (DPKY)
b3 = st_bbox(c(xmin = 101.1, xmax = 103.26,
               ymin = 13.91, ymax = 14.67),
             crs = st_crs(4326)) %>% 
  st_as_sfc()

#Eastern
b4 = st_bbox(c(xmin = 101.56, xmax = 102.34,
               ymin = 12.65, ymax = 13.55),
             crs = st_crs(4326)) %>% 
  st_as_sfc()

#for the best model
#pk, west, dpky, east (b1-b4)

rnm <- 
  tm_shape(pa,bbox=tha)+
  tm_polygons(col="grey96")+
  
  tm_shape(rn,bbox=tha) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = TRUE,
            title="Species numbers")+
  
  tm_shape(asia,bbox=tha) + tm_borders(col="grey10",lwd=1.2)+
  
  tm_shape(pa,bbox=tha) + tm_borders(col = "grey30", lwd = 1.2)+
  
  tm_shape(west) + tm_borders(lw=2.1, col="navy")+
  tm_shape(pk) + tm_borders(lw=2.1, col="navy")+
  tm_shape(dpky) + tm_borders(lw=2.1, col="navy")+
  tm_shape(east) + tm_borders(lw=2.1, col="navy")+
  
  tm_shape(b1) + tm_borders(lw=1.6, col="navy")+
  tm_shape(b2) + tm_borders(lw=1.6, col="navy")+
  tm_shape(b3) + tm_borders(lw=1.6, col="navy")+
  tm_shape(b4) + tm_borders(lw=1.6, col="navy")+
  
  tm_graticules(labels.size = 1.7,lines = FALSE)+
  
  #add legend
  tm_add_legend(type=c("fill"),col = "grey96",border.col = "grey70",
                labels=c("PA in bordering countries"),
                size=5.5)+
  tm_add_legend(type=c("line"), col = "navy",border.col = "navy",
                labels=c("PA with species = 4"),border.lwd = 1,
                size=7)+
  
  tm_layout (main.title= 'Bovidae species numbers, Thailand',
             main.title.size = 2.2, 
             main.title.position = c(0.1,0.98),
             inner.margins= c(0,0,0,0), 
             outer.margins = c(0.001,0.001,0.001,0.001),
             legend.outside = F,
             legend.title.size=2.5,
             legend.text.size = 1.6,
             legend.position=c(0.68, 0.01),
             legend.bg.color = "white",
             legend.height = -0.24,
             legend.width = -0.29)+
  
  tm_compass(type="arrow",size= 1.5,text.size = 1.5 ,position=c(0.01,0.08))+
  tm_scale_bar(breaks = c(0, 100, 200),position=c(0.01,0.01),text.size = 1.5)

# creating inset map
#ref: https://github.com/Robinlovelace/geocompr/issues/532 
norm_dim <- function(obj) {
  bbox <- st_bbox(obj)
  
  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]
  
  w <- width/max(width, height)
  h <- height/max(width, height)
  
  c("w" = w, "h" = h)
}

# creating the main map and inset map viewports
#large acc bbox
main_w <- norm_dim(rn)[["w"]]
main_h <- norm_dim(rn)[["h"]]

#asia bbox
ins_w <- norm_dim(rn)[["w"]]
ins_h <- norm_dim(rn)[["h"]]

main_vp <- viewport(x = 0.5, y = 0.5,
                    width = unit(main_w, "snpc"), height = unit(main_h, "snpc"),
                    name = "main")

# using "cm" ratio to ensure inset map offsets the main map border by 0.5 cm
# x=apart from left side, y=apart from bottom
ins_vp<- viewport(x=0.78,y=0.32,
                  width = unit(ins_w, "snpc") *0.3, 
                  height = unit(ins_h, "snpc")*0.3)

bthai= bb_poly(tha,projection=4326)

#zoom in map
#import LA richness for viewport:
rnla<-raster("/bovidae_enm/result_sample/richness_5bov_mix_besttss.tif")

inmap_rn <-  
  tm_shape(rnla)+
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F)+
  
  tm_shape(asia)+
  tm_polygons(border.col = "grey5", 
              lwd = 0.2,
              alpha=0, 
              border.alpha = 1)+
  tm_shape(bthai) + tm_borders(lw=2, col="red")+
  
  tm_layout(inner.margins = c(0,0,0,0),outer.margins=c(0.003,0.003,0.003,0.003))

# printing maps
grid.newpage()

#print(richness)
print(rnm, vp = main_vp)
pushViewport(main_vp)
print(inmap_rn, vp = ins_vp)

#tmap save
#the best TSS
tmap_save(rnm,filename="/Users/whorpien/OneDrive - Massey University/R/RichnessMap/Richness_thai.png",
          dpi=600, insets_tm=inmap_rn, insets_vp=ins_vp,
          height=40, width=25, units="cm")


#plot :: zoom in map
pam<-
  tm_shape(asia,bbox=tha)+ 
  tm_borders(col="grey10",lwd=1.5)+
  tm_shape(pa,bbox=tha)+
  tm_borders(col = "grey30",lwd = 1.6)

#set result wd
setwd("/bovidae_enm/fig/")
# Western PA
inmap_b1<-
  tm_shape(pa,bbox=b1)+
  tm_polygons(col="grey96", border.col = "grey30",lwd=1.6)+
  
  tm_shape(rn,bbox=b1) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F)+
  pam+     
  tm_shape(west) + tm_borders(lw=3.5, col="navy")+
  
  tm_layout(main.title="A) Western",
            main.title.size = 2.3, 
            main.title.position = c(0.01,0.98),
            inner.margins= c(0,0,0,0), 
            outer.margins = c(0,0,0,0))

tmap_save(inmap_b1,filename="inmap_rn_Western_a1.png",
          height=20, width=25, units="cm")

#Phu Khieo-Nam Nao
inmap_b2<-
  tm_shape(pa,bbox=b2)+
  tm_polygons(col="grey96")+
  
  tm_shape(rn,bbox=b2) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F)+
  pam+     
  tm_shape(pk) + tm_borders(lw=3.5, col="navy")+

  tm_layout(main.title="B) Phu Khieo-Nam Nao",
            main.title.size = 2.7, 
            main.title.position = c(0.01,0.98),
            inner.margins= c(0,0,0,0), 
            outer.margins = c(0,0,0,0))

tmap_save(inmap_b2,filename="inmap_rn_PhuKhieo_NamNao_b2.png",
          height=25, width=25, units="cm")


#Dong Phayayen-Khao Yai
inmap_b3<-
  tm_shape(pa,bbox=b3)+
  tm_polygons(col="grey96", border.col = "grey30",lwd=1.6)+
  
  tm_shape(rn,bbox=b3) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F)+
  
  pam+
  tm_shape(dpky) + tm_borders(lw=3.5, col="navy")+
  
  #tm_graticules(labels.size = 1,lines = FALSE)+
  tm_layout(main.title="C) Dong Phayayen-Khao Yai",
            main.title.size = 2.5, 
            main.title.position = c(0.01,0.98),
            inner.margins= c(0.005,0.005,0.005,0.005), 
            outer.margins = c(0.005,0.005,0.005,0.005))

tmap_save(inmap_b3,filename="inmap_rn_DPKY_c1.png",
          height=20, width=30, units="cm")

#Eastern PA
inmap_b4<-
  tm_shape(pa,bbox=b4)+
  tm_polygons(col="grey96", border.col = "grey30",lwd=1.6)+
  
  tm_shape(rn,bbox=b4) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F)+
  
  pam+
  tm_shape(east) + tm_borders(lw=3.5, col="navy")+
  
  #tm_graticules(labels.size = 1,lines = FALSE)+
  tm_layout(main.title="D) Eastern",
            main.title.size = 2.5, 
            main.title.position = c(0.01,0.98),
            inner.margins= c(0,0,0,0), 
            outer.margins = c(0.002,0.002,0.002,0.002))

tmap_save(inmap_b4,filename="inmap_rn_east_d1.png",
          height=20, width=20, units="cm")

