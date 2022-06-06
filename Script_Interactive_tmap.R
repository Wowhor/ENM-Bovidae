#========Creating Interactive Map for Ecological Niche Model of five Bovidae species============
#layers:
#1) Asia map 
#2) Binary map 
#3) Ensemble models map,
#4) IUCN distribution map 

#LA == large accessible areas
#SSA == species specific accessible areas

library(sf)
library(raster)
library(dplyr)
library(tmap)    # for static and interactive maps
library(tmaptools)
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(ggrepel)

#import Asia map
asia<-st_read("/Users/whorpien/OneDrive - Massey University/R/1working/asiash/asiamap3.shp")

#import binary map 
path2<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS/"
dir(path2)

gaur_binary_la <- raster(paste0(path2,"bg_wm_msdm_null_bin_nw_crop.tif"))
gaur_binary_sa <- raster(paste0(path2,"bg_wm_spac_null_bin_nw_crop.tif"))

banteng_binary_la <- raster(paste0(path2,"bj_wm_msdm_obr_bin_nw_crop.tif"))
banteng_binary_sa <- raster(paste0(path2,"bj_wm_spac_null_bin_nw.tif"))

buffalo_binary_la <- raster(paste0(path2,"ba_wm_msdm_obr_bin_crop.tif"))
buffalo_binary_sa <- raster(paste0(path2,"ba_wm_spac_obr_bin_crop.tif"))

serow_binary_la <- raster(paste0(path2,"cs_wm_msdm_null_bin_nw_crop.tif"))
serow_binary_sa <- raster(paste0(path2,"cs_wm_spac_null_bin_nw.tif"))

goral_binary_la <- raster(paste0(path2,"ng_wm_msdm_obr_bin_crop.tif"))
goral_binary_sa <- raster(paste0(path2,"ng_wm_spac_obr_bin.tif"))

#import ENM 
path3<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS_ens/"
dir(path3)

gaur_ensemble_la<-raster(paste0(path3,"bg_wm_msdm_null_nw.tif"))
gaur_ensemble_sa<-raster(paste0(path3,"bg_wm_spac_null_nw_crop.tif"))

banteng_ensemble_la<-raster(paste0(path3,"bj_wm_msdm_obr_nw_crop.tif"))
banteng_ensemble_sa<-raster(paste0(path3,"bj_wm_spac_null_nw_crop.tif"))

buffalo_ensemble_la<-raster(paste0(path3,"ba_wm_msdm_obr_crop.tif"))
buffalo_ensemble_sa<-raster(paste0(path3,"ba_wm_spac_obr_crop.tif"))

serow_ensemble_la<-raster(paste0(path3, "cs_wm_msdm_null_nw.tif"))
serow_ensemble_sa<-raster(paste0(path3, "cs_wm_spac_null_nw.tif"))

goral_ensemble_la<-raster(paste0(path3,"ng_wm_msdm_obr.tif"))
goral_ensemble_sa<-raster(paste0(path3,"ng_wm_spac_obr.tif"))


#IUCN map
buffalo_iucn<-st_read("/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/Buffalo/Buffalo_data.shp")
gaur_iucn<-st_read("/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/Gaur/Gaur_data.shp")
banteng_iucn<-st_read("/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/Banteng/Banteng_data.shp")
serow_iucn<-st_read("/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/SerowMainland_newAssess/data_0.shp")
goral_iucn<-st_read("/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/Goral/Goral_data.shp")

#---tmap view plot-----
#save as .html == interative map

#set where to save the plots
setwd("/Users/whorpien/OneDrive - Massey University/R/tmaps")
setwd("/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/enmbovid_manuscript")
#pallete color
#tmaptools::palette_explorer()

#mapping
tmap_mode("view")

#xlim = c(68, 111.5), ylim = c(-6.5,37)
b1= st_bbox(c(xmin = 63, xmax = 122.5,
              ymin = -9.5, ymax = 45),
            crs = st_crs(4326)) %>% 
  st_as_sfc()

#Buffalo LA  -------
buffalo_view_la <-
  tm_shape(asia, bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(buffalo_binary_la, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(buffalo_ensemble_la, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(buffalo_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)


print(buffalo_view_la)

tmap_save(buffalo_view_la,"buffalo_LA_view.html")

#Buffalo SSA -------
buffalo_view_sa <-
  tm_shape(asia, bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(buffalo_binary_sa, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(buffalo_ensemble_sa, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(buffalo_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(buffalo_view_sa)

tmap_save(buffalo_view_sa,"buffalo_SSA_view.html")

#Gaur LA -------
gaur_view_la<-  
  tm_shape(asia,bbox = asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(gaur_binary_la, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(gaur_ensemble_la, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(gaur_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(gaur_view_la)

tmap_save(gaur_view_la,"gaur_LA_view.html")

#banteng
#habitat suitability
banteng_view<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(banteng_binary, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(banteng_ensemble, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(banteng_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(banteng_view)

tmap_save(banteng_view,"banteng_view.html")
#serow
#habitat suitability
serow_view<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(serow_binary, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(serow_ensemble, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(serow_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)


print(serow_view)

tmap_save(serow_view,"serow_view.html")

#goral
#habitat suitability
goral_view  <-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(goral_binary, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(goral_ensemble, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(goral_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(goral_view)

tmap_save(goral_view,"goral_view.html")


# Gaue SSA -------
gaur_view_sa<-  
  tm_shape(asia,bbox = asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(gaur_binary_sa, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(gaur_ensemble_sa, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(gaur_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(gaur_view_sa)

tmap_save(gaur_view_sa,"gaur_SSA_view.html")

#Banteng LA---------
banteng_view_la<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(banteng_binary_la, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(banteng_ensemble_la, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(banteng_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(banteng_view_la)

tmap_save(banteng_view_la,"banteng_LA_view.html")


#Banteng SSA---------
banteng_view_sa<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(banteng_binary_sa, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(banteng_ensemble_sa, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(banteng_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(banteng_view_sa)

tmap_save(banteng_view_sa,"banteng_SSA_view.html")

#serow SSA ----------

serow_view_sa<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(serow_binary_sa, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(serow_ensemble_sa, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(serow_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)


print(serow_view_sa)

tmap_save(serow_view_sa,"serow_SSA_view.html")

#serow LA ----------
serow_view_la<-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(serow_binary_la, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(serow_ensemble_la, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(serow_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)


print(serow_view_la)

tmap_save(serow_view_la,"serow_LA_view.html")

#goral LA ----------
goral_view_la  <-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(goral_binary_la, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(goral_ensemble_la, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(goral_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(goral_view_la)

tmap_save(goral_view_la,"goral_LA_view.html")

#goral SSA ----------
goral_view_sa  <-
  tm_shape(asia,bbox=asia)+
  tm_borders(col="grey70",lwd=0.2)+
  tm_text("COUNTRY", size = 0.7,root=3)+
  
  tm_shape(goral_binary_sa, bbox = asia) +
  tm_raster(style = "cat", alpha=0.8,
            palette = c("#d4d4d4","mediumblue"),
            labels=c("unsuitable","suitable"),
            legend.show = TRUE,
            title="Binary")+
  
  tm_shape(goral_ensemble_sa, bbox = asia) +
  tm_raster(style = "cont", alpha=0.9,
            palette = "YlOrBr", 
            legend.show = TRUE,
            title="Suitability")+
  
  tm_shape(goral_iucn)+ 
  tm_fill(col = "bisque3",alpha= 0.8 )+#1a6a1a
  tm_add_legend(type=c("fill"),
                col = "bisque3",
                labels=c("IUCN map"),
                size=1,alpha = 0.7)

print(goral_view_sa)

tmap_save(goral_view_sa,"goral_SSA_view.html")
