############# Plotting figures ####################
# Mapping wild bovid in Thailand 

# Code for plotting the figures:
# 1) Study area (1. large accessibe area (LA), 2. species-specific study areas (SSA))
# 2) Habitat suitability maps, 
# 3) Binary habitat suitability map,  
# 4) Richness map, Thailand
# 5) Suitable area for each country
# 6) Density plot (Suitability and IUCN protected area category)
# 7) PCA biplot

# load packages
library(sf)
library(raster)
library(tidyverse)
library(tmap)    # for static and interactive maps
library(tmaptools) 
library(ggplot2) # tidyverse data visualization package
library(grid) #for insert map
library(gridExtra)
library(purrr)
library(FactoMineR)
library(cowplot)

options(digits=7, scipen= 999)
rm(list = ls())

tmap_mode("plot")

# Set working directory
path <- "/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/GitHub/bovidae_enm"
setwd(path)
########1) Study area ###########
# import files

# IUCN map path

iucn <- "./data_preparation/iucn/"

bufiucn <- st_read(paste0(iucn,"Buffalo.shp"))
gauriucn <- st_read(paste0(iucn,"Gaur.shp"))
bantengiucn <- st_read(paste0(iucn,"Banteng.shp"))
serowiucn <- st_read(paste0(iucn,"Serow.shp"))
goraliucn <- st_read(paste0(iucn,"Goral.shp"))

# occurrence data path (sample ocurrence data from gbif.org)
poi <- "./data_preparation/"

# import occurrence data gaur, banteng, buffalo, serow, goral
gaurp <- read.table(paste0(poi,"bg_test.txt"), header = T, sep = "\t") %>%  st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
bantengp <- read.table(paste0(poi,"bj_test.txt"), header = T, sep ="\t") %>% st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
bufp <- read.table(paste0(poi,"ba_test.txt"), header = T, sep = "\t") %>% st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
serowp <- read.table(paste0(poi,"cs_test.txt"), header = T, sep = "\t") %>% st_as_sf(coords = c('x', 'y'), crs = 4326)
goralp<- read.table(paste0(poi,"ng_test.txt"), header = T, sep = "\t") %>% st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")

#> large accessible area ----------
#background border

accm <- st_read(paste0(poi,"acc/acc_la/acc_ecoregion_msdm.shp"))
asia <- st_read(paste0(poi,"/adm_border/asiamap3.shp"))
world <- st_read(paste0(poi,"/adm_border/World_Countries__Generalized_.shp"))

# bbox
b1 = st_bbox(accm, crs = st_crs(4326))

# plot :: large accessible area 
la <-tm_shape(world, bbox = b1) +
  tm_fill(col = "grey90") +
  
  tm_shape(accm, bbox = b1) +
  tm_fill(col = "grey65") +
  tm_shape(world, bbox = b1) +
  tm_borders(col = "#474747", lwd = 0.2) +
  
  #degree
  tm_graticules(lines = FALSE) + 
  
  #country
  #tm_text("COUNTRY", size = 0.7,root=3) + 
  
  #add legend
  tm_add_legend(type = c("fill"),
                col = "grey60",
                labels = c("Large accessible area"),
                size = 0.6, alpha = 1) +
  #map layout
  tm_layout(main.title = "Large accessible area",
            main.title.size = 1.2,
            main.title.position = c(0.15, 0.99),
            legend.text.size = 1.1,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            #legend.bg.color = "white",
            #legend.frame = T,
            legend.width = 0.4,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) + 
  
  tm_compass(type = "arrow", size = 0.8, text.size = 1, position = c(0.02, 0.17)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position = c(0.02, 0.07), text.size = 1)

la

#plot :: inset map
# asia background: b2
b2 = st_bbox(c(xmin = -16, xmax = 180,
               ymin = -40, ymax = 77),
             crs = st_crs(4326)) %>% 
  st_as_sfc()
b2

#red frame focus large acc.area
bla = bb_poly(accm, projection = 4326)

inmap_la <- tm_shape(world, bbox = b2) +
  tm_fill(col = "grey80") +
  tm_shape(bla) + tm_borders(lw = 0.5, col = "red") +
  tm_shape(accm) + tm_fill(col = "grey65") +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0.01, 0.01, 0.01, 0.01))
#inmap_la

#ref: https://github.com/Robinlovelace/geocompr/issues/532 
norm_dim <- function(obj) {
  bbox <- st_bbox(obj)
  
  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]
  
  w <- width/max(width, height)
  h <- height/max(width, height)
  
  c("w" = w, "h" = h)
}

# creating the main map and inset map viewports ---------------------------
#large acc bbox
main_w <- norm_dim(b1)[["w"]]
main_h <- norm_dim(b1)[["h"]]

#asia bbox
ins_w <- norm_dim(b2)[["w"]]
ins_h <- norm_dim(b2)[["h"]]

main_vp_la <- viewport(x = 0.5, y = 0.5,
                       width = unit(main_w, "snpc"), height = unit(main_h, "snpc"),
                       name = "main")

# using "cm" ratio to ensure inset map offsets the main map border by 0.5 cm
#top left:: x=0.18 (apart from left side), y=0.92 (apart from bottom)
ins_vp_la <- viewport(x = 0.15, y = 0.865,
                      width = unit(ins_w, "snpc") *0.18, 
                      height = unit(ins_h, "snpc")*0.18)

#inmap_vp<-viewport(x=0.4,y=0.12, width = 0.3, height = 0.1)

# printing maps la ------------------------------
grid.newpage()

#print(la)
print(la, vp = main_vp_la)
pushViewport(main_vp_la)
print(inmap_la, vp = ins_vp_la)

# Save large accessible area
tmap_save(la, filename = "studyarea_la.png",
          dpi = 300, insets_tm = inmap_la, insets_vp = ins_vp_la,
          height = 20, width = 20, units = "cm")

# study area for SSA
#extent of merge species accessible area   

sacc <- st_read(paste0(poi, "/adm_border/acc_ecoregion_ssa_disv_cropfinalmodel.shp"))

#check sacc
tm_shape(sacc, bbox = sacc) + tm_polygons()

#> gaur SSA ---------------------

#import gaur ssa
gaur_acc <- st_read(paste0(poi, "/acc/accgaur/gaur_acc_disv_2.shp"))

gaur <-  tm_shape(world,bbox = sacc) +
  tm_fill(col = "grey95") +
  
  tm_shape(gaur_acc) + 
  tm_fill(col = "grey65") +
  
  tm_shape(gauriucn) + 
  tm_fill(col = "#0095cb",alpha = 0.6) +
  
  tm_shape(asia,bbox = b1) +
  tm_borders(col = "#474747",lwd = 0.2) +
  
  tm_shape(gaurp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd = 0.15) +
  #degree
  tm_graticules(lines = FALSE) + 
  tm_add_legend(type = c("symbol"),
                col = "#ffbf3f", 
                labels = c(expression(italic("B. gaurus"))),
                size = 0.5) +
  
  tm_add_legend(type = c("fill"),
                col = "#0095cb",
                labels = c(expression(paste(italic("B. gaurus "), "IUCN map"))),
                size = 0.3, alpha = 0.6) +
  
  tm_add_legend(type = c("fill"),
                col = "grey65",
                labels = c(expression(paste(italic("B. gaurus "), "Acessible area"))),
                size = 0.3, alpha = 0.8)+
  
  tm_layout(main.title = "B. gaurus",
            main.title.fontface = "italic",
            main.title.size = 1.2,
            main.title.position = c(0.1, 0.99),
            legend.text.size = 0.95,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            legend.bg.color = "white",
            legend.frame = F,
            legend.height = 0.22,
            legend.width = 0.6,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.02, 0.3)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position=c(0.02, 0.2, 2),text.size = 0.8)

gaur 

#red frame focus large acc.area
bgaur = bb_poly(gaur_acc,projection = 4326)

inmap_gaur <- tm_shape(world, bbox = b2)+
  tm_fill(col = "grey85")+
  tm_shape(bgaur) + tm_borders(lw = 0.5, col = "red")+
  tm_shape(gaur_acc) + tm_fill(col = "grey65")+
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins=c(0.01, 0.01, 0.01, 0.01))
inmap_gaur

ins_vp_gaur <- viewport(x = 0.5,y = 0.14,
                        width = unit(ins_w, "snpc") *0.2, 
                        height = unit(ins_h, "snpc")*0.2)

#> gaur printing maps -------------------------------
grid.newpage()
print(gaur)

print(inmap_gaur, vp = ins_vp_gaur)

tmap_save(gaur,filename="gaur.png",
          dpi = 300, insets_tm = inmap_gaur, insets_vp = ins_vp_gaur,
          height = 20, width = 20, units = "cm")

#> banteng SSA -------------------------------

#import banteng acc ssa
banteng_acc <- st_read(paste0(poi,"acc/accbanteng/banteng_acc_disv_2.shp"))

banteng <-  tm_shape(world, bbox = sacc) +
  tm_fill(col = "grey95") +
  
  tm_shape(banteng_acc) + 
  tm_fill(col = "grey65") +
  
  tm_shape(bantengiucn) + 
  tm_fill(col = "#0095cb", alpha = 0.6) +
  
  tm_shape(asia, bbox = b1) +
  tm_borders(col = "#474747", lwd = 0.2) +
  
  tm_shape(bantengp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd = 0.15) +
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type = c("symbol"),
                col = "#ffbf3f", 
                labels = c(expression(italic("B. javanicus"))),
                size = 0.5) +
  
  tm_add_legend(type = c("fill"),
                col = "#0095cb",
                labels = c(expression(paste(italic("B. javanicus "), "IUCN map"))),
                size = 0.3, alpha = 0.6) +
  
  tm_add_legend(type = c("fill"),
                col = "grey65",
                labels = c(expression(paste(italic("B. javanicus "), "Accessible area"))),
                size = 0.3, alpha = 0.8) +
  
  tm_layout(main.title = "B. javanicus",
            main.title.fontface = "italic",
            main.title.size = 1.2,
            main.title.position = c(0.1, 0.99),
            legend.text.size = 0.95,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            legend.bg.color = "white",
            legend.frame = F,
            legend.height = 0.22,
            legend.width = 0.6,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.02, 0.3)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position=c(0.02, 0.2, 2),text.size = 0.8)

banteng

#red frame focus large acc.area
bbanteng = bb_poly(banteng_acc,projection = 4326)

inmap_banteng <- tm_shape(world, bbox = b2) +
  tm_fill(col = "grey85") +
  tm_shape(bbanteng) + tm_borders(lw = 0.5, col = "red") +
  tm_shape(banteng_acc) + tm_fill(col = "grey65") +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0.01, 0.01, 0.01, 0.01))
inmap_banteng

ins_vp_banteng <- viewport(x = 0.5, y = 0.14,
                           width = unit(ins_w, "snpc") *0.2, 
                           height = unit(ins_h, "snpc")* 0.2) 

#> banteng printing maps -----------------------------
print(banteng)

print(inmap_banteng, vp = ins_vp_banteng)

tmap_save(banteng, filename = "banteng.png",
          dpi = 300, insets_tm = inmap_banteng, insets_vp = ins_vp_banteng,
          height = 20, width = 20, units = "cm")


#> buffalo SSA --------------------------------

# import buffalo SSA
buffalo_acc <- st_read(paste0(poi, "/acc/accbuffalo/buffalo_acc_disv.shp"))

buf <-  tm_shape(world,bbox = sacc) +
  tm_fill(col = "grey95") +
  
  tm_shape(buffalo_acc) + 
  tm_fill(col = "grey65") +
  
  tm_shape(bufiucn) + 
  tm_fill(col = "#0095cb", alpha = 0.6) +
  
  tm_shape(asia, bbox = b1) +
  tm_borders(col = "#474747", lwd = 0.2) +
  
  tm_shape(bufp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd = 0.15) +
  #degree
  tm_graticules(lines = FALSE) + 
  tm_add_legend(type = c("symbol"),
                col = "#ffbf3f", 
                labels = c(expression(italic("B. arnee "))),
                size = 0.5) +
  
  tm_add_legend(type = c("fill"),
                col = "#0095cb",
                labels = c(expression(paste(italic("B. arnee "), "IUCN map"))),
                size = 0.3, alpha = 0.6) +
  
  tm_add_legend(type = c("fill"),
                col = "grey65",
                labels = c(expression(paste(italic("B. arnee "), "accessible area"))),
                size=0.3, alpha = 0.8)+
  
  tm_layout(main.title = "B. arnee",
            main.title.fontface = "italic",
            main.title.size = 1.2,
            main.title.position = c(0.1, 0.99),
            legend.text.size = 0.95,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            legend.bg.color = "white",
            legend.frame = F,
            legend.height = 0.22,
            legend.width = 0.6,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.02, 0.3)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position=c(0.02, 0.2, 2),text.size = 0.8)

buf

#red frame focus large acc.area
bbuf = bb_poly(buffalo_acc,projection = 4326)

inmap_buf <- tm_shape(world, bbox = b2) +
  tm_fill(col = "grey85") +
  tm_shape(bbuf) + tm_borders(lw = 0.5, col = "red") +
  tm_shape(buffalo_acc) + tm_fill(col = "grey65") +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0.01, 0.01, 0.01, 0.01))
inmap_buf

ins_vp_buf <- viewport(x = 0.5, y = 0.14,
                       width = unit(ins_w, "snpc") *0.2, 
                       height = unit(ins_h, "snpc")* 0.2)

#> buffalo printing maps ---------------------------
grid.newpage()
print(buf)

print(inmap_buf, vp = ins_vp_buf)

tmap_save(buf, filename = "buffalo.png",
          dpi = 300, insets_tm = inmap_buf, insets_vp = ins_vp_buf,
          height = 20, width = 20, units ="cm")

#>  serow SSA -------------------------

#import serow SSA
serow_acc <- st_read(paste0(poi, "acc/accserow/serowthar_acc_disv.shp"))

serow <-  tm_shape(world,bbox = sacc)+
  tm_fill(col = "grey95") +
  
  tm_shape(serow_acc) + 
  tm_fill(col = "grey65") +
  
  tm_shape(serowiucn) + 
  tm_fill(col = "#0095cb", alpha=0.6) +
  
  tm_shape(asia,bbox = b1) +
  tm_borders(col = "#474747", lwd = 0.2) +
  
  tm_shape(serowp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd = 0.15) +
  
  #degree
  tm_graticules(lines = FALSE) + 
  tm_add_legend(type = c("symbol"),
                col = "#ffbf3f", 
                labels = c(expression(italic("C. sumatraensis "))),
                size = 0.8) +
  
  tm_add_legend(type = c("fill"),
                col = "#0095cb",
                labels = c(expression(paste(italic("C. sumatraensis "), "IUCN map"))),
                size = 0.2, alpha = 0.8) +
  
  tm_add_legend(type = c("fill"),
                col = "grey65",
                labels = c(expression(paste(italic("C. sumatraensis "), "accessible area"))),
                size = 0.2, alpha = 0.8)+
  
  tm_layout(main.title = "C. sumatraensis",
            main.title.fontface = "italic",
            main.title.size = 1.2,
            main.title.position = c(0.1, 0.99),
            legend.text.size = 0.95,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            legend.bg.color = "white",
            legend.frame = F,
            legend.height = 0.22,
            legend.width = 0.6,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.02, 0.3)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position=c(0.02, 0.2, 2),text.size = 0.8)

serow  

#red frame focus large acc.area
bserow = bb_poly(serow_acc, projection = 4326)

inmap_serow <- tm_shape(world, bbox = b2) +
  tm_fill(col = "grey85") +
  tm_shape(bserow) + tm_borders(lw = 0.5, col = "red") +
  tm_shape(serow_acc) + tm_fill(col = "grey65") +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0.01, 0.01, 0.01, 0.01))
inmap_serow

ins_vp_serow <- viewport(x = 0.5, y = 0.14,
                         width = unit(ins_w, "snpc")*0.2, 
                         height = unit(ins_h, "snpc")*0.2)
#> serow printing maps ----------------------------
grid.newpage()
print(serow)

print(inmap_serow, vp = ins_vp_serow)

tmap_save(serow,filename = "serow.png",
          dpi = 300, insets_tm = inmap_serow, insets_vp = ins_vp_serow,
          height = 20, width = 20, units = "cm")

#>  goral SSA ---------------------------

#import goral SSA
goral_acc <- st_read(paste0(poi, "acc/accgoral/goral_acc_disv.shp"))

#plot
goral<-  tm_shape(world, bbox = sacc) +
  tm_fill(col = "grey95") +
  
  tm_shape(goral_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(goraliucn) + 
  tm_fill(col = "#0095cb",alpha = 0.6)+
  
  tm_shape(asia, bbox = b1) +
  tm_borders(col = "#474747",lwd = 0.2) +
  
  tm_shape(goralp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd = 0.15) +
  
  #degree
  tm_graticules(lines = FALSE)+ 
  
  tm_add_legend(type = c("symbol"),
                col = "#ffbf3f", 
                labels = c(expression(italic("N. griseus"))),
                size=0.8) +
  
  tm_add_legend(type = c("fill"),
                col = "#0095cb",
                labels = c(expression(paste(italic("N. griseus "), "IUCN map"))),
                #labels = c("N. griseus IUCN map"),
                size = 0.2, alpha = 0.6)+
  tm_add_legend(type = c("fill"),
                col = "grey65",
                labels = c(expression(paste(italic("N. griseus "), "accessible area"))),
                size = 0.2, alpha = 0.8)+
  
  tm_layout(main.title = "N. griseus",
            main.title.fontface = "italic",
            main.title.size = 1.2,
            main.title.position = c(0.1, 0.99),
            legend.text.size = 0.95,
            legend.position = c(0.01, 0.01),
            legend.just = c("left","bottom"),
            legend.bg.color = "white",
            legend.frame = F,
            legend.height = 0.22,
            legend.width = 0.6,
            legend.outside = F,
            inner.margins = c(0, 0, 0, 0), outer.margins = c(0.005, 0.005, 0.005, 0.005)) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.02, 0.3)) +
  tm_scale_bar(breaks = c(0, 500, 1000), position=c(0.02, 0.2, 2),text.size = 0.8)

goral  

#red frame focus large acc.area
bgoral = bb_poly(goral_acc,projection = 4326)

inmap_goral <- tm_shape(world, bbox = b2) +
  tm_fill(col = "grey85") +
  tm_shape(bgoral) + tm_borders(lw = 0.5, col = "red") +
  tm_shape(goral_acc) + tm_fill(col = "grey65") +
  tm_layout(inner.margins = c(0.01, 0.01, 0.01, 0.01), outer.margins = c(0.01, 0.01, 0.01, 0.01))
inmap_goral

ins_vp_goral <- viewport(x = 0.5, y = 0.14,
                         width = unit(ins_w, "snpc") *0.2, 
                         height = unit(ins_h, "snpc")*0.2)

#> goral printing maps -----------------------------
grid.newpage()
print(goral)
print(inmap_goral, vp = ins_vp_goral)

tmap_save(goral, filename ="goral.png",
          dpi = 300, insets_tm = inmap_goral, insets_vp = ins_vp_goral,
          height = 20, width = 20, units = "cm")


## > combine study area -----------------------------------------------------------
# use cowplot

la_grob <- tmap_grob(la)
inm_la_grob <- tmap_grob(inmap_la)

gaur_grob <- tmap_grob(gaur)
banteng_grob <- tmap_grob(banteng)
buf_grob <- tmap_grob(buf)
serow_grob <- tmap_grob(serow)
goral_grob <- tmap_grob(goral)

# x, y can be adjusted to fit the map scale
la_com <- ggdraw() +
  draw_plot(la_grob) +
  draw_plot(inm_la_grob,
            width = 0.13, height = 0.13,
            x = 0.13, y = 0.79)


# the scale may be slightly move, adjust the position of the maps manually

png(filename = "Fig1_tot_studyarea.png", res = 300, width = 46, height = 28, units = "cm")

plot_grid(la_com, gaur_grob, banteng_grob, buf_grob, serow_grob, goral_grob,
          labels = c("A)", "B)", "C)","D)","E)","F)"), nrow = 2, ncol = 3)
dev.off()

############## 2) Habitat suitability maps##########

# Set working directory
#path <- "./GitHub/bovidae_enm"
setwd(path)

path1 <- "./result_sample/ensembles/" 

#> import the best TSS emdemble models ------------
gaur_ens <- raster(paste0(path1, "bg_wm_la_null.tif"))
banteng_ens <- raster(paste0(path1,"bj_wm_la_obr.tif"))
buf_ens <- raster(paste0(path1,"ba_wm_ssa_obr.tif"))
serow_ens <- raster(paste0(path1,"cs_wm_ssa_null.tif"))
goral_ens <- raster(paste0(path1,"ng_wm_ssa_obr.tif"))

# projectraster to the large study areas, so when we plot the maps will have the same extent
serow_ens <- projectRaster(serow_ens, gaur_ens, method = 'ngb')
buf_ens <- projectRaster(buf_ens, gaur_ens, method = 'ngb')
goral_ens <- projectRaster(goral_ens, gaur_ens, method = 'ngb')

# background boundary
poi <- "./data_preparation/"

accm <- st_read(paste0(poi,"acc/acc_la/acc_ecoregion_msdm.shp"))
asia <- st_read(paste0(poi,"/adm_border/asiamap3.shp"))
world <- st_read(paste0(poi,"/adm_border/World_Countries__Generalized_.shp"))

#arrange list in order select = BEST TSS model
ens <- raster::stack(gaur_ens, #gaur
                     banteng_ens, #banteng
                     buf_ens, #buffalo
                     serow_ens, #serow  
                     goral_ens) #goral

#name list
nam2 <- c("B. gaurus","B. javanicus", "B. arnee","C. sumatraensis","N. griseus")

sp = 1:5
m = list()
set.seed(111)

#> create suitability maps list m[i] ----------
for (i in sp) {
  
   names(ens)[i]
  
   m[[i]] <-
    tm_shape(world, bbox = accm) +
    tm_fill(col = "white") +
    
    tm_shape(ens[[i]], bbox = accm) +
    tm_raster(style = "cont", 
              palette = c("YlOrBr"),
              legend.show = TRUE,
              title = "Habitat suitability") +
    
    tm_grid(labels.inside.frame = FALSE, labels.size = 1,labels.cardinal = T,
            col = "black", 
            #n.x = 4, n.y = 4, 
            lines = FALSE,
            labels.rot = c(0, 90)) + 
    #tm_grid(labels.size = 1, lines = FALSE, labels.cardinal = T)+
    
    tm_shape(asia, bbox = accm) +
    tm_polygons(border.col = "black", alpha = 0, border.alpha = 0.5) +  
    
     #degree
     tm_grid(labels.size = 1, lines = FALSE, labels.cardinal = T) +
     
     tm_layout (main.title = nam2[[i]],
               main.title.fontface = "italic",
               main.title.size = 1.2, 
               main.title.position = c(0.1, 0.98),
               inner.margins = c(0.01, 0.01, 0.01, 0.01), 
               outer.margins = c(0.01, 0.01, 0.01, 0.01),
               legend.outside = F,
               legend.title.size = 2.4,
               legend.text.size = 2.4,
               legend.position = c(0.02, 0.01),
               legend.bg.color = "white",
               legend.frame = F,
               legend.height = 0.24,
               legend.width = 0.4) +
    
    tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.29, 0.09)) +
    tm_scale_bar(breaks = c(0, 500, 1000), position = c(0.29, 0.01), text.size = 1.2)
 }


#> Export individual map ----------

getwd() #check result folder before export


for (i in sp) {
  png(filename = paste0(nam[[i]], "_best.png"), 
      res = 300, units = "cm", width = 25, height = 20)
  print(m[[i]])
  dev.off()
  }

# Create richness map figure for inserting with suitability map -------
# Import richness map
# rnn<-raster("/Users/whorpien/OneDrive - Massey University/R/RichnessMap/richness_5bov_mix_besttss.tif")
rnn <- raster("result_sample/richness_5bov_mix_besttss.tif")

vv <- max(na.omit(values(rnn))) + 1

#color code "-Spectral"
cc <- c("grey90", "#3288BD", "#99D594", "#FEE08B", "#FC8D59", "#D53E4F")

rn_map <-
  tm_shape(world, bbox = accm) +
  tm_fill(col = "white") +
  
  tm_shape(rnn, bbox = accm) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = TRUE,
            title = "Species richness") +
  
  tm_grid(labels.inside.frame = FALSE, labels.size = 1, labels.cardinal = T,
          col = "black", 
          #n.x = 4, n.y = 4, 
          lines = FALSE,
          labels.rot = c(0, 90)) + 
  #tm_grid(labels.size=1,lines = FALSE,labels.cardinal=T) +
  
  tm_shape(asia, bbox = accm) +
  tm_polygons(border.col = "black", alpha = 0, border.alpha = 0.5) +  
  
  tm_layout (main.title = 'Species richness',
             main.title.position = c(0.1, 0.98),
             main.title.size = 1.3, 
             inner.margins = c(0.01, 0.01, 0.01, 0.01), 
             outer.margins = c(0.01, 0.01, 0.01, 0.01),
             legend.outside = F,
             legend.title.size = 2.3,
             legend.text.size = 2.3,
             legend.position = c(0.02, 0.01),
             legend.bg.color = "white",
             legend.frame = F,
             legend.height = 0.24,
             legend.width = 0.4) +
  
  tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.29, 0.09))+
  tm_scale_bar(breaks = c(0, 500, 1000), position = c(0.29, 0.01), text.size = 1.5)

# export richness map
#png(filename = "richness_bestTSS_test.png", 
    #res=300, units = "cm", width= 25, height =20)
print(rn_map)
#dev.off()

# Making grob
rn_grb <- tmap_grob(rn_map)

# add richness into m[i] list
mm<-c(m, m6 = list(rn_map))

grob <- map(mm,tmap_grob)

# number of plot
num_plots<-6

## > combine habitat suitability figure using cowplot ------
# adjust the label
# vjust (vertical) More positive  = down 
# hjust (horizontal) More negative = right 
# label_x = adjust small number = left, larger = right
p <- cowplot::plot_grid(plotlist=grob,
             rel_heights = c(-1.01,-1.01,-1.01,-1),
             labels= paste0(c(LETTERS[1:num_plots]),")"),
             nrow=2,ncol=3,
             label_x = 0.06,label_y = 1,
             hjust = 0.08, vjust = 2.2)

# export figure
# res can be changed for a better quality
png(filename = "tot_map_bestTSS.png", 
    res = 300, units = "cm", width= 48, height = 28)
print(p)
dev.off()

############## 3) Binary habitat suitability map ########### 

# setwd 
path2 <-"./result_sample/binary/"

#setwd(path)
dir()

## > import the best binary models -------------
gaur_bi <- raster(paste0(path2, "bg_la_null_bin.tif"))
bj_bi <- raster(paste0(path2, "bj_la_obr_bin.tif"))
buf_bi <- raster(paste0(path2, "ba_ssa_obr_bin.tif"))
serow_bi <- raster(paste0(path2, "cs_ssa_null_bin.tif"))
goral_bi <- raster(paste0(path2, "ng_ssa_obr_bin.tif"))

#projectraster to the large study areas, so when we plot the maps will have the same extent
buf_bi <- projectRaster(buf_bi, gaur_bi,method = 'ngb')
serow_bi <- projectRaster(serow_bi, gaur_bi,method = 'ngb')
goral_bi <- projectRaster(goral_bi, gaur_bi,method = 'ngb')

#arrange list in order select = BEST TSS model
bin<-raster::stack(gaur_bi, #gaur
                   bj_bi, #banteng
                   buf_bi, #buffalo
                   serow_bi, #serow  
                   goral_bi) #goral
bin

sp = 1:5
m = list()
set.seed(111)

## > create binary maps list m[i] ----------
for (i in sp) {
  
  names(bin)[i]
   
  m[[i]] <-
    tm_shape(world, bbox = accm)+
    tm_fill(col = "white")+
    
    tm_shape(bin[[i]],bbox = accm) +
    tm_raster(style = "cat", 
              palette = c("#d4d4d4","mediumblue"),
              labels=c("unsuitable","suitable"),
              legend.show = TRUE,
              title="Suitability")+
    
    tm_grid(labels.inside.frame = FALSE, labels.size = 1,labels.cardinal=T,
            col = "black", 
            #n.x = 4, n.y = 4, 
            lines = FALSE,
            labels.rot = c(0, 90)) + 
    #tm_grid(labels.size=1,lines = FALSE,labels.cardinal=T)+
    
    tm_shape(asia, bbox = accm) +
    tm_polygons(border.col = "black", alpha = 0, border.alpha = 0.5)+  
    
    tm_layout (main.title = nam2[[i]],
               main.title.fontface = "italic",
               main.title.size = 1.2, 
               main.title.position = c(0.1,0.98),
               inner.margins = c(0.01,0.01,0.01,0.01), 
               outer.margins = c(0.01,0.01,0.01,0.01),
               legend.outside = F,
               legend.title.size = 1.3,
               legend.text.size = 1.4,
               legend.position = c(0.02, 0.01),
               legend.bg.color = "white",
               legend.frame = F,
               legend.height = 0.18,
               legend.width = 0.25) +
    
    tm_compass(type = "arrow", size = 1, text.size = 0.8, position = c(0.29, 0.09))+
    tm_scale_bar(breaks = c(0, 500, 1000), position = c(0.29, 0.01), text.size = 1)
 }

## > export individual map ---------
setwd(fig)

for (i in sp) {
png(filename = paste0(nam[[i]],"_bin_best.png"), 
    res=300, units = "cm", width= 25, height =20)
print(m[[i]])
dev.off()
}

grob <- map(m,tmap_grob)

num_plots <- 5

## > combine binary map figure using cowplot ------
#adjust the label
#vjust (vertical) More positive  = down 
#hjust (horizontal) More negative = right 
#label_x = adjust small number = left, larger = right

p <- cowplot::plot_grid(plotlist = grob,
             rel_heights = c(-1.01,-1.01,-1.01,-1),
             #rel_widths = c(1),
             labels = paste0(c(LETTERS[1:num_plots]),")"),
             nrow = 2, ncol = 3,
             label_x = 0.06,label_y = 1,
             hjust = 0.1,vjust = 2.2)

#export figure
png(filename = "tot_map_bestTSS_bin.png", 
    res = 300, units = "cm", width = 45, height = 28)
print(p)
dev.off()

############## 4) Thailand richness map ###########
# Use cropped .tif file (richness_3bov_best_tha.tif) in result_sample
###########################

# import richness map
rn <- raster("./result_sample/richness_3bov_best_tha.tif")
  
###########################

# import shapefiles:
tha <- st_read("./data_preparation/adm_border/gadm36_THA_0.shp")
sacc <- st_read("./data_preparation/adm_border/acc_ecoregion_ssa_disv_cropfinalmodel.shp")
accm <- st_read("./data_preparation/acc/acc_la/acc_ecoregion_msdm.shp")
asia <- st_read("./data_preparation/adm_border/asiamap3.shp")
world <- st_read("./data_preparation/adm_border/World_Countries__Generalized_.shp")
pa <- st_read("./data_preparation/PA_and_country/wdpa/AsiaSelect2_largeacc_wdpar_clean.shp")

# focus on 3 main PAs for gaur, banteng and wild water buffalo [western, eastern and Dong 
pa_path <-"./data_preparation/PA_and_country/wdpa/"

dpky <- st_read(paste0(pa_path, "dpky.shp")) # Dong Phayayen-Khao Yai Forest Complex
west <- st_read(paste0(pa_path, "western.shp")) # Western Forest Complex
east <- st_read(paste0(pa_path, "eastern.shp")) # Eastern Forest Complex
#pk<-st_read(paste0(pa_path,"PhukhewNumNao.shp")) Phu Khieo WS, Nam Nao NP

#fix invalid polygon error
tmap_options(check.and.fix = TRUE)

plot(rn)
# Color options
# color=viridis
# cc <- viridisLite::mako (vv,direction =1) 

# color=mako with red
# cc<-c("#0B0405FF","#395D9CFF", "#3E9BFEFF","#60CEACFF", "#DEF5E5FF","#c03100")

# cc <- c("-Spectral")
# cc<-c("grey90","#3288BD","#99D594","#FEE08B","#FC8D59","#D53E4F")
cc <- c("grey95", "#3288BD", "#FEE08B", "#FC8D59", "#D53E4F") #* This one

# Creating bbox for framing interesting PA: b1,2,3,4
#Western
b1 = st_bbox(c(xmin = 98, xmax = 99.55,
               ymin = 14.1, ymax = 16.65),
             crs = st_crs(4326)) %>% 
  st_as_sfc()


#Khoa Yai (DPKY)
b2 = st_bbox(c(xmin = 101.1, xmax = 103.26,
               ymin = 13.91, ymax = 14.67),
             crs = st_crs(4326)) %>% 
  st_as_sfc()

#Eastern
b3 = st_bbox(c(xmin = 101.56, xmax = 102.34,
               ymin = 12.65, ymax = 13.55),
             crs = st_crs(4326)) %>% 
  st_as_sfc()


#Creating richness map
#with border for 3 PAs: west, dpky, east 
rnm <- 
  tm_shape(pa, bbox = tha)+
  tm_polygons(col = "grey96")+
  
  tm_shape(rn, bbox = tha) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = TRUE,
            title = "Species numbers")+
  
  tm_shape(asia,bbox = tha) + tm_borders(col = "grey10", lwd = 1.2)+
  
  tm_shape(pa,bbox = tha) + tm_borders(col = "grey30", lwd = 1.2)+
  
  tm_shape(west) + tm_borders(lw = 2.1, col = "grey20")+
  tm_shape(dpky) + tm_borders(lw = 2.1, col = "grey20")+
  tm_shape(east) + tm_borders(lw = 2.1, col = "grey20")+
  
  tm_shape(b1) + tm_borders(lw = 2.7, col = "grey30")+
  tm_shape(b2) + tm_borders(lw = 2.7, col = "grey30")+
  tm_shape(b3) + tm_borders(lw = 2.7, col = "grey30")+
  
  tm_graticules(labels.size = 1.7,lines = FALSE)+
  
  #add legend
  tm_add_legend(type = c("fill"), col = "grey96", border.col = "grey70",
                labels = c("PA in bordering countries"),
                size = 5.5) +
  
  tm_layout (main.title = 'Potential habitat of wild bovid in Thailand',
             main.title.size = 2.2, 
             main.title.position = c(0.1, 0.98),
             inner.margins = c(0, 0, 0, 0), 
             outer.margins = c(0.001, 0.001, 0.001, 0.001),
             legend.outside = F,
             legend.title.size = 2.5,
             legend.text.size = 1.6,
             legend.position = c(0.68, 0.01),
             legend.bg.color = "white",
             legend.height = 0.24,
             legend.width = 0.29) +
  
  tm_compass(type = "arrow", size = 1.5, text.size = 1.5 , position = c(0.01, 0.08)) +
  tm_scale_bar(breaks = c(0, 100, 200), position = c(0.01, 0.01),text.size = 1.5)

# proportion function of inset map; ref: https://github.com/Robinlovelace/geocompr/issues/532  
norm_dim <- function(obj) {
  bbox <- st_bbox(obj)
  
  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]
  
  w <- width/max(width, height)
  h <- height/max(width, height)
  
  c("w" = w, "h" = h)
}

#creating the main map and inset map viewports
main_w <- norm_dim(rn)[["w"]]
main_h <- norm_dim(rn)[["h"]]
ins_w <- norm_dim(rn)[["w"]]
ins_h <- norm_dim(rn)[["h"]]

main_vp <- viewport(x = 0.5, y = 0.5,
                    width = unit(main_w, "snpc"), height = unit(main_h, "snpc"),
                    name = "main")
# x = apart from left side, y = apart from bottom
ins_vp<- viewport(x=0.78,y=0.32,
                  width = unit(ins_w, "snpc") *0.3, 
                  height = unit(ins_h, "snpc")*0.3)

#creating polygon border
bthai = bb_poly(tha,projection = 4326)

# creating inset map
# import the richness map for viewport:
rnla <- raster("./result_sample/richness_5bov_mix_besttss.tif")

inmap_rn <-  
  tm_shape(rnla)+
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F) +
  tm_shape(asia)+
  tm_polygons(border.col = "grey5", 
              lwd = 0.2,
              alpha = 0, 
              border.alpha = 1) +
  tm_shape(bthai) + tm_borders(lw = 2, col = "red") +
  
  tm_layout(inner.margins = c(0, 0, 0, 0), outer.margins = c(0.003, 0.003, 0.003, 0.003))

# printing maps
grid.newpage()

print(rnm, vp = main_vp)
pushViewport(main_vp)
print(inmap_rn, vp = ins_vp)

#tmap save

#fig<-"/bovidae_enm/fig/"
#setwd(fig)

tmap_save(rnm,filename = "Fig5_Richness_thai.png",
          dpi = 300, insets_tm = inmap_rn, insets_vp = ins_vp,
          height = 40, width = 25, units = "cm")

#plot :: zoom in PA 
pam <-
  tm_shape(asia,bbox = tha) + 
  tm_borders(col = "grey10", lwd = 1.5) +
  tm_shape(pa, bbox = tha)+
  tm_borders(col = "grey30", lwd = 1.6)

#set result working directory
#setwd("./fig/")

# Western PA
inmap_b1 <-
  tm_shape(pa, bbox = b1)+
  tm_polygons(col = "grey96", border.col = "grey30", lwd = 1.6) +
  
  tm_shape(rn,bbox=b1) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F) +
  pam+     
  tm_shape(west) + tm_borders(lw = 3.5, col = "grey20") +
  
  tm_layout(main.title = "(A) Western",
            main.title.size = 2.5, 
            main.title.position = c(0.01, 0.98),
            inner.margins = c(0, 0, 0, 0), 
            outer.margins = c(0.002, 0.002, 0.002, 0.002))

tmap_save(inmap_b1, filename = "Fig5_inmap_rn_Western.png",
          dpi = 300, height = 20, width = 25, units = "cm")

#Dong Phayayen-Khao Yai
inmap_b2 <-
  tm_shape(pa, bbox = b2)+
  tm_polygons(col = "grey96", border.col = "grey30", lwd = 1.6) +
  
  tm_shape(rn, bbox = b2) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F) +
  
  pam +
  tm_shape(dpky) + tm_borders(lw = 3.5, col = "grey20") +
  
  #tm_graticules(labels.size = 1,lines = FALSE) +
  tm_layout(main.title = "(B) Dong Phayayen-Khao Yai",
            main.title.size = 2.5, 
            main.title.position = c(0.01, 0.98),
            inner.margins = c(0, 0, 0, 0), 
            outer.margins = c(0.002, 0.002, 0.002, 0.002))

tmap_save(inmap_b2, filename = "inmap_rn_DPKY.png",
          dpi = 300, height = 20, width = 25, units = "cm")

#Eastern PA
inmap_b3 <-
  tm_shape(pa, bbox = b3)+
  tm_polygons(col = "grey96", border.col = "grey30", lwd = 1.6)+
  
  tm_shape(rn, bbox = b4) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = F) +
  
  pam +
  tm_shape(east) + tm_borders(lw = 3.5, col = "grey20") +
  
  # tm_graticules(labels.size = 1,lines = FALSE) +
  tm_layout(main.title ="(C) Eastern",
            main.title.size = 2.5, 
            main.title.position = c(0.01, 0.98),
            inner.margins = c(0, 0, 0, 0), 
            outer.margins = c(0.002, 0.002, 0.002, 0.002))

tmap_save(inmap_b3, filename = "inmap_rn_east.png",
          dpi = 300, height = 20, width = 25, units = "cm")


############## 6) Density plot ###########
df<-read.csv("./data_preparation/model_area.csv")

ggplot(df, aes(x=Country, y = area,fill=Model))+
  geom_bar(stat='identity', position=position_dodge())+
  facet_wrap(.~species,scales = "free_y") +
  
  theme_bw()+
  labs(y=expression( area (km^2)),x=NULL)+
  theme(strip.text.x = element_text(face = "italic"))+
  theme(text = element_text(size = 10))  +
  theme(axis.text.x = element_text(angle = 90, size = 8))+
  theme(axis.text.y = element_text(size = 8))+
  scale_fill_manual(values = c("grey60", "#039be5"))+ #"rosy brown"
  scale_y_continuous(breaks = seq(0,300000,50000), labels = scales::comma)

#ggsave("Fig2_area_country.png", width = 25, height = 12, units = "cm")


############## 7) Density plot ###########
#======habitat suitability density (ggridge) for five Bovidae species=====#
# Creating the density plot between the best ensemble model habitat suitability and WDPA protected areas

library(tidyverse)
library(ggplot2)
library(ggridges)
library(cowplot)

#create path
path <- "./result_sample/dataset_combine/"

# these dataset were created from extract data of rasters (environmental rasters, ensemble models, binary map, IUCN protected area)
# Used 2 data columns 1) suitability (0-1); 2) iucn (1-7) for plotting (excluded non-protected area)

# import dataset of the best TSS models ----
df_gaur <- readRDS(paste0(path,"df_gaur_la.rds")) # gaur LA no MSDM
df_banteng <- readRDS(paste0(path,"df_banteng_la.rds")) # banteng LA MSDM
df_buffalo <- readRDS(paste0(path,"df_buffalo_ssa.rds")) # buffalo SSA MSDM
df_serow <- readRDS(paste0(path,"df_serow_ssa.rds")) # serow SSA MSDM
df_goral <- readRDS(paste0(path,"df_goral_la.rds")) # goral LA MSDM

# need two columns 1) habitat suitability, 2) IUCN protected areas 
# class should be == factor
class(df_gaur$iucn)
class(df_banteng$iucn)
class(df_buffalo$iucn)
class(df_serow$iucn)
class(df_goral$iucn)

# iucn == 8 classes : https://wdpa.s3-eu-west-1.amazonaws.com/WDPA_Manual/English/WDPA_WDOECM_Manual_1_6.pdf
# 1-6 == WDPA protected areas (PA) category, 7==not report/applicable, 8 == non PA
table(df_gaur$iucn)
table(df_banteng$iucn)
table(df_buffalo$iucn)
table(df_serow$iucn)
table(df_goral$iucn)

#color: spectral
spectral <- c("#9E0142", "#D53E4F", "#F46D43",
              "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598",
              "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

scale_fill_viridis_c(name = "Suitability")

# set result folder
# setwd("/bovidae_enm/")


# ggridge plot-------

# density of 7 WDPA PA category and suitability,  exclude unprotected areas
# 1-6 == WDPA category
# label the TSS threshold values of the best ENM models
# th = is the TSS threshold values for the best models; use to draw a dashline which is a cuf-off of suitable (> threshold) and unsuitable (< threshold) areas

#> Gaur No MSDM models --------------
# TSS threshold
th <- 0.49

# plotting density
den_gaur <- 
  ggplot(df_gaur %>% filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..))) +

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size = 0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral), name = "Suitability") +
  
  labs(title = 'Bos gaurus',
       x = "Suitability",
       y = "PA category") +

  theme_minimal() +

  #font size  
  theme( plot.title = element_text(size = 18, face = "italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size = 13)) +
  
  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_gaur_lm_prop.png", res = 400, units = "cm", width = 20, height = 15)
print(den_gaur)
dev.off()

#> Banteng MSDM LA models ------------------
# TSS threshold 
th <- 0.41

# plotting density
den_banteng <- 
  ggplot(df_banteng %>% filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
         group = iucn, height = (..count..))) +

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size = 0.3,
                               stat = "density") + # using density

  scale_fill_gradientn(colours = c(spectral), name = "Suitability") +
  
  labs(title = 'Bos javanicus',
       x = "Suitability",
       y = "PA category") +

  theme_minimal() +

  #font size  
  theme( plot.title = element_text(size = 18, face = "italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size = 13)) +

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_banteng_lm_prop.png", res = 400, units = "cm", width = 20, height = 15)
print(den_banteng)
dev.off()

#> Buffalo MSDM SSA models -----------------
# TSS threshold 
th <- 0.44

# plotting density
den_buffalo <- 
  ggplot(df_buffalo %>% filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..))) +

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size = 0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Bubalus arnee',
       x = "Suitability",
       y = "PA category") +

  theme_minimal() +
  
  #font size  
  theme( plot.title = element_text(size = 18, face = "italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size = 13)) +
  
  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_buffalo_sm_prop.png", res = 400, units = "cm", width = 20, height = 15)
print(den_buffalo)
dev.off()

#> Serow No MSDM SSA models -----------------
# TSS threshold 
th <- 0.57

# plotting density
den_serow <- 
  ggplot(df_serow %>% filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..))) +

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size = 0.3,
                               stat = "density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Capricornis sumatraensis',
       x = "Suitability",
       y = "PA category") +

  theme_minimal() +
  
  #font size  
  theme( plot.title = element_text(size = 18, face = "italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size = 13)) +
  
  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_serow_sm_prop.png", res = 400, units = "cm", width = 20, height = 15)
print(den_serow)
dev.off()


#> Goral MSDM LA models -------------------
# TSS threshold 
th <- 0.59

#plotting density
den_goral <- 
  ggplot(df_goral %>% filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..))) +

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size = 0.3,
                               stat = "density") + # using density

  scale_fill_gradientn(colours = c(spectral), name = "Suitability") +
  
  labs(title = 'Naemorhedus griseus',
       x = "Suitability",
       y = "PA category") +

  theme_minimal() +
  
  #font size  
  theme( plot.title = element_text(size = 18, face = "italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size = 11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size = 13)) +
  
  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_goral_lm_prop.png", res = 400, units = "cm", width = 20, height = 15)
print(den_goral)
dev.off()

#save figures with cowplot::plot_grid
png(filename = "tot_den_plotgrid.png", res = 400, width = 50, height = 30, units = "cm")

plot_grid(den_gaur, den_banteng, den_buffalo, den_serow, den_goral,
          labels=c("A)", "B)", "C)","D)","E)","F)"), nrow = 2, ncol = 3)
dev.off()

# 6) PCA plot

# PCA analysis - plot::  5 species of wild Bovidae (Gaur, Banteng, Wild buffalo, Mainland serow, Chinese goral)
# Use metadata in ./result_sample/dataset_combine

# NOTE: Code for one species because metadata is large and will consume space if run all five. 
# If want to run for other species can be repeted the same code

# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under macOS Big Sur 11.6

library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggcorrplot)
library(raster)
library(ggrepel)
library(FactoMineR)

#set plot directory
path <- "/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/GitHub/bovidae_enm/result_sample/"

#dir()

############## 8) PCA biplot ###########

# import metadata sample
df_serow_ssa <- readRDS(paste0(path, "./dataset_combine/df_serow_ssa.rds"))

# rename
df_biplot <- df_serow_ssa

dfpca <- df_serow_ssa %>% dplyr::select(water, urban,
                                        tree, slope, ndvi, hlog, grass, elev, crop,
                                        bio19, bio18, bio17, bio16, bio15, bio14, bio13,
                                        bio12, bio11, bio10, bio09, bio08, bio07, bio06,
                                        bio05, bio04, bio03, bio02, bio01)
#create PCA object
pca <- stats::prcomp(dfpca, 
                     retx = F,
                     center = T,
                     scale = T)
str(pca)

#Extract loadings of the variables
loadings <- data.frame(Variables = rownames(pca$rotation), pca$rotation)

#create variance explained plot
vexp <- data.frame(PC = 1:28,
                   var_explained = ((pca$sdev)^2/sum((pca$sdev)^2))*100)

#select only important PCs used fo modelling
a <- pca$rotation[, 1:12]
a

a2 <- t(a)
a2

sp <- "Mainland serow SSA"
plot <- "serow"

# ?ggcorrplot
# Bar plot explained variance
png(filename = paste0("screeplot_varexp_", plot,".png"), res = 300, units = "cm", width = 25, height = 15)
p1 <- ggplot(vexp[1:12,], aes(x=PC,y=var_explained))+
  geom_col()+
  geom_text(aes(label = round(var_explained,2)), vjust = -0.7,size = 4) +
  scale_x_continuous(name = "PC", breaks = seq(0, 12)) +
  ylab("Explained variance (%)") +
  ggtitle(paste0("Scree plot: ", sp)) + 
  theme_classic() +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 15)) +
  theme(plot.title = element_text(size = 17))

p1
dev.off()

png(filename = paste0("pca_loadings_", plot, ".png"),res=300, units = "cm", width= 20, height=20)
p2 <- ggcorrplot(a2, 
                 #lab=TRUE,lab_size = 2.5,tl.cex = 12, #for single figure
                 lab=TRUE,lab_size = 2.6,tl.cex = 11,
                 colors = c("#E46726","white","#6D9EC1"),
                 legend.title = "",
                 title = paste0("Loadings matrix: ", sp)) + 
  theme(legend.key.width = unit(0.3, "cm"), 
        legend.key.height = unit(0.5, "cm")) +
  theme(legend.position = "right",
        legend.box.margin = margin(-10, -10, -10, -10)) +
  theme(legend.text = element_text(size = 8)) + 
  theme(plot.title = element_text(size = 15)) 
p2   

dev.off()

#> Biplot ---------
#plot PC1 and PC2 from df (metadata), classify by species's observation and prediction locations
# 00 absence-obs, absence-predicted
# 01 absence-obs, presence-predicted
# 10 presence-obs, absence-predicted
# 11 presence-obs, presence-predicted

#create r object for loadings labels&arrows [code from PCATools package]
r <- min(
  (max(pca$rotation[1,]) - min(pca$rotation[1,]) / 
     (max(pca$rotation[1,]) - min(pca$rotation[1,]))),
  (max(pca$rotation[2,]) - min(pca$rotation[2,]) / 
     (max(pca$rotation[2,]) - min(pca$rotation[2,]))))

p3 <- ggplot(df_biplot, aes(x = PC1, y = PC2)) +
  
  geom_point(mapping = aes(color = ob.bi),
             shape = 19,
             stroke = 0.5,
             size = 1,
             alpha = 0.6) +
  
  scale_color_manual(values = c("00" = "#E0ECF4", "01" = "#9EBCDA",
                                "10" = "#6A51A3", "11" = "#c6579a")) +
  
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  
  #loading segments
  geom_segment(data = loadings, 
               aes(x = 0, y = 0,
                   xend = (PC1* 15), 
                   yend = (PC2* 15)),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "#000000") + 
  
  # Avoid overlaps by repelling text labels
  geom_label_repel(data = loadings, aes(label = Variables,
                                        x = PC1*15,
                                        y = PC2*15),
                   size = 4,
                   label.size = NA,  
                   label.padding = .1, 
                   na.rm = TRUE,
                   fill = alpha(c("white"), 0.4),
                   max.overlaps = 28 ) +
  #labels
  labs(title = paste0("PCA biplot: ", sp),
       x = paste0('PC1, ', round(vexp$var_explained[1], digits = 2),'%'),
       y = paste0('PC2 ,', round(vexp$var_explained[2], digits = 2),'%'),
       color = 'Observed-Predicted') +
  theme_classic()+
  theme(legend.text = element_text(size = 12))+ 
  theme(axis.title = element_text(size = 15))+
  theme(axis.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 17))

p3


?plot_grid
pc <- plot_grid(p3, p1,
                labels = c("B", "C"), 
                ncol = 1, 
                nrow = 2, 
                rel_heights = c(3,2.5),
                rel_widths = c(1)) +
  theme(plot.margin = unit(c(top = 0.1,bottom = 0.1, left = 0,right = 0),"cm"))
pc

pp2 <- plot_grid(p2, pc, labels = c('A', ''), 
                 ncol = 2, 
                 rel_widths = c(1, 1.5),rel_heights = c(1)) +
  theme(plot.margin = unit(c(top = 0.1, bottom = 0.1, left = 0,right = 0), "cm")) #top, right

# Important variables -------------
# create PCA for ploting important variables
res.pca_serow <- FactoMineR::PCA(dfpca, graph = FALSE)

#SSA
png(filename = paste0("pca_axes_contrib_", plot, ".png"), 
    res = 300, units = "cm", width= 12, height=8)

fviz_cos2(res.pca_serow, choice = "var") +
  labs(title  = paste0("Important variables PC1-2: ", sp)) +
  theme(axis.title = element_text(size = 7))+
  theme(axis.text = element_text(size = 7))+
  theme(plot.title = element_text(size = 12))

dev.off()
