############# Figures plotting ####################
# Mapping wild Bovidae in Thailand (5 species)  

# Code for plotting the figures:
# 1) Study area (1. large accessibe area (LA), 2. species-specific study areas (SSA))
# 2) Habitat suitability maps, 
# 3) Binary habitat suitability map,  
# 4) Richness map, Thailand
# 5) Density plot (Suitability and IUCN protected area category)
# load packages
library(sf)
library(raster)
library(tidyverse)
library(tmap)    # for static and interactive maps
library(tmaptools) 
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(grid) #for insert map
library(gridExtra)
library(purrr)

tmap_mode("plot")

########1) Study area ###########
# import files

# IUCN map path
#iucn<-"/Users/whorpien/OneDrive - Massey University/GIS data/IUCNMap/"
iucn<-"/bovidae_enm/data_preparation/iucn/"

bufiucn<-shapefile(paste0(iucn,"Buffalo.shp"))
gauriucn<-st_read(paste0(iucn,"Gaur.shp"))
bantengiucn<-st_read(paste0(iucn,"Banteng.shp"))
serowiucn<-st_read(paste0(iucn,"Serow.shp"))
goraliucn<-st_read(paste0(iucn,"Goral.shp"))

# occurrence data path (sample ocurrence data from gbif.org)
poi<-"/bovidae_enm/data_preparation/"

# import occurrence data gaur, banteng, buffalo, serow, goral
gaurp<-read.table(paste0(poi,"bg_test.txt"),header=T, sep="\t") %>%  
  st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
bantengp<-read.table(paste0(poi,"bj_test.txt"),header=T, sep="\t")%>%
  st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
bufp<-read.table(paste0(poi,"ba_test.txt"), header = T, sep="\t") %>%
  st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")
serowp<-read.table(paste0(poi,"cs_test.txt"), header=T, sep="\t") %>%
  st_as_sf(coords = c('x', 'y'), crs = 4326)
goralp<- read.table(paste0(poi,"ng_test.txt"),header=T, sep="\t") %>%
  st_as_sf(coords = c('x', 'y'), crs = "+init=epsg:4326")

##> large accessible area ----------
#background border
accm<-shapefile("/bovidae_enm/data_preparation/acc/accmsdm/acc_ecoregion_msdm.shp")
asia<-shapefile("/bovidae_enm/data_preparation/adm_border/asiamap3.shp")
world<-shapefile("/bovidae_enm/data_preparation/adm_border/World_Countries__Generalized_.shp")

# bbox
b1= st_bbox(accm, crs = st_crs(4326))

# plot :: large accessible area 
la <-tm_shape(world,bbox=b1)+
  tm_fill(col="grey90")+
  
  tm_shape(accm,bbox=b1)+
  tm_fill(col="grey65")+
  tm_shape(world,bbox=b1)+
  tm_borders(col="#474747",lwd=0.2)+
  
  #degree
  tm_graticules(lines = FALSE)+ 
  
  #country
  #tm_text("COUNTRY", size = 0.7,root=3)+ 
  
  #add legend
  tm_add_legend(type=c("fill"),
                col = "grey60",
                labels=c("Large accessible area"),
                size=0.8, alpha = 1)+
  #map layout
  tm_layout(main.title = "Large accessible area",
            main.title.size=1.2,
            legend.text.size = 1.1,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            #legend.bg.color = "white",
            #legend.frame=T,
            #legend.frame.lwd=0.5,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.005,0.005,0.005,0.005))+ 
  
  tm_compass(type="arrow",size=1.5,text.size = 1,position=c(0.02,0.17))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.07),text.size = 1)

la

## plotting :: inset map
# asia background: b2
b2 = st_bbox(c(xmin = -16, xmax = 180,
               ymin = -40, ymax = 77),
             crs = st_crs(4326)) %>% 
  st_as_sfc()

#red frame focus large acc.area
bla = bb_poly(accm,projection=4326)

#plotting inset map
inmap_la<-tm_shape(world,bbox=b2)+
  tm_fill(col="grey80")+
  tm_shape(bla) + tm_borders(lw=0.5, col="red")+
  tm_shape(accm)+tm_fill(col="grey65")+
  tm_layout(inner.margins = c(0.01,0.01,0.01,0.01),outer.margins=c(0.01,0.01,0.01,0.01))
inmap_la

#inset maps proportionate
#ref: https://github.com/Robinlovelace/geocompr/issues/532 
norm_dim <- function(obj) {
  bbox <- st_bbox(obj)
  
  width <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]
  
  w <- width/max(width, height)
  h <- height/max(width, height)
  
  c("w" = w, "h" = h)
}

# creating the main map and inset map viewport
# large acc bbox
main_w <- norm_dim(b1)[["w"]]
main_h <- norm_dim(b1)[["h"]]

# asia bbox
ins_w <- norm_dim(b2)[["w"]]
ins_h <- norm_dim(b2)[["h"]]

main_vp_la <- viewport(x = 0.5, y = 0.5,
                       width = unit(main_w, "snpc"), height = unit(main_h, "snpc"),
                       name = "main")

# using "cm" ratio to ensure inset map offsets the main map border by 0.5 cm
#top left:: x=0.18 (apart from left side), y=0.92 (apart from bottom)
ins_vp_la <- viewport(x=0.38,y=0.15,
                      width = unit(ins_w, "snpc") *0.18, 
                      height = unit(ins_h, "snpc")*0.18)

# printing maps
grid.newpage()

#print(la)
print(la, vp = main_vp_la)
pushViewport(main_vp_la)
print(inmap_la, vp = ins_vp_la)



tmap_save(la,filename="studyarea_la_test.png",
          dpi=300, insets_tm=inmap_la, insets_vp=ins_vp_la,
          height=20, width=20, units="cm")

# study area for SSA
#extent of merge species accessible area   
#sacc<-shapefile("/Users/whorpien/OneDrive - Massey University/GIS Data/SpData/acc_ecoregion_ssa_disv_cropfinalmodel.shp")
sacc<-shapefile("/bovidae_enm/data_preparation/adm_border/acc_ecoregion_ssa_disv_cropfinalmodel.shp")

#check sacc
tm_shape(sacc,bbox=sacc)+ tm_polygons()

##> gaur SSA---------------------
#gaur_acc<-raster::shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accgaur/gaur_acc_disv_2.shp")

#import gaur ssa
gur_acc<-shapefile("/bovidae_enm/data_preparation/acc/accgaur/gaur_acc_disv_2.shp")

gaur<-  tm_shape(world,bbox = sacc)+
  tm_fill(col="grey95")+
  
  tm_shape(gaur_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(gauriucn) + 
  tm_fill(col = "#0095cb",alpha=0.6)+
  
  tm_shape(asia,bbox=b1) +
  tm_borders(col="#474747",lwd=0.2) +
  
  tm_shape(gaurp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd=0.15)+
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type=c("symbol"),
                col="#ffbf3f", 
                labels=c("B. gaurus"),
                size=0.8)+
  tm_add_legend(type=c("fill"),
                col = "#0095cb",
                labels=c("IUCN map"),
                size=0.2, alpha = 0.6)+
  tm_add_legend(type=c("fill"),
                col = "grey65",
                labels=c("Acessible area"),
                size=0.2, alpha = 0.8)+
  
  tm_layout(main.title = "B. gaurus",
            main.title.fontface = "italic",
            main.title.size=1.2,
            main.title.position = c(0.15,0.99),
            legend.text.size = 0.9,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            legend.bg.color = "white",
            legend.frame=F,
            legend.frame.lwd=0.5,
            legend.height = 0.2,
            legend.width = 0.7,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.01,0.01,0.01,0.01))+ 
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.02,0.3))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.2,2),text.size = 0.8)

gaur 

## > banteng SSA -------------------------------
#banteng_acc<-raster::shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accbanteng/banteng_acc_disv_2.shp")

#import banteng ssa
banteng <- ("/bovidae_enm/data_preparation/acc/accbanteng/banteng_acc_disv_2.shp")

banteng<-  tm_shape(world,bbox = sacc)+
  tm_fill(col="grey95")+
  
  tm_shape(banteng_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(bantengiucn) + 
  tm_fill(col = "#0095cb",alpha=0.6)+
  
  tm_shape(asia,bbox=b1) +
  tm_borders(col="#474747",lwd=0.2) +
  
  tm_shape(bantengp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd=0.15)+
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type=c("symbol"),
                col="#ffbf3f", 
                labels=c("B. javanicus"),
                size=0.8)+
  tm_add_legend(type=c("fill"),
                col = "#0095cb",
                labels=c("IUCN map"),
                size=0.2, alpha = 0.6)+
  tm_add_legend(type=c("fill"),
                col = "grey65",
                labels=c("Accessible area"),
                size=0.2, alpha = 0.8)+
  
  tm_layout(main.title = "B. javanicus",
            main.title.fontface = "italic",
            main.title.size=1.2,
            main.title.position = c(0.15,0.99),
            legend.text.size = 0.9,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            legend.bg.color = "white",
            legend.frame=F,
            legend.frame.lwd=0.5,
            legend.height = 0.2,
            legend.width = 0.7,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.01,0.01,0.01,0.01))+ 
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.02,0.3))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.2,2),text.size = 0.8)

banteng

## > buffalo SSA --------------------------------
#buffalo_acc<-shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accbuffalo/buffalo_acc_disv.shp")

# import buffalo SSA
buffalo_acc<-shapefile("/bovidae_enm/data_preparation/acc/accbuffalo/buffalo_acc_disv.shp")

buf<-  tm_shape(world,bbox = sacc)+
  tm_fill(col="grey95")+
  
  tm_shape(buffalo_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(bufiucn) + 
  tm_fill(col = "#0095cb",alpha=0.6 )+
  
  tm_shape(asia,bbox=b1) +
  tm_borders(col="#474747",lwd=0.2) +
  
  tm_shape(bufp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd=0.15)+
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type=c("symbol"),
                col="#ffbf3f", 
                labels=c("B. arnee"),
                size=0.8)+
  
  tm_add_legend(type=c("fill"),
                col = "#0095cb",
                labels=c("IUCN map"),
                
                size=0.2, alpha = 0.6)+
  tm_add_legend(type=c("fill"),
                col = "grey65",
                labels=c("Accessible area"),
                size=0.2, alpha = 0.6)+
  
  tm_layout(main.title = "B. arnee",
            main.title.fontface = "italic",
            main.title.size=1.2,
            main.title.position = c(0.15,0.99),
            legend.text.size = 0.9,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            legend.bg.color = "white",
            legend.frame=F,
            legend.frame.lwd=0.5,
            legend.height = 0.2,
            legend.width = 0.7,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.01,0.01,0.01,0.01))+ 
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.02,0.3))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.2,2),text.size = 0.8)

buf

## >  serow SSA -------------------------
#serow_acc <-shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accserow/serowthar_acc_disv.shp")

#import serow SSA
serow_acc <-shapefile("/bovidae_enm/data_preparation/acc/accserow/serowthar_acc_disv.shp")

#plot
serow<-  tm_shape(world,bbox = sacc)+
  tm_fill(col="grey95")+
  
  tm_shape(serow_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(serowiucn) + 
  tm_fill(col = "#0095cb",alpha=0.6 )+
  
  tm_shape(asia,bbox=b1) +
  tm_borders(col="#474747",lwd=0.2) +
  
  tm_shape(serowp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd=0.15)+
  
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type=c("symbol"),
                col="#ffbf3f", 
                labels=c("C. sumatraensis"),
                size=0.8)+
  
  tm_add_legend(type=c("fill"),
                col = "#0095cb",
                labels=c("IUCN map"),
                size=0.2, alpha = 0.6)+
  
  tm_add_legend(type=c("fill"),
                col = "grey65",
                labels=c("Accessible area"),
                size=0.2, alpha = 0.8)+
  
  tm_layout(main.title = "C. sumatraensis",
            main.title.fontface = "italic",
            main.title.size=1.2,
            main.title.position = c(0.15,0.99),
            legend.text.size = 0.9,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            legend.bg.color = "white",
            legend.frame=F,
            legend.frame.lwd=0.5,
            legend.height = 0.2,
            legend.width = 0.7,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.01,0.01,0.01,0.01))+ 
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.02,0.3))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.2,2),text.size = 0.8)

serow  

## >  goral SSA ---------------------------
#goral_acc<-shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accgoral/goral_acc_disv.shp")

#import goral SSA
goral_acc<-shapefile("/bovidae_enm/data_preparation/accgoral/goral_acc_disv.shp")

#plot
goral<-  tm_shape(world,bbox = sacc)+
  tm_fill(col="grey95")+
  
  tm_shape(goral_acc) + 
  tm_fill(col = "grey65")+
  
  tm_shape(goraliucn) + 
  tm_fill(col = "#0095cb",alpha=0.6 )+
  
  tm_shape(asia,bbox=b1) +
  tm_borders(col="#474747",lwd=0.2) +
  
  tm_shape(goralp) + 
  tm_dots(size = 0.15,
          col = "#ffbf3f",
          shape = 21,
          border.col = "black",
          border.lwd=0.15)+
  
  #degree
  tm_graticules(lines = FALSE)+ 
  tm_add_legend(type=c("symbol"),
                col="#ffbf3f", 
                #fontface = "italic",
                labels=c("N. griseus"),
                size=0.8)+
  
  tm_add_legend(type=c("fill"),
                col = "#0095cb",
                labels=c("IUCN map"),  
                size=0.2, alpha = 0.6)+
  
  tm_add_legend(type=c("fill"),
                col = "grey65",
                labels=c("Accessible area"),
                size=0.2, alpha = 0.8)+
  
  tm_layout(main.title = "N. griseus",
            main.title.fontface = "italic",
            main.title.size=1.2,
            main.title.position = c(0.15,0.99),
            legend.text.size = 0.9,
            legend.position=c(0.01, 0.01),
            legend.just=c("left","bottom"),
            legend.bg.color = "white",
            legend.frame=F,
            legend.height = 0.2,
            legend.width = 0.7,
            legend.outside = F,
            inner.margins = c(0,0,0,0), outer.margins=c(0.01,0.01,0.01,0.01))+ 
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.02,0.3))+
  tm_scale_bar(breaks = c(0,500, 1000), position=c(0.02, 0.2,2),text.size = 0.8)

goral

## > combine study area -----------------------------------------------------------
# use cowplot

la_grob<-tmap_grob(la)
inm_la_grob<-tmap_grob(inmap_la)

goral_grob<-tmap_grob(goral)
serow_grob<-tmap_grob(serow)
gaur_grob<-tmap_grob(gaur)
banteng_grob<-tmap_grob(banteng)
buf_grob<-tmap_grob(buf)

la_com<- ggdraw() +
  draw_plot(la_grob) +
  draw_plot(inm_la_grob,
            width = 0.2, height = 0.2,
            x=0.0711,y=0.755)

#export
#set folder
#fig<-"/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/R/fig/"
fig<-"/bovidae_enm/fig/"
dir.create(fig)
setwd(fig)

png(filename= "tot_studyarea2.png", res=600, width = 45, height = 30, units = "cm")

plot_grid(la_com,gaur_grob,banteng_grob,buf_grob,serow_grob,goral_grob,
          labels=c("A)", "B)", "C)","D)","E)","F)"), nrow=2,ncol=3)
dev.off()


############## 2) Habitat suitability maps##########
# set up working directory
#path1 <-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS_ens"

path1 <- "/bovidae_enm/result_sample/ensembles" 
setwd(path1)

dir()

## > import the best TSS emdemble models ------------
gaur_ens<-raster("bg_wm_la_null.tif")
bj_ens<-raster("bj_wm_la_obr.tif")
buf_ens<-raster("ba_wm_ssa_obr.tif")
serow_ens<-raster("cs_wm_ssa_null.tif")
goral_ens<-raster("ng_wm_la_obr.tif")

#projectraster to the large study areas, so when we plot the maps will have the same extent
serow_ens <- projectRaster(serow_ens,gaur_ens,method = 'ngb')
buf_ens <- projectRaster(buf_ens,gaur_ens,method = 'ngb')

#background boundary
accm<-st_read("/bovidae_enm/data_preparation/adm_border/acc_ecoregion_la_disv_cropfinalmodel.shp")
world<-st_read("/bovidae_enm/data_preparation/adm_border/World_Countries__Generalized_.shp")
asia<-shapefile("/bovidae_enm/data_preparation/adm_border/asiamap3.shp")

#arrange list in order select = BEST TSS model
ens<-raster::stack(gaur_ens, #gaur
                   bj_ens, #banteng
                   buf_ens, #buffalo
                   serow_ens, #serow  
                   goral_ens) #goral

#name list
nam<-c("B. gaurus","B. javanicus", "B. arnee","C. sumatraensis","N. griseus")

sp = 1:5
m = list()
set.seed(111)

## > create suitability maps list m[i] ----------
for (i in sp) {
  
   names(ens)[i]
  
   m[[i]] <-
    tm_shape(world,bbox=accm)+
    tm_fill(col="white")+
    
    tm_shape(ens[[i]],bbox=accm) +
    tm_raster(style = "cont", 
              palette = c("YlOrBr"),
              legend.show = TRUE,
              title="Habitat suitability")+
    
    tm_grid(labels.inside.frame = FALSE, labels.size = 1,labels.cardinal=T,
            col = "black", 
            #n.x = 4, n.y = 4, 
            lines = FALSE,
            labels.rot = c(0, 90)) + 
    #tm_grid(labels.size=1,lines = FALSE,labels.cardinal=T)+
    
    tm_shape(asia,bbox=accm) +
    tm_polygons(border.col = "black", alpha=0, border.alpha = 0.5)+  
    
    tm_layout (main.title= nam[[i]],
               main.title.fontface = "italic",
               main.title.size = 1.3, 
               main.title.position = c(0.1,0.98),
               inner.margins= c(0.01,0.01,0.01,0.01), 
               outer.margins = c(0.01,0.01,0.01,0.01),
               legend.outside = F,
               legend.title.size=4.5,
               legend.text.size =3.5,
               legend.position=c(0.02, 0.01),
               legend.bg.color = "white",
               legend.frame=F,
               legend.width = 0.3,
               legend.height = 0.3)+
    
    tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.29,0.09))+
    tm_scale_bar(breaks = c(0, 500, 1000),position=c(0.29,0.01),text.size = 1.2)
}

## > Export individual map ----------
# getwd() *check result folder before export
setwd(fig)

for (i in sp) {
  png(filename = paste0(nam[[i]],"_best.png"), 
      res=600, units = "cm", width= 25, height =20)
  print(m[[i]])
  dev.off()
  }

## > create richness map figure for inserting with suitability map -------
# import richness map
#rnn<-raster("/Users/whorpien/OneDrive - Massey University/R/RichnessMap/richness_5bov_mix_besttss.tif")
rnn<-raster("bovidae_enm_result_sample/richness_5bov_mix_besttss.tif")

vv <- max(na.omit(values(rnn)))+1

#color code "-Spectral"
cc<-c("grey90","#3288BD","#99D594","#FEE08B","#FC8D59","#D53E4F")

rn_map<-
  tm_shape(world,bbox=accm) +
  tm_fill(col="white") +
  
  tm_shape(rnn,bbox=accm) +
  tm_raster(style = "cat", 
            palette = cc,
            legend.show = TRUE,
            title="Species richness") +
  
  tm_grid(labels.inside.frame = FALSE, labels.size = 1,labels.cardinal=T,
          col = "black", 
          #n.x = 4, n.y = 4, 
          lines = FALSE,
          labels.rot = c(0, 90)) + 
  #tm_grid(labels.size=1,lines = FALSE,labels.cardinal=T)+
  
  tm_shape(asia,bbox=accm) +
  tm_polygons(border.col = "black", alpha=0, border.alpha = 0.5)+  
  
  tm_layout (main.title= 'Species richness',
             main.title.size = 1.3, 
             main.title.position = c(0.1,0.98),
             inner.margins= c(0.01,0.01,0.01,0.01), 
             outer.margins = c(0.01,0.01,0.01,0.01),
             legend.outside = F,
             legend.title.size=4,
             legend.text.size = 3,
             legend.position=c(0.02, 0.01),
             legend.bg.color = "white",
             legend.frame=F,
             legend.width = 0.3,
             legend.height = 0.33)+
  
  tm_compass(type="arrow",size=1,text.size = 0.8,position=c(0.29,0.09))+
  tm_scale_bar(breaks = c(0, 500, 1000),position=c(0.29,0.01),text.size = 1.5)

# export richness map
png(filename = "richness_bestTSS_test.png", 
    res=600, units = "cm", width= 25, height =20)
print(rn_map)
dev.off()

# making grob
rn_grb<-tmap_grob(rn_map)

# add richness into m[i] list
mm<-c(m,m6=list(rn_map))

grob <- map(mm,tmap_grob)

# number of plot
num_plots<-6

## > combine habitat suitability figure using cowplot ------
# adjust the label
# vjust (vertical) More positive  = down 
# hjust (horizontal) More negative = right 
# label_x = adjust small number = left, larger = right
p<-cowplot::plot_grid(plotlist=grob,
             rel_heights = c(-1.01,-1.01,-1.01,-1),
             labels= paste0(c(LETTERS[1:num_plots]),")"),
             nrow=2,ncol=3,
             label_x = 0.08,label_y=0.995,
             hjust = 0.1,vjust = 1.9)

# export figure
png(filename = "tot_map_bestTSS_test.png", 
    res=600, units = "cm", width= 50, height =30)
print(p)
dev.off()


############## 3) Binary habitat suitability map ########### 

# setwd 
# path2<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS"
path2 <-"/bovidae_enm/result_sample/binary"
setwd(path2)
dir()

## > import the best binary models -------------
goral_bi<-raster("ng_la_obr_bin.tif")
serow_bi<-raster("cs_ssa_null_bin.tif")
gaur_bi<-raster("bg_la_null_bin.tif")
bj_bi<-raster("bj_la_obr_bin.tif")
buf_bi<-raster("ba_ssa_obr_bin.tif")

#projectraster to the large study areas, so when we plot the maps will have the same extent
serow_bi <- projectRaster(serow_bi,gaur_bi,method = 'ngb')
buf_bi <- projectRaster(buf_bi,gaur_bi,method = 'ngb')

#arrange list in order select = BEST TSS model
bin<-raster::stack(gaur_bi, #gaur
                   bj_bi, #banteng
                   buf_bi, #buffalo
                   serow_bi, #serow  
                   goral_bi) #goral
bin

nam<-c("B. gaurus","B. javanicus", "B. arnee","C. sumatraensis","N. griseus")

sp = 1:5
m = list()
set.seed(111)

## > create binary maps list m[i] ----------
for (i in sp) {
  names(bin)[i]
  m[[i]] <-
    tm_shape(world,bbox=accm)+
    tm_fill(col="white")+
    
    tm_shape(bin[[i]],bbox=accm) +
    tm_raster(style = "cat", 
              palette = c("#d4d4d4","mediumblue"),
              labels=c("unsuitable","suitable"),
              legend.show = T,legend.is.portrait=T
              )+
    
    tm_grid(labels.inside.frame = FALSE, labels.size = 1,labels.cardinal=T,
            col = "black", 
            #n.x = 4, n.y = 4, 
            lines = FALSE,
            labels.rot = c(0, 90)) + 
    #tm_grid(labels.size=1,lines = FALSE,labels.cardinal=T)+
    
    tm_shape(asia,bbox=accm) +
    tm_polygons(border.col = "black", alpha=0, border.alpha = 0.5)+  
    
    tm_layout (main.title= nam[[i]],
               main.title.fontface = "italic",
               main.title.size = 1.3, 
               main.title.position = c(0.1,0.98),
               inner.margins= c(0.01,0.01,0.01,0.01), 
               outer.margins = c(0.01,0.01,0.01,0.01),
               legend.outside = F,
               legend.title.size=3.5,
               legend.text.size =3.5,
               legend.position=c(0.02, 0.01),
               legend.bg.color = "white",
               legend.frame=F,
               legend.width = 0.2,
               legend.height = 0.2)+
    
    tm_compass(type="arrow",size=1.1,text.size = 0.8,position=c(0.29,0.09))+
    tm_scale_bar(breaks = c(0, 500, 1000),position=c(0.29,0.01),text.size = 1)
}


## > export individual map ---------
setwd(fig)

for (i in sp) {
png(filename = paste0(nam[[i]],"_bin_best.png"), 
    res=600, units = "cm", width= 25, height =20)
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

p<-cowplot::plot_grid(plotlist=grob,
             rel_heights = c(-1.01,-1.01,-1.01,-1),
             #rel_widths = c(1),
             labels= paste0(c(LETTERS[1:num_plots]),")"),
             nrow=2,ncol=3,
             label_x = 0.08,label_y=0.995,
             hjust = 0.1,vjust = 1.9)

#export figure
png(filename = "tot_map_bestTSS_bin.png", 
    res=600, units = "cm", width= 50, height =30)
print(p)
dev.off()


############## 4) Thailand richness map ###########
# Cropping process can be skipped and use cropped .tif file (richness_5bov_mix_besttss_tha.tif) in result_sample
###########################
# import richness map
r<-raster("/bovidae_enm/result_sample/richness_5bov_mix_besttss.tif")

# crop with Thailand extent
# import Thailand polygon
tha<-shapefile("bovidae_enm/data_preparation/adm_border/gadm36_THA_0.shp")

rn <- crop(r, extent(tha))%>% 
  mask(tha)

#Save as Geotiff with Thailand extent
#setwd("/bovidae_enm/result_sample/")
raster::writeRaster(x = rn, 
                    filename=file.path( "richness_5bov_mix_besttss_tha.tif"), 
                    bylayer = TRUE,
                    overwrite = TRUE)
###########################
# import richness map, Thailand:
rn<-raster("/bovidae_enm/result_sample/richness_5bov_mix_besttss_tha.tif")

# import shapefiles:
tha<-shapefile("bovidae_enm/data_preparation/adm_border/gadm36_THA_0.shp")
sacc<-shapefile("/bovidae_enm/data_preparation/adm_border/acc_ecoregion_ssa_disv_cropfinalmodel.shp")
accm<-shapefile("/bovidae_enm/data_preparation/acc/accmsdm/acc_ecoregion_msdm.shp")
asia<-shapefile("/bovidae_enm/data_preparation/adm_border/asiamap3.shp")
world<-shapefile("/bovidae_enm/data_preparation/adm_border/World_Countries__Generalized_.shp")
pa<- shapefile("/bovidae_enm/data_preparation/PA_and_country/wdpa/AsiaSelect2_largeacc_wdpar_clean.shp")

# focus on PA which species number = 4
pa_path<-"/bovidae_enm/data_preparation/PA_and_country/wdpa/"

dpky<-st_read(paste0(pa_path,"dpky.shp"))
west<-st_read(paste0(pa_path,"western.shp"))
east<-st_read(paste0(pa_path,"eastern.shp"))
pk<-st_read(paste0(pa_path,"PhukhewNumNao.shp"))

#fix invalid polygon error
tmap_options(check.and.fix = TRUE)

#color options
#color=viridis
#cc <- viridisLite::mako (vv,direction =1) 

#color=mako with red
#cc<-c("#0B0405FF","#395D9CFF", "#3E9BFEFF","#60CEACFF", "#DEF5E5FF","#c03100")

#cc <- c("-Spectral")
cc<-c("grey90","#3288BD","#99D594","#FEE08B","#FC8D59","#D53E4F")

# Creating bbox for framing interesting PA: b1,2,3,4
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

#Creating richness map
#with border for 4 PAs: pk, west, dpky, east (b1-b4)
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
bthai= bb_poly(tha,projection=4326)

# creating inset map
# import the richness map for viewport:
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

print(rnm, vp = main_vp)
pushViewport(main_vp)
print(inmap_rn, vp = ins_vp)

#tmap save

#fig<-"/bovidae_enm/fig/"
#setwd(fig)

tmap_save(rnm,filename="Richness_thai.png",
          dpi=600, insets_tm=inmap_rn, insets_vp=ins_vp,
          height=40, width=25, units="cm")


#plot :: zoom in PA 
pam<-
  tm_shape(asia,bbox=tha)+ 
  tm_borders(col="grey10",lwd=1.5)+
  tm_shape(pa,bbox=tha)+
  tm_borders(col = "grey30",lwd = 1.6)

#set result working directory
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

############## 5) Density plot ###########
#======habitat suitability density (ggridge) for five Bovidae species=====#
# Creating the density plot between the best ensemble model habitat suitability and WDPA protected areas

library(tidyverse)
library(ggplot2)
library(ggridges)
library(cowplot)

#create path
path<- "/bovidae_enm/result_sample/dataset_combine/"

# these dataset were created from extract data of rasters (environmental rasters, ensemble models, binary map, IUCN protected area)
# Used 2 data columns 1) suitability (0-1); 2) iucn (1-7) for plotting (excluded non-protected area)

# import dataset of the best TSS models
df_gaur<-readRDS(paste0(path,"df_gaur_la.rds")) # gaur LA no MSDM
df_banteng<-readRDS(paste0(path,"df_banteng_la.rds")) # banteng LA MSDM
df_buffalo<-readRDS(paste0(path,"df_buffalo_ssa.rds")) # buffalo SSA MSDM
df_serow<-readRDS(paste0(path,"df_serow_ssa.rds")) # serow SSA MSDM
df_goral<-readRDS(paste0(path,"df_goral_la.rds")) # goral LA MSDM

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

#set result folder
setwd("/bovidae_enm/")


#ggridge plot-------

# density of 7 WDPA PA category and suitability,  exclude unprotected areas
# 1-6 == WDPA category
# label the TSS threshold values of ENM models
# th = is the TSS threshold values for the best models; use to draw a dashline which is a cuf-off of suitable (> threshold) and unsuitable (< threshold) areas

#Gaur No MSDM models --------------
#TSS threshold 
th<-0.49

#plotting density
den_gaur<- 
  ggplot(df_gaur%>%filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..)))+

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size=0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Bos gaurus',
       x= "Suitability",
       y="PA category") +

  theme_minimal()+

  #font size  
  theme( plot.title = element_text(size = 18,face="italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size=11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size=13))+

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_gaur_lm_prop.png",res=600, units = "cm", width= 20, height=15)
print(den_gaur)
dev.off()

#Banteng MSDM LA models ------------------
#TSS threshold 
th<-0.41

#plotting density
den_banteng<- 
  ggplot(df_banteng%>%filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
         group = iucn, height = (..count..)))+

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size=0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Bos javanicus',
       x= "Suitability",
       y="PA category") +

  theme_minimal()+

  #font size  
  theme( plot.title = element_text(size = 18,face="italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size=11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size=13))+

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_banteng_lm_prop.png",res=600, units = "cm", width= 20, height=15)
print(den_banteng)
dev.off()


#Buffalo MSDM SSA models -----------------
#TSS threshold 
th<-0.44

#plotting density
den_buffalo<- 
  ggplot(df_buffalo%>%filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..)))+

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size=0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Bubalus arnee',
       x= "Suitability",
       y="PA category") +

  theme_minimal()+

  #font size  
  theme( plot.title = element_text(size = 18,face="italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size=11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size=13))+

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_buffalo_sm_prop.png",res=600, units = "cm", width= 20, height=15)
print(den_buffalo)
dev.off()

#Serow No MSDM SSA models -----------------
#TSS threshold 
th<-0.57

#plotting density
den_serow<- 
  ggplot(df_serow%>%filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..)))+

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size=0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Capricornis sumatraensis',
       x= "Suitability",
       y="PA category") +

  theme_minimal()+

  #font size  
  theme( plot.title = element_text(size = 18,face="italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size=11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size=13))+

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_serow_sm_prop.png",res=600, units = "cm", width= 20, height=15)
print(den_serow)
dev.off()


#Goral MSDM LA models -------------------
#TSS threshold 
th<-0.59

#plotting density
den_goral<- 
  ggplot(df_goral%>%filter(iucn != "NonPA"), 
         aes(x = suitability, y = iucn,
             group = iucn, height = (..count..)))+

  geom_density_ridges_gradient(aes(fill = ..x..), scale = 1, size=0.3,
                               stat="density") + # using density

  scale_fill_gradientn(colours = c(spectral),name = "Suitability") +
  
  labs(title = 'Naemorhedus griseus',
       x= "Suitability",
       y="PA category") +

  theme_minimal()+

  #font size  
  theme( plot.title = element_text(size = 18,face="italic"),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15),
         legend.title=element_text(size=11),
         legend.text = element_text(size = 11),
         axis.text=element_text(size=13))+

  geom_vline(xintercept = th, linetype = 2, size = 0.3, color = "black") +
  annotate(geom = "text", label = th,
           x = th-0.05, y = 7.5,  size = 5)

png(filename = "den_goral_lm_prop.png",res=600, units = "cm", width= 20, height=15)
print(den_goral)
dev.off()

#save figures with cowplot::plot_grid
png(filename= "tot_den_plotgrid.png", res=600, width = 50, height = 30, units = "cm")

plot_grid(den_gaur, den_banteng, den_buffalo, den_serow, den_goral,
          labels=c("A)", "B)", "C)","D)","E)","F)"), nrow=2,ncol=3)
dev.off()
