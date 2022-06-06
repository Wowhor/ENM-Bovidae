#======habitat suitability density (ggridge) for five Bovidae species=====

# Create the density plot between the best ensemble model habitat suitability and WDPA protected areas

library(tidyverse)
library(ggplot2)
library(ggridges)
library(cowplot)

#create path
path<- "/Users/whorpien/OneDrive - Massey University/R/resultenmtmldf"
path_rds<-paste0(path,"/metadata/")

# Used data from ensemble models (habitat suitability 0-1, raster: 1 cell size == 1km^2) & WDPA polygon 
# Extracted each pixel suitability and WDPA category 
# 1 cell contained 2 data 1) suitability (0-1) 2) PA category (1-7)
# Then converted to dataframe and save in RDS file

# import data
df_gaur<-readRDS(paste0(path_rds,"Gaur_lm.rds"))
df_banteng<-readRDS(paste0(path_rds,"Banteng_lm.rds"))
df_buffalo<-readRDS(paste0(path_rds,"Buffalo_sm.rds"))
df_serow<-readRDS(paste0(path_rds,"Serow_sm.rds"))
df_goral<-readRDS(paste0(path_rds,"Goral_lm.rds"))

# need two columns 1) habitat suitability, 2) WDPA category for each pixels
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


setwd(path)

#ggridge plot-------

# density of 7 WDPA PA category and suitability,  exclude unprotected areas
# 1-6 == WDPA category
# label the TSS threshold values of ENM models

#Gaur lm

th<-0.49

den_gaur<- 
  ggplot(df_gaur%>%filter(iucn != "NonPA"), 
         aes(x = bg_wm_msdm_null_nw, y = iucn,
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

png(filename = "den_gaur_lm_prop_copy.png",res=600, units = "cm", width= 20, height=15)
print(den_gaur)
dev.off()

#Banteng lm

th<-0.41

den_banteng<- 
  ggplot(df_banteng%>%filter(iucn != "NonPA"), 
         aes(x = bj_wm_msdm_obr_nw_crop, y = iucn,
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

png(filename = "den_banteng_lm_prop_copy.png",res=600, units = "cm", width= 20, height=15)
print(den_banteng)
dev.off()


#Buffalo sm

th<-0.44

den_buffalo<- 
  ggplot(df_buffalo%>%filter(iucn != "NonPA"), 
         aes(x = ba_wm_spac_obr_crop, y = iucn,
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

png(filename = "den_buffalo_sm_prop_copy.png",res=600, units = "cm", width= 20, height=15)
print(den_buffalo)
dev.off()

#Serow sm

th<-0.57

den_serow<- 
  ggplot(df_serow%>%filter(iucn != "NonPA"), 
         aes(x = cs_wm_spac_null_nw, y = iucn,
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

png(filename = "den_serow_sm_prop_copy.png",res=600, units = "cm", width= 20, height=15)
print(den_serow)
dev.off()


#Goral lm

th<-0.59

den_goral<- 
  ggplot(df_goral%>%filter(iucn != "NonPA"), 
         aes(x = ng_wm_msdm_obr, y = iucn,
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

png(filename = "den_goral_lm_prop_copy.png",res=600, units = "cm", width= 20, height=15)
print(den_goral)
dev.off()


#save figures

png(filename= "tot_den_plotgrid_copy.png", res=600, width = 50, height = 30, units = "cm")

plot_grid(den_gaur, den_banteng, den_buffalo, den_serow, den_goral,
          labels=c("A)", "B)", "C)","D)","E)","F)"), nrow=2,ncol=3)
dev.off()

