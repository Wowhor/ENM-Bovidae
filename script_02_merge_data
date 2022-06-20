# create dataframe from the best models

# install.packages("tidyverse")
library(raster)
library(tidyverse)

# ** change the file path to download path

#import ENM 
#path1<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS_ens/"
path1<-"/bovidae_enm/result_sample/ensembles/"
setwd(path1)

#making the list of the best ensemble models
enslist_ssa <- list.files(path = path1, pattern="_ssa") %>% lapply(raster)
enslist_la <- list.files(path = path1, pattern="_la") %>% lapply(raster)

#name the ensemble models == "suitability"
for (i in 1:5) {
  names (enslist_ssa[[i]])<- c("suitability")
  names (enslist_la[[i]])<- c("suitability")
}

# making binary map list
#path2<-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/Best_TSS/"
path2<-"/bovidae_enm/result_sample/binary/"
setwd(path2)
dir(path2)

#LA binary list
binlist_la <- list.files(path = path2, pattern="_la")%>% 
              lapply(raster)
binlist_la

#SSA binary list
binlist_ssa <- list.files(path = path2, pattern="_ssa") %>% lapply(raster)
binlist_ssa

#name the binary map == "bin"
for (i in 1:5) {
  names (binlist_la[[i]])<- c("bin")
  names (binlist_ssa[[i]])<- c("bin")
}

#making variables raster stacks
#list variable folder name
#path3<-"/Users/whorpien/OneDrive - Massey University/R/1working/"
path3<-"/bovidae_enm/data_preparation/env/"

var_list_la <-list.files(paste0(path3, "env_la"), pattern = ".tif", full.names=T) %>% 
  raster::stack()

# making SSA variable list
var_file_ssa <- c("envbuffalo",
                  "envgaur",
                  "envbanteng",
                  "envserow",
                  "envgoral")
var_list_ssa<-list()
for (i in 1:5) {
  var_list_ssa[[i]] <-list.files(paste0(path3, var_file_ssa[[i]]), pattern = ".tif", full.names=T) %>% 
    raster::stack()
}
var_list_ssa


#making occurrence data list
#path4<-"/Users/whorpien/OneDrive - Massey University/R/1working/occurrence"
path4<-"/bovidae_enm/"
setwd(path4)

oblist<- list.files(path = path4,pattern = ".*.txt")%>%
  lapply(FUN=read.table, header=TRUE,sep = "\t")
str(oblist)

#Protected Area raster (CRS = WGS84)
#path5 <-"/Users/whorpien/OneDrive - Massey University/R/1working/area_calculation/PA/"
path5<-"/bovidae_enm/data_preparation/PA_and_country"
dir(path5)
setwd(path5)

#large acc. 
pa_la<-raster(paste0(path5,"PA_la.tif"))
names (pa_la)<- c("PA")
# species specific acc.
pa_ssa_list <- intersect(list.files(path = path5, pattern="_ssa"),
                         list.files(path = path5, pattern="PA")) %>% 
  lapply(raster)
#name the PA map == "PA"
for (i in 1:5) {
  names (pa_ssa_list[[i]])<- c("PA")
}
pa_ssa_list

# create the dataframe with loop-----------

nam<-c("Buffalo","Gaur","Banteng", "Serow","Goral")

sp = 1:5

#empty list for store all df
df_ssa10_list<-list() 
df_la10_list<-list()

set.seed(111)
#setwd and creating target directory
#dir.create(paste0(path, "/metadata"))
#result path
#path_rds<-"/Users/whorpien/OneDrive - Massey University/R/1working/test/"
path_rds<-"..result folder here.."

#Starting loop--------
for (i in sp) {
  
  names(enslist_ssa)[i]
  names(enslist_la)[i]
  
  #extract points from ENM raster and merge with data frame
  en_ssa<- rasterToPoints(enslist_ssa[[i]], spatial=TRUE)
  en_la<- rasterToPoints(enslist_la[[i]], spatial=TRUE)
  
  #merge variables and ENM 
  en_ssa2 <- as.data.frame(en_ssa)%>%
    cbind(raster::extract(var_list_ssa[[i]], en_ssa))
  en_la2 <- as.data.frame(en_la)%>%
    cbind(raster::extract(var_list_la, en_la))
  
  xy_ssa<-dplyr::select(en_ssa2,x,y)
  xy_la<-dplyr::select(en_la2,x,y)
  
  #extract ENM binary map values and bind with en2
  #making as a dataframe
  en_ssa3 <-base::cbind(raster::extract(binlist_ssa[[i]], xy_ssa, df = T),en_ssa2)
  en_la3 <-base::cbind(raster::extract(binlist_la[[i]], xy_la, df = T),en_la2)
  
  #merge with occurrence data
  obr_ssa<-raster::rasterize(base::cbind(oblist[[i]]$x, oblist[[i]]$y), var_list_ssa[[i]], field = 1, background = 0)
  obr_la<-raster::rasterize(base::cbind(oblist[[i]]$x, oblist[[i]]$y), var_list_la, field = 1, background = 0)
  
  names(obr_ssa) <- c("obsv") 
  names(obr_la) <- c("obsv")
  
  en_ssa4<- cbind(raster::extract(obr_ssa,xy_ssa, df = T),en_ssa3)
  en_la4<- cbind(raster::extract(obr_la,xy_la, df = T),en_la3)
  
  table(en_ssa4$obsv)
  table(en_la4$obsv)
  
  #create new column called ob.bi, use paste combine value in colums
  en_ssa4$sp<-c(nam[[i]])
  en_ssa4$ob.bi<-paste(en_ssa4$obsv,en_ssa4$bin,sep="")
  en_ssa4$ob.bi<-as.factor(as.character(en_ssa4$ob.bi))
  
  en_la4$sp<-c(nam[[i]])
  en_la4$ob.bi<-paste(en_la4$obsv,en_la4$bin,sep="")
  en_la4$ob.bi<-as.factor(as.character(en_la4$ob.bi))
  
  #merge with PA
  en_ssa5<-base::cbind(raster::extract(pa_ssa_list[[i]], xy_ssa, df = T),en_ssa4)
  en_la5<-base::cbind(raster::extract(pa_la, xy_la, df = T),en_la4)
  
  #assign PA as factor level (8 category)
  en_ssa5$PA<-as.factor(as.integer(en_ssa5$PA))
  en_la5$PA<-as.factor(as.integer(en_la5$PA))
  
  en_ssa5<-en_ssa5%>%tidyr::drop_na()
  en_la5<-en_la5%>%tidyr::drop_na()
  
  #rename
  df_ssa <- en_ssa5
  df_la <- en_la5
  
  #change dataframe type
  df_ssa$obsv[is.na(df_ssa$obsv)] <- 0            #replace obs NA with zero
  df_ssa$bin<-as.factor(as.integer(df_ssa$bin))   #change int to factor
  df_ssa$obsv<-as.factor(as.integer(df_ssa$obsv)) #change int to factor
  
  df_la$obsv[is.na(df_la$obsv)] <- 0            #replace obs NA with zero
  df_la$bin<-as.factor(as.integer(df_la$bin))   #change int to factor
  df_la$obsv<-as.factor(as.integer(df_la$obsv)) #change int to factor
  
  #sampling subset
  #rearrange the row 1 come before 0
  df_ssa1<-df_ssa[df_ssa$obsv==1,]
  df_ssa0<-df_ssa[df_ssa$obsv==0,]
  df_ssa10<-bind_rows(df_ssa0,df_ssa1)
  
  df_la1<-df_la[df_la$obsv==1,]
  df_la0<-df_la[df_la$obsv==0,]
  df_la10<-bind_rows(df_la0,df_la1)
  
  # *Check the the observe occurrence has to be = TRUE
  table(df_ssa10$ob.bi) == table(df_ssa10$ob.bi)
  table(df_la10$ob.bi) == table(df_la10$ob.bi) 
  
  #add iucn columns 
  df_ssa10$iucn<-df_ssa10$PA
  df_la10$iucn<-df_la10$PA
  
  #rename iucn category =1-7 and 8=non PA
  df_ssa10<-df_ssa10 %>% mutate(iucn=recode(iucn, 
                                            "1"="I",
                                            "2"="II",
                                            "3"="III",
                                            "4"="IV",
                                            "5"="V",
                                            "6"="VI",
                                            "7"="NotReport",
                                            "8"="NonPA"))
  
  df_la10<-df_la10 %>% mutate(iucn=recode(iucn, 
                                          "1"="I",
                                          "2"="II",
                                          "3"="III",
                                          "4"="IV",
                                          "5"="V",
                                          "6"="VI",
                                          "7"="NotReport",
                                          "8"="NonPA"))
  
  #store df10 as list
  df_ssa10_list[[i]]<-df_ssa10
  df_la10_list[[i]]<-df_la10
  
  #saving -------
  saveRDS(df_ssa10_list[[i]], file = paste0("df_",nam[[i]],"_ssa.rds")) 
  saveRDS(df_la10_list[[i]], file = paste0("df_"nam[[i]],"_la.rds")) 
  
  # done :)
  
}
