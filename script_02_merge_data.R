# Script 02 - 
# Creates a dataframe and merges it with environmental variables to calculate the protected areas based on the best models.

# Note: this process requires the variables and observation data from the folder 'bovidae_enm'.
# It may be be slow and consume storage
# It can be skipped and proceed with script 03 by using the dataframe in the 'result_sample' folder.

# Remove all objects from the environment
rm(list=ls())

library(raster)
library(tidyverse)
library(dplyr)

# Set up working directory ----
# ** Change the file path to your download path
path1 <- "./GitHub/bovidae_enm"
setwd(path1)

# Prepare the list of all data for merging into the dataframe ----

#> Best ENM list ----
enslist_la <- list.files(path = "./result_sample/ensembles", pattern = "_la", full.names = T) |> lapply(raster)
enslist_ssa <- list.files(path = "./result_sample/ensembles", pattern = "_ssa", full.names = T) |> lapply(raster)

enslist_la 
enslist_ssa

# Name the ensemble models == "suitability"
for (i in 1:5) {
  
  names (enslist_la[[i]]) <- c("suitability")
  names (enslist_ssa[[i]]) <- c("suitability")

}

#> Large accessible area (LA) binary list ----- 
binlist_la <- list.files(path = "./result_sample/binary/", pattern = "_la", full.names = T) |> lapply(raster)
binlist_la

#> Species specific accessible area (SSA) binary list ----
binlist_ssa <- list.files(path = "./result_sample/binary/", pattern = "_ssa", full.names = T) |> lapply(raster)
binlist_ssa

# Name the binary map == "bin"
for (i in 1:5) {
  names (binlist_la[[i]]) <- c("bin")
  names (binlist_ssa[[i]]) <- c("bin")
}

#> Environmental variables list ----

# Create LA variable list
var_list_la <- list.files(path = "./data_preparation/env/env_la", pattern = ".tif", full.names=T) |> raster::stack()

# Create SSA variable list
var_file_ssa <- c("envgaur",
                  "envbuffalo",
                  "envbanteng",
                  "envserow",
                  "envgoral")
var_list_ssa<-list()

for (i in 1:5) {
  
    var_list_ssa[[i]] <- list.files(path = paste0("./data_preparation/env/", var_file_ssa[[i]]), pattern = ".tif", full.names=T) |>
    raster::stack()
}

var_list_ssa

#> Occurrence data list ----

ob <- list.files(path = "./data_preparation", pattern = ".*.txt", full.names = T) |>
      lapply(FUN = read.table, header = TRUE, sep = "\t")
ob

str(ob)

#> Protected Area raster (CRS = WGS84) ----
path_pa <- "./data_preparation/PA_and_country/"
#dir(path_pa)

# -- Large accessible area (This is similar for five species) 
pa_la <- raster(paste0(path_pa, "PA_la.tif"))
names (pa_la) <- c("PA")
pa_la

# -- Species specific accessible area
pa_ssa_list <- intersect(list.files(path = "./data_preparation/PA_and_country/", pattern="_ssa", full.names = T),
                         list.files(path = "./data_preparation/PA_and_country/", pattern="PA", full.names = T)) |> 
 lapply(raster)

pa_ssa_list

# name the PA map == "PA"
for (i in 1:5) {
  names (pa_ssa_list[[i]]) <- c("PA")
}
pa_ssa_list


# Create empty lists for loop ----

# Species name list
nam <- c("Buffalo","Gaur","Banteng", "Serow","Goral")

sp = 1:5

# Empty list for store all df
df_ssa10_list <- list() 
df_la10_list <- list()

set.seed(111)

# Create target directory

path_rds <- "./merge"
dir.create(path_rds)
setwd(path_rds)

# Starting loop --------

# This loop can be slow and requires memory storage.

for (i in sp) {
  
  names(enslist_ssa)[i]
  names(enslist_la)[i]
  
  #extract points from ENM raster and merge with data frame
  en_ssa <- rasterToPoints(enslist_ssa[[i]], spatial=TRUE)
  en_la <- rasterToPoints(enslist_la[[i]], spatial=TRUE)
  
  #merge variables and ENM 
  en_ssa <- as.data.frame(en_ssa) |>
    cbind(raster::extract(var_list_ssa[[i]], en_ssa))
  en_la <- as.data.frame(en_la) |>
    cbind(raster::extract(var_list_la, en_la))
  
  xy_ssa <- dplyr::select(en_ssa,x,y)
  xy_la <- dplyr::select(en_la,x,y)
  
  #extract ENM binary map values and bind with en2
  #create a dataframe
  en_ssa <- base::cbind(raster::extract(binlist_ssa[[i]], xy_ssa, df = T),en_ssa)
  en_la <- base::cbind(raster::extract(binlist_la[[i]], xy_la, df = T),en_la)
  
  #merge with occurrence data
  obr_ssa <- raster::rasterize(base::cbind(ob$x, ob$y), var_list_ssa[[i]], field = 1, background = 0)
  obr_la <- raster::rasterize(base::cbind(ob$x, ob$y), var_list_la, field = 1, background = 0)
  
  names(obr_ssa) <- c("obsv") 
  names(obr_la) <- c("obsv")
  
  en_ssa <- cbind(raster::extract(obr_ssa,xy_ssa, df = T),en_ssa)
  en_la <- cbind(raster::extract(obr_la,xy_la, df = T), en_la)
  
  table(en_ssa$obsv)
  table(en_la$obsv)
  
  #create new column called ob.bi, use paste combine value in colums
  en_ssa$sp <- c(nam[[i]])
  en_ssa$ob.bi <- paste(en_ssa$obsv,en_ssa$bin,sep="")
  en_ssa$ob.bi <- as.factor(as.character(en_ssa$ob.bi))
  
  en_la$sp <- c(nam[[i]])
  en_la$ob.bi <- paste(en_la$obsv,en_la$bin,sep="")
  en_la$ob.bi <- as.factor(as.character(en_la$ob.bi))
  
  #merge with PA
  en_ssa <- base::cbind(raster::extract(pa_ssa_list[[i]], xy_ssa, df = T), en_ssa)
  en_la <- base::cbind(raster::extract(pa_la, xy_la, df = T), en_la)
  
  #assign PA as factor level (8 category)
  en_ssa$PA <- as.factor(as.integer(en_ssa$PA))
  en_la$PA <- as.factor(as.integer(en_la$PA))
  
  en_ssa <- en_ssa |> tidyr::drop_na()
  en_la <- en_la |> tidyr::drop_na()
  
  #rename
  df_ssa <- en_ssa
  df_la <- en_la
  
  #change dataframe type
  df_ssa$obsv[is.na(df_ssa$obsv)] <- 0            #replace obs NA with zero
  df_ssa$bin <- as.factor(as.integer(df_ssa$bin))   #change int to factor
  df_ssa$obsv <- as.factor(as.integer(df_ssa$obsv)) #change int to factor
  
  df_la$obsv[is.na(df_la$obsv)] <- 0            #replace obs NA with zero
  df_la$bin <- as.factor(as.integer(df_la$bin))   #change int to factor
  df_la$obsv <- as.factor(as.integer(df_la$obsv)) #change int to factor
  
  #sampling subset
  #rearrange the row 1 come before 0
  df_ssa1 <- df_ssa[df_ssa$obsv==1,]
  df_ssa0 <- df_ssa[df_ssa$obsv==0,]
  
  df_ssa10 <- bind_rows(df_ssa0,df_ssa1)
  
  
  df_la1 <- df_la[df_la$obsv==1,]
  df_la0 <- df_la[df_la$obsv==0,]
  
  df_la10 <- bind_rows(df_la0,df_la1)
  
  # *Check the the observe occurrence has to be = TRUE
  table(df_ssa10$ob.bi) == table(df_ssa10$ob.bi)
  table(df_la10$ob.bi) == table(df_la10$ob.bi) 
  
  #add iucn columns 
  df_ssa10$iucn <- df_ssa10$PA
  df_la10$iucn <- df_la10$PA
  
  #rename iucn category =1-7 and 8=non PA
  df_ssa10 <- df_ssa10 |> mutate(iucn=recode(iucn, 
                                            "1"="I",
                                            "2"="II",
                                            "3"="III",
                                            "4"="IV",
                                            "5"="V",
                                            "6"="VI",
                                            "7"="NotReport",
                                            "8"="NonPA"))
  
  df_la10 <- df_la10 |> mutate(iucn=recode(iucn, 
                                          "1"="I",
                                          "2"="II",
                                          "3"="III",
                                          "4"="IV",
                                          "5"="V",
                                          "6"="VI",
                                          "7"="NotReport",
                                          "8"="NonPA"))
  
  #store df10 as list
  df_ssa10_list[[i]] <- df_ssa10
  df_la10_list[[i]] <- df_la10
  
  #saving -------
  saveRDS(df_ssa10_list[[i]], file = paste0("df_",nam[[i]],"_ssa.rds")) 
  saveRDS(df_la10_list[[i]], file = paste0("df_",nam[[i]],"_la.rds")) 
  
  # done :)
  
}
