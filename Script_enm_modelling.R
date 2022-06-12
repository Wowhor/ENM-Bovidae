# ecological model for five Bovidae species in Thailand.
# This can be applied to other occurrence dataset 
# This code perform in R version 4.0.1 
# Using ENMTML package for modelling [https://github.com/andrefaa/ENMTML] 
# 1) Bos gaurus 
# 2) Bos javanicus 
# 3) Bubalus arnee 
# 4) Capricornis sumatraensis
# 5) Naemorhedus griseus

############## LOAD PACKAGES ##########
library(devtools)  
library(rgdal)
library(ENMTML)
library(rstan)
library(raster)
library(dplyr)
library(openxlsx)

# file that we need for modelling 
# 3 components: 

# 1) occurrence data (sample of dataset from gbif.org) file name:      
#       - bg_test.txt = gaur,
#       - bj_test.txt = banteng,               
#       - ba_test.txt = wild water buffalo       
#       - cs_test.txt = Mainland serow
#       - ng_test.txt = Chinese goral
# 2) 28 environmental variables: folder name 'env' -> inside contained the environmental variable for each species.
#       - cropped environmental variables for five species based on accessible areas (the shapefile of accessible areas are in the shapefile folder)
#       - envgaur, envbanteng, envbuffalo, envserow, envgoral are the species-specific accessible area (SSA) which different for each species
#       - env_la is a single large accessible area used for every species (LA)
#       
# 3) accessible areas shapefile (two types species-specific and large accessible areas)
#       - folder 'shapefile' is contained SSA and LA shapefile for spatial restriction
#       - accgaur, accbanteng, accbuffalo, accserow, accgoral are selected from ecoregions based on literature review and IUCN polygon
#       - acc_la is a LA which covers all five species accessible areas

# 1) set up working directory -----------------
# path <-  "..." this can be the folder path for the dataset.
# path<-"/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/R/1working/"

path<-"./bovidae_enm/..path here.."

setwd(path)

d_ex <- file.path(getwd())
d_ex

# species specific-accessible area (SSA) 
# use gaur as a sample dataset
# species occurrence data (species,x,y)
d_occ <- file.path(d_ex,"bg_test.txt")

occ<- read.table(file.path(d_ex, 'bg_test.txt'),header = T)

str(occ)

# 28 environmental variables 
d_env <- file.path(d_ex, "envgaur")
dir(d_env)

# check accessible area shapefile:
plot(shapefile(file.path(d_ex,"accgaur/gaur_acc_disv_2.shp")))

# species specific accessible area shapefile path
m_path <- file.path(d_ex,"accgaur/gaur_acc_disv_2.shp") 

# 2)  SSA Modelling ---------------------------

# - Gaur SSA MSDM OBR  ----------------------
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_OBRmsdm", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ=c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var=c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type=c('MAX_TSS')),
       msdm= c(method='OBR'),
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)
# clean memmory garbage
gc()

# Gaur SSA no MSDM  ----------------------
path<-"./bovidae_enm/..path here.."

setwd(path)

d_ex <- file.path(getwd())
d_ex

# species specific-accessible area (SSA) 
# use gaur as a sample dataset
# species occurrence data (species,x,y)
d_occ <- file.path(d_ex,"bg_test.txt")

# 28 environmental variables 
d_env <- file.path(d_ex, "envgaur")
dir(d_env)

# species specific accessible area shapefile path
m_path <- file.path(d_ex,"accgaur/gaur_acc_disv_2.shp") 

ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_SSA_nomsdm", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ=c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var=c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type=c('MAX_TSS')),
       msdm= NULL,
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)

# 3) LA modelling --------------------------------
path<-"./bovidae_enm/..path here.."

setwd(path)

d_ex <- file.path(getwd())
d_ex

# species occurrence data (species,x,y)
d_occ <- file.path(d_ex,"bg_test.txt")

# 28 environmental variables 
d_env <- file.path(d_ex, "env_la")
dir(d_env)

# check mask shapefile:
m<-shapefile(file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp"))
plot(m)

# large accessible area shapefile path
m_path <- file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp") 

# - Gaur LA no MSDM------------
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_LA_OBR", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ=c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var=c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type=c( 'MAX_TSS')),
       msdm= NULL,
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)

gc()

# - Gaur LA MSDM OBR------------

path<-"./bovidae_enm/..path here.."

setwd(path)

d_ex <- file.path(getwd())
d_ex

# species occurrence data (species,x,y)
d_occ <- file.path(d_ex,"bg_test.txt")

# 28 environmental variables 
d_env <- file.path(d_ex, "env_la")
dir(d_env)

# check mask shapefile:
m<-shapefile(file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp"))
plot(m)

# large accessible area shapefile path
m_path <- file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp") 
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_LA_OBR", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ=c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var=c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type=c('MAX_TSS',)),
       msdm= c(method='OBR'),
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)
gc()

# Gaur LA no MSDM

path<-"./bovidae_enm/..path here.."

setwd(path)

d_ex <- file.path(getwd())
d_ex

# species occurrence data (species,x,y)
d_occ <- file.path(d_ex,"bg_test.txt")

# 28 environmental variables 
d_env <- file.path(d_ex, "env_la")
dir(d_env)

# check mask shapefile:
m<-shapefile(file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp"))
plot(m)

# large accessible area shapefile path
m_path <- file.path(d_ex,"acc_la/acc_ecoregion_msdm.shp") 
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ=c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var=c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type=c('MAX_TSS')),
       msdm= NULL,
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)

gc()