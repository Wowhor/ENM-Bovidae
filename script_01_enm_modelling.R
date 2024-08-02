# ----------------------------------------------------------------------
# ecological model for five Bovidae species in Thailand
# Code from: Horpiencharoen et al. 
# Mapping threatened Thai Bovidae species provides opportunities for improved conservation outcomes in Asia 
# R version used: 4.0.1 
# ----------------------------------------------------------------------

# Script 01 - Build ENM --------

# This workflow can be adapted to other occurrence datasets 
# We use ENMTML package for modelling [https://github.com/andrefaa/ENMTML] 
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

# Download the sample dataset and files for modelling here:
# https://drive.google.com/drive/folders/1tKMkeltE1eSnrmTh4obpIPjYEJaJTChX?usp=sharing 
# 
# Data files are in the folder "data_preparation" folder
# We used 3 components: Occurrence data (this code used bg_test.txt), environmental variables, accessible area (for restricting the species distribution)
# 1) occurrence data (sample of dataset from gbif.org):      
#       - bg_test.txt = Gaur,
#       - bj_test.txt = Banteng,               
#       - ba_test.txt = Wild water buffalo       
#       - cs_test.txt = Mainland serow
#       - ng_test.txt = Chinese goral

# 2) 28 environmental variables: folder name 'env' -> inside contained the environmental variable for each species.
#       - These files were cropped based on accessible areas (the shapefile of accessible areas are in the data_preperation folder)
#       - envgaur, envbanteng, envbuffalo, envserow, envgoral are the species-specific accessible area (SSA) which different for each species
#       - env_la is a single large accessible area used in every species (LA)
#       
# 3) accessible areas shapefile (two types species-specific and large accessible areas)
#       - folder 'acc' is contained SSA and LA shapefile for spatial restriction
#             - accgaur, accbanteng, accbuffalo, accserow, accgoral are SSA that selected from ecoregions based on literature review and IUCN polygon
#             - acc_la is a LA which use for all five species 
# We create 4 models for each species (SSA with spatially restriction, SSA without spatially restriction, LA with spatially restriction, and LA without spatially restriction)
# Spatially restriction: reference Mendes et al. (2020; https://doi.org/10.1016/j.ecolmodel.2020.109180)

# These are sample codes
# 1) Start from setting up your working directory -----------------
# path<-"./GitHub/ENM-Bovidae"

# Data folder location assumes you start RStudio from clicking on ENM-Bovidae.R through RStudio:

#path <-"./bovidae_enm"
path <- "./GitHub/bovidae_enm"
getwd()
setwd(path)

d_ex <- file.path(getwd()) 

d_ex

# 2) SSA Modelling ---------------------------

# using gaur as a sample

# species occurrence data contain three columns: species,x,y
# occurrence data path

d_occ <- file.path(d_ex,"data_preparation/bg_test.txt")

occ <- read.table(file.path(d_ex, 'data_preparation/bg_test.txt'),header = T)

str(occ)

# 28 environmental variables path
d_env <- file.path(d_ex, "data_preparation/env/envgaur")
dir(d_env)

# check accessible area shapefile:
plot(shapefile(file.path(d_ex, "data_preparation/acc/accgaur/gaur_acc_disv_2.shp")))

# species specific accessible area shapefile path
m_path <- file.path(d_ex, "data_preparation/acc/accgaur/gaur_acc_disv_2.shp") 

# - Gaur SSA MSDM with spatially restriction (MSDM) OBR
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_SSA_OBR", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ = c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var = c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method = 'MASK', filepath = m_path ),
       pseudoabs_method = c(method = 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method = 'BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr = c(type = c('MAX_TSS')),
       msdm = c(method = 'OBR'),
       ensemble = c(method = c('W_MEAN'), metric = 'TSS'),
       extrapolation = FALSE, 
       cores = 2)
       
# clean memory garbage
gc()

# Next
# Gaur SSA without MSDM  ----------------------

# occurrence data path
d_occ <- file.path(d_ex, "data_preparation/bg_test.txt")

occ <- read.table(file.path(d_ex, 'data_preparation/bg_test.txt'), header = T)

str(occ)

# 28 environmental variables path
d_env <- file.path(d_ex, "data_preparation/env/envgaur")
dir(d_env)

# check accessible area shapefile:
plot(shapefile(file.path(d_ex, "data_preparation/acc/accgaur/gaur_acc_disv_2.shp")))

# species specific accessible area shapefile path
m_path <- file.path(d_ex, "data_preparation/acc/accgaur/gaur_acc_disv_2.shp") 

# Gaur SSA without MSDM OBR 
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_SSA_nomsdm", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ = c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var = c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method= 'MASK', filepath= m_path ),
       pseudoabs_method = c(method= 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method='BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr = c(type = c('MAX_TSS')),
       msdm = NULL,
       ensemble = c(method = c('W_MEAN'), metric = 'TSS'),
       extrapolation = FALSE, 
       cores = 2)
       
# clean memory garbage
gc()

# 3) LA modelling --------------------------------

# Gaur LA with MSDM OBR ------------------

# occurrence data path
d_occ <- file.path(d_ex,"data_preparation/bg_test.txt")

occ <- read.table(file.path(d_ex, 'data_preparation/bg_test.txt'), header = T)

str(occ)

# 28 environmental variables path
d_env <- file.path(d_ex, "data_preparation/env/env_la")
dir(d_env)

# check mask shapefile:
m <-shapefile(file.path(d_ex,"data_preparation/acc/acc_la/acc_ecoregion_msdm.shp"))
plot(m)

# large accessible area shapefile path
m_path <- file.path(d_ex, "data_preparation/acc/acc_la/acc_ecoregion_msdm.shp") 

# Gaur LA with MSDM OBR
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_LA_OBR", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ = c(method='CELLSIZE'),
       eval_occ = NULL, 
       colin_var = c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method = 'MASK', filepath = m_path ),
       pseudoabs_method = c(method = 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method ='BOOT', replicates ='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr=c(type = c('MAX_TSS')),
       msdm = c(method='OBR'),
       ensemble = c(method = c('W_MEAN'), metric = 'TSS'),
       extrapolation = FALSE, 
       cores = 2)
       
# clean up memory       
gc()

# Gaur LA without MSDM OBR------------------

# occurrence data path
d_occ <- file.path(d_ex, "data_preparation/bg_test.txt")

occ <- read.table(file.path(d_ex, 'data_preparation/bg_test.txt'), header = T)

str(occ)

# 28 environmental variables path
d_env <- file.path(d_ex, "data_preparation/env/env_la")
dir(d_env)

# check mask shapefile:
m <-shapefile(file.path(d_ex, "data_preparation/acc/acc_la/acc_ecoregion_msdm.shp"))
plot(m)

# large accessible area shapefile path
m_path <- file.path(d_ex, "data_preparation/acc/acc_la/acc_ecoregion_msdm.shp") 

# Gaur LA without MSDM
ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaurTest_LA_nomsdm", 
       occ_file = d_occ,
       sp = 'species', 
       x = 'x', 
       y = 'y', 
       min_occ = 10,
       thin_occ = c(method ='CELLSIZE'),
       eval_occ = NULL, 
       colin_var = c(method ='PCA'), 
       imp_var = TRUE, 
       sp_accessible_area = c(method = 'MASK', filepath = m_path ),
       pseudoabs_method = c(method = 'RND'),    
       pres_abs_ratio = 1, 
       part = c(method = 'BOOT', replicates='10', proportion='0.75'),
       save_part = FALSE, 
       save_final = TRUE,
       algorithm = c("BIO","GLM","GAM","SVM","RDF","MXD","MLK","GAU"), 
       thr = c(type = c('MAX_TSS')),
       msdm = NULL,
       ensemble = c(method = c('W_MEAN'), metric = 'TSS'),
       extrapolation = FALSE, 
       cores = 2)
       
# clean up memory
gc()
rm(list = ls())
# END ---------------------------------------------------------------------
