#ENMTML
# R version 4.0.1 
# install.packages("devtools")  
# install new version from GITHUB
if (!"devtools"%in%installed.packages()){install.packages("devtools")}  
devtools::install_github("andrefaa/ENMTML")  
remotes::install_github('jmarshallnz/ENMTML', ref='no_parallel')

#remove objects
rm(list = ls(all.names = T))
memory.limit(size = 10000000000000)

library(devtools)  
library(rgdal)
library(ENMTML)
library(rstan)
library(raster)
library(dplyr)

#create path
path<-"D:/enmtml"

setwd(path)
d_ex <- file.path(getwd())
d_ex


#example: ================gaur====================
#species occurrence data (species,x,y)
d_occ<- file.path(d_ex, 'bg20210722.txt')

# bioclimatic variables for current conditions
d_env <- file.path(d_ex, 'envgaurcr')
d_env
dir(d_env) #bio1-19, elev,hplog,land_cgls2015,ndvi,waterG3W

#check mask shapefile:
b<-shapefile("/Users/whorpien/OneDrive - Massey University/R/1working/accgaur/gaur_cropdissv09052021.shp")
plot(b)
m_path <- "/Users/whorpien/OneDrive - Massey University/R/1working/accgaur/gaur_cropdissv09052021.shp" 

ENMTML(pred_dir = d_env, 
       proj_dir = NULL, 
       result_dir = "ResultGaur", 
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
       thr=c(type=c('LPT', 'MAX_TSS', 'MAX_KAPPA','SENSITIVITY'), sens='0.5'),
       msdm= c(method='OBR'),
       ensemble=c(method=c('W_MEAN'), metric='TSS'),
       extrapolation = FALSE, 
       cores = 2)