# Script 03:
# Calculate suitable areas for five species of wild Bovidae 
# gaur, banteng, wild water buffalo, serow, goral.

# Using the binary maps from the best models for 
# both Large Accessible Areas (LA) and Species-Specific accessible area (SSA).
# 10 rasters in the total (LA = 5 raster layers and SSA = 5 raster layers for each species)

# The binary maps values: 
# 0 = unsuitable area
# 1 = suitable area

# The binary map will be used to calculate the suitable and unsuitable areas overlapping with:
# 1. IUCN protected area categoty (1-7) and non-protected area (8) based on WDPA polygons: 
#    Reference https://wdpa.s3-eu-west-1.amazonaws.com/WDPA_Manual/English/WDPA_WDOECM_Manual_1_6.pdf (page 49; IUCN Management Category)
# 2. Countries within the study areas
# 3. Suitable area in Thailand

# Uses Zonal function in the package raster 

#load packages
library(rgdal)
library(raster)
library(tidyverse)
library(xlsx)
library(sf) 
library(sp)

rm(list = ls())
options(digits=7, scipen= 999)

# Set up working directory ----
path <- "./Github/bovidae_enm"
setwd(path)

# Create result directory
result<-"./result_sample/binary/cal_result"
dir.create(result)

######### Binary map & PAs ######### 

#> LA Models --------------------
# List of LA the best models
biglist <- list.files(path = "./result_sample/binary", pattern = "la", full.names = T)

# Import LA raster using lapply
big <- lapply(biglist, raster)
big

# Import Protected Area WDPA for LA (CRS = WGS84) [These files were from polygons to rasters using rasterize]
# File name: "PA_la.tif"

pal<- raster ("./data_preparation/PA_and_country/PA_la.tif")
pal
# Check resolution: should be true
res(pal)==res(big[[1]])

# Create empty lists
zonlist <- list()
zonalboth <- list()
check <- list()

nam <- c("Buffalo", "Gaur", "Banteng", "Serow", "Goral")

# Set up result directory
setwd(result)


#> LA Zonal statistic calculation for binary map & PAs --------
for (i in 1:length(big)){
    names(big)[i]
    max(na.omit(values(pal)))
  
    values(big[[i]])[values(big[[i]]) > 0] = 1
  
    # Now croping by presence or absence 
    pres <- big[[i]]
  
    unique(values(pres) )
  
    values(pres)[values(pres) < 1] = NA
  
    absences <- big[[i]]
    values(absences)[values(absences) > 0] = NA
  
    plot(absences,col='red')
  
    plot(pres, col='green')# add=TRUE,
  
    # whole raster layer
    ar<- area(big[[i]], na.rm=TRUE)
  
    # size of the cell in km2
    ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
    # Total area
    ar2 
  
    area_present <- sum(values(area(pres, na.rm=TRUE)), na.rm=TRUE)
  
    area_absences<- sum(values(area(absences, na.rm=TRUE)), na.rm=TRUE)
  
    # Almost equal, so estimate is good enough
    sum(area_present,area_absences)==ar2
    round((area_present + area_absences), digits = 2) -  round(ar2, digits=2)
  
    # Now crossing with protected areas and presences
    zonalpres<- data.frame(zonal(area(pres, na.rm=TRUE), pal, fun='sum') )
  
    colnames(zonalpres) <- c('Protected area code', 'Area sqkm')
  
    # Checking 
    sum(zonalpres$`Area sqkm`) - area_present
  
    # Area of presence per IUCN type
    zonalpres$codef <- as.factor(zonalpres$`Protected area code`)
  
    zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
    # rename IUCN PA category
    zonaltidy <-zonalpres %>%  mutate(label = fct_recode(codef,
                                                       "IUCN PA category Iab" = "1" ,
                                                       "IUCN PA category II" ="2" ,
                                                       "IUCN PA category III" = "3"  ,
                                                       "IUCN PA category IV" = "4"  ,
                                                       "IUCN PA category V" = "5",
                                                       "IUCN PA category VI" ="6"  ,
                                                       "Not Applicable"="7",
                                                       "Not protected" = "8")) %>% 
    select(-c(codef)) %>% arrange(desc(-`Protected area code`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
    # Crossing with absences
    # Now crossing with protected areas and ABSENCES
    zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), pal, fun='sum') )
  
    colnames(zonalabsent) <- c('Protected area code', 'Area sqkm')
  
    # Checking 
    sum(zonalabsent$`Area sqkm`) - area_absences
  
    # Area of absence per IUCN type
    # 8 == unprotected
  
    zonalabsent$codef <- as.factor(zonalabsent$`Protected area code`)
  
    zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
    zonaltidya<-zonalabsent %>%  mutate(label = fct_recode(codef,
                                                         "IUCN PA category Iab" = "1" ,
                                                         "IUCN PA category II" ="2" ,
                                                         "IUCN PA category III" = "3"  ,
                                                         "IUCN PA category IV" = "4"  ,
                                                         "IUCN PA category V" = "5",
                                                         "IUCN PA category VI" ="6"  ,
                                                         "Not Applicable"="7",
                                                         "Not protected" = "8")) %>% 
    select(-c(codef)) %>% arrange(desc(-`Protected area code`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
    zonalboth[[i]]<- rbind(zonaltidy, zonaltidya) 
    str(zonalboth[[i]])
  
    # Checking small divergence due to distortion
    check[[i]]<-sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
    # calculate total amount of area inside protected areas (km^2)
    zonlist[[i]] <-
    zonalboth[[i]]%>%
    as.data.frame()%>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
}  
  
# Exporting 
df <- bind_rows(zonlist)
str(df)

write.xlsx(df, paste0('table_area_la_allbov.xlsx'), row.names = FALSE)
  

#> SSA Models --------------------
#path
#setwd(path)

# List of SSA the best models
sl <- list.files(path = "./result_sample/binary", pattern = "ssa", full.names = T)
sl

# Import SSA raster using lapply
sm <- lapply(sl, raster)
sm

# Import Protected Area WDPA for SSA (CRS = WGS84) [These files were from polygons to rasters using rasterize]
# 5 Protected Area raster (CRS = WGS84)]

palist <- intersect(list.files(path = "./data_preparation/PA_and_country", pattern = "_ssa", full.names = T),
                    list.files(path = "./data_preparation/PA_and_country", pattern = "PA", full.names = T))
palist

# import SSA rasters using lapply
pal <- lapply(palist, raster)
pal

#create list
zonlist <- list()
zonalboth <- list()
check <- list()

nam<-c("Buffalo","Gaur","Banteng","Serow","Goral")

# set up result directory
setwd(result)

#> SSA Zonal statistic calculation binary map & PAs ----------
for (i in 1:length(sm)){
  names(sm)[i]
  names(pal)[i]
  hist(values(pal[[i]]))
  max(na.omit(values(pal[[i]])))
  
  values(sm[[i]])[values(sm[[i]]) > 0] = 1
  
  # Now croping by presence or absence 
  pres <- sm[[i]]
  
  unique(values(pres) )
  
  values(pres)[values(pres) < 1] = NA
  
  absences <- sm[[i]]
  values(absences)[values(absences) > 0] = NA
  
  plot(absences,col='red')
  
  plot(pres, col='green')# add=TRUE,
  
  # whole raster layer
  ar<- area(sm[[i]], na.rm=TRUE)
  
  # size of the cell in km2
  ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
  # Total area
  ar2 
  
  area_present <- sum(values(area(pres, na.rm=TRUE)), na.rm=TRUE)
  
  area_absences<- sum(values(area(absences, na.rm=TRUE)), na.rm=TRUE)
  
  # Almost equal, so estimate is good enough
  sum(area_present,area_absences)==ar2
  round((area_present + area_absences), digits = 2) -  round(ar2, digits=2)
  
  # Now crossing with protected areas and presences
  zonalpres<- data.frame(zonal(area(pres, na.rm=TRUE), pal[[i]], fun='sum') )
  
  colnames(zonalpres) <- c('Protected area code', 'Area sqkm')
  
  # Checking 
  sum(zonalpres$`Area sqkm`) - area_present
  
  # Area of presence per IUCN type
  
  zonalpres$codef <- as.factor(zonalpres$`Protected area code`)
  
  zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
  # rename IUCN PA category
  zonaltidy <-zonalpres %>%  mutate(label = fct_recode(codef,
                                                       "IUCN PA category Iab" = "1" ,
                                                       "IUCN PA category II" ="2" ,
                                                       "IUCN PA category III" = "3"  ,
                                                       "IUCN PA category IV" = "4"  ,
                                                       "IUCN PA category V" = "5",
                                                       "IUCN PA category VI" ="6"  ,
                                                       "Not Applicable"="7",
                                                       "Not protected" = "8")) %>% 
    arrange(desc(-`Protected area code`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  # Crossing with absences
  # Now crossing with protected areas and ABSENCES
  zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), pal[[i]], fun='sum') )
  
  colnames(zonalabsent) <- c('Protected area code', 'Area sqkm')
  
  # Checking 
  sum(zonalabsent$`Area sqkm`) - area_absences
  
  # Area of absence per IUCN type
  
  zonalabsent$codef <- as.factor(zonalabsent$`Protected area code`)
  
  zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
  zonaltidya<-zonalabsent %>%  mutate(label = fct_recode(codef,
                                                         "IUCN PA category Iab" = "1" ,
                                                         "IUCN PA category II" ="2" ,
                                                         "IUCN PA category III" = "3"  ,
                                                         "IUCN PA category IV" = "4"  ,
                                                         "IUCN PA category V" = "5",
                                                         "IUCN PA category VI" ="6"  ,
                                                         "Not Applicable"="7",
                                                         "Not protected" = "8")) %>% 
    arrange(desc(-`Protected area code`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  zonalboth[[i]]<- rbind(zonaltidy, zonaltidya) 
  str(zonalboth[[i]])
  
  # Checking small divergence due to distortion
  check[[i]]<-sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
  # calculate total amount of area inside protected areas (km^2)
  zonlist[[i]] <-
    zonalboth[[i]]%>%
    as.data.frame()%>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
  
  write.xlsx(zonlist[[i]],paste0('table_area_ssa_',nam[[i]],'.xlsx'), row.names = FALSE)
  
  df<-bind_rows(zonlist)
  str(df)
  write.xlsx(df,paste0('table_area_ssa_allbov.xlsx'), row.names = FALSE)
 
  }

######### Binary map & country areas ######### 

#> LA Models -------------
#path <- "./GitHub/bovidae_enm"
setwd(path)

# Already run above
biglist <- list.files(path = "./result_sample/binary", pattern = "la", full.names = T)

# Import LA raster using lapply
big <- lapply(biglist, raster)

big


# Import country raster
lm<- raster("./data_preparation/PA_and_country/country_la.tif")
#writeRaster(lm, "/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/country_la.tif")

res(lm)==res(big[[1]])
lm

# Create empty lists
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Buffalo", "Gaur", "Banteng", "Serow", "Goral")

# Set up result directory
# setwd(result) 

#> LA Zonal statistic calculation binary map & country --------
for (i in 1:length(big)){
  names(big)[i]
  hist(values(lm))
  max(na.omit(values(lm)))
  
  values(big[[i]])[values(big[[i]]) > 0] = 1
  
  values(big[[i]])[values(big[[i]]) > 0] = 1
  
  # Now croping by presence or absence 
  pres <- big[[i]]
  
  unique(values(pres) )
  
  values(pres)[values(pres) < 1] = NA
  
  absences <- big[[i]]
  values(absences)[values(absences) > 0] = NA
  
  plot(absences,col='red')
  
  plot(pres, col='green')# add=TRUE,
  
  # whole raster layer
  ar<- area(big[[i]], na.rm=TRUE)
  
  # size of the cell in km2
  ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
  # Total area
  ar2 
  
  area_present <- sum(values(area(pres, na.rm=TRUE)), na.rm=TRUE)
  
  area_absences<- sum(values(area(absences, na.rm=TRUE)), na.rm=TRUE)
  
  # Almost equal, so estimate is good enough
  sum(area_present,area_absences)==ar2
  round((area_present + area_absences), digits = 2) -  round(ar2, digits=2)
  
  # Now crossing with protected areas and presences
  
  zonalpres<- data.frame(zonal(area(pres, na.rm=TRUE), lm, fun='sum') )
  
  colnames(zonalpres) <- c('country', 'Area sqkm')
  
  # Checking 
  sum(zonalpres$`Area sqkm`) - area_present
  
  # Area of presence per country
  
  zonalpres$codef <- as.factor(zonalpres$`country`)
  
  zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
  # rename country using ISO country code
  zonaltidy <-zonalpres %>%  mutate(codef = fct_recode(codef,
                                                       "Afghanistan"="192",
                                                       "India"="194",
                                                       "Iran"="195",
                                                       "Nepal"="198",
                                                       "Pakistan"="200",
                                                       "Sri Lanka"="204",
                                                       "Tajikistan"="205",
                                                       "Kazakhstan"="211",
                                                       "Kyrgyzstan"="212",
                                                       "Uzbekistan"="213",
                                                       "Indonesia"="216",
                                                       "Cambodia"="226",
                                                       "Laos"="227",
                                                       "Malaysia"="228",
                                                       "Myanmar"="229",
                                                       "Thailand"="231",
                                                       "Vietnam"="232",
                                                       "Bangladesh"="233",
                                                       "Bhutan"="234",
                                                       "China"="235",
                                                       "Brunei Darussalam"="236",
                                                       "South Korea"="238",
                                                       "Mongolia"="239",
                                                       "North Korea"="240")) %>% 
    arrange(desc(-`country`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  # Crossing with absences
  # Now crossing with protected areas and ABSENCES
  
  zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), lm, fun='sum') )
  
  colnames(zonalabsent) <- c('country', 'Area sqkm')
  
  # Checking 
  sum(zonalabsent$`Area sqkm`) - area_absences
  
  # Area of absence per country
  
  zonalabsent$codef <- as.factor(zonalabsent$`country`)
  
  zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
  # rename country using ISO country code
  zonaltidya<-zonalabsent %>%  mutate(codef = fct_recode(codef,
                                                         "Afghanistan"="192",
                                                         "India"="194",
                                                         "Iran"="195",
                                                         "Nepal"="198",
                                                         "Pakistan"="200",
                                                         "Sri Lanka"="204",
                                                         "Tajikistan"="205",
                                                         "Kazakhstan"="211",
                                                         "Kyrgyzstan"="212",
                                                         "Uzbekistan"="213",
                                                         "Indonesia"="216",
                                                         "Cambodia"="226",
                                                         "Laos"="227",
                                                         "Malaysia"="228",
                                                         "Myanmar"="229",
                                                         "Thailand"="231",
                                                         "Vietnam"="232",
                                                         "Bangladesh"="233",
                                                         "Bhutan"="234",
                                                         "China"="235",
                                                         "Brunei Darussalam"="236",
                                                         "South Korea"="238",
                                                         "Mongolia"="239",
                                                         "North Korea"="240")) %>% 
    arrange(desc(-`country`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  zonalboth[[i]]<- rbind(zonaltidy, zonaltidya) 
  str(zonalboth[[i]])
  # Checking small divergence due to distortion
  check[[i]]<-sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
  # calculate total amount of area inside protected areas (km^2)
  zonlist[[i]] <-
    zonalboth[[i]]%>%
    as.data.frame()%>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
  
  # write.xlsx(zonlist[[i]],paste0('table_area_country_la_',nam[[i]],'.xlsx'), row.names = FALSE)
  
}  
  
df<-bind_rows(zonlist)

df$Model <- "LA"

str(df)

#write.xlsx(df,paste0('table_area_country_la_allbov.xlsx'), row.names = FALSE)
  

#> SSA Models -------------- 

# List of SSA the best models

# Already run above
# sl <- list.files(path = "./result_sample/binary", pattern = "ssa", full.names = T)
sl

# Import SSA raster using lapply
# sm <- lapply(smalllist, raster)
sm

# Import country raster

cl <- intersect(list.files(path = "./data_preparation/PA_and_country", pattern = "country", full.names = T),
                list.files(path = "./data_preparation/PA_and_country", pattern = "ssa", full.names = T))
cl

pal <- lapply(cl, raster)
pal

# Create empty lists
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Buffalo", "Gaur", "Banteng", "Serow", "Goral")

# Set up result directory
#setwd(result) 

#> SSA Zonal statistic calculation binary map & country --------
for (i in 1:length(sm)){
  names(sm)[i]
  names(pal)[i]
  
  hist(values(pal[[i]]))
  max(na.omit(values(pal[[i]])))
  
  
  values(sm[[i]])[values(sm[[i]]) > 0] = 1
  
  values(sm[[i]])[values(sm[[i]]) > 0] = 1
  
  # Now croping by presence or absence 
  pres <- sm[[i]]
  
  unique(values(pres) )
  
  values(pres)[values(pres) < 1] = NA
  
  absences <- sm[[i]]
  values(absences)[values(absences) > 0] = NA
  
  plot(absences,col='red')
  
  plot(pres, col='green')# add=TRUE,
  
  # whole raster layer
  ar<- area(sm[[i]], na.rm=TRUE)
  
  # size of the cell in km2
  ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
  # Total area
  ar2 
  
  area_present <- sum(values(area(pres, na.rm=TRUE)), na.rm=TRUE)
  
  area_absences<- sum(values(area(absences, na.rm=TRUE)), na.rm=TRUE)
  
  # Almost equal, so estimate is good enough
  sum(area_present,area_absences)==ar2
  round((area_present + area_absences), digits = 2) -  round(ar2, digits=2)
  
  # Now crossing with protected areas and presences
  
  zonalpres<- data.frame(zonal(area(pres, na.rm=TRUE), pal[[i]], fun='sum') )
  
  colnames(zonalpres) <- c('country', 'Area sqkm')
  
  # Checking 
  sum(zonalpres$`Area sqkm`) - area_present
  
  # Area of absence per country

  zonalpres$codef <- as.factor(zonalpres$`country`)
  
  zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
  # country code based on ISO country code
  zonaltidy <-zonalpres %>%  mutate(codef = fct_recode(codef,
                                                       "Afghanistan"="192",
                                                       "India"="194",
                                                       "Iran"="195",
                                                       "Nepal"="198",
                                                       "Pakistan"="200",
                                                       "Sri Lanka"="204",
                                                       "Tajikistan"="205",
                                                       "Kazakhstan"="211",
                                                       "Kyrgyzstan"="212",
                                                       "Uzbekistan"="213",
                                                       "Indonesia"="216",
                                                       "Cambodia"="226",
                                                       "Laos"="227",
                                                       "Malaysia"="228",
                                                       "Myanmar"="229",
                                                       "Thailand"="231",
                                                       "Vietnam"="232",
                                                       "Bangladesh"="233",
                                                       "Bhutan"="234",
                                                       "China"="235",
                                                       "Brunei Darussalam"="236",
                                                       "South Korea"="238",
                                                       "Mongolia"="239",
                                                       "North Korea"="240")) %>% 
    arrange(desc(-`country`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  # Crossing with absences 
  # Now crossing with protected areas and ABSENCES
  zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), pal[[i]], fun='sum') )
  
  colnames(zonalabsent) <- c('country', 'Area sqkm')
  
  # Checking 
  sum(zonalabsent$`Area sqkm`) - area_absences
  
  # Area of absence per country
  # using ISO country code 
  
  zonalabsent$codef <- as.factor(zonalabsent$`country`)
  
  zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
  zonaltidya<-zonalabsent %>%  mutate(codef = fct_recode(codef,
                                                         "Afghanistan"="192",
                                                         "India"="194",
                                                         "Iran"="195",
                                                         "Nepal"="198",
                                                         "Pakistan"="200",
                                                         "Sri Lanka"="204",
                                                         "Tajikistan"="205",
                                                         "Kazakhstan"="211",
                                                         "Kyrgyzstan"="212",
                                                         "Uzbekistan"="213",
                                                         "Indonesia"="216",
                                                         "Cambodia"="226",
                                                         "Laos"="227",
                                                         "Malaysia"="228",
                                                         "Myanmar"="229",
                                                         "Thailand"="231",
                                                         "Vietnam"="232",
                                                         "Bangladesh"="233",
                                                         "Bhutan"="234",
                                                         "China"="235",
                                                         "Brunei Darussalam"="236",
                                                         "South Korea"="238",
                                                         "Mongolia"="239",
                                                         "North Korea"="240")) %>% 
    arrange(desc(-`country`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  zonalboth[[i]]<- rbind(zonaltidy, zonaltidya) 
  str(zonalboth[[i]])
  # Checking small divergence due to distortion
  check[[i]]<-sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
  # calculate total amount of area inside protected areas (km^2)
  zonlist[[i]] <-
    zonalboth[[i]]%>%
    as.data.frame()%>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
  
  #write.xlsx(zonlist[[i]],paste0('table_area_country_ssa_',nam[[i]],'.xlsx'), row.names = FALSE)
  
}

#write.xlsx(df,paste0('table_area_country_ssa_allbov.xlsx'), row.names = FALSE)

# Crop to Thailand --------------

# Calculate suitable area in Thailand from the best model

# Import Thai PA (CRS = WGS84) 
pa_th <- raster("./data_preparation/PA_and_country/PA_la.tif")
plot(pa_th)

pa_th <- pa_th %>% 
  raster::crop(tha) %>% 
  raster::mask(tha)

plot(pa_th)

#> import the best binary models -----
rbg <- raster("./result_sample/binary/bg_la_null_bin.tif")  #gaur
rbj <- raster("./result_sample/binary/bj_la_obr_bin.tif")   #banteng
rba <- raster("./result_sample/binary/ba_ssa_obr_bin.tif")  #buffalo
rcs <- raster("./result_sample/binary/cs_ssa_null_bin.tif") #serow  
rng <- raster("./result_sample/binary/ng_ssa_obr_bin.tif")  #goral; use ng ssa it predict suitable area in Thailand

plot(rng)

# Add NA to fix extent of a goral map 
# Correcting empty values for chinese goral area filling with zero
rng <- projectRaster(rng, rbg,method = 'ngb')
rng[is.na(rng[])] <- 0

rs <- list(rbg, rbj, rba, rcs, rng)
cr <- list()

for ( i in 1:length(rs)) {
  
  names(rs)[i]
  
  cr[[i]] <- rs[[i]] %>% 
    raster::crop(tha) %>% 
    raster::mask(tha)
}

cr

# Correct a projection
cr[[4]] <- projectRaster(cr[[4]], cr[[1]],method = 'ngb')

tlist <- raster::stack(cr)

plot(tlist)

names(tlist) <- c("Gaur",
                  "Banteng",           
                  "Wild water buffalo", 
                  "Mainland serow",    
                  "Chinese goral")
dim(pa_th) == dim(tlist[[1]])

# Create list
zonlist <- list()
zonalboth <- list()
check <- list()

nam <- c("Gaur", "Banteng", "Buffalo", "Serow", "Goral")

#result <- "./result_sample"

# Calculate suitable areas

for (i in 1:length(tlist)) {
  
  names(tlist)[i]
  values(tlist[[i]])[values(tlist[[i]]) > 0] = 1
  
  # Now croping by presence or absence 
  pres <- tlist[[i]]
  
  unique(values(pres))
  
  values(pres)[values(pres) < 1] = NA
  
  absences <- tlist[[i]]
  values(absences)[values(absences) > 0] = NA
  
  plot(absences,col='red')
  
  plot(pres, col='green') # add=TRUE,
  
  # Whole raster layer
  ar<- area(tlist[[i]], na.rm=TRUE)
  
  # Size of the cell in km2
  ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
  # Total area
  ar2 
  
  area_present <- sum(values(area(pres, na.rm=TRUE)), na.rm=TRUE)
  
  area_absences<- sum(values(area(absences, na.rm=TRUE)), na.rm=TRUE)
  
  # Almost equal, so estimate is good enough
  sum(area_present,area_absences)==ar2
  round((area_present + area_absences), digits = 2) -  round(ar2, digits=2)
  
  # Now crossing with protected areas and presences
  
  zonalpres<- data.frame(zonal(area(pres, na.rm=TRUE), pa_th, fun='sum'))
  
  colnames(zonalpres) <- c('Protected area code', 'Area sqkm')
  
  # Checking 
  sum(zonalpres$`Area sqkm`) - area_present
  
  # Area of presence per UC type
  # 8 == unprotected
  
  zonalpres$codef <- as.factor(zonalpres$`Protected area code`)
  
  zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
  zonaltidy <-zonalpres %>%  mutate(label = fct_recode(codef,
                                                       "IUCN PA category Iab" = "1" ,
                                                       "IUCN PA category II" ="2" ,
                                                       "IUCN PA category III" = "3"  ,
                                                       "IUCN PA category IV" = "4"  ,
                                                       "IUCN PA category V" = "5",
                                                       "IUCN PA category VI" ="6"  ,
                                                       "Not Applicable"="7",
                                                       "Not protected" = "8")) %>% 
    arrange(desc(-`Protected area code`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2)) 
  
  # Crossing with absences now
  
  # Now crossing with protected areas and ABSENCES
  
  zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), pa_th, fun='sum'))
  
  colnames(zonalabsent) <- c('Protected area code', 'Area sqkm')
  
  # Checking 
  sum(zonalabsent$`Area sqkm`) - area_absences
  
  # Area of absence per UC type
  # 8 == unprotected
  
  zonalabsent$codef <- as.factor(zonalabsent$`Protected area code`)
  
  zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
  zonaltidya<-zonalabsent %>%  mutate(label = fct_recode(codef,
                                                         "IUCN PA category Iab" = "1" ,
                                                         "IUCN PA category II" ="2" ,
                                                         "IUCN PA category III" = "3"  ,
                                                         "IUCN PA category IV" = "4"  ,
                                                         "IUCN PA category V" = "5",
                                                         "IUCN PA category VI" ="6"  ,
                                                         "Not Applicable"="7",
                                                         "Not protected" = "8")) %>% 
    arrange(desc(-`Protected area code`)) %>%
    as.data.frame() %>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100),digits=2))
  
  zonalboth[[i]] <- rbind(zonaltidy, zonaltidya) 
  str(zonalboth[[i]])
  
  # Checking small divergence due to distortion
  check[[i]] <- sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
  # Calculate total amount of area inside protected areas (km^2)
  zonlist[[i]] <-
    zonalboth[[i]] %>%
    as.data.frame() %>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
  
  # Exporting 
  # write.xlsx(zonlist[[i]],paste0('table_area_thai_null_', nam[[i]],'.xlsx'), row.names = FALSE)
  
}  

# Combine dataframe
df_thai <- bind_rows(zonlist)
str(df_thai)

write.xlsx(df_thai, file = "table_area_all_Thai_best.xlsx", row.names = FALSE)

# Calculate suitable area in  Thai forest (V.2019) -----

# Import forest raster 

forest <- raster("./data_preparation/PA_and_country/forestthai61.tif")

table(values(forest))

forest

res(forest) == res(tlist[[1]])

forest <- resample(forest, tlist[[1]], method = "ngb")  

plot(forest)

#create list
zonlist <- list()
zonalboth <- list()
check <- list()

nam<-c("Gaur", "Banteng", "Buffalo", "Serow", "Goral")

for (i in 1:length(tlist)){
  
  names(tlist)[i]
  values(tlist[[i]])[values(tlist[[i]]) > 0] = 1
  
  # Now croping by presence or absence 
  pres <- tlist[[i]]
  
  unique(values(pres) )
  
  values(pres)[values(pres) < 1] = NA
  
  absences <- tlist[[i]]
  values(absences)[values(absences) > 0] = NA
  
  plot(absences,col='red')
  
  plot(pres, col='green')# add=TRUE,
  
  # whole raster layer
  ar<- area(tlist[[i]], na.rm=TRUE)
  
  # size of the cell in km2
  ar2 <- sum(values(ar), na.rm = TRUE) ### applies a correction for latitude, to km2
  
  # Total area
  ar2 
  
  area_present <- sum(values(area(pres, na.rm = TRUE)), na.rm = TRUE)
  
  area_absences <- sum(values(area(absences, na.rm = TRUE)), na.rm = TRUE)
  
  # Almost equal, so estimate is good enough
  sum(area_present,area_absences)==ar2
  round((area_present + area_absences), digits = 2) -  round(ar2, digits = 2)
  
  # Now crossing with protected areas and presences
  
  zonalpres<- data.frame(zonal(area(pres, na.rm = TRUE), forest, fun = 'sum'))
  
  colnames(zonalpres) <- c('land', 'Area sqkm')
  
  # Checking 
  sum(zonalpres$`Area sqkm`) - area_present
  
  # Area of presence per UC type
  # 8 == unprotected
  
  zonalpres$codef <- as.factor(zonalpres$land)
  
  zonalpres$sp_condition <- paste0(nam[[i]],'_present')
  
  zonaltidy <-zonalpres %>%  mutate(label = fct_recode(codef,
                                                       "Moist Evergreen Forest"	=	"111",
                                                       "Dry Evergreen Forest" =	"112",
                                                       "Montane Forest"	=	"113",
                                                       "Pine Forest"	=	"114",
                                                       "Peat Swamp Forest" =	"115",
                                                       "Mangrove Forest"	=	 "116",
                                                       "Freshwater Swamp Forest"	=	"117",
                                                       "Beach Forest" 	=	"118",
                                                       "Mixed Deciduous Forest" = "121", 
                                                       "Dry Dipterocarp Forest"	=	"122",
                                                       "Bamboo Forest" = "123",
                                                       "Teak Plantation" = "131",
                                                       "Other Plantations" = "133",
                                                       "Secondary Forest" = "140",
                                                       "Savanna" = "410",
                                                       "Vegetation on Pen Rock Platform" = "740",
                                                       "Non-forest Area" = "900")) %>% 
    arrange(desc(-`land`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`)*100),digits=2))
  
  # Crossing with absences now
  
  # Now crossing with protected areas and ABSENCES
  
  zonalabsent <- data.frame(zonal(area(absences, na.rm=TRUE), forest, fun='sum'))
  
  colnames(zonalabsent) <- c('land', 'Area sqkm')
  
  # Checking 
  sum(zonalabsent$`Area sqkm`) - area_absences
  
  # Area of absence
  
  zonalabsent$codef <- as.factor(zonalabsent$`land`)
  
  zonalabsent$sp_condition <- paste0(nam[[i]],'_absent')
  
  zonaltidya<-zonalabsent %>%  mutate(label = fct_recode(codef,
                                                         "Moist Evergreen Forest"	=	"111",
                                                         "Dry Evergreen Forest" =	"112",
                                                         "Montane Forest"	=	"113",
                                                         "Pine Forest"	=	"114",
                                                         "Peat Swamp Forest" =	"115",
                                                         "Mangrove Forest"	=	 "116",
                                                         "Freshwater Swamp Forest"	=	"117",
                                                         "Beach Forest" 	=	"118",
                                                         "Mixed Deciduous Forest" = "121", 
                                                         "Dry Dipterocarp Forest"	=	"122",
                                                         "Bamboo Forest" = "123",
                                                         "Teak Plantation" = "131",
                                                         "Other Plantations" = "133",
                                                         "Secondary Forest" = "140",
                                                         "Savanna" = "410",
                                                         "Vegetation on Pen Rock Platform" = "740",
                                                         "Non-forest Area" = "900")) %>% 
    arrange(desc(-`land`))%>%
    as.data.frame()%>%
    mutate(percentage_sep = round((`Area sqkm`/ sum(`Area sqkm`) * 100), digits=2))
  
  zonalboth[[i]]<- rbind(zonaltidy, zonaltidya) 
  str(zonalboth[[i]])
  
  # Checking small divergence due to distortion
  check[[i]]<-sum(zonalboth[[i]]$`Area sqkm`) - ar2
  
  # calculate total amount of area inside protected areas (km^2)
  zonlist[[i]] <-
    zonalboth[[i]]%>%
    as.data.frame()%>%
    mutate(percentage_tot = (`Area sqkm`/ sum(`Area sqkm`) * 100)) 
  
}

# Exporting 
df_forest <- bind_rows(zonlist)
str(df_forest) 

write.xlsx(df_forest, 'table_area_Thai_forest_v2019.xlsx', row.names = FALSE)

# END #
