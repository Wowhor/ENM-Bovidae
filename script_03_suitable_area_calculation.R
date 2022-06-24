####### Suitable area calculation for five species of wild Bovidae ########
# gaur, banteng, wild water buffalo, serow, goral

# Using the binary maps of the best model from both large accessibe areas (LA) and species specific accessible area (SSA) raster files (.tif)
# 10 rasters in the total (LA = 5 rasters and SSA = 5 rasters for each species)

# The binary maps were contained binary data
#  0 = unsuitable area or 1 = suitable area

# The values of binary map will be used to calculate the suitable and unsuitable areas that overlapped with:
# 1. IUCN protected area categoty (1-7) and non-protected area (8) baed on WDPA polygons: Reference https://wdpa.s3-eu-west-1.amazonaws.com/WDPA_Manual/English/WDPA_WDOECM_Manual_1_6.pdf (page 49; IUCN Management Category)
# 2. countries in the study areas
# Using Zonal function in the package raster 

#load packages
library(rgdal)
library(raster)
library(tidyverse)
library(xlsx)
library(sf) 
library(sp)

rm(list = ls())

# set up working directory
path <- "/bovidae_enm/result_sample/binary"
setwd(path)

#create result directory
result<-"/bovidae_enm/cal_result"
dir.create(result)

#########binary map & PAs######### 

# LA Models --------------------
# list of LA the best models
biglist <- list.files(path = path, pattern = "la")

# import LA raster using lapply
big <- lapply(biglist, raster)
big

# import Protected Area WDPA for LA (CRS = WGS84) [These files were from polygons to rasters using rasterize]
# file name: "PA_la.tif"

pal<- raster ("/bovidae_enm/data_preparation/PA_and_country/PA_la.tif")

#check resolution: should be true
res(pal)==res(big[[1]])

#create list
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Buffalo","Gaur","Banteng","Serow","Goral")

# set up result directory
setwd(result)


# LA Zonal statistic calculation for binary map & PAs--------
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
  
  # Exporting 
  write.xlsx(zonlist[[i]],paste0('table_area_la_',nam[[i]],'.xlsx'), row.names = FALSE)
  
  df<-bind_rows(zonlist)
  str(df)
  write.xlsx(df,paste0('table_area_la_allbov.xlsx'), row.names = FALSE)
  
}


# SSA Models --------------------
path <- "/bovidae_enm/result_sample/binary"
setwd(path)

#list of SSA the best models
sl <- list.files(path = path, pattern = "ssa")
sl

# import SSA raster using lapply
sm <- lapply(sl, raster)
sm

# import Protected Area WDPA for SSA (CRS = WGS84) [These files were from polygons to rasters using rasterize]
# 5 Protected Area raster (CRS = WGS84)]
# path2 <- "/Users/whorpien/Library/CloudStorage/OneDrive-MasseyUniversity/R/1working/area_calculation/PA"

path2 <- "bovidae_enm/data_preparation/PA_and_country"
setwd(path2)
palist <- intersect(list.files(path = path2, pattern = "_ssa"),
                    list.files(path = path2, pattern = "PA"))
palist

# import SSA rasters using lapply
pal <- lapply(palist, raster)
pal

#create list
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Buffalo","Gaur","Banteng","Serow","Goral")

# set up result directory
setwd(result)

# SSA Zonal statistic calculation binary map & PAs ----------
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

#########binary map & country######### 
# LA Models -------------
path <- "/bovidae_enm/result_sample/binary"
setwd(path)

#list of LA the best models
biglist <- list.files(path = path, pattern = "la")

# import LA raster using lapply
big <- lapply(biglist, raster)
big


#import country raster
lm<- raster("/bovidae_enm/data_preparation/PA_and_country/country_la.tif")

res(lm==res(big[[1]]))

#create list
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Gaur","Banteng","Buffalo","Serow","Goral")

# set up result directory
setwd(result) 

# LA Zonal statistic calculation binary map & country --------
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
  
  
  write.xlsx(zonlist[[i]],paste0('table_area_country_la_',nam[[i]],'.xlsx'), row.names = FALSE)
  
  df<-bind_rows(zonlist)
  str(df)
  write.xlsx(df,paste0('table_area_country_la_allbov.xlsx'), row.names = FALSE)
  
}

# SSA Models -------------- 
path <- "/bovidae_enm/resul_sample/binary"
setwd(path)

#list of SSA the best models
smalllist <- list.files(path = path, pattern = "ssa")
smalllist

# import SSA raster using lapply
sm <- lapply(smalllist, raster)
sm

# import country raster
path2 <- "bovidae_enm/data_preparation/PA_and_country"
setwd(path2)

cl <- intersect(list.files(path = path2, pattern = "country"),
                list.files(path = path2, pattern = "ssa"))
cl

pal <- lapply(cl, raster)
pal

#create list
zonlist<-list()
zonalboth<-list()
check<-list()

nam<-c("Buffalo","Gaur","Banteng","Serow","Goral")

# set up result directory
setwd(result) 

# SSA Zonal statistic calculation binary map & country --------
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
  
  write.xlsx(zonlist[[i]],paste0('table_area_country_ssa_',nam[[i]],'.xlsx'), row.names = FALSE)
  
  df<-bind_rows(zonlist)
  str(df)
  write.xlsx(df,paste0('table_area_country_ssa_allbov.xlsx'), row.names = FALSE)
  
}
