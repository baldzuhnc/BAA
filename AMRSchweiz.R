library(tidyverse)
library(rgdal)
library(sp)

schweiz <- haven::read_sav(file = "Shapefiles/Schweiz/669_SMM_Data_SUF_spss_EN_v6.0.0.sav") %>% 
  dplyr::filter(country == 0, year >= 2016) #nur anzeigen aus der schweiz 



rawdata <- df_list <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
  map_df(function(x){
    read_rds(x)
  })

min(rawdata$aktuelleVeroeffentlichungsdatum)
max(rawdata$aktuelleVeroeffentlichungsdatum)

test <- c(as.numeric(rawdata$anzahlOffeneStellen))
table(test)

viele <- rawdata %>% dplyr::filter(anzahlOffeneStellen == "218")
