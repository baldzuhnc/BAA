library(tidyverse)

archi <- adata_each %>% dplyr::filter(industry == "Construction, architecture") %>%
  slice(1:15)

baa_key <- readxl::read_excel("Data/ISCO/Alphabetisches-Verzeichnis-Berufsbenennungen.xlsx", sheet = 2) %>%
  slice(5:18723) %>%
  rename(Beruf = '...1', baa_key = 'Klassifikation der Berufe 2010 – überarbeitete Fassung 2020') %>%
  glimpse()

isco_baa <- readxl::read_excel("Data/ISCO/Umsteigeschluessel-KLDB2020-ISCO08.xlsx", sheet = 2) %>%
  rename("baa_key" = '...1',
         "baa_bezeichnung" = '...2',
         "baa_bezeichnung_eng" = '...3',
         "isco_key" = '...4',
        "isco_bezeichnung" = '...5',
        "unit_group_eng" = '...6') %>%
  slice(5:1530) %>%
  select(baa_key, baa_bezeichnung, baa_bezeichnung_eng, isco_key, unit_group_eng) %>%
  glimpse()
  
  
archi_key <- merge(archi, baa_key, by = "Beruf", all.x = T)
archi_key_isco <- left_join(archi_key, isco_baa, by = "baa_key")


creative_industries <- c("211", "226", "212", "251", "252", "214", "215", "216", "221", 
                         "213", "225", "221", "225", "226", "231", "232", "234", "235", 
                         "263", "335", "1", "223", "241", "242", "243", "333", "261", "31",
                         "35", "32", "226", "223", "331", "332", "333", "334", "341", 
                          "264", "265", "3431", "343", "342")


archi_key_isco <- archi_key_isco %>% 
  mutate(creative = map_int(isco_key, ~any(str_starts(., creative_industries)))) %>%
  relocate(creative, isco_key)



