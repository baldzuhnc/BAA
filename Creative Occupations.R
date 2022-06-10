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
  mutate(creative = map_int(isco_key, ~any(str_starts(., creative_industries))),
         occupation_major = case_when(str_starts(isco_key, "1") ~ "Managers",
                                      str_starts(isco_key, "2") ~ "Professionals",
                                      str_starts(isco_key, "3") ~ "Technicians and Associate Professionals",
                                      str_starts(isco_key, "4") ~ "Clerical Support Workers",
                                      str_starts(isco_key, "5") ~ "Services and Sales Workers",
                                      str_starts(isco_key, "6") ~ "Skilled Agricultural, Forestry and Fishery Workers",
                                      str_starts(isco_key, "7") ~ "Craft and Related Trades Workers",
                                      str_starts(isco_key, "8") ~ "Plant and Machine Operators And Assemblers",
                                      str_starts(isco_key, "9") ~ "Elementary Occupations",
                                      str_starts(isco_key, "0") ~ "Armed Forces Occupations"),
         occupation_submajor = case_when(str_starts(isco_key, "11") ~ "Chief Executives, Senior Officials and Legislators",
                                         str_starts(isco_key, "12") ~ "Administrative and Commercial Managers",
                                         str_starts(isco_key, "13") ~ "Production and Specialized Services Managers",
                                         str_starts(isco_key, "14") ~ "Hospitality, Retail and Other Services Managers",
                                         str_starts(isco_key, "21") ~ "Science and Engineering Professionals",
                                         str_starts(isco_key, "22") ~ "Health Professionals",
                                         str_starts(isco_key, "23") ~ "Teaching Professionals",
                                         str_starts(isco_key, "24") ~ "Business and Administration Professionals",
                                         str_starts(isco_key, "25") ~ "Information and Communications Technology Professionals",
                                         str_starts(isco_key, "26") ~ "Legal, Social and Cultural Professionals",
                                         str_starts(isco_key, "31") ~ "Science and Engineering Associate Professionals",
                                         str_starts(isco_key, "32") ~ "Health Associate Professionals",
                                         str_starts(isco_key, "33") ~ "Business and Administration Associate Professionals",
                                         str_starts(isco_key, "34") ~ "Legal, Social, Cultural and Related Associate Professionals",
                                         str_starts(isco_key, "35") ~ "Information and Communications Technicians",
                                         str_starts(isco_key, "41") ~ "General and Keyboard Clerks",
                                         str_starts(isco_key, "42") ~ "Customer Services Clerks",
                                         str_starts(isco_key, "43") ~ "Numerical and Material Recording Clerks",
                                         str_starts(isco_key, "44") ~ "Other Clerical Support Workers",
                                         str_starts(isco_key, "51") ~ "Personal Services Workers",
                                         str_starts(isco_key, "52") ~ "Sales Workers",
                                         str_starts(isco_key, "53") ~ "Personal Care Workers",
                                         str_starts(isco_key, "54") ~ "Protective Services Workers",
                                         str_starts(isco_key, "61") ~ "Market-oriented Skilled Agricultural Workers",
                                         str_starts(isco_key, "62") ~ "Market-oriented Skilled Forestry, Fishery and Hunting Workers",
                                         str_starts(isco_key, "63") ~ "Subsistence Farmers, Fishers, Hunters and Gatherers",
                                         str_starts(isco_key, "71") ~ "Building and Related Trades Workers (excluding Electricians)",
                                         str_starts(isco_key, "72") ~ "Metal, Machinery and Related Trades Workers",
                                         str_starts(isco_key, "73") ~ "Handicraft and Printing Workers",
                                         str_starts(isco_key, "74") ~ "Electrical and Electronic Trades Workers",
                                         str_starts(isco_key, "75") ~ "Food Processing, Woodworking, Garment and Other Craft and Related Trades Workers",
                                         str_starts(isco_key, "81") ~ "Stationary Plant and Machine Operators",
                                         str_starts(isco_key, "82") ~ "Assemblers",
                                         str_starts(isco_key, "83") ~ "Drivers and Mobile Plant Operators",
                                         str_starts(isco_key, "91") ~ "Cleaners and Helpers",
                                         str_starts(isco_key, "92") ~ "Agricultural, Forestry and Fishery Labourers",
                                         str_starts(isco_key, "93") ~ "Labourers in Mining, Construction, Manufacturing and Transport",
                                         str_starts(isco_key, "94") ~ "Food Preparation Assistants",
                                         str_starts(isco_key, "95") ~ "Street and Related Sales and Services Workers",
                                         str_starts(isco_key, "96") ~ "Refuse Workers and Other Elementary Workers",
                                         str_starts(isco_key, "01") ~ "Commissioned Armed Forces Officers",
                                         str_starts(isco_key, "02") ~ "Non-commissioned Armed Forces Officers",
                                         str_starts(isco_key, "03") ~ "Armed Forces Occupations, Other Ranks"
                                      )) %>%
  relocate(creative, occupation_major, occupation_submajor, isco_key, baa_key)



