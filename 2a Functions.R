############## Libraries and initialization ##############
library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(tmap)
library(spacyr)
library(quanteda)
library(quanteda.textstats)

#Spacy initialization
spacy_initialize(model = "de_core_news_sm")

############## Funktionen ##############

############## Cleaned den rohen Datensatz ##############

clean <- function(rawdata){
  
  rawdata[rawdata == 0] <- NA
  
  cleantibble <- tibble(ID = c(1:nrow(rawdata)),
                        Titel = rawdata$titel,
                        Beruf = rawdata$beruf,
                        Veroeffentlicht = rawdata$ersteVeroeffentlichungsdatum,
                        Branche = rawdata$branchengruppe,
                        Angebotsart = rawdata$angebotsart,
                        AnzahloffeneStellen = rawdata$anzahlOffeneStellen,
                        Arbeitgeber = rawdata$arbeitgeber,
                        Region = rawdata$arbeitsorte.region,
                        Ort = rawdata$arbeitsorte.ort,
                        PLZ = as.integer(rawdata$arbeitsorte.plz),
                        Lat = parzer::parse_lat(rawdata$arbeitsorte.koordinaten.lat),
                        Lon = parzer::parse_lon(rawdata$arbeitsorte.koordinaten.lon),
                        Befristung = as.integer(rawdata$befristung),
                        Uebernahme = as.logical(rawdata$uebernahme),
                        Verguetung = rawdata$verguetung, 
                        Tarifvertrag = rawdata$tarifvertrag,
                        Arbeitszeit = rawdata$informationenZurArbeitszeit,
                        Stellenbeschreibung = stringi::stri_enc_toutf8(rawdata$stellenbeschreibung),
                        FuerGefluechtete = as.logical(rawdata$fuerFluechtlingeGeeignet),
                        NurFuerSchwerbehinderte = as.logical(rawdata$nurFuerSchwerbehinderte),
                        Staerken = paste(rawdata$staerken1,
                                         rawdata$staerken2,
                                         rawdata$staerken3,
                                         rawdata$staerken4,
                                         rawdata$staerken5, sep = ", "),
                        Sprachkentnisse = paste(rawdata$sprachkenntnisse.1,
                                                rawdata$sprachkenntnisse.4,
                                                sep = ", "),
                        Fuehrerschein = rawdata$mobilitaet.fuehrerscheine.1
                        ) %>% drop_na(Lon) #dropna
  
  return(cleantibble)
  
} 

############## Industry classification ##############

classify_industry <- function(cleantibble){
  
  baa_key <- readxl::read_excel("Data/ISCO/Alphabetisches-Verzeichnis-Berufsbenennungen.xlsx", sheet = 2) %>%
    slice(5:18723) %>%
    rename(Beruf = '...1', baa_key = 'Klassifikation der Berufe 2010 – überarbeitete Fassung 2020')
  
  isco_baa <- readxl::read_excel("Data/ISCO/Umsteigeschluessel-KLDB2020-ISCO08.xlsx", sheet = 2) %>%
    rename("baa_key" = '...1',
           "baa_bezeichnung" = '...2',
           "baa_bezeichnung_eng" = '...3',
           "isco_key" = '...4',
           "isco_bezeichnung" = '...5',
           "unit_group_eng" = '...6') %>%
    slice(5:1530) %>%
    select(baa_key, baa_bezeichnung, baa_bezeichnung_eng, isco_key, unit_group_eng)
  
  
  creative_industries <- c("211", "226", "212", "251", "252", "214", "215", "216", "221", 
                           "213", "225", "221", "225", "226", "231", "232", "234", "235", 
                           "263", "335", "1", "223", "241", "242", "243", "333", "261", "31",
                           "35", "32", "226", "223", "331", "332", "333", "334", "341", 
                           "264", "265", "3431", "343", "342")
  
  cleantibble_key <- merge(cleantibble, baa_key, by = "Beruf", all.x = T)
  
  cleantibble_key_isco <- left_join(cleantibble_key, isco_baa, by = "baa_key") %>%
    mutate(creative = map_int(isco_key, ~any(str_starts(., creative_industries,))),
           occupation_major = case_when(str_starts(isco_key, "1") ~ "Managers",
                                        str_starts(isco_key, "2") ~ "Professionals",
                                        str_starts(isco_key, "3") ~ "Technicians and Associate Professionals",
                                        str_starts(isco_key, "4") ~ "Clerical Support Workers",
                                        str_starts(isco_key, "5") ~ "Services and Sales Workers",
                                        str_starts(isco_key, "6") ~ "Skilled Agricultural,\n Forestry and Fishery Workers",
                                        str_starts(isco_key, "7") ~ "Craft and Related Trades Workers",
                                        str_starts(isco_key, "8") ~ "Plant and Machine Operators,\n Assemblers",
                                        str_starts(isco_key, "9") ~ "Elementary Occupations",
                                        str_starts(isco_key, "0") ~ "Armed Forces Occupations"),
           occupation_submajor = case_when(str_starts(isco_key, "11") ~ "Chief Executives, Senior Officials\n and Legislators",
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
                                           str_starts(isco_key, "03") ~ "Armed Forces Occupations, Other Ranks")
    )
  
  return(cleantibble_key_isco)
}

############## Ordnet jeder Anzeige die Arbeitsmarktregion (AMR) zu, in die sie fällt ##############

mapamr <- function(cleantibble){
  ergebnis <- cleantibble
  
  #Shapefile, einlesen
  amr_sp <- readOGR(dsn = "Shapefiles/Deutschland/amr250/", layer = "AMR250")
  
  #SpatialPointsDataframe aus DF erzeugen
  coordinates(cleantibble) <- c("Lon", "Lat")
  
  #CRS initialisieren
  proj4string(cleantibble) <- CRS(paste("+init=epsg:4326"))
  
  #Cleantibble auf Shapefile transformieren
  cleantibble.trans <- spTransform(cleantibble, CRS = "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs")
  
  #proj4string(amr_sp)
  #proj4string(amr_sp.trans)
  #proj4string(cleantibble.trans)
  #identicalCRS(amr_sp.trans, cleantibble.trans)
  
  #Shapefile auch transformieren, da es sonst irgendwie nicht geht
  amr_sp.trans <- spTransform(amr_sp, CRS  = "+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +towgs84=598.1,73.7,418.2,0.202,0.045,-2.455,6.7 +units=m +no_defs")
  
  #Iteriert über die Koordinaten, ordnet AMR zu und erzeugt Ergebnisvektor der Länge cleantibble
  #identicalCRS(kaufsub.trans, amr_sp.trans
  cleantibble.matches <- over(cleantibble.trans, amr_sp.trans[,"AMR"])
  
  ergebnis <- cbind(ergebnis, cleantibble.matches)
  
  return(ergebnis)
} 


############## Populationsdaten hinzufügen ##############

add_popamr <- function(mapped_cleantibble){
  
  #Population einlesen
  pop_amr <- readxl::read_excel("Shapefiles/Deutschland/11-arbeitsmarktregion.xlsx") %>%
    slice(7:263)
  
  pop_amr <- tibble(SN_AMR = as.integer(pop_amr$`Arbeitsmarktregionen nach Fläche, Bevölkerung und Bevölkerungsdichte`),
                    AMR = pop_amr$...2,
                    bevoelkerung = as.integer(pop_amr$...4),
                    maennlich = as.integer(pop_amr$...5),
                    weiblich = as.integer(pop_amr$...6),
                    dichte = as.integer(pop_amr$...7)) %>%
    select(SN_AMR, AMR, bevoelkerung, maennlich, weiblich, dichte)
  
  
  #inner join nach AMR
  result <- inner_join(mapped_cleantibble, pop_amr, by = "AMR")
  
  return(result)
  
}


############## Komplexitätsmaße hinzufügen ##############

textual_complexity_measures <- function(cleantibble){
  
  #Create corpus####
  corp <- corpus(cleantibble$Stellenbeschreibung)
  
  #Syntactic complexity####
  
  #Average sentence length & average syllables per word (complete corpus)
  avg_LenSyl <- textstat_readability(corp, 
                                     measure = c("meanSentenceLength", "meanWordSyllables" ))
  
  #merge with textstat summary
  syntact <- inner_join(textstat_summary(corp), avg_LenSyl, by = "document") %>% 
    mutate(tokens_woPunct = tokens - puncts) %>%
    relocate(tokens_woPunct, .after = tokens)
  
  
  #Lexical complexity####
  
  ##Type/Toke ratio, Moving-Average Type-Token Ratio####
  
  ##TTR (works with dfm objects)
  corp_trim <- corpus_trim(corp, 
                           what = "documents", 
                           min_ntoken = 59, # mean - 2sd -> 95% of texts
                           max_ntoken = 550) # mean + 2sd -> 95% of texts
  
  tok_corp <- tokens(corp_trim)
  
  ttr <- textstat_lexdiv(tok_corp,
                         measure = c("TTR"), 
                         remove_punct = T)
  
  
  ## ENTROPY ####
  dfm_corp <- dfm(tokens(corp, remove_punct = T, remove_symbols = T, remove_url = T))
  entropy <- textstat_entropy(dfm_corp, margin = "documents", base = 2)
  
  
  ## Pos Ratio ####
  #parse 
  spacyParsed <- spacy_parse(corp, dependency = T)
  
  pos_proportions <- spacyParsed %>% 
    dplyr::filter(pos != "SPACE") %>% #remove spaces as seperators
    group_by(doc_id) %>%
    summarise(tokens_woSpace = n(), #remove maybe, token count
              pr_noun = sum(str_count(pos, "NOUN")/n()),
              pr_verb = sum(str_count(pos, "VERB")/n()),
              pr_adjective = sum(str_count(pos, "ADJ")/n()),
              pr_adverb = sum(str_count(pos, "ADV")/n())
    ) %>%
    rename(document = doc_id)
  
  #merge syntactical and lexical (syntact, ttr, entropy, pos_proportions)####
  
  final_lexical <- inner_join(syntact, pos_proportions, by = "document") %>%
    left_join(., entropy, by = "document") %>%
    relocate(tokens_woSpace, .after = tokens) %>%
    left_join(., ttr, by = "document") %>%
    mutate(ID = as.integer(str_replace(document, "text", ""))) %>%
    relocate(ID) %>%
    select(-document)
  
  
  result <- left_join(cleantibble, final_lexical, by = "ID")
  
  return(result)
  
}

############## en ingles ##############

en_ingles <- function(adata){
  
  #variables
  adata <- adata %>% rename(industry = "Branche")
  
  #industries
  adata$industry[adata$industry == "Abfallwirtschaft, Energieversorgung, Wasserversorgung"] <- "Waste management, energy supply,\n water supply"
  adata$industry[adata$industry == "Banken, Finanzdienstleistungen, Immobilien, Versicherungen"] <- "Banks, financial services,\n real estate, insurance"
  adata$industry[adata$industry == "Bau, Architektur"] <- "Construction, architecture"
  adata$industry[adata$industry == "Bildung, Erziehung, Unterricht"] <- "Education, upbringing, teaching"
  adata$industry[adata$industry == "Chemie, Pharma, Biotechnologie"] <- "Chemistry, pharmaceuticals,\n biotechnology"
  adata$industry[adata$industry == "Einzelhandel, Großhandel, Außenhandel"] <- "Retail, wholesale, foreign Trade"
  adata$industry[adata$industry == "Elektro, Feinmechanik, Optik, Medizintechnik"] <- "Electrical, precision mechanics,\n optics, medical technology"
  adata$industry[adata$industry == "Fahrzeugbau, Fahrzeuginstandhaltung"] <- "Vehicle construction,\n vehicle maintenance"
  adata$industry[adata$industry == "Gesundheit, Soziales"] <- "Health, social sector"
  adata$industry[adata$industry == "Hotel, Gaststätten, Tourismus, Kunst, Kultur, Freizeit"] <- "Hotel, restaurants, tourism,\n art, culture, leisure"
  adata$industry[adata$industry == "IT, Computer, Telekommunikation"] <- "IT, computers, telecommunication"
  adata$industry[adata$industry == "Konsum- und Gebrauchsgüter"] <- "Consumer goods and durables"
  adata$industry[adata$industry == "Landwirtschaft, Forstwirtschaft, Gartenbau"] <- "Agriculture, forestry, horticulture"
  adata$industry[adata$industry == "Logistik, Transport, Verkehr "] <- "Logistics, transport, traffic"
  adata$industry[adata$industry == "Luftfahrttechnik, Raumfahrttechnik"] <- "Aeronautical engineering,\n space technology"
  adata$industry[adata$industry == "Management, Beratung, Recht, Steuern"] <- "Management, consulting, law, taxes"
  adata$industry[adata$industry == "Medien, Informationsdienste"] <- "Media, information services"
  adata$industry[adata$industry == "Metall, Maschinenbau, Anlagenbau"] <- "Metal, mechanical engineering, \nplant engineering"
  adata$industry[adata$industry == "Nahrungs- / Genussmittelherstellung"] <- "Food, luxury food production"
  adata$industry[adata$industry == "Öffentlicher Dienst, Organisationen"] <- "Public service, organisations"
  adata$industry[adata$industry == "Papier, Druck, Verpackung"] <- "Paper, printing, packaging"
  adata$industry[adata$industry == "Rohstoffgewinnung, Rohstoffaufbereitung"] <- "Raw material extraction,\n raw material processing"
  adata$industry[adata$industry == "Rohstoffverarbeitung, Glas, Keramik, Kunststoff, Holz"] <- "Raw materials processing, glass,\n ceramics, plastics, wood"
  adata$industry[adata$industry == "Sicherheits-, Reinigungs-, Reparatur- und weitere Dienstleistungen"] <- "Security, cleaning,\n repair and other services"
  adata$industry[adata$industry == "Werbung, Öffentlichkeitsarbeit"] <- "Advertising, public relations"
  adata$industry[adata$industry == "Wissenschaft, Forschung, Entwicklung"] <- "Science, research,\ development"
  
  return(adata)
}

############## Einzeilige Komplexitätsauswertung #############

#sowie grouped_cleantibble -> mapped_cleantibble nach gruppierung
#Ausgabe: Tabelle (one row) mit Komplexitäts-Proxys und scaling koeffizienten der Branche

complex_beta <- function(mapped_cleantibble, 
                         grouped_cleantibble){
  
  model1 <- lm(formula = log(n_anzeigen) ~ log(bevoelkerung),
               data = grouped_cleantibble)
  
  komplexität <- tibble(Branche = mapped_cleantibble$Branche[1],
                        Scaling_Koeffizient = round(model1$coefficients[2], digits = 2),
                        Anzeigenlaenge_mean = round(mean(str_length(mapped_cleantibble$Stellenbeschreibung)), digits = 2),
                        Englisch_Prozent = round(sum(mapped_cleantibble$Sprachkentnisse %>% str_count("Englisch"))/nrow(mapped_cleantibble)*100, digits = 2),
                        Fuergefluechtete_Prozent = round(sum(mapped_cleantibble$FuerGefluechtete == T)/nrow(mapped_cleantibble)*100, digits = 2),
                        Fuerschwerbehinderte_Prozent = round(sum(mapped_cleantibble$NurFuerSchwerbehinderte == T)/nrow(mapped_cleantibble)*100, digits = 2),
                        Befristet_Prozent = round(sum(mapped_cleantibble$Befristung == 1)/nrow(mapped_cleantibble)*100, digits = 2) 
                        )
  
} 



############## Sandbox ##############





