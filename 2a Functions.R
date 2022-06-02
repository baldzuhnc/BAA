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
                        ) %>% drop_na(Lon)
  
  #cleantibble$Stellenbeschreibung <- cleantibble$Stellenbeschreibung %>%
  #  str_to_lower() %>%
  #  str_replace_all("[^[:alnum:] ]", " ") %>%
  #  str_squish()
  
  return(cleantibble)
  
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
#function takes datatable and returns tibble with complexity measures for each job ad


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





########## OLD VERSIONS ###############

#uunötig

group_amr <- function(mapped_cleantibble){
  
  #Population einlesen
  pop_amr <- readxl::read_excel("Shapefiles/11-arbeitsmarktregion.xlsx") %>%
    slice(7:263)
  
  pop_amr <- tibble(SN_AMR = as.integer(pop_amr$`Arbeitsmarktregionen nach Fläche, Bevölkerung und Bevölkerungsdichte`),
                    AMR = pop_amr$...2,
                    bevoelkerung = as.integer(pop_amr$...4),
                    maennlich = as.integer(pop_amr$...5),
                    weiblich = as.integer(pop_amr$...6),
                    dichte = as.integer(pop_amr$...7)) %>%
    select(SN_AMR, AMR, bevoelkerung, maennlich, weiblich, dichte)
  
  #Nach AMR gruppieren und mit Populationsdaten verbinden
  gruppiert_pop <- mapped_cleantibble %>% 
    group_by(AMR) %>% 
    summarise(n_anzeigen = n()) %>%
    inner_join(pop_amr, by = "AMR")
  
  return(gruppiert_pop)
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





