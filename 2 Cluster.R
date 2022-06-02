#Script für den Cluster

############## Libraries and initialization ##############
library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(tmap)
library(spacyr)
library(quanteda)
library(quanteda.textstats)

#Spacy initialization with correct model
spacy_initialize(model = "de_core_news_sm")

############## Funktionen ##############

clean <- function(rawdata){
  
  rawdata[rawdata == 0] <- NA
  
  cleantibble <- tibble(ID = c(1:nrow(rawdata)),
                        Titel = rawdata$titel,
                        Beruf = rawdata$beruf,
                        Branche = rawdata$branchengruppe,
                        Angebotsart = rawdata$angebotsart,
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

textual_complexity_measures <- function(cleantibble){
  
  #Create corpus####
  corp <- corpus(cleantibble$Stellenbeschreibung) #1. complete
  
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
  
  
  ### ENTROPY ####
  dfm_corp <- dfm(tokens(corp, remove_punct = T, remove_symbols = T, remove_url = T))
  entropy <- textstat_entropy(dfm_corp, margin = "documents", base = 2)
  
  
  ##Pos Ratio####
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


############## RUN ##############

############## Ergebnistibble Liste ##############
df_list <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
  map(function(x){
    read_rds(x)
  })

############## ResultDFs ##############

map(df_list, function(x){
  
  branche <- x$branchengruppe[1]
  print(branche)
  
  df <- slice(x, 1:5) %>% clean() %>%
    mapamr() %>%
    add_popamr() %>%
    textual_complexity_measures() %>% 
    group_by(AMR) %>%
    summarize(n = n(),
              mean_chars = mean(chars, na.rm = T),
              mean_tok = mean(tokens, na.rm = T),
              mean_types = mean(types, na.rm = T),
              mean_TTR = mean(TTR, na.rm = T),
              mean_slenth = mean(meanSentenceLength, na.rm = T),
              mean_syl = mean(meanWordSyllables, na.rm = T),
              mean_pr_noun = mean(pr_noun, na.rm = T),
              mean_entropy = mean(entropy, na.rm = T),
              dichte = mean(dichte),
              bevoelkerung = mean(bevoelkerung)) %>%
    mutate(branche = branche) %>%
    relocate(branche, .after = AMR)
  
  
  #ergebnis df pro branche in finaldata abspeichern
  branche <- str_replace_all(branche, "/ ", "")
  saveRDS(df, file = paste0("/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Testfolder/", branche, ".rds"))
  
})


############## Final DF from ResultDFs ##############

adata <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Finaldata//", 
                    pattern = "*.rds*", 
                    full.names = T) %>% 
  
  map_df(function(x){
    df <- read_rds(x)
    
    return(df)
  })


saveRDS(adata, file = paste0("Data/", "final_", Sys.time() ,"rds"))






