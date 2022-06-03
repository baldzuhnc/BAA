library(tidyverse)
library(spacyr)
library(quanteda)
library(quanteda.textstats)

#Spacy initialization
spacy_initialize(model = "de_core_news_sm")
#spacy_finalize()

#Texts
b19 <- readRDS("Rawdata/B19, Papier, Druck, Verpackung.rds") %>%
  slice(1:100) %>%
  clean() %>%
  mutate(ID = 1:nrow(.)) %>%
  relocate(ID)

#Descriptives
adlength <- str_length(b19$Stellenbeschreibung)
boxplot(adlength)
min(adlength)
max(adlength)
mean(adlength)
sd(adlength)

####################################Pipeline############################################################
#function takes cleantibble and returns tibble with 
# complexity measures for each job ad


  
  #Create 2 corpora####
  corp <- corpus(b19$Stellenbeschreibung) #1. complete
  
  corp_trim <- corpus_trim(corp, 
                           what = "documents", 
                           min_ntoken = 101) #2. documents with more than 100 tokens (about 700 characters)
  
  
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
  
  #Create token object from trimmed corpus
  tok_corp_trim <- tokens(corp_trim)
  
  #Create measures, trimmed corpus is used -> less cases
  ttr <- textstat_lexdiv(tok_corp_trim, 
                         measure = c("TTR", "MATTR"), 
                         remove_punct = T,
                         MATTR_window = 100)
  
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
  
  #merge syntactical and lexical (syntact, ttr, pos_proportions)####
  
  final_lexical <- inner_join(syntact, pos_proportions, by = "document") %>%
    relocate(tokens_woSpace, .after = tokens) %>%
    left_join(., ttr, by = "document") %>%
    mutate(ID = as.integer(str_replace(document, "text", ""))) %>%
    relocate(ID) %>%
    select(-document)
    


####################################Analysis############################################################ 
library(psy)



cm <- create_complexity_measure(b19)


glimpse(cm)

#cronbachs alpha -> super low, howver, having in mind that all these measures measure different aspects of complexity this make sense theoretically
cronbach <- cronbach(cm %>% select(chars, sents, meanSentenceLength, meanWordSyllables, pr_noun, MATTR))

#density
d <- density(cm$sents, na.rm = T)
plot(d)




