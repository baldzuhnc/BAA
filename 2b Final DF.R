library(tidyverse)

############## Ergebnistibble Liste ##############
df_list <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
  map(function(x){
    read_rds(x)
  })

############## ResultDFs ##############

map(df_list, function(x){
  
  branche <- x$branchengruppe[1]
  print(branche)
  
  df <- clean(x) %>%
    mapamr() %>%
    add_popamr() %>%
    textual_complexity_measures()
    
#    group_by(AMR) %>%
#    summarize(n = n(), #rename zu n_anzeigen considern
#              mean_chars = mean(chars, na.rm = T),
#              mean_tok = mean(tokens, na.rm = T),
#             mean_types = mean(types, na.rm = T),
#              mean_TTR = mean(TTR, na.rm = T),
#              mean_slenth = mean(meanSentenceLength, na.rm = T),
#              mean_syl = mean(meanWordSyllables, na.rm = T),
#              mean_pr_noun = mean(pr_noun, na.rm = T),
#              mean_entropy = mean(entropy, na.rm = T),
#              density = mean(dichte),
#              population = mean(bevoelkerung)) %>%
#    mutate(industry = branche) %>%
#    relocate(industry, .after = AMR)
  
  
  #ergebnis df pro branche in finaldata abspeichern
  branche <- str_replace_all(branche, "/ ", "")
  saveRDS(df, file = paste0("/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Final_2/", branche, ".rds"))
  
})


############## Final DF from ResultDFs ##############

adata <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Final_2/", 
                    pattern = "*.rds*", 
                    full.names = T) %>% 
  
  map_df(function(x){
    
    df <- read_rds(x)
    return(df)
  })


saveRDS(adata, file = paste0("Data/", "final_each", Sys.time() ,".rds"))

test <- readRDS("Data/final_2022-05-25 10:41:28.rds")
