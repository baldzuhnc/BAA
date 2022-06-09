library(tidyverse)
source(file = "2a Functions.R") #should be cluster ready with this

############## Liste Tibbles Rawdata ##############
df_list <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
  map(function(x){
    read_rds(x) %>% slice(1:10)
  })

############## from df_list create cleaned, mapped, analyzed tibbles for each industry ##############
map(df_list, function(x){
  
  branche <- x$branchengruppe[1]
  print(branche)
  
  #pipeline
  df <- clean(x) %>%
    classify_industry() %>%
    mapamr() %>%
    add_popamr() %>%
    textual_complexity_measures() %>%
    en_ingles()
  
  #ergebnis df pro branche in finaldata abspeichern
  branche <- str_replace_all(branche, "/ ", "")
  saveRDS(df, file = paste0("/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/1d_1withduplicates", branche, ".rds"))
  
})


############## One dataframe ##############
df <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/1d_1withduplicates/", 
                    pattern = "*.rds*", 
                    full.names = T) %>% 
  
  map_df(function(x){
    
    df <- read_rds(x)
    return(df)
  })


saveRDS(df, file = paste0("Data/", "final_each_categorized", Sys.time() ,".rds"))




each <- readRDS("Data/final_each2022-06-03 02:34:19.rds")


