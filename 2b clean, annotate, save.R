library(tidyverse)
source(file = "2a Functions.R") #should be cluster ready with this

############## Liste Tibbles Rawdata ##############
df_list <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
  map(function(x){
    read_rds(x)
  })

############## from df_list create cleaned, mapped, analyzed tibbles for each industry ##############
map(df_list, function(x){
  
  branche <- x$branchengruppe[1]
  print(branche)
  
  df <- clean(x) %>%
    mapamr() %>%
    add_popamr() %>%
    textual_complexity_measures()
  
  #ergebnis df pro branche in finaldata abspeichern
  branche <- str_replace_all(branche, "/ ", "")
  saveRDS(df, file = paste0("/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/1_cleaned_mapped_analyzed/", branche, ".rds"))
  
})


############## One dataframe ##############
df <- list.files(path = "/home/sc.uni-leipzig.de/cu780nfaa/rstudio01/BAA/Data/1_cleaned_mapped_analyzed/", 
                    pattern = "*.rds*", 
                    full.names = T) %>% 
  
  map_df(function(x){
    
    df <- read_rds(x)
    return(df)
  })


saveRDS(df, file = paste0("Data/", "final_each", Sys.time() ,".rds"))
