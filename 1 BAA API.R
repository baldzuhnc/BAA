library(tidyverse)
library(httr)
library(jsonlite)
library(progressr)

#Handlers für Progressbar, einmal aktivieren ####
handlers(global = TRUE)
handlers("rstudio")

get_token <- function(){
  
  cookies <- c('JSESSIONID' = 'DF392405E8F00714FBFE17EDAB5DDD98')
  
  headers <- c(
    `Host` = 'api-con.arbeitsagentur.de',
    `Accept` = '*/*',
    `Content-Type` = 'application/x-www-form-urlencoded; charset=utf-8',
    `Accept-Language` = 'en-us',
    `User-Agent` = 'Jobsuche/1070 CFNetwork/1220.1 Darwin/20.3.0'
  )
  
  data <- 'client_id=c003a37f-024f-462a-b36d-b001be4cd24a&client_secret=32a39620-32b3-4307-9aa1-511e3d7f48a8&grant_type=client_credentials'
  
  res <- POST(url = 'https://api-con.arbeitsagentur.de/oauth/gettoken_cc', 
              add_headers(.headers=headers), 
              set_cookies(.cookies = cookies), 
              body = data)
  
  token <- content(res) %>% 
    .$access_token
  
  return(token)
}


#Funktionen ####
get_refnrs <- function(was = NULL,
                       wo = NULL,
                       branche = NULL,
                       berufsfeld = NULL,
                       page = NULL, 
                       size = NULL, 
                       arbeitgeber = NULL,
                       veroeffentlichtseit = NULL,
                       zeitarbeit = NULL,
                       angebotsart = NULL,
                       befristung = NULL,
                       arbeitszeit = NULL,
                       behinderung = NULL,
                       corona = NULL,
                       umkreis = NULL,
                       sleep = NULL){
  
  #create list of queries
  urls <- list()
  
  for (i in page) {
    urls <-paste0("https://rest.arbeitsagentur.de/jobboerse/jobsuche-service/pc/v4/app/jobs?",
                  if (!is.null(was)){paste0("&was=", was)}, 
                  if (!is.null(wo)){paste0("&wo=", wo)},
                  if (!is.null(branche)){paste0("&branche=", branche)},
                  if (!is.null(berufsfeld)){paste0("&berufsfeld=", berufsfeld)},
                  paste0("&page=", page),
                  if (!is.null(size)){paste0("&size=", size)},
                  if (!is.null(arbeitgeber)){paste0("&arbeitgeber=", arbeitgeber)},
                  if (!is.null(veroeffentlichtseit)){paste0("&veroeffentlichtseit=", veroeffentlichtseit)},
                  if (!is.null(zeitarbeit)){paste0("&zeitarbeit=", zeitarbeit)},
                  if (!is.null(angebotsart)){paste0("&angebotsart=", angebotsart)},
                  if (!is.null(befristung)){paste0("&befristung=", befristung)},
                  if (!is.null(arbeitszeit)){paste0("&arbeitszeit=", arbeitszeit)},
                  if (!is.null(behinderung)){paste0("&behinderung=", behinderung)},
                  if (!is.null(corona)){paste0("&corona=", corona)},
                  if (!is.null(umkreis)){paste0("&umkreis=", umkreis)})
  }
  
  #print(urls)
  
  #GET list of results
  geturls <- function(urls){
    
    h <- progressr::progressor(along = urls)
    
    tok <- get_token()
    
    map(urls, function(x){
      r <- GET(x,
               add_headers(OAuthAccessToken = tok),
               httr::progress(),
               accept_json())
      
      stop_for_status(r)
      
      content <- fromJSON(content(r, "text", encoding = "UTF-8"))
      
      Sys.sleep(sleep)
      h()
      
      return(content)
    })
  }
  
  #create list of reference numbers, these are used later to get the details for each job
  datalist <- geturls(urls)
  
  refnr_list <- list()
  
  for (i in 1:length(datalist)) {
    
    refnr_list <- c(refnr_list, datalist[[i]][["stellenangebote"]][["refnr"]])
    
  }
  
  print(paste("Anzahl mögliche Ergebnisse:", 
              datalist[[1]][["maxErgebnisse"]],
              " ", "Anzahl Ergebnisse im Querry:",
              length(refnr_list)))
  
  #return the list of reference numbers 
  return(refnr_list)
}


get_jobdetails <- function(refnr_list, sleep = NULL){
  
  p <- progressr::progressor(along = refnr_list)
  tok <- get_token()
  
  result_list <- list()
  
  jobnotfound <- 0
  
  for(refnr in refnr_list){
    
    url <- paste0("https://rest.arbeitsagentur.de/jobboerse/jobsuche-service/pc/v2/jobdetails/", 
                  base64_enc(refnr))
    
    res <- GET(url,
               config = add_headers(OAuthAccessToken = tok), 
               httr::progress(),
               accept_json())
    stop
    
    # Skip Refnr if http error
    if(http_error(res)){
      jobnotfound <- jobnotfound +1
      next
    }
    
    # extract content
    cont <- list(fromJSON(content(res, "text", encoding = "UTF-8")))
    
    result_list <- c(result_list, cont)
    
    #sleep after one iteration, if specified
    Sys.sleep(sleep) 
    
    # count progress
    p()
    
  }
  
  # make df from result_list
  result_df <- result_list %>% map_df(function(x){
    
    t <- as_tibble(t(unlist(x)))
    
    return(t)
  })
  
  # return result and print notfoundcount
  print(paste(jobnotfound, "jobs not found"))
  
  return(result_df)
  
}



#Ergebnisse ####

#Leerzeichen wird %20 (Deutsche%20Bahn), Problem: Kann pro request nur 100 results

#Page: Vector, z.B. 1:20 -> jeweils size ergebnisse, limitiert auf 100, darf nicht leer sein!
#Size: String, Ergebnisse pro Seite, kann nicht größer als 100
#Veroeffentlichtseit: String Anzahl der Tage, seit der Job veröffentlicht wurde. Kann zwischen 0 und 100 Tagen liegen
#Zeitarbeit: String Gibt an, ob Jobs von Zeitarbeitsfirmen in die Suchergebnisse einbezogen werden sollen (default true)
#Angebotsart:String 1=ARBEIT; 2=SELBSTAENDIGKEIT, 4=AUSBILDUNG/Duales Studium, 34=Praktikum/Trainee
#Angebotsart:String 1 = befristet; 2 = unbefristet
#Arbeitszeit:String Semikolon separirte mehrere Werte möglich (z.B. arbeitszeit=vz;tz) vz=VOLLZEIT, tz=TEILZEIT, snw=SCHICHT_NACHTARBEIT_WOCHENENDE, ho=HEIM_TELEARBEIT, mj=MINIJOB
#Behinderung:String boolean, kp was default ist
#Corona: String Wenn true , werden nur Jobs die im Kontext von Corona angeboten werden angezeigt
#Umkreis:String in kilometern
#Sleep: Integer, #Schläft sleep Sekunden nach Anfrage von Size x


query <- get_refnrs(was = NULL,
                    wo = NULL,
                    branche = "11",
                    berufsfeld = NULL,
                    page = c(1:10), 
                    size = "100", 
                    arbeitgeber = NULL,
                    veroeffentlichtseit = NULL,
                    zeitarbeit = NULL,
                    angebotsart = NULL,
                    befristung = NULL,
                    arbeitszeit = NULL,
                    behinderung = NULL,
                    corona = NULL,
                    umkreis = NULL,
                    sleep = 0.00)


#################################################################################################Sandbox




##################################################################################################Sandbox









