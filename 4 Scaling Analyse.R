library(tidyverse)
library(ggthemes)
library(ggrepel)
library(stargazer)

####Ergebnistibble erstellen und speichern ####
df_list <- list.files(path = "Data/Rawdata/", pattern = "*.rds*", full.names = T) %>% 
    map(read_rds)

Final <- map_dfr(df_list, function(x){
  
  mapped_cleantibble <- clean(x) %>% 
    mapamr()
  
  grouped_cleantibble <- group_amr(mapped_cleantibble)
  
  res <- complex_beta(mapped_cleantibble, grouped_cleantibble)
  
  return(res)
  })



#Z-Standardisieren
Final$Z_Anzeigenlaenge_mean <- scale(Final$Anzeigenlaenge_mean)
Final$Z_Englisch_Prozent <- scale(Final$Englisch_Prozent)
Final$Z_Fuergefluechtete_Prozent <- scale(Final$Fuergefluechtete_Prozent)
Final$Z_Fuerschwerbehinderte_Prozent <- scale(Final$Fuerschwerbehinderte_Prozent)
Final$Z_Befristet_Prozent <- scale(Final$Befristet_Prozent)

#Komplexitätsindex
Final$Komplexität <- (Final$Z_Anzeigenlaenge_mean + 1-(Final$Z_Fuergefluechtete_Prozent))/2




#Informatik####
rawdata <- readRDS("Rawdata/B11, IT, Computer, Telekommunikation.rds")
info <- clean(rawdata) %>% mapamr()
info_amr <- group_amr(info)
complex_info <- complex_beta(info, info_amr)

#Reinigung####
rawdata <- readRDS("Rawdata/B23, Sicherheits-, Reinigungs-, Reparatur- und weitere Dienstleistungen.rds")
reinigungs <- clean(rawdata) %>% mapamr()
reinigungs_amr <- group_amr(reinigungs)
complex_reinigungs <- complex_beta(reinigungs, reinigungs_amr)


####Plotten ####
plot(Final$Komplexität, Final$Scaling_Koeffizient)
cor(Final$Komplexität, Final$Scaling_Koeffizient)

#plot(Final$Englisch_Prozent, Final$Scaling_Koeffizient)
#cor(Final$Englisch_Prozent, Final$Scaling_Koeffizient)

#plot(Final$Befristet_Prozent, Final$Scaling_Koeffizient)
#cor(Final$Befristet_Prozent, Final$Scaling_Koeffizient)


#####Komplexität Scaling####
m <- ggplot(data = Final, aes(x = Komplexität, y = Scaling_Koeffizient)) +
  geom_point(colour = "darkorange") +
  ggpubr::stat_cor(method = "pearson") +
  geom_smooth(method = "lm", colour = "darkred") +
  geom_text_repel(aes(label = Branche)) +
  labs(x = "Average length of job ads", y = "Scaling coefficient (β)") +
  theme_minimal()
m


#####Reinigung und Informatik####
s <- ggplot() +
  geom_point(data = reinigungs_amr, aes(x = bevoelkerung, y = n_anzeigen), colour = "orange1", alpha = 0.8) +
  geom_smooth(data = reinigungs_amr, aes(x = bevoelkerung, y = n_anzeigen), method = "lm", colour = "orange1", se = F) +
  geom_point(data = info_amr, aes(x = bevoelkerung, y = n_anzeigen), colour = "darkred", alpha = 0.8) +
  geom_smooth(data = info_amr, aes(x = bevoelkerung, y = n_anzeigen), method = "lm", colour = "darkred", se = F) +
  scale_x_continuous(trans = "log10", labels = scales:: comma) +
  scale_y_continuous(trans = "log2") +
  labs(title = "", x = "Bevölkerung", y = "Anzahl Jobanzeigen") +
  theme_minimal()
s

#####Maurer und Architektur####
s <- ggplot() +
  geom_point(data = maurer_amr, aes(x = bevoelkerung, y = n_anzeigen), colour = "orange1", alpha = 0.8) +
  geom_smooth(data = maurer_amr, aes(x = bevoelkerung, y = n_anzeigen), method = "lm", colour = "orange1", se = F) +
  geom_point(data = archi_amr, aes(x = bevoelkerung, y = n_anzeigen), colour = "darkred", alpha = 0.8) +
  geom_smooth(data = archi_amr, aes(x = bevoelkerung, y = n_anzeigen), method = "lm", colour = "darkred", se = F) +
  scale_x_continuous(trans = "log10", labels = scales:: comma) +
  scale_y_continuous(trans = "log2") +
  labs(title = "", x = "Bevölkerung", y = "Anzahl Jobanzeigen") +
  theme_minimal()
s


####Alle Anzeigen Map####
Alle_anzeigen <- map_dfr(df_list, function(x){
  mapped_cleantibble <- clean(x) %>% mapamr()

})

alle_anzeigen_gruppiert <- group_amr(Alle_anzeigen)

amr_sf <- st_read("Shapefiles/amr250/AMR250.shp") %>%
  inner_join(alle_anzeigen_gruppiert, by = "AMR")

a <- ggplot(data = amr_sf) +
  geom_sf(aes(fill=n_anzeigen)) +
  scale_fill_continuous(low = "white", high = "darkred") +
  labs(fill = "Anzahl Jobanzeigen") +
  theme_minimal() 
a

#Tabelle
tabelle <- readRDS("Results/Tabelle.rds") %>% arrange(desc(Scaling_Koeffizient))

xtable::xtable(tabelle, type = "latex")



#Regressionsmodell
model1 <- lm(formula = Scaling_Koeffizient ~ Anzeigenlaenge_mean,
             data = Final)
summary(model1)




#### Sandbox #####################


######Linear Model ####
model1 <- lm(formula = n_anzeigen ~ bevoelkerung,
             data = kaufmaenner_amr)
summary(model1)

plot(kaufmaenner_amr$bevoelkerung, kaufmaenner_amr$n_anzeigen)
#Klares Ding, eine Exponentialfunktion würde das Model besser treffen!


####### log log model ####
#da wir annehmen, dass *Y(Anzahl Anzeigen) = N(Population)^beta(Scaling Koeffizient)* brauchen wir anderes, nicht lineares Modell
#-> 

model2 <- lm(formula = log(n_anzeigen)~ log(bevoelkerung),
             data = kaufmaenner_amr)
summary(model2)
plot(model2)


#befristung: 1 = befristet; 2 = unbefristet
#angebotsart: 1=ARBEIT; 2=SELBSTAENDIGKEIT, 4=AUSBILDUNG/Duales Studium, 34=Praktikum/Trainee


#Befristung replace
#Replace verguetung, schwer

### Sandbox #######################################

