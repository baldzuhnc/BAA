bip_amr <- readxl::read_excel("Data/inkar.xls") %>% 
  slice (2:224) %>%
  rename(AMR = "Raumeinheit", BIP_pc = 'Bruttoinlandsprodukt je Erwerbstätigen')

entropy_amr <- adata_occ %>% group_by(AMR) %>% summarise(mean_entropy = mean(entropy, na.rm = T))

t <- left_join(entropy_amr, bip_amr, by = "AMR") %>% drop_na(Kennziffer)

model1 <- lm(BIP_pc ~ mean_entropy, data = t)
summary(model1)

