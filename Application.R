library(tidyverse)

load("Homedata.Rda")

data_horsens <- homedata %>% 
  filter(Bynavn== "Horsens",
         EjdType == "Villa",
         KommuneNavn == "Horsens") %>% 
  mutate(Garage = if_else(Areal_GarageCarport > 0, 1, 0),
         Areal_Kaelder = replace_na(Areal_Kaelder, 0),
         Kaelder = if_else(Areal_Kaelder > 0, 1, 0),
         Areal_Grund = case_when(
           row_id == 15417 ~ 618,
           row_id == 30332 ~ 936,
           row_id == 148570 ~ 1372,
           TRUE ~ Areal_Grund
         )) %>% 
  filter(row_id != 151188) %>%  #This is removed since it is a Commercial.
  mutate(Ejd_AntalRum = replace_na(Ejd_AntalRum, 6),
         Ejd_Opfoerelsesaar = replace_na(Ejd_Opfoerelsesaar, 1974),
         AarSidenOpførsel = Salgsaar - Ejd_Opfoerelsesaar) %>%  #3 rooms had NA but we found on BBR Data that all 3 had 6 rooms.
  rename(AarSidenOmbygning = Alder) %>% 
  mutate(AarSidenOmbygning = replace_na(AarSidenOmbygning, 2020-1974)) %>% #The house with missing values from before was sold in 2020.
  select(-c("Pris_FoersteUdbud",
            "Dato_AktuelUdbudPris",
            "KVHX",
            "Adresse_Etage",
            "Areal_Bolig_Commercial",
            "Adresse_Fuld",
            "Ejd_AntalToiletter",
            "RegionNavn",
            "SagsStatus",
            "Postnr",
            "SagsType",
            "DWID_Projektsalg",
            "AntalStatusmoederSaelger",
            "Ejd_AntalSoveVaerelser",
            "NK_SagId",
            "DWID_Periode_Formid",
            "DWID_Periode_Annoncering",
            "DWID_Periode_Oprettet",
            "Beloeb_Udbetaling",
            "Beloeb_MDBrutto",
            "Beloeb_MDNetto",
            "Hoejhus",
            "StorGrund",
            "Pris_EjdVurdering",
            "AntalFremvisninger",
            "GisY_Wgs84",
            "GisX_Wgs84",
            "Adresse_PostNr",
            "Afstemningsomraade",
            "Pris_AktuelUdbud",
            "Salgstid",
            "DWID_Periode_Salg",
            "Dato_Annoncering",
            "Ejd_Energimaerke",
            "Sag_AnnonceretNettet",
            "corona",
            "Beloeb_EjerUdgift",
            "Ejd_Altan",
            "Salgsmaaned")) 

#colSums(is.na(data_horsens))

#Data subset
data_horsens_subdata <- data_horsens %>% 
  select(c(Areal_Bolig,
           Areal_GarageCarport,
           Areal_Grund,
           Areal_Kaelder,
           Ejd_AntalRum,
           Salgsaar,
           AarSidenOpførsel,
           AarSidenOmbygning,
           Dist_skole,
           Dist_raadhus,
           Pris_Salg)
  )

#Data subset with log prices
data_horsens_subdata_logprices <- data_horsens_subdata %>% 
  mutate(Log_Prices = log(Pris_Salg)) %>% 
  select(-Pris_Salg) # Fjern almindelig salgspris fra denne

#An initial model
model <- lm(Pris_Salg ~ ., data = data_horsens_subdata)

#Another model with log prices
model_log <-lm(Log_Prices ~ ., data = data_horsens_subdata_logprices)

#Assumption checking starts now

#Assumption 1
model$rank # The rank is 11 thus full rank

#Assumption 1 model with log prices
model_log$rank # The rank is 11 thus full rank

#Assumption 2 (Residuals)
plot(model)

#Assumption 2 (Residuals) model with log prices
plot(model_log)

#Histogram over the sales prices
data_horsens_subdata %>% 
  ggplot(aes(x = Pris_Salg)) +
  geom_histogram(bins = 50,
                 fill = "steelblue") +
  labs(title = "Sales Prices",
       x = "Sales Price in DDK",
       y = "Amount of Residence") +
  theme_minimal()

#Histogram over the log of the sales prices
data_horsens_subdata_logprices %>% 
  ggplot(aes(x = Log_Prices)) +
  geom_histogram(bins = 50,
                 fill = "steelblue") +
  labs(title = "Log of Sales Prices",
       x = "Log of Sales Prices in DDK",
       y = "Amount of Residence") +
  theme_minimal()