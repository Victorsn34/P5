data_horsens <- homedata %>% 
  filter(Bynavn== "Horsens",
         EjdType == "Villa",
         KommuneNavn == "Horsens") %>% 
  mutate(Garage = if_else(Areal_GarageCarport > 0, 1, 0),
         Areal_Kaelder = replace_na(Areal_Kaelder, 0),
         Kaelder = if_else(Areal_Kaelder > 0, 1, 0),
         Areal_Grund = case_when(row_id == 15417 ~ 618,
                                 row_id == 30332 ~ 936,
                                 row_id == 148570 ~ 1372,
                                 TRUE ~ Areal_Grund),
         Ejd_AntalRum = replace_na(Ejd_AntalRum, 6),
         Ejd_Opfoerelsesaar = replace_na(Ejd_Opfoerelsesaar, 1974),
         AarSidenOpførsel = Salgsaar - Ejd_Opfoerelsesaar) %>% 
  filter(row_id != 151188) |> 
  rename(AarSidenOmbygning = Alder) %>% 
  mutate(AarSidenOmbygning = replace_na(AarSidenOmbygning, 46)) |> 
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
            "row_id",
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
            "Sag_AnnonceretNettet",
            "corona",
            "Beloeb_EjerUdgift",
            "Ejd_Altan",
            "Salgsmaaned",
            "Ejd_Energimaerke")) 
#Start
subdata <- data_horsens |> 
  select(c(Areal_Bolig, Areal_GarageCarport, Areal_Grund, Areal_Kaelder, Ejd_AntalRum, Salgsaar, AarSidenOpførsel, AarSidenOmbygning, Dist_skole, Dist_raadhus, Pris_Salg))

LogSubdata <- subdata |>
  mutate(Pris_Salg = log(Pris_Salg))
model1 <- lm(Pris_Salg ~ Areal_Bolig + Areal_GarageCarport + Areal_Grund + Areal_Kaelder + Ejd_AntalRum + Salgsaar + AarSidenOpførsel + AarSidenOmbygning + Dist_skole + Dist_raadhus, data = LogSubdata)

#Ass: 1
rank <- model$rank
kollonner <- ncol(model.matrix(model))

rank2 <- model2$rank
kollonner2 <- ncol(model.matrix(model2))

#Ass: 2
plot <- plot(model)
logplot <- plot(model2)
