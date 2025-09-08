
ericatable <- read_csv("E:/UNI/4. DOCTORADO/99. OTROS/8888_Terminados/2024_06_14 EDGE Workshop/materials_workshop/local/files/ericatable.csv") |> 
  mutate(species = stringr::word(taxa, 1, 2, sep = "_") ) |> 
  group_by(species) |> 
  summarise(cats = paste0(unique(RL.cat), collapse = ",")) |> 
  mutate(RL.cat = case_when(!grepl(",", cats) ~ cats,
                            grepl("LC", cats) ~ "LC",
                            grepl("VU", cats) ~ "VU",
                            grepl("EN", cats) ~ "EN",
                            grepl("CR", cats) ~ "CR")) |> 
  select(-cats)

write_csv(ericatable, "datos/Erica_IUCN.csv")
