library(ape)
library(tidyverse)

# "https://github.com/iramosgutierrez/IBC_EDGE_workshop/blob/main/files/Erica_trees2.tre"
tree <- read.tree("E:/UNI/4. DOCTORADO/99. OTROS/8888_Terminados/2024_06_14 EDGE Workshop/materials_workshop/local/files/Erica_trees2.tre")[[1]]
tree$tip.label <- stringr::word(tree$tip.label, 2, sep = "&")

spp <- stringr::word(tree$tip.label, 1, 2, sep="_")

tree <- ape::drop.tip(tree, tree$tip.label[which(duplicated(spp))])

tree$tip.label <- stringr::word(tree$tip.label, 1, 2, sep = "_")


# "https://github.com/iramosgutierrez/IBC_EDGE_workshop/blob/main/files/Erica_occ_cleaned.csv"
occ <- read.csv("E:/UNI/4. DOCTORADO/99. OTROS/8888_Terminados/2024_06_14 EDGE Workshop/materials_workshop/local/files/Erica_occ_cleaned.csv") |> 
  mutate(species = stringr::word(taxon, 1, 2, sep = "_")) |> 
  mutate(region = stringr::word(trnames, 1,  sep = "_")) |> 
  filter(species %in% tree$tip.label) |> 
  select(species, x, y, region) |> 
  distinct()


tree <- keep.tip(tree, occ$species)

setdiff(tree$tip.label, occ$species)
setdiff(occ$species, tree$tip.label)

write_csv(occ, "datos/Erica_occurrences.csv")
write.tree(tree, "datos/Erica_tree.tre")
