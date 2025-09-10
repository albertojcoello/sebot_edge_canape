library(tidyverse)
library(EDGEcalc)
library(ape)

###############################################################
################ Taller SEBOT Sevilla 2025 ####################
###############################################################
####   Análisis de patrones geográficos y evolutivos en    ####
#### Biogeografía y Biología de la Conservación en plantas ####
###############################################################
###############################################################

antirrhineae.tree <- read.tree("datos/Antirrhineae_all.tre")
antirrhineae.iucn <- read_csv("datos/Antirrhineae_IUCN.csv")


plot(antirrhineae.tree, cex = 0.5)
head(antirrhineae.iucn)

# Cálculo de índice EDGE clásico (EDGE 1) ####

EDGElist <- calculate_EDGE1(tree = antirrhineae.tree, 
                            table = antirrhineae.iucn,
                            sort.list = T)

print(EDGElist, max = 50)




# Cálculo de índice EDGE 2  ####

EDGE2list <- calculate_EDGE2(tree = antirrhineae.tree, 
                            table = antirrhineae.iucn,
                            sort.list = T)

print(EDGE2list, max = 100)


# Cálculo de índice EDGE 2 - multiple ####

EDGE2list_mult <- calculate_EDGE2_multiple(tree = antirrhineae.tree, 
                                      table = antirrhineae.iucn,
                                      sort.list = T,
                                      n.iter = 50, 
                                      parallelize = T )



print((EDGE2list_mult[[1]]), max = 100)
print((EDGE2list_mult[[2]]), max = 100)


EDGE2list_summ <- bind_rows(EDGE2list_mult) |> 
                    group_by(species) |> 
                    summarise(RL.cat = unique(RL.cat),
                              meanTBL = mean(TBL),
                              meanpext = mean(pext),
                              meanED = mean(ED),
                              meanEDGE = mean(EDGE)) |> 
                    arrange(desc(meanEDGE))


print(EDGE2list_summ, n = 15)

