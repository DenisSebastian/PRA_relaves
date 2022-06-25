
# Librerias ---------------------------------------------------------------
library(sf)
library(dplyr)

relaves_sf <-  readRDS("data/rds/relaves_sf.rds")


relaves_selected <- relaves_sf %>% 
  filter(estado_ins == "ACTIVO" &
           metodo_cons == "EMBALSE") %>% 
  arrange(desc(vol_autorizado)) %>% 
  slice(1:10)

relaves_selected %>%
  mapview::mapview()



saveRDS(relaves_selected, "data/rds/relaves_selected.rds")