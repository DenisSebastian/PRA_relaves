# Función Revisión de imágenes
# Autor: Denis Berroeta


# Cargar librerias --------------------------------------------------------

library(raster)
library(rgee)
library(sf)
library(dplyr)
ee_Initialize()


# Definción de Área de Estudio --------------------------------------------

relaves_ee <- readRDS("data/rds/relaves_selected.rds") %>% 
  dplyr::select(id, nombre_fae) %>% 
  sf_as_ee()
  

roi_buffer <- relaves_ee$first()$geometry()$buffer(50000)


# Definción de Periodo de Estudio -----------------------------------------
inicio <- '2018-01-01'
fin <- '2021-03-01'


disponible <- ee$ImageCollection('LANDSAT/LC08/C01/T1_TOA')$
  filterDate(inicio, fin)$
  filterBounds(roi_buffer)$
  filterMetadata('CLOUD_COVER','less_than', 10)

# df_disponible <- ee_get_date_ic(disponible)%>%
#   arrange(time_start)# ordenar por fecha
# 
# df_disponible

landsat = disponible$median()$clip(roi_buffer)


viz <- list(min = 0, max = 0.5,  bands = c("B4", "B3", "B2"),
               gamma = c(0.95, 1.1, 1))
Map$centerObject(eeObject = landsat, zoom = 8) 
Map$addLayer(eeObject = landsat,visParams = viz)







