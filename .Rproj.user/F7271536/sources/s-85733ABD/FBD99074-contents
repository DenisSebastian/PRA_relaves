# Función cartura de imagenes con gee
# Autor: Denis Berroeta


# Cargar librerias --------------------------------------------------------

library(raster)
library(rgee)
library(sf)
library(dplyr)
ee_Initialize()


# Definción de Área de Estudio --------------------------------------------
coordendas <- c(-71.6838, -33.4427) 
point_ee <- ee$Geometry$Point(coords = coordendas)

# Definción de Periodo de Estudio -----------------------------------------
inicio <- '2021-01-01'
fin <- '2021-03-01'


disponible <- ee$ImageCollection('LANDSAT/LC08/C02/T1_L2')$
  filterDate(inicio, fin)$
  filterBounds(point_ee)$
  filterMetadata('CLOUD_COVER','less_than', 10)

df_disponible <- ee_get_date_ic(disponible)%>%
  arrange(time_start)# ordenar por fecha

df_disponible

landsat = disponible$first()


viz = list(min = 0, max = 65455, 
           bands = c('SR_B4','SR_B3','SR_B2'),
           gamma =c(1, 1, 1)        
           )
Map$centerObject(eeObject = landsat, zoom = 8) 
Map$addLayer(eeObject = landsat,visParams = viz)

