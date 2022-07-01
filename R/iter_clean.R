
# Objetivo Iterar depurar resultados de clasifcación y calcular superficie
# Autor: Denis Berroeta
# Fecha: 30-06-2022


# Opciones Generales ------------------------------------------------------
options(scipen = 999)
source("R/funciones.R")

# librerías ---------------------------------------------------------------

library(sf)
library(dplyr)
library(purrr)
library(raster)
library(terra)



# Iterador por Periodo ----------------------------------------------------


relave <- "105"
path_images_res <- paste0("data/images/resultados/Res_R",relave_n)

images_res <- list.files(path = path_images_res, pattern = ".tif$",
                     full.names = T)

resultados_area <- data.frame()
for (periodo in images_res) {
  name_salida <-  substr(x = periodo, start = 33, 
                         stop = nchar(periodo)-6)
  print(name_salida)
  # lectura de imagenes
  relave_img_res <- raster(x = periodo)
  plot(relave_img_res)
  
  #limpieza
  img_res_clean <- terra::focal(relave_img_res, w=matrix(1,nrow=5,ncol=5),
                               fun=mean)
  # plot(img_res_clean)

  
  # calculo de Relave
  relaves <- calc(img_res_clean, fun = select_class(num_class = 3))
  pixel_relaves <- cellStats(relaves, "sum") 
  # mapview::mapview(img_res_clean) +
  #   mapview::mapview(relaves,  layer.name ="relaves")

  
  # calculo de aguas
  aguas <- calc(img_res_clean, fun = select_class(num_class = 1))
  pixel_aguas <- cellStats(aguas, "sum") 
  # mapview::mapview(img_res_clean) +
  #   mapview::mapview(relaves,  layer.name ="relaves")+
  #   mapview::mapview(aguas, col = "orange",  layer.name ="aguas")
  # 
  data_values <-  data.frame(periodo = name_salida,
                             aguas = pixel_aguas,
                             relaves = pixel_relaves,
                             area_agua = pixel_aguas *30*30,
                             area_relaves = pixel_relaves *30*30,
                             area_agua_hec = pixel_aguas *30*30/10000,
                             area_relaves_hec = pixel_relaves *30*30/10000)
  
  resultados_area <-  rbind(resultados_area,data_values)
}


saveRDS(resultados_area, "data/rds/resultados_area.rds")
write.csv(resultados_area, "data/tablas/resultados_area.csv")



