
# Objetivo Iterar xpara predecir
# Autor: Denis Berroeta
# Fecha: 29-06-2022


# Opciones Generales ------------------------------------------------------
options(scipen = 999)
source("R/funciones.R")

# librerías ---------------------------------------------------------------

library(sf)
library(dplyr)
library(purrr)
library(raster)
library(terra)


# lectura del Modelo ------------------------------------------------------

relave_n <- "105"
year <- 2014
trimestre <- "Q4"
mod_rf_relaves_rf <-  readRDS(paste0("data/rds/model_R",relave_n,
                                     "_A", year, "_", trimestre, ".rds"))


# Iterador por Periodo ----------------------------------------------------

path_images <- paste0("data/images/Relave_",relave_n)

images <- list.files(path = path_images, pattern = ".tif$",
                          full.names = T)


for (periodo in images[c(1, 32)]) {
  name_salida <-  substr(x = periodo, start = 24, 
                         stop = nchar(periodo)-4)
  print(name_salida)
  # lectura de imagenes
  relave_img_q <- brick(x = periodo)
  relave_img_q <-dropLayer(x = relave_img_q,
                             i =c("B1","B7","B8", "B9", "B11"))
  names(relave_img_q) <-  c("blue", "green", "red", "nir", "swir1", 
                              "thermal1")
  
  #predicción
  beginCluster()
  predict_img <- clusterR(relave_img_q,  raster::predict, 
                       args = list(model = mod_rf_relaves_rf))
  endCluster()
  
  # mapview::viewRGB(relave_img_q, 3, 2, 1) +
  #   mapview::mapview(predict_img,  layer.name ="predict")
  
  writeRaster(predict_img, 
              filename = paste0("data/images/resultados/Res_R",
                                relave_n,"/", name_salida, "_p.tif" ),
              overwrite=TRUE)
  
}

