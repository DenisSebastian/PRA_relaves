# Objetivo Orquestar flujo general captura de imagenes
# Autor: Denis Berroeta
# Fecha: 25-06-2022


# Opciones Generales ------------------------------------------------------
options(scipen = 999)
source("R/funciones.R")

# librerías ---------------------------------------------------------------

library(sf)
library(dplyr)
library(purrr)
library(rgee)
ee_Initialize(drive = T)


# Definir paràmetros ------------------------------------------------------
sensor <- 'LANDSAT/LC08/C01/T1_TOA' 
buffer_size <- 8000 # metros
cloud_cover <- 10 # %porcentaje

#vparamentros de vusalizacion de mapas
viz <- list(min = 0, max = 0.5,  bands = c("B4", "B3", "B2"),
            gamma = c(0.95, 1.1, 1))
zoom = 11



# Lectura de Insumos ------------------------------------------------------
relaves <-  readRDS("data/rds/relaves_selected.rds")
periodos <-  readRDS("data/rds/periodos.rds") %>% 
  mutate_all(as.character)


# tanformar a listas ------------------------------------------------------

list_relaves_ee <- relaves %>%
  split(.$id) %>%
  map(st_as_ee_buffer, buffer_size)


list_periodos <- periodos %>% split(.$periodo)

# Lectura de Imagenes periodo ---------------------------------------------



series <-  list()
for(i in 1:length(list_relaves_ee)){
  print(i)
  list_images <- list_periodos %>% 
    map(function(x) {
      extract_ee(sensor = sensor, periodo_ini = x$inicios, 
                 periodo_fin = x$finales, cloud_cover = 10,
                 ee_geometry = list_relaves_ee[[i]])
    })
  
  series[[i]] <- list_images
}

names(series) <-  names(list_relaves_ee)



# revisión de resultados --------------------------------------------------


vis_map(image_ee = series$`105`$A2014_Q3, vis_list = viz, 
        zoom = 11)
  


# Descarga ----------------------------------------------------------------
folders <- names(series) %>% 
  paste0("Relave_", .)

path_folders <-  paste0("data/images/", folders)
path_folders %>% map(make_dir)

# for(relave in 1:length(series)){#
relave <- "105"
  print(relave)
  for (periodo in names(series[[relave]])[28]) {
    local_path <- paste0("data/images/Relave_",
                         relave, "/R", relave, "_", periodo, ".tif")
    print(local_path)
    
    task_img <- ee_image_to_drive(
      image = series[[relave]][[periodo]],
      scale = 30,
      crs = 'EPSG:4326',
      folder = paste0("Relave_",relave),
      fileFormat = "GEO_TIFF",
      fileNamePrefix = paste0("R", relave, "_", periodo)
    )
    task_img$start()
    ee_monitoring(task_img)
    task_img$status()
    Sys.sleep(25)
    if(task_img$status()$state=="COMPLETED"){
      img <- ee_drive_to_local(task = task_img, 
                               overwrite = T,
                               dsn = local_path)
      
    }
  }
  # }

