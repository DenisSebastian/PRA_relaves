# Objetivo: Crear funciones deuso general 
# Autor: Denis Berroeta
# Fecha: 26-06-2022



# función para extraer una iamgene consolidada
extract_ee <-  function(sensor, periodo_ini, periodo_fin, cloud_cover, 
                        ee_geometry){
  image <- ee$ImageCollection(sensor)$
    filterDate(periodo_ini, periodo_fin)$
    filterBounds(ee_geometry)$
    filterMetadata('CLOUD_COVER','less_than', cloud_cover)$
    median()$
    clip(ee_geometry)$
    select(paste0("B", 1:11))
  
  
  return(image)
}

# sf to ee  + buffer
st_as_ee_buffer <- function(sf_object, buffer_size = 10000){
  feature_ee <- sf_object %>% 
    sf_as_ee() 
  
  feature_ee <-  feature_ee$geometry()$buffer(buffer_size)
  return(feature_ee)
}


vis_map <- function(image_ee, vis_list, zoom){

  m <- Map$centerObject(eeObject = image_ee, zoom = zoom)
  m <- Map$addLayer(eeObject = image_ee,visParams = vis_list)
  return(m)
  
}


# Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}


# vis_map_list <-  function(list_img,  vis_list, zoom){
#   m <- Map$centerObject(eeObject = list_img[[1]], zoom = zoom)
#   
#   for(i in 1:4){
#     m <- Map$addLayer(eeObject = list_img[[i]], visParams = vis_list)
#   }
#   return(m)
# }
# 
# 
# manyImagesEE <- function(vector,geom_ee,vizParams){
#   
#   vectorAsString <- deparse(substitute(vector))
#   geom_eeAsString <- deparse(substitute(geom_ee))
#   vizParamsAsString <- deparse(substitute(vizParams))
#   
#   vectorLenght<-1:length(vector)
#   
#   firstLayerString <- paste0("Map$addLayer(ee$Image(",vectorAsString,"[1])$clip(",geom_eeAsString,"), visParams =", vizParamsAsString,",name='id[1]')")
#   
#   anyLayerString <- function(n){
#     iLayer <- str_replace_all(firstLayerString,"1", as.character(n))
#     return(iLayer)
#   }
#   
#   mapString <- paste(lapply(vectorLenght, anyLayerString), collapse = '+')
#   
#   map <- eval(parse(text=mapString))
#   
#   return(map)
# }
# 
# 
