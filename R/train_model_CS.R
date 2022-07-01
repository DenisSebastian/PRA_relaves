# Objetivo Orquestar flujo general captura de imagenes
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


# Parametros Generales ----------------------------------------------------

relave_n <- 105
year <- 2014
trimestre <- "Q4"
path_images <- "data/images/Relave_"
path_img <-  paste0(path_images,relave_n, "/R",relave_n,
                    "_A", year, "_", trimestre, ".tif")
# path_shp <- "data/shp"
# paste(path_shp, SE, sep = "/")


# Lectura de Imagenes -----------------------------------------------------

#Lectura de imagen
relave_img <- brick(x = path_img)
#eliminar columnas innecesarias

relave_img <-dropLayer(x = relave_img,i = c("B1","B7","B8", "B9", "B11"))
names(relave_img) <- c("blue", "green", "red", "nir", "swir1", 
                       # "swir2", 
                       "thermal1"
                       # "thermal2"
                       )



# Lectura de Sitios de Entrenamiento --------------------------------------

se <- st_read("data/shp/clases.shp")%>%
  mutate(id = 1:nrow(.),
         CATEGORIA = as.factor(toupper(etiqueta)),
         clase = case_when(
           CATEGORIA == "RELAVE" ~ "3",
           CATEGORIA == "TIERRA" ~ "2",
           CATEGORIA == "LIQUIDO" ~ "1")) %>% 
  filter(!is.na(CATEGORIA)) %>% 
  dplyr::select(-etiqueta)


mapview::viewRGB(relave_img, 3, 2, 1) +
  mapview::mapview(se, zcol= "CATEGORIA")


se_sp <- as(se, "Spatial") 


# Extracción de Valores de Sitios de Entrenamiento ------------------------

dfAll <- raster::extract(relave_img, 
                        se_sp,  
                        df=TRUE)


# agrega Categoria
dfAll <- dfAll %>%
  left_join(st_drop_geometry(se), by = c("ID"="id")) %>% 
  filter(!is.na(clase)) %>% 
  mutate(across(all_of(names(relave_img)),
                .fns = as.numeric)) 
summary(dfAll)
head(dfAll)

pearson_cor <- cor(x = dfAll[,names(relave_img)], 
                   use = "everything", method = "pearson")

corrplot::corrplot(pearson_cor, method="circle")

saveRDS(dfAll, paste0("data/rds/R",relave_n,
        "_A", year, "_", trimestre, "_dfAll_data.rds"))


dfAll <- readRDS(paste0("data/rds/R",relave_n,
                      "_A", year, "_", trimestre, "_dfAll_data.rds"))


# Seleccionar base Train y Test -------------------------------------------

smp_size <- floor(0.70 * nrow(dfAll))

## set the seed to make your partition reproducible
set.seed(42)
train_ind <- sample(seq_len(nrow(dfAll)), size = smp_size)

train <- dfAll[train_ind, ]
test <- dfAll[-train_ind, ]



# Entrenar Modelo con Random Forest ---------------------------------------


library(caret)
mod_rf_relaves_rf <- train(as.factor(clase) ~ blue+ green + red + nir+ swir1+
                    # + swir2 +
                      thermal1,
                      # thermal2,
                    method = "rf",
                    data = train)



saveRDS(mod_rf_relaves_rf,
        file = paste0("data/rds/model_R",relave_n,
                    "_A", year, "_", trimestre, ".rds"))

mod_rf_relaves_rf <-  readRDS(paste0("data/rds/model_R",relave_n,
                      "_A", year, "_", trimestre, ".rds"))



# Prediction --------------------------------------------------------------

prediccion <- predict(mod_rf_relaves_rf, 
                      test %>% dplyr::select(-clase, -CATEGORIA))
# Matriz de Confusión
MC <- table(test[,"clase"], prediccion)
# Acierto
acierto <- (sum(diag(MC))) / sum(MC)
acierto

# predecir el resto de la imagen

beginCluster()
relaves_rf <- clusterR(relave_img,  raster::predict, 
                   args = list(model = mod_rf_relaves_rf))
endCluster()





writeRaster(relaves_rf, 
            filename = paste0("data/images/resultados/R",
                              relave_n, "_A", year, "_", trimestre, ".tif"))


relaves_rf <- brick(paste0("data/images/resultados/R",
                              relave_n, "_A", year, "_", trimestre, ".tif"))
mapview::viewRGB(relave_img, 3, 2, 1) +
  mapview::mapview(relaves_rf,  layer.name ="sa") 
# Validación --------------------------------------------------------------


relave_n <- 105
year <- 2021
trimestre <- "Q1"
path_images <- "data/images/Relave_"
path_img <-  paste0(path_images,relave_n, "/R",relave_n,
                    "_A", year, "_", trimestre, ".tif")




relave_img_rev <- brick(x = path_img)
relave_img_rev <-dropLayer(x = relave_img_rev,
                           i =c("B1","B7","B8", "B9", "B11"))
names(relave_img_rev) <-  c("blue", "green", "red", "nir", "swir1", 
                            # "swir2", 
                            "thermal1"
                            # "thermal2"
)
beginCluster()
revision <- clusterR(relave_img_rev,  raster::predict, 
                   args = list(model = mod_rf_relaves_rf))
endCluster()




writeRaster(revision, 
            filename = paste0("data/images/resultados/R",
                              relave_n, "_A", year, "_", trimestre, "_p.tif"),
            overwrite=TRUE)


revision <- brick(paste0("data/images/resultados/R",
                           relave_n, "_A", year, "_", trimestre, "_p.tif"))



mapview::viewRGB(relave_img_rev, 3, 2, 1)+
  mapview::mapview(revision, layer.name ="sa")

# Correccion de Predicción ------------------------------------------------
# plot(relaves_rf)
library(terra)
relaves_rf_3 <- terra::focal(revision, w=matrix(1,nrow=5,ncol=5)  , fun=mean)
relaves_rf_3 <- calc(relaves_rf_3, fun = select_class(num_class = 3))
# relaves_rf_3 <- focal(relaves_rf_3, w=matrix(1,nrow=5,ncol=5)  , fun=mean)
plot(relaves_rf_3)
# 
# relaves_rf_1 <- calc(revision, fun = select_class(num_class = 1)) 
# mapview::viewRGB(relave_img_rev, 3, 2, 1)+
#   mapview::mapview(relaves_rf_3)+
#   mapview::mapview(relaves_rf_1, col = "orange")



# Guardar Resultados ------------------------------------------------------


