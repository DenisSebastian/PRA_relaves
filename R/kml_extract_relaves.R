# Función cartura de imagenes con gee
# Autor: Denis Berroeta, Leonardo Rojas
# Fecha de Creación: 17-06-2022

# Librerias ---------------------------------------------------------------
library(sf)
library(dplyr)
library(stringr)
library(tidyr)



# Funciones ---------------------------------------------------------------

cor_especial <-  function(x){
  x <-  gsub("m2", "", x)
  x <-  gsub('<tr bgcolor="#D4E4F3"> <td>', " ", x)
  x <-  gsub("</tr>", "", x)
  x <-  gsub("<tr>", "", x)
  x <-  gsub("</td> <td>", "", x)
  x <-  gsub("</td>   <td>", "", x)
  x <-  gsub("</table>", " ", x)
  x <-  gsub("</body>", " ", x)
  x <-  gsub("</html>", " ", x)
  x <-  gsub("</td>", " ", x)
  x <-  gsub("&lt;Nulo&gt;", NA, x)
  x <-  gsub("N_A", NA, x)
  x <-  gsub("S/I", NA, x)
  return(x)
}

clean_vars <-  function(x){
  y <- cor_especial(x)
  y <- trimws(y)
  return(y)
}

clean_vars <-  function(x){
  y <- cor_especial(x)
  y <- trimws(y)
  return(y)
}


# Lectura de insumos ------------------------------------------------------

kml_file <- "data/kml/relaves_2018.kml"
kml_pto <- st_read(kml_file) %>% st_zm() %>% mutate(id = 1:nrow(.))



# Extraer información -----------------------------------------------------

campo_xml <- c("NOMBRE_EMP", "NOMBRE_FAE", "REGIÓN_IN",
               "PROVINCIA", "COMUNA_INS", "NOMBRE_INS",
               "TIPO_INSTA", "RECOBRERSO", "UTM_NORTE",
               "UTM_ESTE", "ESTADO_INS", "METODO_CONS",
               "VOL_AUTORIZADO", "TON_AUTORIZADO",
               "VOL_ACTUAL", "TON_ACTUAL", "RES_APRUEBA",
               "RES_APRUEBA_FECHA", "RES_PDC_APRUEBA",
               "RES_PDC_FECHA")


df <-  kml_pto

for(i in seq_along(campo_xml)){
  data <- kml_pto %>%
    st_drop_geometry() %>% 
    select(id, Description) %>% 
    mutate(origen = Description) %>% 
    separate(origen, into = c("origen", campo_xml[i]),
             sep = campo_xml[i], remove = T) %>% 
    mutate(salida= gsub(paste0(campo_xml[i+1], ".*"),
                        replacement = "",!!rlang::sym(campo_xml[i]))) %>% 
    pull(salida)
  
  df <-  cbind(df, data)
  names(df)[names(df) == "data"] <- campo_xml[i]
  
  
}

# Limpiar Los Resultados --------------------------------------------------

relaves_sf <- df %>% 
  select(-Description) %>%
  mutate(across(all_of(campo_xml), .fns = clean_vars))


# Formato a las variables -------------------------------------------------
numeros <- c("UTM_NORTE", "UTM_ESTE", 
             "VOL_AUTORIZADO", "TON_AUTORIZADO",
             "VOL_ACTUAL", "TON_ACTUAL")
fechas <- c("RES_APRUEBA_FECHA", "RES_PDC_APRUEBA",
             "RES_PDC_FECHA")

relaves_sf <- relaves_sf %>% 
  mutate(across(all_of(numeros), .fns = function(x) as.numeric(gsub(",", ".",x)))) %>% 
  mutate(across(all_of(fechas), .fns = lubridate::dmy)) %>% 
  janitor::clean_names()


# guardar los resultados --------------------------------------------------

st_write(relaves_sf, dsn = "data/shp/relaves_2018.shp", delete_dsn = T)
openxlsx::write.xlsx(st_drop_geometry(df), 
                     file = "data/tablas/relaves_2018.xlsx")




relaves_sf %>% 
  filter(estado_ins == "ACTIVO" & metodo_cons == "EMBALSE") %>% 
  arrange(desc(vol_autorizado)) %>% 
  # head(5) %>% 
  mapview::mapview()
