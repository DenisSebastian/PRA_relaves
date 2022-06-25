# Objetivo crear periodos de estudio
# Autor: Denis Berroeta
# Fecha: 25-06-2022


# Opciones Generales ------------------------------------------------------
options(scipen = 999)



# librerías ---------------------------------------------------------------

library(dplyr)
library(lubridate)

library(timeperiodsR)


# creación de periodos ----------------------------------------------------

inicio <-  ymd("2014-01-01")
fin <- ymd("2021-12-31")

periodos <- data.frame(inicios = inicio, finales = ymd("2014-03-31"))
finales <-  inicio
inicios <-  inicio
while(finales != fin){
  inicios <- next_n_quarters(x = inicios, n = 1,
                            part = "start") %>% 
    ymd()
  
  finales <- next_n_quarters(x = finales, n = 1,
                             part = "end") %>% 
    ymd()
  res <-  data.frame(inicios, finales)
  print(res)
  periodos <- rbind(periodos, res)
}


# consolidar --------------------------------------------------------------

periodos <- periodos %>% 
  mutate(periodo = paste0(year(inicios),        
       " Q", quarter(inicios)),
       id = 1:nrow(.)) %>% 
  select(id, everything())


# guardar -----------------------------------------------------------------

saveRDS(periodos, "data/rds/periodos.rds")
