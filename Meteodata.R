#PROYECTO DE REGRESIÓN: Análisis de la Relación entre la Calidad del Aire y otros factores medioambientales en Vigo
#Trabajo realizado por Antón Soto Martínez y Xoel Trasancos Atadell.


################################################################################
## Analisis del datos meteorológicos en 4 ubicaciones en Vigo de 1 Mayo al 30 Sept 2024 ##
################################################################################
# Librerias
library(tidyverse)
library(skimr)
library(easystats) # easystats::easystats_update()
library(correlation) # se usa para tabla y gráficas
library(lme4)
library(lmerTest)
library(brms)
library(lmtest)
library(marginaleffects)
library(modelsummary) 
library(tinytable)
library(bayesplot)
library(ggdist)
library(lubridate)
library(PrettyCols)
#Ponemos la ruta al directorio en el que queremos trabajar:
setwd("~/MATEMATICAS/Cuarto/Primercuatri/Regresión/Proyecto/Meteodata_20240907/Meteodata")


# Descarga de los datos de meteogalicia en csv y importarlos:
Vigo_Coia <- read.csv("Vigo_Coia_May_to_September_2024.csv")
colnames(Vigo_Coia) #Nombre de las variables
skimr::skim(Vigo_Coia) #Este comando efectua un resumen de los datos Vigo_Coia
#Tenemos 153 observaciones y 5 variables. 
#Podemos ver 2 valores en rojo relativos a la Humedad relativa que veremos más adelante.
Vigo_Porto <- read.csv("Vigo_Porto_May_to_September_2024.csv")
colnames(Vigo_Porto)
skimr::skim(Vigo_Porto)
#Tenemos 153 observaciones y 11 variables.
Vigo_Cies <- read.csv("Vigo_Cies_May_to_September_2024.csv", encoding="UTF8")
colnames(Vigo_Cies)
skimr::skim(Vigo_Cies)
#Tenemos 153 observaciones y 10 variables
Vigo_Campus <- read.csv("Vigo_Campus_May_to_September_2024.csv")
colnames(Vigo_Campus)
skimr::skim(Vigo_Campus)
#Tenemos 153 observaciones y 11 varibales. Varios datos en rojo que nos indican que ahí pasa algo.

## Air Quality
Vigo_QoA_Coia  <- read.csv("Vigo_Air_data_May_Sept_2024.csv", sep=";")
colnames(Vigo_QoA_Coia)
skimr::skim(Vigo_QoA_Coia)
#153 observacines y 9 variables

# Renombro las variables para hacerlo mas acesible
Vigo_QoA_Coia <- Vigo_QoA_Coia %>% rename("Day"="Data" )
colnames(Vigo_QoA_Coia) #Chequear el nombre de las variables

Vigo_Coia <- Vigo_Coia %>% rename("Day"="Instante.lectura" ,
                     "Rain_coia"="Chuvia",
                     "Coldday_coia"="Horas.de.fr.o.....7.C."  ,
                     "Humi_coia"="Humidade.relativa.media.a.1.5m",
                     "Temp_coia"="Temperatura.media.a.1.5m")
colnames(Vigo_Coia) #Chequear el nombre de las variables 

Vigo_Cies <- Vigo_Cies %>% rename("Day"="Instante.lectura" ,
                                      "Rain_cies"="Chuvia",
                                      "Wind_dir_cies"="Direcci.n.do.vento.predominante.a.10m",
                                      "Coldday_cies"="Horas.de.fr.o.....7.C.",
                                      "SunnyHours_cies"="Horas.de.sol",
                                      "Humi_cies"="Humidade.relativa.media.a.1.5m",
                                      "Insolat_cies"="Insolaci.n",
                                      "Irradia_cies"="Irradiaci.n.global.diaria",  
                                      "Temp_cies"="Temperatura.media.a.1.5m",
                                      "WindSpeed_cies"="Velocidade.do.vento.a.10m")
colnames(Vigo_Cies) #Chequear el nombre de las variables

Vigo_Campus <- Vigo_Campus %>% rename("Day"="Instante.lectura" ,
                                  "Rain_campus"="Chuvia",
                                  "Wind_dir_campus"="Direcci.n.do.vento.predominante.a.10m",
                                  "Coldday_campus"="Horas.de.fr.o.....7.C.",
                                  "SunnyHours_campus"="Horas.de.sol",
                                  "Humi_campus"="Humidade.relativa.media.a.1.5m",
                                  "Insolat_campus"="Insolaci.n",
                                  "Irradia_campus"="Irradiaci.n.global.diaria",  
                                  "Press_campus" ="Presi.n",
                                  "Temp_campus"="Temperatura.media.a.1.5m",
                                  "WindSpeed_campus"="Velocidade.do.vento.a.10m")
colnames(Vigo_Campus) #Chequear el nombre de las variables

Vigo_Porto <- Vigo_Porto %>% rename("Day"="Instante.lectura" ,
                                      "Rain_porto"="Chuvia",
                                      "Wind_dir_porto"="Direcci.n.do.vento.predominante.a.10m",
                                      "Coldday_porto"="Horas.de.fr.o.....7.C.",
                                      "SunnyHours_porto"="Horas.de.sol",
                                      "Humi_porto"="Humidade.relativa.media.a.1.5m",
                                      "Insolat_porto"="Insolaci.n",
                                      "Irradia_porto"="Irradiaci.n.global.diaria",  
                                      "Press_porto" ="Presi.n",
                                      "Temp_porto"="Temperatura.media.a.1.5m",
                                      "WindSpeed_porto"="Velocidade.do.vento.a.10m")
colnames(Vigo_Porto) #Chequear el nombre de las variables


skimr::skim(Vigo_Campus)


# Ver si hay datos que faltan o datos incorrectos en los datasets: Nos damos cuenta que hay 
# algunos valores raros (-9999) los cuales vamos a interpretar que son fallos del instrumento que mide. 

# Esto ocurre en:
#   a) 7 observaciones de la variable Wind_dir_cies (consecutivamente desde el 7 al 13 de Junio).
#   b) 2 observaciones de la variable Wind_dir_campus & WindSpeed_campus (consecutivamente 
#     desde el 17 al 18 de Junio. Además, todas las variables relacionadas coon 
#     efectos del Sol (SunnyHours_campus, Insolat_campus, Irradia_campus) el 
#     17 de Junio están mal (-9999) debido a un fallo de los sensores.
#   c) 2 observaciones en Humi_coia el 28 y 29 de Sept

#El siguiente paso es unir los datasets por 'Day' en uno nuevo llamado "Vigo_data"
# Primero chequeamos que todas tengan el mismo numero de filas (n=153). 
dim(Vigo_Coia)
dim(Vigo_Cies)
dim(Vigo_Campus)
dim(Vigo_Porto)


#attach(Vigo_Coia)
#attach(Vigo_Cies)
#attach(Vigo_Campus)
#attach(Vigo_Porto)
#attach(Vigo_QoA_Coia)


#Usamos la library(tidyverse) para poder unir todo en un dataset
# a) Nos aseguramos que los datos tienen el mismo formato
Vigo_Coia$Day <- lubridate::ymd_hms(Vigo_Coia$Day)
Vigo_Cies$Day <- lubridate::ymd_hms(Vigo_Cies$Day)
Vigo_Campus$Day <- lubridate::ymd_hms(Vigo_Campus$Day)
Vigo_Porto$Day <- lubridate::ymd_hms(Vigo_Porto$Day)
Vigo_QoA_Coia$Day <- lubridate::dmy(Vigo_QoA_Coia$Day)

# b) Ponemos todos los datos en una list
Vigo_data <- list(Vigo_Coia, Vigo_Porto,Vigo_Cies, Vigo_Campus,Vigo_QoA_Coia)


# c) Juntamos todos los datos por "Day"
Vigo_data %>% reduce(full_join, by='Day') %>% dim() # Vemos la dimension
Vigo_data %>% reduce(full_join, by='Day') %>%  head(12) # Vemos las 12 primeras observaciones

# d) La Juntamos
Vigo_data <- Vigo_data %>% reduce(full_join, by='Day')
colnames(Vigo_data)

# Let's use lubridate to a) para identificar que día de la semana es (Monday, Tuesday,..) y mes
#Se añade al dataset una columna  (WeekDay) para saber que día de la semana es.
Vigo_data <- Vigo_data %>% mutate(WeekDay=wday(Day, label=TRUE,  abbr = FALSE,week_start = 1),
                                  Month=as.factor(months(Day)))

# b) Identificamos día laborales, vacaciones y findes de semana. Basicamente añadimos una columna al dataset
#llamada Workday
holidays <- as.Date(c( "2024-05-01","2024-08-16"))
Vigo_data <- Vigo_data %>%  mutate(Workday = as.factor(ifelse(Day %in% holidays,"Holiday",
                                                              ifelse(wday(Day,week_start = 1) < 6,"Business",
                                                                     "Weekend")))) 


     
# Vamos a crear un variable secundaria para obtener las diferencias de temperatura                                                                                                                                 
# entre Coia y la media de temperatura de las otras 3 ubicaciones:
Vigo_data <- Vigo_data %>% rowwise() %>%  
  mutate(DIFFtcoia = round(Temp_coia - mean(c(Temp_porto,Temp_cies,Temp_campus)),2) ) %>% as.data.frame()

# Reubicamos las varibales tipo factor primero
Vigo_data <- Vigo_data %>% relocate(where(is.factor))

# Reasigno los niveles de algunos factores.
Vigo_data <- mutate(Vigo_data, # Reajustar los factores para hacer que el primero sea el factor predeterminado para análisis posteriores
       Month = forcats::fct_relevel(Month,c("mayo","junio","julio","agosto","septiembre")))
       

# Crear un gráfico sencillo con la función plot 
#DUDA, como hago la linea de tendencia del ggplot
plot(Vigo_data$Day, Vigo_data$Temp_coia,
     xlab = "Day of month", 
     ylab = "Temperature in Coia - Vigo (ºC)",
     type = "l")  # type = "l" para gráfico de líneas




 

  

################################################################################
# Tenemos que pensar que hacer con los valores -999. Tenemos diferentes opciones
# podemos eliminar las observaciones de esos días, no tomar esas variables en cuenta
# o substituir esas variables por otras cogiendo esos dias en otras observaciones.
# Eliminar las observaciones de estos días es una estrategia defendible por dos razones:
#la primera es que los valores problemáticos son solo una pequeña fracción y, 
#por lo tanto, eliminarlos no debería afectar gravemente a tus conclusiones.
#Una alternativa es olvidar estas variables problemáticas y completar el proyecto 
#con las restantes. Otra alternativa es ver si estos valores problemáticos pueden ser
#sustituidos de manera segura y razonable por otros recopilados en una estación meteorológica similar.
#Nos decantaremos por eliminar dichas observaciones que nos dan valores de -9999, manteniendo las variables.
################################################################################





#########################




colnames(Vigo_data)

Vigo_data %>% group_by(Month) %>% 
  dplyr::summarize(n=n(),T_Mean=mean(Temp_coia,na.rm =TRUE),
                   T_SD = sd(Temp_coia,na.rm =TRUE),
                   T_MAX = max(Temp_coia,na.rm =TRUE),
                   R_Mean=mean(Rain_coia,na.rm =TRUE),
                   R_SD = sd(Rain_coia,na.rm =TRUE),
                   R_MAX = max(Rain_coia,na.rm =TRUE),
                   W_Mean=mean(WindSpeed_porto,na.rm =TRUE),
                   W_SD = sd(WindSpeed_porto,na.rm =TRUE),
                   W_MAX = max(WindSpeed_porto,na.rm =TRUE)) 

 







######################################
#A continuación vamos a calcular el indice de calidad de aire para los datos Vigo_QoA_Coia
# Cargar datos
data <- read.csv("Vigo_Air_data_May_Sept_2024.csv", sep = ";")

# Ver las primeras filas de los datos
head(data)

# Función para calcular el AQI de un contaminante específico
#Nos hemos guiado siguiendo este documento:
##https://document.airnow.gov/technical-assistance-document-for-the-reporting-of-daily-air-quailty.pdf.
calcular_aqi <- function(concentracion, bp_low, bp_high, i_low, i_high) {
  # Fórmula de AQI
  aqi <- ((i_high - i_low) / (bp_high - bp_low)) * (concentracion - bp_low) + i_low
  return(round(aqi))
}

# Función para obtener los puntos de corte de AQI para cada contaminante
obtener_breakpoints <- function(contaminante, concentracion) {
  # Breakpoints para cada contaminante según estándares comunes (ajustar si es necesario)
  breakpoints <- list(
    "SO2" = data.frame(bp_low = c(0, 36, 76, 186, 305, 605, 805), 
                       bp_high = c(35, 75, 185, 304, 604, 804, 1004), 
                       i_low = c(0, 51, 101, 151, 201, 301, 401), 
                       i_high = c(50, 100, 150, 200, 300, 400, 500)),
    "CO" = data.frame(bp_low = c(0, 4.5, 9.5, 12.5, 15.5, 30.5, 50.5), 
                      bp_high = c(4.4, 9.4, 12.4, 15.4, 30.4, 50.4, 100), 
                      i_low = c(0, 51, 101, 151, 201, 301, 401), 
                      i_high = c(50, 100, 150, 200, 300, 400, 500)),
    "PM25" = data.frame(bp_low = c(0, 12.1, 35.5, 55.5, 150.5, 250.5, 350.5), 
                        bp_high = c(12, 35.4, 55.4, 150.4, 250.4, 350.4, 500.4), 
                        i_low = c(0, 51, 101, 151, 201, 301, 401), 
                        i_high = c(50, 100, 150, 200, 300, 400, 500)),
    "PM10" = data.frame(bp_low = c(0, 55, 155, 255, 355, 425, 505), 
                        bp_high = c(54, 154, 254, 354, 424, 504, 604), 
                        i_low = c(0, 51, 101, 151, 201, 301, 401), 
                        i_high = c(50, 100, 150, 200, 300, 400, 500)),
    "O3" = data.frame(bp_low = c(0, 54, 71, 86, 106, 201), 
                      bp_high = c(53, 70, 85, 105, 200, 404), 
                      i_low = c(0, 51, 101, 151, 201, 301), 
                      i_high = c(50, 100, 150, 200, 300, 500)),
    "NO2" = data.frame(bp_low = c(0, 54, 101, 361, 650, 1250), 
                       bp_high = c(53, 100, 360, 649, 1249, 2049), 
                       i_low = c(0, 51, 101, 151, 201, 301), 
                       i_high = c(50, 100, 150, 200, 300, 500))
  )
  
  # Seleccionar el rango de breakpoints para el contaminante específico
  rango <- breakpoints[[contaminante]] %>%
    filter(concentracion >= bp_low & concentracion <= bp_high)
  
  return(rango)
}

# Calcular AQI diario
data$AQI_SO2 <- sapply(data$SO2.µg.m., function(x) {
  bp <- obtener_breakpoints("SO2", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})

data$AQI_CO <- sapply(data$CO.mg.m., function(x) {
  bp <- obtener_breakpoints("CO", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})

data$AQI_PM25 <- sapply(data$PM25.µg.m., function(x) {
  bp <- obtener_breakpoints("PM25", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})

data$AQI_PM10 <- sapply(data$PM10.µg.m., function(x) {
  bp <- obtener_breakpoints("PM10", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})

data$AQI_O3 <- sapply(data$O3.µg.m., function(x) {
  bp <- obtener_breakpoints("O3", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})

data$AQI_NO2 <- sapply(data$NO2.µg.m., function(x) {
  bp <- obtener_breakpoints("NO2", x)
  calcular_aqi(x, bp$bp_low, bp$bp_high, bp$i_low, bp$i_high)
})


#################
# Asegurarse de que cada columna de AQI sea numérica
data$AQI_SO2 <- as.numeric(data$AQI_SO2)
data$AQI_CO <- as.numeric(data$AQI_CO)
data$AQI_PM25 <- as.numeric(data$AQI_PM25)
data$AQI_PM10 <- as.numeric(data$AQI_PM10)
data$AQI_O3 <- as.numeric(data$AQI_O3)
data$AQI_NO2 <- as.numeric(data$AQI_NO2)

# Calcular el AQI máximo diario
data$AQI_Diario <- apply(data[, c("AQI_SO2", "AQI_CO", "AQI_PM25", "AQI_PM10", "AQI_O3", "AQI_NO2")], 1, max, na.rm = TRUE)

# Mostrar los resultados
data$AQI_Diario <- ifelse(is.infinite(data$AQI_Diario), NA, data$AQI_Diario) #Los valores -inf los convertimos en Na
head(data[, c("Data", "AQI_Diario")])
#View(data[, c("Data", "AQI_Diario")])
####################


######################################
######################################
Vigo_Coia$Humi_coia <- ifelse(Vigo_Coia$Humi_coia == -9999, NA, Humi_coia)
#View(Vigo_Coia)
      
#Fusionamos los datasets de Coia con el de la calidad del aire por día
# Cambiar el nombre de una columna específica
# Verificar las longitudes de cada columna
length(Vigo_Coia)
length(data$AQI_Diario)
length(Vigo_Porto.WindSpeed_porto)
length(Vigo_Porto.Wind_dir_porto)
length(Vigo_Porto.Press_porto)
length(Vigo_data.Month)

colnames(data)[colnames(data) == "Data"] <- "Day"
#Newdata= data.frame(Vigo_Coia$Rain_coia, Vigo_Coia$Coldday_coia, Vigo_Coia$Humi_coia, Vigo_Coia$Temp_coia, data$AQI_Diario, Vigo_Porto$WindSpeed_porto, Vigo_Porto$Wind_dir_porto, Vigo_Porto$Press_porto, Vigo_data$Month )
#Newdata= data.frame(Vigo_Coia, data$AQI_Diario, Vigo_Porto.WindSpeed_porto, Vigo_Porto.Wind_dir_porto, Vigo_Porto.Press_porto, Vigo_data.Month )
Newdata= data.frame( data$AQI_Diario,Vigo_Coia, Vigo_Porto$WindSpeed_porto, Vigo_Porto$Wind_dir_porto, Vigo_Porto$Press_porto, Vigo_data$Month )
#View(Newdata)
#Omitimos ahora las observaciones que contienen los valores NA
Newdata= na.omit(Newdata)
#View(Newdata)
#attach(Newdata)
#Redefinimos las variables que vamos a usar para nuestro trabajo de modelos de regresión
Rain_coia = Newdata$Rain_coia
Temp_coia = Newdata$Temp_coia
Humi_coia= Newdata$Humi_coia
Coldday_coia= Newdata$Coldday_coia
AQI= Newdata$data.AQI_Diario
WindSpeed_porto = Newdata$Vigo_Porto.WindSpeed_porto
Vigo_data.Month= Newdata$Vigo_data.Month
Vigo_Porto.Wind_dir_porto= Newdata$Vigo_Porto.Wind_dir_porto
Vigo_Porto.Press_porto = Newdata$Vigo_Porto.Press_porto
length(Vigo_Porto.Press_porto)
length(AQI)
length(Humi_coia)
####MODELOS MULTIPLES####
mod.m <- lm(AQI ~ Rain_coia+ Temp_coia+ Humi_coia+  + WindSpeed_porto + Vigo_Porto.Press_porto , data= Newdata )
summary(mod.m)
#Podemos ver que las variables de temperatura y presión son bastante significativas. 
# También interpretamos que debido a los estimadores negativos de estas dos variables el indice
# de la calidad del aire es menor cuando la temperatura y la presion son altas.
#Tenemos una variabilidad explicada de entorno al 13%. (No demasiado)
#Vamos a axustar o modelo sin la variable WindSpeed debido a su pvalor alto. (Metodo backward)

mod.m.2 <- lm(AQI ~  Rain_coia + Temp_coia+ Humi_coia + Vigo_Porto.Press_porto, data= Newdata)
summary(mod.m.2)
#Nos encontramos con un modelo bastante similar al anterior. Parece que la lluvia no es influyente en
#la calidad del aire
#Quitamos la variable de la lluvia
mod.m.3 <- lm(AQI ~  Temp_coia+ Humi_coia+ Vigo_Porto.Press_porto, data= Newdata)
summary(mod.m.3)

#Estimaciones para los betas
coef(mod.m.3)
#Vamos a crear un intervalo de confianza para el intercepto en el mod.m.3
confint(mod.m.3, level=0.95)
#Tenemos que para el intercepto (Beta0) su intervalo de confianza al 95% será [170.163352 , 2677.26121080]
#Para el parametro relacionado con la temperatura será [-3.702656   -1.28895390]
#Para el parametro relacionado con la Humedad será [ -0.669005    0.03449673]
#Para el parametro relacionado con la presion será [-2.503610   -0.0434340 ]


#Vamos a representar graficamente el modelo respecto a la humedad
mod.hum <- lm(AQI  ~ Humi_coia, data=Newdata)
summary(mod.hum)
windows()
plot(AQI, Humi_coia, main = "AQI vs Humedad", xlab="AQI", ylab='HUM',  ylim = c(0, 100))
abline(mod.hum, col='blue', lwd=2)
#Vamos a representar graficamente el modelo respecto a la temperatura
mod.temp <- lm(AQI  ~ Temp_coia, data=Newdata)
windows()
plot(AQI, Temp_coia, main = "AQI vs Temperatura", xlab="AQI", ylab='TEMP')
abline(mod.temp, col='red', lwd=2)
#No parece ser una buena representación, los datos no parecen ser de un modelo lineal.
#Vamos a representar graficamente el modelo respecto a la velocidad del viento
mod.press <- lm(AQI  ~ Vigo_Porto.Press_porto, data=Newdata)
windows()
plot(AQI, Vigo_Porto.Press_porto, main = "AQI vs Presion", xlab="AQI", ylab='presion',  )
abline(mod.press, col='green', lwd=2)
#No parece ser una buena representación, los datos no parecen ser de un modelo lineal.
#de feito, no aparece ni la recta ajustada del modelo.

#VALIDACION Y DIAGNOSE para mod.hum
windows()
par(mfrow= c(2,2))
plot(mod.hum)
#En los gráficos podemos ver que el modelo no es del todo homocedastico debido a su tendencia en arco 
#del primer gráfico. Tampoco parece seguir normalidad debido a que en el grafico de Q-Qresiduals no sigue
# en todo momento la linea diagonal.
#Podemos ver que existen observaciones influyentes en el modelo como los son la 144, 142, 141, 91.
# Extraer los residuos del modelo
residuos <- residuals(mod.hum)

# Prueba de Shapiro-Wilk
shapiro.test(residuos)
#Con un pvalor de 2.693e-7 podemos intuir que los residuos NO siguen una distribución normal.
#VALIDACION Y DIAGNOSE para mod.temp
windows()
par(mfrow= c(2,2))

plot(mod.temp)
#En los gráficos podemos ver que el modelo no es homocedastico debido a su tendencia en U 
#del primer gráfico. Tampoco parece seguir normalidad debido a que en el grafico de Q-Qresiduals no sigue
# en todo momento la linea diagonal.
#Podemos ver que existen observaciones influyentes en el modelo como los son la 1,2, 142, 141, 91.
# Extraer los residuos del modelo
residuos <- residuals(mod.temp)

# Prueba de Shapiro-Wilk
shapiro.test(residuos)
#Con un pvalor de 2.788e-7 podemos intuir que los residuos NO siguen una distribución normal.


###DIAGNOSE DEL MODELO LINEAL MULTIPLE###


#Con todo parece que podría ir a mejor.
windows()
par(mfrow= c(2,2))
plot(mod.m.3)

#Distancias de cook
Dcooktemp = cooks.distance(mod.temp)
max(Dcooktemp)
# Umbral común: 4/n
threshold <- 4 / length(Dcooktemp)
# Identificar observaciones influyentes
influential_points <- which(Dcooktemp > threshold)
influential_points
Dcookhum = cooks.distance(mod.hum)
max(Dcookhum)
# Umbral común: 4/n
threshold <- 4 / length(Dcookhum)
# Identificar observaciones influyentes
influential_points <- which(Dcookhum > threshold)
influential_points
Dcookm3 = cooks.distance(mod.m.3)
max(Dcookm3)
# Umbral común: 4/n
threshold <- 4 / length(Dcookm3)

# Identificar observaciones influyentes
influential_points <- which(Dcookm3 > threshold)
influential_points  # Imprime los índices de las observaciones influyentes
#tenemos varias observaciones influyentes (1,2 ,10, 27, 33, 90, 87, 91,88, 129, 126, 141, 131, 132, 142)

# Calcular los residuos estandarizados
residuos_estandarizados <- rstandard(mod.m.3)
shapiro.test(residuos_estandarizados) #parece que los residuos no siguen una distribución normal
residuos_estudentizados <- rstudent(mod.m.3)
shapiro.test(residuos_estudentizados) #parece que los residuos no siguen una distribución normal
# Identificar atípicos usando residuos studentizados
outliers_studentizados <- which(abs(residuos_estudentizados) > 3)  # Usa > 3 si quieres ser más estricto
outliers_studentizados
#tenemos los siguientes datos atípicos 91, 141, 88,131, 142, 132

#Vamos a eliminar los atipicos del dataset
Newdatasinatipicos <- Newdata[-c(88,91,131, 132,141,142),]
Newdatasinatipicos <- data.frame(Newdatasinatipicos)
#modelo_sin_atipicos <- lm(Newdatasinatipicos$AQI ~  Temp_coia+ Humi_coia+ Vigo_Porto.Press_porto, data= Newdatasinatipicos)
modelo_sin_atipicos <- lm(Newdatasinatipicos$data.AQI_Diario ~ Newdatasinatipicos$Temp_coia+ Newdatasinatipicos$Humi_coia+ Newdatasinatipicos$Vigo_Porto.Press_porto, data= Newdatasinatipicos)

summary(modelo_sin_atipicos)
summary(mod.m.3)
#Wow, hemos mejorado la variabilidad explicada y la significación. Pero la pression deja de ser significativa

####ANALISIS ADICIONAL###
#Quiero ver si la diferencia entre la temperatura del puerto es significativamente distinta a la del campus(montaña)
attach(Vigo_Campus)
attach(Vigo_Porto)
attach(Vigo_Cies)
Temp_campus
Temp_porto
windows()
hist(Temp_campus- Temp_porto)
t.test( Temp_campus, Temp_porto,alternative = "two.sided", var.equal = TRUE) #pvalue muy bajo
#Por tanto, es bastante claro que hay una diferencia significativa entre la temperatura registrada en 
#el puerto(costa) y la registrada en el campus (montaña)
summary(lm(Temp_campus~Temp_porto)) #A partir de la temp del puerto podemos explicar el 84% de la temp del Campus
t.test(Temp_campus,Temp_coia) #Un poco menos que el anterior, pero muy significativa
t.test(Temp_coia, Temp_porto,  alternative = "two.sided", var.equal = TRUE) #En cambio, estas dos 
#que son más cercanas a la costa no presentan diferencias significativas
t.test(Temp_cies, Temp_porto)
t.test(Temp_cies, Temp_campus) #Mira que raro, la diferencia entre temp cies y campus no es muy significativa

#Veamos que pasa con la humedad
t.test( Humi_campus, Humi_porto,alternative = "two.sided", var.equal = TRUE)
t.test( Rain_campus, Rain_porto,alternative = "two.sided", var.equal = TRUE)


t.test(WindSpeed_campus, WindSpeed_porto)
t.test(WindSpeed_cies, WindSpeed_porto)
t.test(WindSpeed_campus, WindSpeed_cies)


#Otra conclusión sobre el OZONO
#Una pregunta interesante que nos podemos hacer es si la lluvia reduce el nivel de los 
#gases contaminantes del aire.
summary(lm(NOX.µg.m. ~  Rain_coia   , data= Vigo_data)) # Si pero no significativamente
summary(lm(SO2.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí, significativamente
summary(lm(CO.mg.m. ~  Rain_coia   , data= Vigo_data)) # Sí, significativamente
summary(lm(O3.µg.m. ~  Rain_coia   , data= Vigo_data)) # No,de hecho aumenta el ozono
summary(lm(PM10.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí pero no significativamente
summary(lm(PM25.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí pero no significativamente

#A primera vista podría sorprender que la lluvia no limpie el aire de todos los contaminantes
#como se esperaría. El ozono incluso puede aumentar en días de lluvia y los óxidos de 
#nitrógeno (NOx) y las partículas PM apenas son eliminados.

#### ANOVA #### 
#Uso de la función anova: Contidos: axuste, interpretacións,...

#Definimos cuatro modelos respectivos a Coia:
#attach(Newdata)
#Month = Vigo_data$Month
mu_global =mean(AQI, na.rm = TRUE)
mu_global
var(AQI, na.rm = TRUE)
mu_local=c()
mu_local[1]=mean(AQI[Vigo_data.Month=='mayo'], na.rm=TRUE)
mu_local[2]=mean(data.AQI_Diario[Vigo_data.Month=='junio'], na.rm=TRUE)
mu_local[3]=mean(data.AQI_Diario[Vigo_data.Month=='julio'], na.rm=TRUE)
mu_local[4]=mean(data.AQI_Diario[Vigo_data.Month=='agosto'], na.rm=TRUE)
mu_local[5]=mean(data.AQI_Diario[Vigo_data.Month=='septiembre'], na.rm=TRUE)
mu_local
Month= Newdata$Vigo_data.Month

lm(AQI~Month-1)
alfa=mu_local- mu_global

alfa
#attach(Newdata)
mod.1 <- lm(AQI  ~ Month, data= Newdata )

summary(mod.1)
anova(mod.1)
#Por tanto se rechaza la igualdad en las medias del indice del aire en los distintos meses. 
windows()
par(mfrow=c(2,2))
plot(mod.1)
#Distancia de cook para el modelo anova
Dcookanova = cooks.distance(mod.1)
max(Dcookanova)
# Umbral común: 4/n
threshold <- 4 / length(Dcookanova)
# Identificar observaciones influyentes
influential_pointsanova <- which(Dcookanova > threshold)
influential_pointsanova

#Atípicos##
# Calcular los residuos estandarizados
residuos_estandarizados <- rstandard(mod.1)
shapiro.test(residuos_estandarizados)
residuos_estudentizados <- rstudent(mod.1)
shapiro.test(residuos_estudentizados)
# Identificar atípicos usando residuos studentizados
outliers_studentizadosanova <- which(abs(residuos_estudentizados) > 2)  # Usa > 3 si quieres ser más estricto
outliers_studentizadosanova
#tenemos los siguientes datos atípicos 1 ,11, 33, 88, 91, 89, 92, 93, 90, 141, 131, 142, 132, 144, 134
#Vamos a eliminar los atipicos del dataset
Newdatasinatipicosanova <- Newdata[-c(1, 11, 33,88, 89, 91, 92, 93, 131, 132,134,141, 142, 144),]
Newdatasinatipicosanova <- data.frame(Newdatasinatipicosanova)
modeloanova_sin_atipicos <- lm(Newdatasinatipicosanova$data.AQI_Diario ~  Newdatasinatipicosanova$Vigo_data.Month, data= Newdatasinatipicosanova)

summary(modeloanova_sin_atipicos)
summary(mod.1) #Comparamos con el modelo con atipicos
windows()
par(mfrow=c(2,2))
plot(modeloanova_sin_atipicos)
windows()
par(mfrow=c(2,2))
plot(mod.1)
#Tenemos un incremento de la variabilidad explicada. Y tambien podemos ver como el mes de septiembre es algo significativo.



#calculamos los residuos absolutos del modelo anova (mod.1)
abs_res_anova= abs(residuals(mod.1))
length(abs_res_anova)
length(Vigo_data.Month)
levene = lm(abs_res_anova ~ Vigo_data.Month)
anova(levene)




#########################
#ANCOVA
#Vamos  a considerar las continuas temp y presion y categoricas mes. La var respuesta es el AQI
mod_t_p_m <- lm(AQI  ~ Temp_coia +Vigo_Porto.Press_porto +Month )
summary(mod_t_p_m)
mod_t_p <- lm(AQI  ~ Temp_coia + Vigo_Porto.Press_porto)
summary(mod_t_p)
anova(mod_t_p, mod_t_p_m)
#El efecto del mes es muy significativo en este modelo
mod_t_p_m_it <- lm(AQI  ~ Temp_coia*Vigo_Porto.Press_porto*Month, data=Newdata)
summary(mod_t_p_m_it)


#Contraste modelo de iteración y no iteración
anova(mod_t_p_m, mod_t_p_m_it)
#No podemos decir que la iteración sea significativa.

###Solo temp##
mod_t_m <- lm(AQI  ~ Temp_coia+ Month)
mod_t_m_it <- lm(AQI ~ Temp_coia*Month)
summary(mod_t_m_it)
anova(mod_t_m, mod_t_m_it)
##Solo press##
mod_p_m <- lm(AQI ~ Vigo_Porto.Press_porto+Month, Newdata)
summary(mod_p_m)
mod_p_m_it <- lm(AQI ~ Vigo_Porto.Press_porto*Month, data=Newdata)
summary(mod_p_m_it)
anova(mod_p_m, mod_p_m_it)

###Diagnosis
#Calculamos la distancia de Cook para el modelo con iteración:
Dcook_it <- cooks.distance(mod_t_p_m_it)
max(Dcook_it)
# Umbral común: 4/n
threshold <- 4 / length(Dcook_it)
# Identificar observaciones influyentes
influential_points_it <- which(Dcook_it > threshold)
influential_points_it
#Tenemos los siguientes puntos influyentes: 8, 10, 30, 53, 54, 88, 91, 124, 126, 127, 129, 130, 131, 132, 140, 141, 142
#Un total de 17 observaciones influyentes
#Atípicos##
# Calcular los residuos estandarizados
residuos_estandarizados_it <- rstandard(mod_t_p_m_it)
shapiro.test(residuos_estandarizados_it)
residuos_estudentizados_it <- rstudent(mod_t_p_m_it)
shapiro.test(residuos_estudentizados_it)
# Identificar atípicos usando residuos studentizados
outliers_studentizados_it <- which(abs(residuos_estudentizados_it) > 2)  # Usa > 3 si quieres ser más estricto
outliers_studentizados_it
#tenemos los siguientes datos atípicos 10 ,11, 36, 88, 91, 89, 92, 126, 129, 130, 131, 132, 140, 141, 142
#Tenemos un total de 15 observaciones atípicas
#Vamos a eliminar los atipicos del dataset
Newdatasinatipicos_it <- Newdata[-c(10, 11, 36, 88, 89,91, 92,126, 129, 130, 131, 132, 140,141,142),]
Newdatasinatipicos_it <- data.frame(Newdatasinatipicos_it)
#modelo_sin_atipicos <- lm(Newdatasinatipicos$AQI ~  Temp_coia+ Humi_coia+ Vigo_Porto.Press_porto, data= Newdatasinatipicos)
modelo_sin_atipicos_it <- lm(Newdatasinatipicos_it$data.AQI_Diario ~ Newdatasinatipicos_it$Temp_coia * Newdatasinatipicos_it$Vigo_data.Month * Newdatasinatipicos_it$Vigo_Porto.Press_porto, data= Newdatasinatipicos_it)
modelo_sin_atipicos_t_p_m <- lm(Newdatasinatipicos_it$data.AQI_Diario ~ Newdatasinatipicos_it$Temp_coia + Newdatasinatipicos_it$Vigo_data.Month + Newdatasinatipicos_it$Vigo_Porto.Press_porto, data= Newdatasinatipicos_it)
summary(modelo_sin_atipicos_it)
summary(modelo_sin_atipicos_t_p_m)
anova(modelo_sin_atipicos_it, modelo_sin_atipicos_t_p_m)
#Es evidente que el modelo sin atipicos sin iteracion (el más simple ) es mejor que el otro.
#Se consigue un 38% de la variabilidad explicada, mucho mejor que el modelo con atípicos

#Validacion
# Calcular los residuos estandarizados
#Mayo
datos_mayo <- Newdatasinatipicos_it[Newdatasinatipicos_it$Vigo_data.Month== "mayo", ]
datos_mayo
mod_mayo <- lm(datos_mayo$data.AQI_Diario ~ datos_mayo$Temp_coia*datos_mayo$Vigo_Porto.Press_porto, data=datos_mayo)
summary(mod_mayo)
residuos_estandarizados_mayo <- rstandard(mod_mayo)
shapiro.test(residuos_estandarizados_mayo)
residuos_estudentizados_mayo <- rstudent(mod_mayo)
shapiro.test(residuos_estudentizados_mayo)
windows()
par(mfrow= c(2,2))
plot(mod_mayo)
#Utilizamos el test de Breusch Pagan para ver si los errores son homocedasticos:
bptest(mod_mayo)
#Parece que los errores son homocedassticos, a pesar de que en la grafica de residuals vs fitted no parecen serlo.
resettest(mod_mayo)
#Aplicado el test de Ramsey podemos decir que los datos siguen un modelo lineal en mayo
#JUNIO
datos_junio <- Newdatasinatipicos_it[Newdatasinatipicos_it$Vigo_data.Month== "junio", ]
datos_junio
mod_junio <- lm(datos_junio$data.AQI_Diario ~ datos_junio$Temp_coia*datos_junio$Vigo_Porto.Press_porto, data=datos_junio)
summary(mod_junio)
residuos_estandarizados_junio <- rstandard(mod_junio)
shapiro.test(residuos_estandarizados_junio)
residuos_estudentizados_junio <- rstudent(mod_junio)
shapiro.test(residuos_estudentizados_junio)
#Los errores siguen una distribución normal, p valor de 0.54
windows()
par(mfrow= c(2,2))
plot(mod_junio)
#Utilizamos el test de Breusch Pagan para ver si los errores son homocedasticos:
bptest(mod_junio)
#Parece que los errores son homocedassticos
resettest(mod_junio)
#Aplicado el test de Ramsey podemos decir que los datos siguen un modelo lineal en junio
#JULIO
datos_julio <- Newdatasinatipicos_it[Newdatasinatipicos_it$Vigo_data.Month== "julio", ]
datos_julio
mod_julio <- lm(datos_julio$data.AQI_Diario ~ datos_julio$Temp_coia*datos_julio$Vigo_Porto.Press_porto, data=datos_julio)
summary(mod_julio)
residuos_estandarizados_julio <- rstandard(mod_julio)
shapiro.test(residuos_estandarizados_julio)
residuos_estudentizados_julio <- rstudent(mod_julio)
shapiro.test(residuos_estudentizados_julio)
windows()
par(mfrow= c(2,2))
plot(mod_julio)
#Utilizamos el test de Breusch Pagan para ver si los errores son homocedasticos:
bptest(mod_julio)
#Parece que los errores son homocedassticos
resettest(mod_julio)
#Aplicado el test de Ramsey podemos decir que los datos siguen un modelo lineal en julio

#AGOSTO
datos_agosto <- Newdatasinatipicos_it[Newdatasinatipicos_it$Vigo_data.Month== "agosto", ]
datos_agosto
mod_agosto <- lm(datos_agosto$data.AQI_Diario ~ datos_agosto$Temp_coia*datos_agosto$Vigo_Porto.Press_porto, data=datos_agosto)
summary(mod_agosto)
residuos_estandarizados_agosto <- rstandard(mod_agosto)
shapiro.test(residuos_estandarizados_agosto)
residuos_estudentizados_agosto <- rstudent(mod_agosto)
shapiro.test(residuos_estudentizados_agosto)
windows()
par(mfrow= c(2,2))
plot(mod_agosto)

#Utilizamos el test de Breusch Pagan para ver si los errores son homocedasticos:
bptest(mod_agosto)
#Parece que los errores son homocedassticos
resettest(mod_agosto)
#Aplicado el test de Ramsey podemos decir que los datos siguen un modelo lineal en agosto

#SEPTIEMBRE
datos_sept <- Newdatasinatipicos_it[Newdatasinatipicos_it$Vigo_data.Month== "septiembre", ]
datos_sept
mod_sept <- lm(datos_sept$data.AQI_Diario ~ datos_sept$Temp_coia*datos_sept$Vigo_Porto.Press_porto, data=datos_sept)
summary(mod_sept)
residuos_estandarizados_sept <- rstandard(mod_sept)
shapiro.test(residuos_estandarizados_sept)
residuos_estudentizados_sept <- rstudent(mod_sept)
shapiro.test(residuos_estudentizados_sept)
windows()
par(mfrow= c(2,2))
plot(mod_sept)
#Utilizamos el test de Breusch Pagan para ver si los errores son homocedasticos:
bptest(mod_sept)
#Parece que los errores son homocedassticos
resettest(mod_sept)
#Aplicado el test de Ramsey podemos decir que los datos siguen un modelo lineal en sept

##### CONCLUSIÓN##############
modelo_sin_atipicos_t_p_m #modelo sin atipicos sin iteración ANCOVA
summary(modelo_sin_atipicos_t_p_m)
modeloanova_sin_atipicos #modelo sin atipicos ANOVA (sin la variable mes)
summary(modeloanova_sin_atipicos)
modelo_sin_atipicos_it
summary(modelo_sin_atipicos_it)
#modelo_sin_atipicos #modelo de regresión multple
#summary(modelo_sin_atipicos)
modelo_sin_atipicos_sinhum <- lm(Newdatasinatipicos$data.AQI_Diario ~ Newdatasinatipicos$Temp_coia+ Newdatasinatipicos$Vigo_Porto.Press_porto, data= Newdatasinatipicos)
summary(modelo_sin_atipicos_sinhum) #multiple

#R^2 más alto : ANCOVA
#R^2 ajustado mas alto: múltiple
#Significancia : ANOVA sin atípicos claramente

#CONCLUSION
#Para finalizar, una vez analizado cada modelo por separado hemos llegado a la conclusión que el modelo
#que mejor ajusta nuestros datos es el modelo ANOVA, es decir bastaría saber el mes en el que nos encontramos
#para ajustar los datos y predecir nuevas observaciones. Este hecho, al principio nos causó bastante dolor de
#cabeza debido a que no veíamos posible que el modelo ANOVA fuese mejor que el ANCOVA en nuestro caso. 
#Sin embargo, llegamos a la conclusión siguiente: Las variables de temperatura y presión están de alguna manera
#contenidas implícitamente en la variable mes.\\
#El $R^2$ ajustado para el modelo ANCOVA es más alto que para el ANOVA. Pero esto es debido a que el ANCOVA
#tienen muchas más variables e iteraciones y por eso es lógico aumentar la variabilidad. A pesar de eso, 
#seguimos prefiriendo el modelo ANOVA debido a su significancia, simplicidad. 

###############



#Analisis adicionales:

#Una pregunta interesante que nos podemos hacer es si la lluvia reduce el nivel de los 
#gases contaminantes del aire.
summary(lm(NOX.µg.m. ~  Rain_coia   , data= Vigo_data)) # Si pero no significativamente
summary(lm(SO2.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí, significativamente
summary(lm(CO.mg.m. ~  Rain_coia   , data= Vigo_data)) # Sí, significativamente
summary(lm(O3.µg.m. ~  Rain_coia   , data= Vigo_data)) # No,de hecho aumenta el ozono
summary(lm(PM10.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí pero no significativamente
summary(lm(PM25.µg.m. ~  Rain_coia   , data= Vigo_data)) # Sí pero no significativamente


#A primera vista podría sorprender que la lluvia no limpie el aire de todos los contaminantes
#como se esperaría.
#El ozono incluso puede aumentar en días de lluvia y los óxidos de 
#nitrógeno (NOx) y las partículas PM apenas son eliminados.
#¿Por qué sucede esto? Veamos si la temperatura afecta.
summary(lm(NOX.µg.m. ~  Rain_coia + Temp_coia, data= Vigo_data))
# Temperatura hace que el NOX crezca
summary(lm(SO2.µg.m. ~  Rain_coia + Temp_coia, data= Vigo_data)) 
#La temperatura aumenta significativamente el nivel de dióxido de azufre y ahora,
#la lluvia disminuye ese nivel, pero no de manera significativa. 
#Observamos la alta varianza explicada por este modelo (R-cuadrado ajustado: 0.2655).
summary(lm(CO.mg.m. ~  Rain_coia + Temp_coia , data= Vigo_data)) 
# La temperatura aumenta significativamente y ahora, la lluvia decrezce pero no significativamente.
summary(lm(O3.µg.m. ~  Rain_coia + Temp_coia   , data= Vigo_data)) 
# Interesante, la temperatura aumenta el nivel de ozono pero ahora,
# la lluvia lo reduce pero no significativamente. 



summary(lm(PM10.µg.m. ~  Rain_coia + Temp_coia   , data= Vigo_data)) # La temperatura apenas afecta.
summary(lm(PM25.µg.m. ~  Rain_coia + Temp_coia   , data= Vigo_data)) # La temperatura apenas afecta


##########################
#Definiciones Wikipedia:
# PM10: Son partículas con un diámetro aerodinámico menor o igual a 10 micrómetros (µm).
#Estas partículas pueden ser inhaladas y llegar hasta el sistema respiratorio superior (nariz y garganta).
# PM2.5: Son partículas aún más pequeñas, con un diámetro aerodinámico menor o igual a 2.5 
#micrómetros (µm). Debido a su pequeño tamaño, pueden penetrar más profundamente en los pulmones e
#incluso llegar al torrente sanguíneo.

#Las partículas PM provienen de diversas fuentes, tanto naturales como antropogénicas, incluyendo:

#Emisiones de vehículos (motores de combustión interna).
#Procesos industriales.
#Quema de biomasa y combustibles fósiles.
#Polvo del suelo.
#Actividades agrícolas.
############################

# Otro estudio interesante podría ser la influencia de la actividad humana (principalmente el tráfico) 
#en los niveles de óxidos de nitrógeno.
#https://www.miteco.gob.es/es/calidad-y-evaluacion-ambiental/temas/atmosfera-y-calidad-del-aire/calidad-del-aire/salud/oxidos-nitrogeno.html




