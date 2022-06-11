#Instalación de paquetes

install.packages("tidyr")
install.packages('dplyr')
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")#funcion word: extrae parte de un texto
library(tidyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

#Extracción y lectura de los datos

name_zip<-"usedCarsFinal.csv.zip"#dir<-"./R/Proyectos/Uber/uber.csv.zip"
unzip(name_zip,lis=TRUE)#Archivos contenidos en el .zip
Data<-read.table(unzip(name_zip,"usedCarsFinal.csv"),sep=",",head=TRUE)


#Resumen de los datos

head(Data,10)
summary(Data)

#Correjimos los datos de las siguientes variables que estan mal codificadas

#mileage
Data$mileage <-as.numeric(gsub(",","",gsub(" km","",Data$mileage))) 

#engine_capacity
Data$engine_capacity <- as.numeric(gsub(" cc","",Data$engine_capacity))
Data <- Data[is.na(Data$engine_capacity)==FALSE,]

#Creación de nuevas variables

Data["marca"] <- word(Data$name,start = 1,end=1)
Data["provincia"] <- word(Data$location,-1)

#Separamos componentes del auto contenidas en other_feature_list

df_features <- data.frame(id=1:dim(Data)[1],features=Data$other_features_list) 

df_features <- df_features %>% separate_rows(features,sep=",") %>% 
  mutate(i=1) %>% spread(features,i,fill=0)

#Eliminamos espacio de nombres de columnas
colnames(df_features) <- gsub(" ","",colnames(df_features))

#Reemplazamos valores perdidos de features por la moda y eliminamos variables con menor varianza
porcent_features <- colSums(df_features[,3:dim(df_features)[2]])/dim(df_features)[1]
df_features[df_features$V1==1,3:dim(df_features)[2]] <- t(ifelse(porcent_features>=0.5,1,0))
nombres <- names(porcent_features[porcent_features<0.05|porcent_features>0.95 ])
df_features[nombres] <- NULL

#Agregamos features a Data y eliminamos variables
Data <- data.frame(Data,df_features)

sum(grepl("pakwheel",Data$url))#Todos los autos están en pakwheel.com
Data <- subset(Data,select = -c(name,location,url,other_features_list,id,V1))

rm(df_features)

#Gráficos de los datos

#Boxplots
ggplot(Data,aes(x=mileage))+geom_boxplot()

ggplot(Data,aes(x=engine_capacity))+geom_boxplot()

ggplot(Data,aes(x=price))+geom_boxplot()

#Histograms
ggplot(Data,aes(x=mileage))+geom_histogram(fill="steelblue")

ggplot(Data,aes(x=engine_capacity))+geom_histogram(fill="steelblue")

ggplot(Data,aes(x=log(price)))+geom_histogram(fill="steelblue")

#grafico de barras de valiables categóricas
ggplot(Data,aes(x=model_year))+geom_bar()

ggplot(Data,aes(x=engine_type))+geom_bar()

ggplot(Data,aes(x=transmission))+geom_bar()

ggplot(Data,aes(x=assembly))+geom_bar()

ggplot(Data,aes(y=body_type))+geom_bar()

ggplot(Data,aes(y=marca))+geom_bar()

ggplot(Data,aes(y=provincia))+geom_bar()

#Gráfico de puntos de las variables
ggplot(Data,aes(x=mileage,y=price))+geom_point()

ggplot(Data,aes(x=engine_capacity,y=log(price)))+geom_point()

ggplot(Data,aes(x=model_year,y=mileage))+geom_point()

#Revisamos datos en blanco

colSums(Data=="")

#Imputamos valores perdidos o en blanco y creamos nuevas variables
Data$body_type[Data$body_type==""] <- "Sedan"

Data$price[is.na(Data$price)] <- median(Data$price,na.rm = TRUE)

Data["years"] <- 2022-Data$model_year

Data$model_year <- NULL

#Agrupamos las categorías que ocupan menos del 5%
#Funcion que crea tabla de frecuencias
table_frec <- function(df){
  df1 <- data.frame(table(df))
  df1["percent"] <- df1[,2]/sum(df1[,2])*100
  df1 <- df1[order(df1[,2],decreasing = TRUE),]
  return(df1)
}

tf_registered <- table_frec(Data$registered_in)
Data["registered_in"] <- lapply(Data["registered_in"],function(x) ifelse(x %in% tf_registered$df[tf_registered$percent<5],"Others",x))

tf_color <- table_frec(Data$color)
Data["color"] <- lapply(Data["color"],function(x) ifelse(x %in% tf_color$df[tf_color$percent<4],"Others",x))

tf_body_type <- table_frec(Data$body_type)
Data["body_type"] <- lapply(Data["body_type"],function(x) ifelse(x %in% tf_body_type$df[tf_body_type$percent<1.9],"Others",x))


tf_marca <- table_frec(Data$marca)
Data["marca"] <- lapply(Data["marca"],function(x) ifelse(x %in% tf_marca$df[tf_marca$percent<3],"Others",x))

tf_provincia <- table_frec(Data$provincia)
Data["provincia"] <- lapply(Data["provincia"],function(x) ifelse(x %in% tf_provincia$df[tf_provincia$percent<5],"Others",x))

rm(list=c('tf_registered','tf_color','tf_body_type','tf_marca','tf_provincia'))

#Lectura de datos guardado
Data <- read.table("DataDepurada.csv",head=TRUE,sep=",")

#Aplicamos one hot encoding para variables categoricas

library(caret)
dmy <- dummyVars(" ~ .", data = Data, fullRank = F)

Data <- data.frame(predict(dmy, newdata = Data))


#Eliminamos otras variables para evitar multicolinealidad

name_var <- colnames(Data)[grepl('Others',colnames(Data))]
name_var <- c(name_var,c("engine_typeLPG","transmissionManual","assemblyLocal"))
Data[name_var] <- NULL

#Separamos la data en train y test (75 y 25 %)
set.seed(25)
rows_data <- nrow(Data)
rows_test <- sample(1:rows_data,size = floor(0.25*rows_data),replace = FALSE)
Train <- Data[-rows_test,]
Test <- Data[rows_test,]

#Ajustamos modelo de Regresion Lineal Multiple

model <- lm(log(price)~.-colorBlue-colorSilver-marcaHonda
            -provinciaIslamabad-provinciaKPK-AM.FMRadio-CDPlayer-CoolBox-CassettePlayer
            -PowerWindows, data=Data)

summary(model)
plot(model)

Data[]

Data2 <- Data %>% mutate_at(c('price','mileage'), ~( scale (.)%>% as.vector ))
model <- lm(price~.,data=Data2)

