#Instalación de paquetes
install.packages("tidyr")
install.packages('dplyr')
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("stringr")#funcion word: extraer parte de un texto
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

# #Eliminamos filas con na y duplicados
# sum_na <- sum(is.na(Data))#filas con nulos
# Data <- Data[rowSums(is.na(Data))==0,]
# Data <- distinct(Data)

#Correjimos los datos de las siguientes variables
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

#Reemplazamos valores perdidos de features por la moda
porcent_features <- colSums(df_features[,3:dim(df_features)[2]])/dim(df_features)[1]
df_features[df_features$V1==1,3:dim(df_features)[2]] <- t(ifelse(porcent_features>=0.5,1,0))

#Agregamos features a Data y eliminamos variables
Data <- data.frame(Data,df_features)

sum(grepl("pakwheel",Data$url))#Todos los autos están en pakwheel.com
Data <- subset(Data,select = -c(name,location,url,other_features_list,id,V1))

#Gráficos de los datos
#Boxplots
ggplot(Data,aes(x=mileage))+geom_boxplot()

ggplot(Data,aes(x=engine_capacity))+geom_boxplot()

ggplot(Data,aes(x=price))+geom_boxplot()

#Histograms
ggplot(Data,aes(x=mileage))+geom_histogram(fill="steelblue")

ggplot(Data,aes(x=engine_capacity))+geom_histogram()

ggplot(Data,aes(x=log(price)))+geom_histogram()

shapiro.test(log(Data$price))
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
#attach(Data)
#detach(Data)
colSums(Data=="")

#Imputamos valores perdidos y creamos nuevas variables
Data$body_type[Data$body_type==""] <- "Sedan"

Data$price[is.na(Data$price)] <- mean(Data$price,na.rm = TRUE)

Data["years"] <- 2022-Data$model_year

Data$model_year <- NULL


#Agrupamos entre categorías
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
Data["color"] <- lapply(Data["color"],function(x) ifelse(x %in% tf_color$df[tf_color$percent<1.5],"Others",x))

tf_body_type <- table_frec(Data$body_type)
Data["body_type"] <- lapply(Data["body_type"],function(x) ifelse(x %in% tf_body_type$df[tf_body_type$percent<1.3],"Others",x))
table_frec(Data$marca)

tf_marca <- table_frec(Data$marca)
Data["marca"] <- lapply(Data["marca"],function(x) ifelse(x %in% tf_marca$df[tf_marca$percent<1.3],"Others",x))

tf_provincia <- table_frec(Data$provincia)
Data["provincia"] <- lapply(Data["provincia"],function(x) ifelse(x %in% tf_provincia$df[tf_provincia$percent<1.3],"Others",x))

#Convertimos en factores
col_names <- c("engine_type","transmission","registered_in","color","assembly","body_type","marca","provincia")
Data[col_names] <- lapply(Data[col_names],factor)
str(Data)

model <- lm(log(price) ~.-mileage+I(1/mileage),data = Data)
summary(model)
plot(model)

