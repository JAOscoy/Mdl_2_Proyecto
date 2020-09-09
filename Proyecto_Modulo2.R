is.float <- FALSE
class(is.float)
is.caracter <- "Borty"
class(is.caracter)
as.logical(is.float)

borty.vector <- c(2,6,9,14)
?matrix

iris
?iris

iris.chiquito <- select(iris, Sepal.Length, category2)
library(dplyr)

iris.chiquito
mtcars
summarise(mtcars, avg =mean(mpg))
mean(mtcars$mpg)
mtcars

gearing <- group_by(mtcars, gear)
gearing

mutate(mtcars, gpm)

saludo <- function(nombre){
  mensaje <- paste('Hola', nombre)
  return(mensaje)
}
saludo('Ana')

operacion <- function(a,b,c) {
  paso.1 <- (a+b)*4 + (2*a)
  paso.2 <- paso.1**2 + (c/2)
  paso.3 <- paso.2**3
  return(paso.3)
}

operacion(1, 2, 3)

length(mi.vector)

mi.vector[50]

mi.vector.transformaciòn
iris

iris[1:35,]
mtcars
names(mtcars)
nrow(mtcars)
ncol(mtcars)
mtcars[5,3]
mtcars[1:20, 4:5]
data(mtcars)
¿data
iris[iris$Sepal.Width == 160, 1:3]
mtcars[mtcars$disp == 160, 5]
5

getwd()
setwd('documents/BEDU_Data_Analyst/Modulo_2_WebApp')

OperacionSQL <- read.csv('Operacion_SQL.csv')
OperacionSQL$Total <- c(OperacionSQL$Quantity * OperacionSQL$UnitPrice)

mtcars

write.csv(mtcars, file = 'mtcars')
OperacionSQL
?readline

welcome.2 <- function(nombre, edad){
  my.msg <- paste('Bienvenido',nombre, 'de', edad)
  return(my.msg)
}
welcome.2('Ana',24)
str(OperacionSQL)
levels(mtcars$cyl)
?levels

Metro_Interstate_Traffic_Volume 
<- read.csv('https://github.com/beduExpert/A2-Estadistica-Programacion-con-R-2020/blob/master/Data/Metro_Interstate_Traffic_Volume.csv?raw=true', header = TRUE)

levels(Metro_Interstate_Traffic_Volume$weather_main_ordered)

niveles.orden <- c('Christmas Day','Columbus Day','Independence Day','Labor Day','Martin Luther King Jr Day',
                   'Memorial Day','New Years Day','None','State Fair', 'Thanksgiving Day', 'Veterans Day',
                   'Washingtons Birthday')

Metro_Interstate_Traffic_Volume$weather_main_ordered <- factor(x = Metro_Interstate_Traffic_Volume$weather_main, levels = ordered.levels, ordered = TRUE)


unique(Metro_Interstate_Traffic_Volume$weather_main)
ordered.levels <- c("Clear", "Clouds", "Drizzle","Fog","Haze","Mist","Rain","Smoke","Snow","Squall","Thunderstorm")


library(dplyr)

OperacionSQL


mean.traffic <- mean(Metro_Interstate_Traffic_Volume$traffic_volume)
mean.traffic

df.traffic.filter <- Metro_Interstate_Traffic_Volume %>% 
  select(weather_main, traffic_volume) %>% 
  rename(clima = weather_main) %>% 
  rename(trafico = traffic_volume) %>% 
  filter(trafico >= mean.traffic)

dim(df.traffic.filter)

df.traffic.grouped <- df.traffic.filter %>% 
  group_by(clima) %>% 
  summarise(max.traffic = max(trafico),
            min.traffic = min(trafico)) 

str(OperacionSQL)

OperacionSQL.Proveedor <- OperacionSQL %>%
  group_by(Proveedor) %>%
  summarise(Total = sum(Total),
            Unidades_vendidas = sum(Quantity),
            Precio = sum(UnitPrice),
            Pais = mfv(Pais_de_Origen),
            Clientes_atendidos = n_distinct(Cliente)) %>%
arrange(desc(Clientes_atendidos))

OperacionSQL.Proveedor.sorted <- OperacionSQL.Proveedor %>%
  mutate(SRT = sort(Clientes_atendidos, decreasing = TRUE))

OperacionSQL.Proveedor

max.p
install.packages("modeest")
library(modeest)
mtcars$vs = as.logical(mtcars$vs)
mtcars

summary(OperacionSQL)

wt <- (mtcars$wt*1000)

summary(mtcars)

OperacionSQL

mtcars.new <- transform(mtcars, wt = wt * 1000 / 2.204623)
summary(mtcars.new)
?transform
mtcars

names('mtcars$Nombre carro', [1,0])

select(OperacionSQL, Pais_Venta, Cliente, ProductName)

filter(OperacionSQL, grepl("France", Pais_Venta))

variable <- 41
if (variable > 40){
  print("La variable es superior a 40")
} else {
  print ("La variable no es superior a 40")
}

funcion <- function(c)
  
  vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2))
print(result)
rm(list=ls())

install.packages('class')
library(class)


iris
# x's: Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# y's: Species

set.seed(1)
0.8*nrow(iris)

sample(x = 1:nrow(iris), size = 0.8*nrow(iris)

random <- sample(x = 1:nrow(iris), size = 0.8*nrow(iris))
random

iris_train <- iris[random, -5]
iris_train <- iris[]
predict <- knn

install.packages("ggplot2")
library(ggplot2)

diamonds <- as.data.frame(diamonds)

str(diamonds)
nrow(diamonds)*0.8
sample(x = 1:nrow(diamonds), size = 0.8*nrow(diamonds))
diamonds
random <- sample(x = 1:nrow(diamonds), size = 0.8*nrow(diamonds))
random
diamonds_train <- diamonds[random, Sesion_9_samples]
diamonds_train
diamonds <- select(diamonds$cut'')

diamonds <- diamondsfilter(['cut','depth','table','x','y','z'], axis=1)
                             
              
diamonds_test <- diamonds[-random, Sesion_9_samples]

Sesion_9_samples <- c(5, 6, 8, 9, 10)
Sesion_9_cathegory <- 2

diamonds_cathegory_train <- diamonds[random, Sesion_9_cathegory]
diamonds_cathegory_test <- diamonds[-random, Sesion_9_cathegory]
diamonds_test


predict <- knn(train = diamonds_train,
               test = diamonds_test,
               cl = diamonds_cathegory_train,
               k = 3)

predict

con_mat <-table(predict, diamonds_cathegory_test)
Diagonal <- diag(con_mat)
str(diamonds_cathegory_test)
con_mat
dim(con_mat)
Total_train <- colSums(con_mat, )
sum(Total_train)
sum(Diagonal)
Resultado_final <- sum(Diagonal)/sum(Total_train)*100
sum(Resultado_final)

install.packages("DBI")

Mydatabase <- dbConnect(
                    drv = RMySQL::MySQL(),
                    dbname = "Postwork",
                    host = "127.0.0.1",
                    username = "root",
                    password = "Ginette!986")

MyDataBase <- dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest")

install.packages("odbc")
library(DBI)

MyDataBase <- dbConnect(
                        drv = RMySQL::MySQL(),
                        dbname = "shinydemo",
                        host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
                        username = "guest",
                        password = "guest")

install.packages("dbmss")
library(dbmss)
?dbConnect

data <- iris
data.sample <- sample(x = nrow(iris), size = 30)
data.sample2 <- sample(x = nrow(iris), size = 30)
data.sample3 <- sample(x = nrow(iris), size = 30)
data.sample4 <- sample(x = nrow(iris), size = 30)
data.sample5 <- sample(x = nrow(iris), size = 30)
data.sample6 <- sample(x = nrow(iris), size = 30)
data.sample7 <- sample(x = nrow(iris), size = 30)
data.sample8 <- sample(x = nrow(iris), size = 30)
data.sample9 <- sample(x = nrow(iris), size = 30)
data.sample10 <- sample(x = nrow(iris), size = 30)
data.sample
Media_Prework <- mean(data)
Media_Prework
dim(sample)
Sample_1 <- data[data.sample, 2]
mean(Sample_1)
Sample_2 <- data[data.sample3, 1]
mean(Sample_2)

str(data)
install.packages("boot")


OperacionSQL
OperacionSQL.Proveedor
library(ggplot2)
library(dplyr)
library(modeest)

OperacionSQL.Proveedor <- OperacionSQL %>%
  group_by(Pais_de_Origen) %>%
  summarise(Total = sum(Total),
            Unidades_vendidas = sum(Quantity),
            Precio = sum(UnitPrice),
            Importadores = n_distinct(Pais_Venta)) %>%
  arrange(desc(Importadores))

OperacionSQL.Pais_Venta <- OperacionSQL %>%
  group_by(Pais_Venta) %>%
  summarise(Total = sum(Total),
            Unidades_vendidas = sum(Quantity),
            Precio = sum(UnitPrice),
            Ordenes = n_distinct(Id),
            Proveedores = n_distinct(Proveedor),
            Mayor_exportador = mfv(Pais_de_Origen)) %>%
  arrange(desc(Total))
OperacionSQL.Pais_Venta

OperacionSQL.USA <- OperacionSQL %>% 
  filter(Pais_Venta == 'USA') %>%
select(ProductName, Id, Total, Pais_de_Origen)

OperacionSQL.USA

library(ggplot2)
library(RColorBrewer)
install.packages('RColorBrewer')
display.brewer.all()
co <- unique(OperacionSQL.USA$Pais_de_Origen)
grafica1 <- ggplot(OperacionSQL.USA, aes(y = Total, x = Pais_de_Origen))
grafica5 <- grafica4 + theme(axis.text.x = element_text(angle = 45, hjust = 1))
grafica2 <- grafica1 + geom_bar(stat='identity', width = 1, fill = '#00008B')
grafica3 <- grafica2 + ggtitle('Importaciones a USA')
grafica4 <- grafica3 + scale_y_continuous(breaks = seq(0,70000,10000))
grafica5

co
grafica 1 <- ggplot(OperacionSQL.USA, aes(x = OperacionSQL.Producto$ProductName, y = OperacionSQL.Producto$Total))
vis + geom_bar(aes(fill=class), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplot(data = OperacionSQL, aes)
OperacionSQL.Producto$ProductName
str(Operacion_SQL)
unique(Operacion_SQL$ProductName)
OperacionSQL.Producto
OperacionSQL.Proveedor
write.csv(OperacionSQL.Producto, file = 'OperacionSQL.Producto.csv')
diamonds
summary(OperacionSQL)

xa <- c(2, 3, 10, 5, 6, 7, 8)
xa
OperacionSQL.ggplot <- OperacionSQL[,xa]
OperacionSQL.ggplot
OperacionSQL.ProductoII <- OperacionSQL.Producto[20:60,]
OperacionSQL.ProductoII
grafica1 <- ggplot(OperacionSQL.ProductoII, aes(OperacionSQL.ProductoII$))
grafica1
grafica1 <- ggplot(OperacionSQL.ggplot, aes(Pais_Venta))
grafica2<- grafica1 + geom_bar(aes(fill=ProductName), width = 5)                                        
grafica2
last_plot()
ggsave(grafica2.png, width = 30, height = 30)
plot(grafica2)

vis <- ggplot(mpg, aes(manufacturer))
vis + geom_bar(aes(fill=class), width = 5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

str(OperacionSQL)
