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
