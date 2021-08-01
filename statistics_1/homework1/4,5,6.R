"
5. Para las variables numéricas Superficie y Precio de Venta calcule las medidas
de dispersión Varianza muestral, Desviación estándar, RIC (Rango intercuartílico).
"

# Cargamos la data  
data <- read.csv('BASE.csv')

# Calculamos las medidas para la columna Superficie
var_sup <- var(data$Superficie)
sd_sup <- sd(data$Superficie)
ric_sup <- quantile(data$Superficie, 0.75) - quantile(data$Superficie, 0.25)

# Calculamos las medidas para la columna Precio de Venta
var_pv <- var(data$Precio_Venta)
sd_pv <- sd(data$Precio_Venta)
ric_pv <- quantile(data$Precio_Venta, 0.75) - quantile(data$Precio_Venta, 0.25)

sum(is.na(data$Precio_Venta))

library(tigerstats)
hist(data$Precio_Venta)
typeof(data$Precio_Venta)
?hist

vec <- gsub(",", "", data$Precio_Venta)
vec <- as.numeric(vec) 
hist(vec)
var(vec)

data$Precio_Venta[,1]
vec[1]

as.numeric(data$Precio_Venta[1])

col1 <- format(data$Precio_Venta, digits=2)
strtoi(col1, base=10L)


duplicated((data$Precio_Venta)[-1L])

var <- as.integer(1,700.95)
var_num <- as.numeric(gsub(",","", var))

install.packages('eeptools', dependencies = TRUE, repos='http://cran.rstudio.com/')
library(eeptools)

input <- c("10,243", "11,212", "7,011", "5443", "500")
output <- decomma(input)
is.numeric(output)