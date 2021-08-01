"1. Lea el archivo"

# Cargamos el archivo con su dirección relativa. Es decir, primero nos situamos
# en la carpeta en donde tenemos el *.csv con >setwd("absolute_dir_path")
data <- read.csv('BASE.csv')

"2. Para cada una de las variables categóricas Tipo_de_inmueble, Operacion, 
Provincia, realice una tabla que cuente cuánto se repite cada categoría (ver 
función table) y lleve a cabo una gráfica de barras de conteo (ver función 
barplot)"

# Grafiquemos la tabla de Tipo_de_inmueble. 
barplot(table(data$Tipo_de_Inmueble),col=c("yellow", "magenta"))
# Grafiquemos la tabla de Operación 
barplot(table(data$Operacion),col=c("red1","limegreen"))
# Graficamos la tabla de Provincia 
barplot(
  table(data$Provincia),
  col=c("darkorange1","yellowgreen","mediumvioletred","deepskyblue1")
  )

"3. Investigue cómo hacer una gráfica de pie o pastel en R para describir la 
variable Provincia y grafiquela en R"

# Grafiquemos la variable Provincia en una grafica de pastel
pie(table(data$Provincia),col=c("darkturquoise","gold1","olivedrab2","plum1"))
text(0.40,0.23,"27.019%",cex=1.1)
text(-0.40,0.20,"27.81%",cex=1.1)
text(0.40,-0.23,"24.23%",cex=1.1)
text(-0.27,-0.40,"20.92%",cex=1.1)

"4. Para las variables numèricas Superficie y Precio de Venta calcule las medidas
de tendencia central Media, Mediana y los percentiles 10%, 25%, 50%, 75%, 90%"

# Calculemos las Medidas de Tendencia Central para Superficie 
# Mediana
median(data$Superficie)
# Media 
mean(data$Superficie)
# Percentiles 
quantile(data$Superficie,0.10)
quantile(data$Superficie,0.25)
quantile(data$Superficie,0.50)
quantile(data$Superficie,0.75)
quantile(data$Superficie,0.90)

# Calculemos las Medidas de Tendencia Central para Precio de Venta
# Primero,convertimos los datos de la columna Precio_Venta de "character" a 
# "numeric" para poder calcular sus medidas.

# Substraemos las comas de los valores
data$Precio_Venta <- sub(",","", 
                         sub(",","", 
                             data$Precio_Venta,
                             fixed=TRUE), 
                         fixed=TRUE)
# Convertimos de "chr" a "num"
data$Precio_Venta  <- as.numeric(data$Precio_Venta)
#Media 
mean(data$Precio_Venta)
#Mediana 
median(data$Precio_Venta)
#Percentiles 
quantile(data$Precio_Venta, 0.10)
quantile(data$Precio_Venta, 0.25)
quantile(data$Precio_Venta, 0.50)
quantile(data$Precio_Venta, 0.75)
quantile(data$Precio_Venta, 0.90)

"5. Para las variables numéricas Superficie y Precio de Venta calcule las 
medidas de dispersión Varianza muestral, Desviación estándar, RIC (Rango 
intercuartílico)."

# Calculamos las medidas para Superficie
#Varianza muestral
var(data$Superficie)
#Desviacion estándar 
sd(data$Superficie)
#RIC
quantile(data$Superficie, 0.75) - quantile(data$Superficie, 0.25)

#Calculamos las medidas para Precio de Venta 
#Varianza muestral 
var(data$Precio_Venta)
#Desviación Estandar 
sd(data$Precio_Venta)
#RIC
quantile(data$Precio_Venta, 0.75) - quantile(data$Precio_Venta, 0.25)

"6. Para las variables numéricas Superficie y Precio de Venta investigue cómo 
calcular el coeficiente de asimetría y mencione si los datos son simétricos o 
asimétricos (si son asimétricos son sesgados a la izquierda o a la derecha)."

# Como vimos en clase, el Coeficiente de Asimetría (skewness) respeta la 
# siguiente ecuación:

# Cargamos la librería necesaria para calcular dicho coeficiente
install.packages("moments")
library(moments)

# Calculamos los coeficientes para ambas columnas
skewness(data$Superficie)
skewness(data$Precio_Venta)

# Notamos que el coeficiente de la columa Superficie es negativo, siendo así que
# dicha variable está sesgada a la derecha. Mientras que el coeficiente de la 
# columna Precio_Venta es positivo, diciendo que esta variable está sesgada a la
# izquierda.
# Naturalmente podemos afirmar que los datos son asimétricos.

"7. Para las variables numéricas Superficie y Precio de Venta investigue cómo 
calcular el coeficiente de curtosis y mencione si los datos son mesocráticos, 
platicurticos o leptocurticos."

# Sabemos que la forma de la ecuación para calcular la kurtosis de ua muestra de
# datos respeta lo siguiente:

# Usando la misma librería que en el ejercicio anterior, calculamos el
# coeficiente de curtosis (kurtosis) de ambas columnas, obteniendo:

kurtosis(data$Superficie)
kurtosis(data$Precio_Venta)

# Con la información anterior, podemos decir que los datos de la columna de 
# Superficie son platicurtica (porque el coeficiente es negativo), mientras que 
# los datos correspondientes a la columna de Punto_Venta son leptocurtica 
# (porque) el coeficiente es positivo).


