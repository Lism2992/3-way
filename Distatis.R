library(DistatisR)

Data.práctica <- data("DistAlgo")

summary(Data.práctica)

data(SortingBeer)
Sort

data(SortingSpice)
SortingSpice
SortSpice

## Son tablas de datos donde se dan puntuaciones de cervezas o especias, respectivamente.

## Para utilizar Distatis necesitamos matrices de distancias, nosotros tenemos ratings.

## Podemos utilizar la distancia chi cuadrado por ejemplo.

?Chi2Dist

Dist <- Chi2Dist(SortSpice)

summary(Dist)
 
Dist

## Ya tenemos una matriz de distancias entre filas, cojonudo.

?DistanceFromSort

## Es una matriz donde da los parecidos para cada uno de los jueces, está en 3 dimensiones.

?distatis

##Normalización, divide por la raíz cuadrada del valor propio, hace la variabilidad de las tablas comparables.

## En la ayuda, la matriz C es de los productos internos entre individuos.

distatisres <- distatis(DistAlgo)
summary(distatisres)
distatisres
distatisres$res4Cmat
distatisres$res4Cmat$C

## Aquí podemos pedirle todo lo que queramos, hay ficheros de ficheros. 
## Podemos cambiar el número de dimensiones que nos saca el análisis con un argumento.

distatisres$res4Splus

## Aquí en el plus tenemos para poder pintar las matrices en el espacio compromiso.

GraphDistatisAll(distatisres)
?GraphDistatisAll

GraphDistatisRv(distatisres$res4Cmat$eigVector)

distatisres$res4Cmat$alpha

## C es la matriz intraestructura. El tamaño de los puntos es la contribución de cada matriz en el compromiso.
## En ésta, los que más pesan son los que mayor valor tengan en la componente 1 del PCA.

## Los alphas son los pesosen teoría.

GraphDistatisCompromise(distatisres$res4Splus$F)

## Éstas eran las matrices originales representadas en el espacio compromiso.

## Podemos también plotear las trayectorias.

GraphDistatisPartial(distatisres$res4Splus$F,distatisres$res4Splus$PartialF)

## Le damos los individuos medios y las parciales de la intraestructura para que pinte las dos partes.
## Nos representa en el espacio compromiso las matrices originales.

## Si los 4 algoritmos llevaran a mismo resultado, los puntos pequeños covergirían en los grandes, 
## son las diferencias en función del algoritmo que se esté usando.
## Las pequeñas son las proyecciones de 

## podemos ver cuál difiere menos o más.

## Interestructura, espacio compromiso y trayectorias es lo que hemos representado.

## Si metemos distance = "FALSE" como el argumento podríamos trabajar con matrices covarianza.

## Este paquete tiene una versión inferencial con boostrap que incluye regiones de confianza.

## Las resplus F, cuando tenemos el gráfico de los individuos en espacio compromiso, es un MDS, o eso creo.

## Existe el ANISOSTATIS, que está ahora en plena investigación.

## Dos versiones, según se quiera maximizar variabilidad compromiso o calcular el compromiso más similar.

##STATIS4 no existe en R, podríamos diseñarla.
