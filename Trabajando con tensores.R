library(readxl)
ria <- read_excel("C:/Users/Lism_/Desktop/Prctica Tucker/matriz ria MASTER.xlsx")
View(ria)

## Nuestros datos están desdoblados por individuos!! ##

#Deberiamos tener 198 columnas y tenemos 199

dim(ria)
class(ria)
colnames(ria)

ria<-as.data.frame(ria)

row.names(ria)<-ria[,1]
head(ria)

ria <- ria[,-1]

head(ria)
dim(ria)

## rrcov3way

library(rrcov3way)

### Para volver a doblar por el modo A.

X<-toArray(ria,6,11,18)
dim(X)
class(X)

## threeway

Y<-rarray(ria,6,11,18)

## Seguimos, vamos a etiquetar el array.

labA<-row.names(ria)
labA
labB<-colnames(ria)[1:11]
labB
labC<-c(paste(c("ENE","FEB","Mar","Abr","May","jun","jul","ago","sept","oct","nov","dic"),"02",sep=""),paste(c("ENE","FEB","Mar","Abr","May","jun"),"03",sep=""))
labC

dimnames(X)<-list(labA,labB,labC)

dimnames(X)

#### Producto de Kronecker, cada elemento de una matriz por la otra matriz entera.

##Creamos matrices fusca para hacerlo

X2<-matrix(c(1,2,1,4,0,3,4,2,2),nrow=3,ncol=3)
Y2<-matrix(c(1,0,0,2,0,0,3,0,0),nrow = 3,ncol=3)
kronecker(X,Y)

#### Producto de Hadamard, multiplicación elemento a elemento de una matriz por otra, el sueño húmedo de todos.

hadamard_list(list(X,Y))

#### Producto de Khatri-Rao, es el produco de Kronecker por columnas

khatri_rao(X,Y)

## Producto de modo n del tensor por una matriz.

ttm()

## Norma de Frobenius para un tensor.

fnorm(as.tensor(arr)) #rTensor

###### Empezamos el Tucker

##rrcov3way 6, 11, 18.

library(rrcov3way)
X
dim(X)
str(X)

## Primero estandarizamos y centramos los datos.

ria.norm<-do3Scale(X, center=T, scale = T, center.mode = "A", scale.mode = "B")
ria.norm
dim(ria.norm)

#Tucker3, con una reducción 2x4x4, que es el modelo que mejor ajusta. Ésto es la dimensión del Core (g).

res<-Tucker3(ria.norm, P=2,Q=4,R=4, center=F, scale=F)

#Mostramos la varianza retenida.55.39%
res$fp

res$A
res$B
res$C

## La amtriz core desdoblada por el modo A

res$GA

## Podemos vovler a hacer array la matriz desdoblada.

X<-toArray(res$GA,2,4,4)
dim(X)
class(X)
X

##Nos centramos en los elementos de la core que son mayores en términos absolutos.
##Representan las mayores interacciones en el modelo. También están asociados a mayor varianza explicada.

##La varianza explicada por cada elemento de la matriz core la podemos calcular.

round(res$GA,3)
res$fp

##Sabemos el total,elevamos los elementos al cuadrado.

core2<-res$GA^2
core2
round(core2/11,3)

##Porcentaje de varianza explicada por cada elemento, por cada interacción.

##Podemos calcular tambien el porcentaje de varianza explicada por cada componente de cada modo.

##Sumamos las varianzas explicadas de las casillas etiquetadas con esa componente.

#####Interpretamos interacciones.

#g111=-1.711, es grande, con signo negativo. P=1, Q=1, R=1.

### Ploteamos los PCA de las matrices A,B y C.

plot(res,which="comp",mode="A", main="Modo A")
plot(res,which="comp",mode="B", main="Modo B")
plot(res,which="comp",mode="C", main="Modo C")

### aHORA HACEMOS UN BIPLOT CONJUNTO, normalmente se fija una componente del modo C, en nuestro caso van a ser 4 biplots, uno para cada componente de C.

### Nosotros vamos a tomar como referencia la componente 1 del modo C