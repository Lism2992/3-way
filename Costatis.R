## En anisostatis tenemos ponderaciones, y ahora al formar la matriz de correlaciones lo hacemos elemento a elemento,
## tiene dimensiones p x p, las p variables son la suma de todas las variables de todas las matrices.

## STATICO & COSTATIS

library(ade4)

?statico

## Vamos a tener que montar las matrices de datos igual que antes, poniendo lo común en las columnas
## y apilando las filas una tras otra hasta tener una mega matriz que separaremos según cierta etiqueta.

data("meau")
meau

## Tiene 3 matrices,
## 1ª variables ambientales en 24 puntos muestreeo
## 2ª son poblaciones de especies en puntos de muestreo, es para organizar la info.
## 3ª matrices de diseño, a qué estación corresponde cada una de las observaciones.

## Vamos a hacer el Statico con ésta matriz de variables y poblaciones en ubicaciones.

## necesitamos 2 objetos de tipo k_tabla, los montamos.

## Variables ambientales PCA, es cuanti, recuentos de poblaciones le hacemos correspondencias, son datos de frecuencia.

wit1 <- withinpca(meau$env, meau$design$season, scan=FALSE, scal = "total")

wit1

wit2 <- withinpca(meau$spe, meau$design$season, scan=FALSE, scal = "total")

## El centrado va a ser del total, el escalado puede ser total o parcial a cada una de las matrices.

spec <- dudi.pca(df=meau$spe, scale=FALSE, scan=FALSE, nf=2)

## tiene que ver con las correspondencias, creo que está por eso.

wit2 <- wca(spec,meau$design$season, scan=FALSE, nf=2)
wit2

### Ya hemos elaborado las 2 k_tabs.

## Podemos utilizar ya el STATICO.

kta1 <- ktab.within(wit1)
kta2 <- ktab.within(wit2)

statires <- statico(kta1, kta2, scan=FALSE )
statires
str(statires)

kplot(statires)

##Debería plotearlos pero no se que pasa con la ventana de los plots que está funcando bastante mal.

?costatis

##El scan es para que no nos pida los ejes a retener, que los calcule él.



kta1b <- ktab.within(wit1, colnames = rep(c("S1","S2","S3","S4","S5","S6"),4))

kta2b <- ktab.within(wit2, colnames = rep(c("S1","S2","S3","S4","S5","S6"),4))

statires2 <- costatis(kta1b, kta2b, scan=FALSE )


plot(statires2)

## Cuanto más corto el vector, más se parecen en una representación o en otra.