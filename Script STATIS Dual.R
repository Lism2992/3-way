
install.packages("ade4",repos="http://cran.r-project.org")
library(ade4)
library(corrplot)
library(xlsx)
library(openxlsx)
library(sp)

paquetes <- c("ade4", "corrplot", "xlsx", "openxlsx")
lapply(paquetes, require, character.only=TRUE)

setwd("C:/Users/Lism_/Desktop/Clima")

Data2 <- data(morpho)

##STATIS dual. Utilizaremos los datos jv73, disponibles en R en el paquete ade4, pero en esta ocasi?n ser?n le?dos desde excel:
#data(jv73) 


#Lectura de los datos (6 variables fisicas de medidas en 92 lugares, pertenecientes a 12 r?os): 

# 1- Cambiar el directorio a la carpeta donde est?n disponibles los datos. Para ello:

#Session -> Set Working Directory -> Choose Directory (abrir la carpeta donde est?n los datos)

# 2- Leer los datos txt (otra opcion: leer desde el excel, import?ndolos)


#Leer texto delimitado por tabulaciones


#Comprobacion datos matriz:
 morfo <- data(Data2$morpho)
dim(morfo)
morfo
head(morfo)
row.names(morfo)
colnames(morfo)

datosexp <- jsonlite::fromJSON("https://opendata.aemet.es/opendata/sh/86b6fe55")
datosexp


write.xlsx(datosexp, clima, sheetName="Sheet1")

#Creamos el factor que organizar? las observaciones en las diferentes submatrices:
morfo$Rio
rio<-morfo$Rio
class(rio)

#Quitamos dicho factor de la matriz num?rica y lo guardamos con el nombre tablas

datos<-morfo[,-1]
datos
dim(datos) #Matriz de 92 lugares sobre los que se han medido 6 variables f?sicas


#Creamos el objeto ktab:
#Primero hacemos el PCA normalizado (normalizar los datos por tablas)
withinrio<-withinpca(datos,rio, scann=FALSE)
withinrio
withinrio$fac

#Convertimos los datos normalizados en un objeto de clase ktab (es el tipo de objeto con el que necesita
#trabajar la funci?n statis)
ktab1<-ktab.within(withinrio)
ktab1

#STATIS: En este paso diagonaliza la matriz de correlaciones entre matrices.

## Se calculan los pesos y se descompone la matriz de compromiso.

statis1<-statis(ktab1, scann=FALSE)
print(statis1)

#Informaci?n relacionada con la interestructura:

statis1$RV #matriz de coeficientes RV
statis1$RV.eig  #Valores propios
statis1$RV.coo  #Tabla con las puntuaciones
statis1$tab.names  #Nombres de cada submatriz
statis1$RV.tabw  #Los pesos asignados a cada submatriz

cor.plot <- corrplot(statis1$RV)

#Informaci?n relacionada con el compromiso

statis1$C.li #a data frame with the row coordinates
statis1$C.Co  #a data frame with the column coordinates


#Gr?ficamente, podemos representar la interestructura, el compromiso
#las proyecciones de las componentes sobre el espacio compromiso y un diagrama de dispersi?n que nos muestra
#la relaci?n entre la calidad de representaci?n y el peso de cada tabla

plot(statis1,plabels.boxes.draw=FALSE)

#Podemos obtener cada gr?fico por separado:

s.corcircle(statis1$RV.coo, lab=statis1$tab.names, sub="INTERESTRUCTURA")

s.arrow(statis1$C.li, plabels.boxes.draw=FALSE, sub="VARIABLES EN EL ESPACIO COMPROMISO")
s.label(statis1$C.li, plabels.boxes.draw=FALSE, sub="VARIABLES EN EL ESPACIO COMPROMISO")

#Coordenadas de los 92 lugares en el compromiso 
plot(statis1$C.Co[,1:2], col=rio,pch=19)

#Por ?ltimo, si queremos ver el comportamiento (trayectoria) de las variables en cada uno de los r?os. 
#CUIDADO, en el caso del STATIS dual no nos muestra esta informaci?n sino que
#simplemente vemos la posici?n de cada uno de los lugares en cada tabla.

#ITrayectorias de puntos de muestreo en cada submatriz:

kplot(statis1, traj = TRUE, arrow = FALSE,plabels.boxes.draw=FALSE)
kplot(statis1, traj = T, arrow = FALSE)

if(adegraphicsLoaded()) {
  s.arrow(statis1$C.li, pgrid.text.cex = 0)
  kplot(statis1, traj = TRUE, arrow = FALSE, plab.cex = 0, psub.cex = 3, ppoi.cex = 3)
} else {
  s.arrow(statis1$C.li, cgrid = 0)
  kplot(statis1, traj = TRUE, arrow = FALSE, unique = TRUE, 
        clab = 0, csub = 3, cpoi = 3)
}


#OTRO GRAFICO:

if(adegraphicsLoaded()) {
  if(requireNamespace("sp", quietly = TRUE)) {
    g11 <- s.label(jv73$xy, Sp = jv73$Spatial, pori.incl = FALSE, plab.cex = 0.75, plot = FALSE)
    g12 <- s.class(jv73$xy, jv73$fac.riv, ellipseSize = 0, pellipses.axes.draw = FALSE, 
                   starSize = 0, ppoints.cex = 0, plab.cex = 1.25, plot = FALSE)
    g1 <- superpose(g11, g12, plot = TRUE)
    
    g2 <- kplot(statis(w), perm = TRUE, row.plab.cex = 0, posieig = "none")
  }
  
} else {
  s.label(jv73$xy, contour = jv73$contour, incl = FALSE, clab = 0.75)
  s.class(jv73$xy, jv73$fac.riv, add.p = TRUE, cell = 0, axese = FALSE, csta = 0, 
          cpoi = 0, clab = 1.25)
  
  kplot(statis(w), perm = TRUE, clab.r = 0, clab.c = 2, show = FALSE)
}

jv73$xy
