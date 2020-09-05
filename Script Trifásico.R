#STATIS FAMILIAS y VARIABLES riaño
library(readxl)
FAMILIAS <- read_excel("FAMILIAS.xlsx")
View(FAMILIAS)
#SI QUEREMOS IMPORTAR DESDE EXCEL.
library(readxl)

#importamos desde txt con el botón import dataset y ponemos la primera columna como etiquetas

variables
variables$semana
str(variables)

#Comprobacion datos matriz:
dim(variables)
head(variables)
row.names(variables)
colnames(variables)
variables$semana
semanafac<-variables$semana
class(semanafac)
semanafac

#Quitamos el factor semana de los datos
riañovar=variables[,-1]
colnames(riañovar)
riañovar
str(riañovar)

#otra manera de hacer los factores
semana=as.factor(rep(c("A","B","C","D","E","F","G","H","I","J"),each=7))
semana
profundidad=as.factor(rep(c("p0","p2","p5","p10","p15","p20","pfdo"),times=10))
profundidad
varfq=as.factor(rep(c("T","O2","pH", "Con","SIO","PO4","Cha","Chb","Chc","IM"),times=10))
varfq

## Es importante que todas las tablas tengan los mismos nombres en las filas y en las variables, si
## no se hace el lío.



#hacemos el wihinpca
withinpca1<-withinpca(riañovar, semanafac, scal="partial", scann=FALSE)
withinpca1
summary(withinpca1)

#formamos las ktablas
kta.env=ktab.within(withinpca1,colnames = rep(c("p1","p2","p3","p4","p5","p6","p7"),10))
kta.env

#necesitamos transponer la tabla por que nos ha puesto las variables como filas y no van a ser vectores
## si no puntos, y eso no mola nada.
ktapta=t(kta.env)

#hacemos el statis
pta.env<-pta(ktapta, scann=FALSE)
pta.env
print(pta.env)

#para pintar los valores propios de la interestructura
plot(pta.env$RV.eig, col=colors)
barplot(pta.env$RV.eig, col=colors)
pta.env$RV.eig

#para pintar la interestructura:

IS.VAR= s.corcircle(pta.env$RV.coo,xax=1,yax=2,labels=pta.env$tab.names,plabels.boxes.draw=FALSE,sub="INTERESTRUCTURA",pgrid.draw = FALSE)

#pesos de las tablas para formar el compromiso
pta.env$tabw
pta.env$cos2
Pesos=barplot(pta.env$tabw,names.arg=pta.env$tab.names, sub="pesos")
Cosenos2=barplot(pta.env$cos2,names.arg=pta.env$tab.names, sub="cos2")

#representamos el statis
plot(pta.env,plabels.boxes.draw=FALSE)
summary(pta.env)

## 1º Pesos de las matrices, antes de elaborar el compromiso.


#más resultados numéricos, calidades de representación de items y vars.
inertia.dudi(pta.env, col=T)
inertia.dudi(pta.env, row=T)


#para pintar sólo laS variables compromiso:
g0=s.arrow(pta.env$co,plabels.boxes.draw=FALSE,sub="COMPROMISO",plabels.cex=1,plabels.col = "red")
g0
#para pintar sólo las profundidades compromiso:
g1=s.label(pta.env$l1,plabels.boxes.draw=FALSE,sub="COMPROMISO",plabels.cex=1.5,plabels.col = "blue")
g1
#para pintarlos superpuestos:
g01=superpose(g0,g1,plot=TRUE)


#Trayectorias
kplot(pta.env,plabels.boxes.draw=FALSE)


# MAS TRAYECTORIAS
s.label(pta.env$Tli,facets = pta.env$TL[,1],labels=profundidad)

s.label(pta.env$Tli,facets=pta.env$TL[,2],plines.lty=1:nlevels(profundidad), labels=row.names(variables))

s.label(pta.env$Tli,facets=pta.env$TL[,2],plines.lty=1:nlevels(profundidad), labels=row.names(variables))

s.arrow(pta.env$Tco,facets=pta.env$TC[,1])



#más gráficos
g3=s.class(dfxy = pta.env$Tli, fac = profundidad, xax = 1, yax = 2, plot = FALSE, 
           add = FALSE, plabels.cex = 1, ppoints.pch = 20, ppoints.cex = 1, psub.cex = 1, 
           paxes.draw = FALSE, pgrid.draw = FALSE, pgrid.text.cex = 1, porigin.include = TRUE, 
           porigin.origin = c(0, 0), pellipses.axes.draw = FALSE, psub.position = "bottomleft",
           wt = rep(1, NROW(profundidad)), labels = levels(profundidad), ellipseSize = 1.5, 
           starSize = 1, chullSize = NULL, col = NULL, facets = NULL, storeData = TRUE, pos = -1)
plot(g3)

library(RColorBrewer)
display.brewer.all() 
brewer.pal(n = 10, name = "Set3")
colors=brewer.pal(n = 10, name = "Spectral")
colors

g4=s.class(dfxy = pta.env$Tli, fac = profundidad, xax = 1, yax = 2, plot = FALSE,
           add = FALSE, plabels.cex = 1, ppoints.pch = 20, ppoints.cex = 1, psub.cex = 1, 
           paxes.draw = FALSE, pgrid.draw = FALSE, pgrid.text.cex = 1, porigin.include = TRUE, 
           porigin.origin = c(0, 0), pellipses.axes.draw = FALSE, psub.position = "bottomleft", 
           wt = rep(1, NROW(profundidad)), labels = levels(profundidad), ellipseSize = 1.5, 
           starSize = 1, chullSize = NULL, col = NULL, facets = NULL, storeData = TRUE, pos = -1)

plot(g4)

g5=s.class(dfxy = pta.env$Tli, fac = profundidad, xax = 1, yax = 2, ellipseSize = 1.5, 
           starSize = 1, col = colors, plot = FALSE, add = FALSE, plabels.cex = 1, ppoints.pch = 20,
           ppoints.cex = 1, psub.cex = 1, paxes.draw = FALSE, pgrid.draw = TRUE, pgrid.text.cex = 1, 
           porigin.include = TRUE, porigin.origin = c(0, 0), pellipses.axes.draw = TRUE,
           psub.position = "bottomleft", wt = rep(1, NROW(profundidad)), labels = levels(profundidad),
           chullSize = NULL, facets = NULL, storeData = TRUE, pos = -1)

plot(g5)
g6=s.class(dfxy = pta.env$Tli, fac = profundidad, xax = 1, yax = 2, ellipseSize = 0, 
           starSize = 1, col = colors, plot = FALSE, add = FALSE, plabels.cex = 1, ppoints.pch = 20,
           ppoints.cex = 1, psub.cex = 1, paxes.draw = FALSE, pgrid.draw = TRUE, pgrid.text.cex = 1, 
           porigin.include = TRUE, porigin.origin = c(0, 0), pellipses.axes.draw = FALSE,
           psub.position = "bottomleft", wt = rep(1, NROW(profundidad)), labels = levels(profundidad),
           chullSize = FALSE, facets = NULL, storeData = TRUE, pos = -1)

plot(g6)

g7=s.class(dfxy = pta.env$Tco, fac = varfq, xax = 1, yax = 2, ellipseSize = 0, 
           starSize = 1, col = colors, plot = FALSE, add = FALSE, plabels.cex = 1, ppoints.pch = 20,
           ppoints.cex = 1, psub.cex = 1, paxes.draw = FALSE, pgrid.draw = TRUE, pgrid.text.cex = 1, 
           porigin.include = TRUE, porigin.origin = c(0, 0), pellipses.axes.draw = FALSE,
           psub.position = "bottomleft", wt = rep(1, NROW(varfq)), labels = levels(varfq),
           chullSize = NULL, facets = NULL, storeData = TRUE, pos = -1 )
plot(g7)


ADEgS(c(g0, g5, g6, g7), layout = c(2, 2))




#más colores
palette(brewer.pal(n = 10, name = "Set2"))
colors=palette(brewer.pal(n = 10, name = "Set2"))
colors

