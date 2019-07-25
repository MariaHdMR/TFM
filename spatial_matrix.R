#matriz espacial
#coordenadas ----
sitios <- data.frame(name = c("1A","2A","3A","4A","5A","6A","1B","2B", "3B", "4B", "5B","6B","1C","2C","3C","4C","5C","6C","1D","2D","3D","4D","5D","6D","1E","2E","3E","4E", "5E","6E","1F","2F","3F","4F","5F","6F"),lat= c(0.5,0.5,0.5,0.5,0.5,0.5,2,2,2,2,2,2,3.5,3.5,3.5,3.5,3.5,3.5,5,5,5,5,5,5,6.5,6.5,6.5,6.5,6.5,6.5,8,8,8,8,8,8), lng= c(0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8))
<<<<<<< HEAD
h <- dist(sitios[,c(2,3)], method= "euclidean", diag=T, upper=T)
# h2 <- dist(sitios, method= "euclidean", diag=T, upper=T)

summary(h)
dist.matrix <- as.matrix(h)
colnames(dist.matrix) <- sitios[,1]
rownames(dist.matrix) <- sitios[,1]

library(tidyverse)
tabla.completa.19$Plot <- as.factor(tabla.completa.19$Plot)
tabla.completa.19$Subplot <- as.factor(tabla.completa.19$Subplot)
t.19<- tabla.completa.19[which(!is.na(tabla.completa.19$Plot)),] 
t.19.1 <- t.19[which(!is.na(t.19$Subplot)),]

#para hacerlo con la tabla de visitors abundances
r.1.b <- subset(V, Plot=='1')

b <- r.1.b%>%group_by(Subplot, )%>% summarise(ab = sum(abun))
d.visits <- dist(b [,c(2)], method = "euclidean", diag = T, upper = T)
d.visits.matrix <- as.matrix(d.visits)



#ahora tendria que hacer lo mismo con las abundancias
r.1.b <- subset(ab.19, Plot=='1')
b <- r.1.b%>%group_by(Subplot)%>% summarise(ab = sum(abun))
d.visits <- dist(b [,c(2)], method = "euclidean", diag = T, upper = T)
d.visits.matrix <- as.matrix(d.visits)


install.packages("ade4")
library(ade4)
mantel.rtest(h, d.visits,nrepet = 9999)
################################################################### Buenos datos----

#crear una matriz con todos los polinizadores 
library(tidyverse)

Abun_19 <-read.table("Abun_19.csv", header=T, sep=";")
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
V_a_16_19 <- read.table ("V_a_16_19.csv", header= T, sep =";")
V.19 <- subset(V_a_16_19, Year == 2019)
V.19$Plot <- as.numeric(as.character(V.19$Plot))
V <-V.19 %>% group_by(Plot, Subplot,Plant_Simple, Group, Order, Family, Species)%>% summarise (abun =sum(Abundances))
abun.F <- dplyr::left_join(V, ab.19)
abun.F1 <- abun.F[which(complete.cases(abun.F)),]

    


abun.subplot <-abun.F1 %>% group_by(Subplot) %>% summarise (n.abun =sum(abun))
sp.subplot <- abun.F1 %>% group_by(Subplot) %>% summarise(n.plantas = sum(num.plantas))
total <-dplyr::left_join(abun.subplot, sp.subplot ) #no entiendo de donde sale el NA del A6, creo que porque hay spp con su abundancia que nosotros no tenemos
total <- total[which(complete.cases(total)),]
ac <- dist(abun.subplot, method= "euclidean", diag=T, upper=T)
summary(ac)
ac.matrix <- as.matrix(ac)

ac1 <- dist(abun.subplot, method= "euclidean", diag=T, upper=T)
summary(ac1)
ac1.matrix <- as.matrix(ac1)


ac2 <- dist(sp.subplot, method= "euclidean", diag=T, upper=T)
p.matrix <- as.matrix(ac2)
mantel.rtest(ac1, ac2,nrepet = 9999) 



###solo con una spp de planta y 1 polinizador ----
abun.P.F <-tabla.P.F %>% group_by(Subplot) %>% summarise(n.abun = sum(abun))
plantas.P.F <- tabla.P.F %>% group_by(Subplot) %>% summarise(n.plantas = sum(num.plantas))
Pupa.Flies <-dplyr::left_join(abun.P.F, plantas.P.F )

ab <- dist(abun.P.F, method= "euclidean", diag=T, upper=T)
summary(ab)
ab.matrix <- as.matrix(ab)
#colnames(dist.matrix) <- abun.P.F[,1]
#☺rownames(dist.matrix) <- abun.P.F[,1]

library(geoR)
library(gstat)
library(aqfig)
library(lattice)

plantas1 <- dist(plantas.P.F, method= "euclidean", diag=T, upper=T)
plantas.matrix <- as.matrix(plantas1)
mantel.rtest(ab, plantas,nrepet = 9999)


#Ahora para mostrar las distancias en un mapa de calor --> script que me pasó Oscar
geo_data <-total[,c(1,2:3)] 
row.names(geo_data) <- geo_data[,1]#me sale error
geo_data <-total[,-1]
ph_113 <-as.geodata(geo_data[,1:2], coords.col = 2, data.col = 2) #asi solo tengo el numero de plantas
ph_114 <-as.geodata(geo_data[, 2], coords.col = 1, data.col = 1)#solo consigo que me de el número de plantas, no consigo los polinizadores

#estimación de parametros
ml_ph_113 <- likfit(ph_113, ini = c(1,23), fix.nugget = T) # no entiendo 

# definir la malla de análisis
pred.grid <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))

# cálculos de interpolación.
kc_ph_113 <- krige.conv(ph_113, loc = pred.grid, krige = krige.control(obj.m = ml_ph_113)) 

# representacion de los datos 

image(kc_ph_113, loc = pred.grid, col=rainbow(15), xlab=NA, ylab="Coordenadas Y (m)", main="plantas") 
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_113$predict),max(kc_ph_113$predict))) 


=======
row.names(sitios) <- sitios$name
sitios <- sitios[,-1] #quito el nombre, y dejo solo lat long
h <- dist(sitios, method= "euclidean", diag=T, upper=T) 
summary(h)
#h es la matriz de distancias de todos a los sitios a todos los sitios. Lista para ser usada.

plot(sitios$lat~sitios$lng)
plot(h)# ni idea de lo que esta pasando aqui 
#IB: ploteas distancias (h), en funcion del numero de celdas. No relevante.


>>>>>>> 155c47957f48989c86557b9b4954d5a2887a5d21

##########################################
#crear una matriz de polinizadores, coger la de abundancias plantas+abun pol
abun.F1
#af <-subset(abun.F1, Plot & Subplot & Group)
af <-abun.F1 %>% group_by(Plot,Subplot,Group, Plant_Simple) %>% summarise(n.abun = sum(abun))
af1 <- abun.F1 %>% group_by(Plot,Subplot,Group, Plant_Simple) %>% summarise(n.plantas = sum(num.plantas))
pol.1 <-subset(af, Plot== '1')
pol1.a <- pol.1 %>% group_by(Subplot, Plant_Simple)
subset(pol1.a, Subplot)
