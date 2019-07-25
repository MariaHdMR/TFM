#matriz espacial
#coordenadas ----
sitios <- data.frame(name = c("1A","2A","3A","4A","5A","6A","1B","2B", "3B", "4B", "5B","6B","1C","2C","3C","4C","5C","6C","1D","2D","3D","4D","5D","6D","1E","2E","3E","4E", "5E","6E","1F","2F","3F","4F","5F","6F"),lat= c(0.5,0.5,0.5,0.5,0.5,0.5,2,2,2,2,2,2,3.5,3.5,3.5,3.5,3.5,3.5,5,5,5,5,5,5,6.5,6.5,6.5,6.5,6.5,6.5,8,8,8,8,8,8), lng= c(0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8,0.5,2,3.5,5,6.5,8))
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
#voy a crear una matriz con PUPA y Flies
#primer paso: crear un subset de los datos de Flies y Pupa para todos los  Plots

abun.P.F <-tabla.P.F %>% group_by(Subplot) %>% summarise(n.abun = sum(abun))
plantas.P.F <- tabla.P.F %>% group_by(Subplot) %>% summarise(n.plantas = sum(num.plantas))
Pupa.Flies <-dplyr::left_join(abun.P.F, plantas.P.F )

ab <- dist(abun.P.F, method= "euclidean", diag=T, upper=T)
summary(ab)
ab.matrix <- as.matrix(ab)
#colnames(dist.matrix) <- abun.P.F[,1]
#☺rownames(dist.matrix) <- abun.P.F[,1]



plantas1 <- dist(plantas.P.F, method= "euclidean", diag=T, upper=T)
plantas.matrix <- as.matrix(plantas1)
mantel.rtest(ab, plantas,nrepet = 9999)


#str(Pupa.Flies)
#geo_data <-Pupa.Flies[,c(3,23:46)] # no me sale, seguir investigando
geo_data <-Pupa.Flies[,-1]
#ph_113<-as.geodata(geo_data[1:46,], coords.col = 2:3, data.col = 3)




