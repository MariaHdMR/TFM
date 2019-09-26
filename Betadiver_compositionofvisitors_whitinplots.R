#en este script queremos sacar las b-diversidades de las especies de visitors por plots (es decir, tendremos al final 9 valores
#   uno por plot). Para ello esta vez, vamos a sacar las matrices de b-diversidad con betadiver, y segun el metodo 15 de 
# Kolef et al. (2003) == Binomial. 

#libraries
library(tidyverse)
library(reshape2)
library(devtools)
#install_github("ibartomeus/betalink", ref = "new_features") #just once
#library(betalink)
library(vegan)
library(ade4)

#datos
va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
head(va)
va19 <- subset(va, Year== "2019")
#distancias
distances <- read.csv("data/caracolesplotposition.csv", sep = ";")
distances <- distances[seq(2,72,2),]
distances2 <- rbind(distances, distances, distances,
                    distances, distances, distances,
                    distances, distances, distances) #For publication, this need to be properly measured from Klm.
distances2$plot <- c(rep(1,36),rep(2,36),rep(3,36),rep(4,36),rep(5,36),
                     rep(6,36),rep(7,36),rep(8,36),rep(9,36))
tesaurus <- data.frame(plot = c(1:9),
                       add.x = c(0, 33, 45, 21, 23, 20, 91, 39, 40),
                       add.y = c(0, 26, 28, 91, 7, 2, 133, -19, 16))
tesaurus$cumulative_x <- cumsum(tesaurus$add.x)
tesaurus$cumulative_y <- cumsum(tesaurus$add.y)

dis <- merge(distances2, tesaurus[,c(1,4,5)])
head(dis)
dis$x_coor2 <- dis$x_coor + dis$cumulative_x
dis$y_coor2 <- dis$y_coor + dis$cumulative_y
#dis is at subplot level.

#tapply(dis$x_coor2, INDEX = dis$plot, FUN = mean)
#tapply(dis$y_coor, INDEX = dis$plot, FUN = mean)

dis_plot <- tesaurus[,c(1,4,5)]
#
pol.9 <- va19 %>% group_by(Plot, Subplot, Group, Species) %>% summarise (num.visitors = sum(Visits))
#pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")

dist2<-dis[,c( "plot","cell", "position", "x_coor2", "y_coor2")]#subplots
dist2$Subplot <- dist2$position

# analisis
c.1.1 <- dcast(pol.9, Plot +Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
c.1.1$plot <- c.1.1$Plot
c.1.1$plot <- as.numeric(c.1.1$plot)
species <- c.1.1
species1<- subset(species, plot== "1")
uno <- subset(dist2, plot =="1")
s.uno <- dplyr::left_join (uno,species1)
s.uno[is.na(s.uno)] <- 0
temp <- s.uno[which(rowSums(s.uno [,c(8:40)]) != 0),8:40]
#rowSums(temp)
de <-s.uno[which(rowSums(s.uno [,c(8:40)]) != 0),4:5]
m.todo <- betadiver(temp [,c(1:33)], method=15) 
sites.uno <-dist(de [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo , sites.uno,nrepet = 9999)

#plot 2
species2<- subset(species, plot== "2")
posiciones <- dis[,c( "plot","cell", "position", "x_coor", "y_coor")]
posiciones$Subplot <- posiciones$position
posicion2 <-subset(posiciones, plot== "2")
plot2.sitios <- dplyr::left_join (posicion2,species2)
plot2.sitios[is.na(plot2.sitios)] <- 0
temp2 <- plot2.sitios[which(rowSums(plot2.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot2.sitios [,c(8:40)])
de.2 <-plot2.sitios[which(rowSums(plot2.sitios [,c(8:40)]) != 0),4:5]
m.todo.2 <- betadiver(temp2 [,c(1:33)], method=15) 
sites.2 <-dist(de.2 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.2 , sites.2,nrepet = 9999)

#plot 3
species3<- subset(species, plot== "3")
posicion3 <- subset(posiciones,plot=="3")
plot3.sitios <- dplyr::left_join (posicion3,species3)
plot3.sitios[is.na(plot3.sitios)] <- 0
temp3 <- plot3.sitios[which(rowSums(plot3.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot3.sitios [,c(8:40)])
de.3 <-plot3.sitios[which(rowSums(plot3.sitios [,c(8:40)]) != 0),4:5]
m.todo.3 <- betadiver(temp3 [,c(1:33)], method=15) 
sites.3 <-dist(de.3 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.3 , sites.3,nrepet = 9999)

#plot 4
species4<- subset(species, plot== "4")
posicion4 <- subset(posiciones,plot=="4")
plot4.sitios <- dplyr::left_join (posicion4,species4)
plot4.sitios[is.na(plot4.sitios)] <- 0
temp4 <- plot4.sitios[which(rowSums(plot4.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot4.sitios [,c(8:40)])
de.4 <-plot4.sitios[which(rowSums(plot4.sitios [,c(8:40)]) != 0),4:5]
m.todo.4 <- betadiver(temp4 [,c(1:33)], method=15) 
sites.4 <-dist(de.4 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.4 , sites.4,nrepet = 9999)

#plot 5
species5<- subset(species, plot== "5")
posicion5 <- subset(posiciones,plot=="5")
plot5.sitios <- dplyr::left_join (posicion5,species5)
plot5.sitios[is.na(plot5.sitios)] <- 0
temp5 <- plot5.sitios[which(rowSums(plot5.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot5.sitios [,c(8:40)])
de.5 <-plot5.sitios[which(rowSums(plot5.sitios [,c(8:40)]) != 0),4:5]
m.todo.5 <- betadiver(temp5 [,c(1:33)], method=15) 
sites.5 <-dist(de.5 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.5 , sites.5,nrepet = 9999)

#plot 6
species6<- subset(species, plot== "6")
posicion6 <- subset(posiciones,plot=="6")
plot6.sitios <- dplyr::left_join (posicion6,plot6.t)
plot6.sitios[is.na(plot6.sitios)] <- 0
temp6 <- plot6.sitios[which(rowSums(plot6.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot6.sitios [,c(8:40)])
de.6 <-plot6.sitios[which(rowSums(plot6.sitios [,c(8:40)]) != 0),4:5]
m.todo.6 <- betadiver(temp6 [,c(1:33)], method=15) 
sites.6 <-dist(de.6 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.6 , sites.6,nrepet = 9999)

#plot 7
species7<- subset(species, plot== "7")
posicion7 <- subset(posiciones,plot=="7")
plot7.sitios <- dplyr::left_join (posicion7,species7)
plot7.sitios[is.na(plot7.sitios)] <- 0
temp7 <- plot7.sitios[which(rowSums(plot7.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot7.sitios [,c(8:40)])
de.7 <-plot7.sitios[which(rowSums(plot7.sitios [,c(8:40)]) != 0),4:5]
m.todo.7 <- betadiver(temp7 [,c(1:33)], method=15) 
sites.7 <-dist(de.7 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.7 , sites.7,nrepet = 9999)

#plot 8
species8<- subset(species, plot== "8")
posicion8 <- subset(posiciones,plot=="8")
plot8.sitios <- dplyr::left_join (posicion8,species8)
plot8.sitios[is.na(plot8.sitios)] <- 0
temp8 <- plot8.sitios[which(rowSums(plot8.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot8.sitios [,c(8:40)])
de.8 <-plot8.sitios[which(rowSums(plot8.sitios [,c(8:40)]) != 0),4:5]
m.todo.8 <- betadiver(temp8 [,c(1:33)], method=15) 
sites.8 <-dist(de.8 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.8 , sites.8,nrepet = 9999)

#plot 9
species9<- subset(species, plot== "9")
posicion9 <- subset(posiciones,plot=="9")
plot9.sitios <- dplyr::left_join (posicion9,species9)
plot9.sitios[is.na(plot9.sitios)] <- 0
temp9 <- plot9.sitios[which(rowSums(plot9.sitios [,c(8:40)]) != 0),8:40]
#rowSums(plot9.sitios [,c(8:40)])
de.9 <-plot9.sitios[which(rowSums(plot9.sitios [,c(8:40)]) != 0),4:5]
m.todo.9 <- betadiver(temp9 [,c(1:33)], method=15) 
sites.9 <-dist(de.9 [,c(1,2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.todo.9 , sites.9,nrepet = 9999)
