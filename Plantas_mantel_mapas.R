#tengo que hacer lo mismo que he hecho para polinizadores pero para plantas
library(vegan) #me sirve para bioenv
library(tidyverse)
library(ade4) #este es el paquete para el mantel test que he usado
library(geoRmantel)
library(gstat)
library(aqfig)
library(lattice)
library(lme4) #para el glmm
#datos

sitios <- read.table("data/caracolesplotposition.csv", header=T, sep= ";")
sitios <-sitios[which(complete.cases(sitios)),]
sitios$Subplot <- sitios$position
sitios1 <- sitios [,c("Subplot", "x_coor", "y_coor")]
plantas <- read.table("data/Abun_19.csv", header=T, sep= ";")
ab.simple <- plantas %>% group_by(plot, subplot, Sp.Focal) %>% summarise (num.plantas = sum(Plantas))
ab.simple <-ab.simple[which(complete.cases(ab.simple)),]
h <- dist(sitios[,c(4,5)], method= "euclidean", diag=T, upper=T)
#analisis
#ABUNDANCIAS plantas ----
#CHFU ----
CHFU <- subset(ab.simple, Sp.Focal == "CHFU")
CHFU.tabla <- tidyr::spread(CHFU,key = plot, value = num.plantas)
#Ahora vamos a ver la relacion espacial entre la abundancia de chfu en el plot 1
#datos del plot 1 de chfu y posiciones
d.chfu1 <-dist(CHFU.tabla [,c(3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu1, h,nrepet = 9999)

sitios1$subplot <- sitios1$Subplot
chfu.sitios <- dplyr::left_join (sitios1,CHFU.tabla)

chfugeo_data1 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 6)
chfuvis1 <- likfit(chfugeo_data1, ini = c(1,0.5), fix.nugget = T)
chfupred.grid1 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_1<- krige.conv(chfugeo_data1, loc = chfupred.grid1, krige = krige.control(obj.m = chfuvis1)) 
image(kc_chfu_1, loc = chfupred.grid1, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 1")

vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_7$predict),max(kc_f_7$predict)))
#datos del plot 2 de chfu y posiciones
d.chfu2 <-dist(CHFU.tabla [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu2, h,nrepet = 9999)


chfugeo_data2 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 7)
chfuvis2 <- likfit(chfugeo_data2, ini = c(1,0.5), fix.nugget = T)
chfupred.grid2 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_2<- krige.conv(chfugeo_data2, loc = chfupred.grid2, krige = krige.control(obj.m = chfuvis2)) 
image(kc_chfu_2, loc = chfupred.grid2, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 2")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_chfu_2$predict),max(kc_chfu_2$predict)))

#datos del plot 3 de chfu y posiciones
d.chfu3 <-dist(CHFU.tabla [,c(5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu3, h,nrepet = 9999)
#datos del plot 4 de chfu y posiciones
d.chfu4 <-dist(CHFU.tabla [,c(6)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu4, h,nrepet = 9999)

chfugeo_data4 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 9)
chfuvis4 <- likfit(chfugeo_data4, ini = c(1,0.5), fix.nugget = T)
chfupred.grid4 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_4<- krige.conv(chfugeo_data4, loc = chfupred.grid4, krige = krige.control(obj.m = chfuvis4)) 
image(kc_chfu_4, loc = chfupred.grid4, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 4")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_chfu_4$predict),max(kc_chfu_4$predict)))
#datos del plot 5 de chfu y posiciones
d.chfu5 <-dist(CHFU.tabla [,c(7)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu5, h,nrepet = 9999)

chfugeo_data5 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 10)
chfuvis5 <- likfit(chfugeo_data5, ini = c(1,0.5), fix.nugget = T)
chfupred.grid5 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_5<- krige.conv(chfugeo_data5, loc = chfupred.grid5, krige = krige.control(obj.m = chfuvis5)) 
image(kc_chfu_5, loc = chfupred.grid5, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 5")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_chfu_5$predict),max(kc_chfu_5$predict)))
#datos del plot 6 de chfu y posiciones
d.chfu6 <-dist(CHFU.tabla [,c(8)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu6, h,nrepet = 9999)
#datos del plot 7 de chfu y posiciones
d.chfu7 <-dist(CHFU.tabla [,c(9)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu7, h,nrepet = 9999)

chfugeo_data7 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 12)
chfuvis7 <- likfit(chfugeo_data7, ini = c(1,0.5), fix.nugget = T)
chfupred.grid7 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_7<- krige.conv(chfugeo_data7, loc = chfupred.grid7, krige = krige.control(obj.m = chfuvis7)) 
image(kc_chfu_7, loc = chfupred.grid7, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_chfu_7$predict),max(kc_chfu_7$predict)))
#datos del plot 8 de chfu y posiciones
d.chfu8 <-dist(CHFU.tabla [,c(10)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu8, h,nrepet = 9999)

chfugeo_data8 <- as.geodata(chfu.sitios[1:36,], coords.col = 2:3, data.col = 13)
chfuvis8 <- likfit(chfugeo_data8, ini = c(1,0.5), fix.nugget = T)
chfupred.grid8 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_chfu_8<- krige.conv(chfugeo_data8, loc = chfupred.grid8, krige = krige.control(obj.m = chfuvis8)) 
image(kc_chfu_8, loc = chfupred.grid8, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of CHFU abundances in plot 8")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_chfu_7$predict),max(kc_chfu_7$predict)))
#datos del plot 9 de chfu y posiciones
d.chfu9 <-dist(CHFU.tabla [,c(11)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.chfu9, h,nrepet = 9999)



#PUPA ----
PUPA <- subset(ab.simple, Sp.Focal == "PUPA")
PUPA.tabla <- tidyr::spread(PUPA,key = plot, value = num.plantas)
pupa.sitios <- dplyr::left_join (sitios1,PUPA.tabla)
#datos del plot 1 de pupa
d.pupa1 <-dist(PUPA.tabla [,c(3)], method= "euclidean", diag =T, upper =T)#matriz de 0, no se puede hacer
#mantel.rtest (d.pupa1, h,nrepet = 9999)
#datos del plot 2 de pupa
d.pupa2 <-dist(PUPA.tabla [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.pupa2, h,nrepet = 9999)
#datos del plot 3 de pupa
d.pupa3 <-dist(PUPA.tabla [,c(5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.pupa3, h,nrepet = 9999)
#datos del plot 4 de pupa
d.pupa4 <-dist(PUPA.tabla [,c(6)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.pupa4, h,nrepet = 9999)
#datos del plot 5 de pupa
d.pupa5 <-dist(PUPA.tabla [,c(7)], method= "euclidean", diag =T, upper =T#matriz de 0
#mantel.rtest (d.pupa5, h,nrepet = 9999)
#datos del plot 6 de pupa
d.pupa6 <-dist(PUPA.tabla [,c(8)], method= "euclidean", diag =T, upper =T)#matriz de 0
#mantel.rtest (d.pupa6, h,nrepet = 9999)
#datos del plot 7 de pupa
d.pupa7 <-dist(PUPA.tabla [,c(9)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.pupa7, h,nrepet = 9999)

pupageo_data7 <- as.geodata(pupa.sitios[1:36,], coords.col = 2:3, data.col = 12)
pupavis7 <- likfit(pupageo_data7, ini = c(1,0.5), fix.nugget = T)
pupapred.grid7 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_pupa_7<- krige.conv(pupageo_data7, loc = pupapred.grid7, krige = krige.control(obj.m = pupavis7)) 
image(kc_pupa_7, loc = pupapred.grid7, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of PUPA abundances in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_pupa_7$predict),max(kc_pupa_7$predict)))
#datos del plot 8 de pupa
d.pupa8 <-dist(PUPA.tabla [,c(10)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.pupa8, h,nrepet = 9999)

pupageo_data8 <- as.geodata(pupa.sitios[1:36,], coords.col = 2:3, data.col = 13)
pupavis8 <- likfit(pupageo_data8, ini = c(1,0.5), fix.nugget = T)
pupapred.grid8 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_pupa_8<- krige.conv(pupageo_data8, loc = pupapred.grid8, krige = krige.control(obj.m = pupavis8)) 
image(kc_pupa_8, loc = pupapred.grid8, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of PUPA abundances in plot 8")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_pupa_8$predict),max(kc_pupa_8$predict)))
#datos del plot 9 de pupa
d.pupa9 <-dist(PUPA.tabla [,c(11)], method= "euclidean", diag =T, upper =T) #matriz de 0
#mantel.rtest (d.pupa9, h,nrepet = 9999)
#lema----
LEMA <- subset(ab.simple, Sp.Focal == "LEMA")
LEMA.tabla <- tidyr::spread(LEMA,key = plot, value = num.plantas)

lema.sitios <- dplyr::left_join (sitios1,LEMA.tabla)
#datos del plot 1 de lema
d.lema1 <-dist(LEMA.tabla [,c(3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema1, h,nrepet = 9999)
#datos del plot 2 de lema
d.lema2 <-dist(LEMA.tabla [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema2, h,nrepet = 9999)
#datos del plot 3 de lema
d.lema3 <-dist(LEMA.tabla [,c(5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema3, h,nrepet = 9999)

lemageo_data3 <- as.geodata(lema.sitios[1:36,], coords.col = 2:3, data.col = 8)
lemavis3 <- likfit(lemageo_data3, ini = c(1,0.5), fix.nugget = T)
lemapred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_lema_3<- krige.conv(lemageo_data3, loc = lemapred.grid3, krige = krige.control(obj.m = lemavis3)) 
image(kc_lema_3, loc = lemapred.grid3, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of LEMA abundances in plot 3")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_lema_3$predict),max(kc_lema_3$predict)))

#datos del plot 4 de lema
d.lema4 <-dist(LEMA.tabla [,c(6)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema4, h,nrepet = 9999)

lemageo_data4 <- as.geodata(lema.sitios[1:36,], coords.col = 2:3, data.col = 9)
lemavis4 <- likfit(lemageo_data4, ini = c(1,0.5), fix.nugget = T)
lemapred.grid4 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_lema_4<- krige.conv(lemageo_data4, loc = lemapred.grid4, krige = krige.control(obj.m = lemavis4)) 
image(kc_lema_4, loc = lemapred.grid4, col=rainbow(15), xlab=" Coordinate  X (m)", ylab="Coordinate  Y (m)", main="Distribution of LEMA abundances in plot 4") #no sig
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_lema_4$predict),max(kc_lema_4$predict)))
#datos del plot 5 de lema
d.lema5 <-dist(LEMA.tabla [,c(7)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema5, h,nrepet = 9999)

lemageo_data5 <- as.geodata(lema.sitios[1:36,], coords.col = 2:3, data.col = 10)
lemavis5 <- likfit(lemageo_data5, ini = c(1,0.5), fix.nugget = T)
lemapred.grid5 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_lema_5<- krige.conv(lemageo_data5, loc = lemapred.grid5, krige = krige.control(obj.m = lemavis5)) 
image(kc_lema_5, loc = lemapred.grid5, col=rainbow(15), xlab=" Coordinate  X (m)", ylab="Coordinate  Y (m)", main="Distribution of LEMA abundances in plot 5")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_lema_5$predict),max(kc_lema_5$predict)))
#datos del plot 6 de lema
d.lema6 <-dist(LEMA.tabla [,c(8)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema6, h,nrepet = 9999)
#datos del plot 7 de lema
d.lema7 <-dist(LEMA.tabla [,c(9)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema7, h,nrepet = 9999)

lemageo_data7 <- as.geodata(lema.sitios[1:36,], coords.col = 2:3, data.col = 12)
lemavis7 <- likfit(lemageo_data7, ini = c(1,0.5), fix.nugget = T)
lemapred.grid7 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_lema_7<- krige.conv(lemageo_data7, loc = lemapred.grid7, krige = krige.control(obj.m = lemavis7)) 
image(kc_lema_7, loc = lemapred.grid7, col=rainbow(15), xlab=" Coordinate  X (m)", ylab="Coordinate  Y (m)", main="Distribution of LEMA abundances in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_lema_7$predict),max(kc_lema_7$predict)))
#datos del plot 8 de lema
d.lema8 <-dist(LEMA.tabla [,c(10)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema8, h,nrepet = 9999)


lemageo_data8 <- as.geodata(lema.sitios[1:36,], coords.col = 2:3, data.col = 13)
lemavis8 <- likfit(lemageo_data8, ini = c(1,0.5), fix.nugget = T)
lemapred.grid8 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_lema_8<- krige.conv(lemageo_data8, loc = lemapred.grid8, krige = krige.control(obj.m = lemavis8)) 
image(kc_lema_8, loc = lemapred.grid8, col=rainbow(15), xlab=" Coordinate  X (m)", ylab="Coordinate  Y (m)", main="Distribution of LEMA abundances in plot 8")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_lema_8$predict),max(kc_lema_8$predict)))
#datos del plot 9 de lema
d.lema9 <-dist(LEMA.tabla [,c(11)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.lema9, h,nrepet = 9999)
#me----

ME <- subset(ab.simple, Sp.Focal == "ME")
ME.tabla <- tidyr::spread(ME,key = plot, value = num.plantas)
me.sitios <- dplyr::left_join (sitios1,ME.tabla)
#datos del plot 1 de me
d.me1 <-dist(ME.tabla [,c(3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me1, h,nrepet = 9999)
#datos del plot 2 de me
d.me2 <-dist(ME.tabla [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me2, h,nrepet = 9999)
#datos del plot 3 de me
d.me3 <-dist(ME.tabla [,c(5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me3, h,nrepet = 9999)
#datos del plot 4 de me
d.me4 <-dist(ME.tabla [,c(6)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me4, h,nrepet = 9999)
#datos del plot 5 de me
d.me5 <-dist(ME.tabla [,c(7)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me5, h,nrepet = 9999)


#datos del plot 6 de me
d.me6 <-dist(ME.tabla [,c(8)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me6, h,nrepet = 9999)
#datos del plot 7 de me
d.me7 <-dist(ME.tabla [,c(9)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me7, h,nrepet = 9999)
#datos del plot 8 de me
d.me8 <-dist(ME.tabla [,c(10)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me8, h,nrepet = 9999)
#datos del plot 9 de me
d.me9 <-dist(ME.tabla [,c(11)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.me9, h,nrepet = 9999)
megeo_data9<- as.geodata(me.sitios[1:36,], coords.col = 2:3, data.col = 14)
mevis9 <- likfit(megeo_data9, ini = c(1,0.5), fix.nugget = T)
mepred.grid9 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_me_9<- krige.conv(megeo_data9, loc = mepred.grid9, krige = krige.control(obj.m = mevis9)) 
image(kc_me_9, loc = mepred.grid9, col=rainbow(15), xlab=" Coordinate  X (m)", ylab="Coordinate  Y (m)", main="Distribution of ME abundances in plot 9")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_me_9$predict),max(kc_me_9$predict)))
####
#B-diversidad de plantas----
Plantas <- dcast(ab.simple, plot+ subplot ~ Sp.Focal, fun.aggregate = sum, value.var = "num.plantas")
Plantas$Subplot <- Plantas$subplot
Plantas.s<- dplyr::left_join (sitios1,Plantas)
#plot 1 +chfu
plot1 <-subset(Plantas.s, plot== '1')
d.b.chfu1 <-dist(plot1 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu1 <- vegdist(d.b.chfu1, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
c1 <-dist(plot1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu1, c1,nrepet = 9999)
#plot 2+chfu
plot2 <-subset(Plantas.s, plot== '2')
d.b.chfu2 <-dist(plot2 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu2 <- vegdist(d.b.chfu2, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c2 <-dist(plot2 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu2, c2,nrepet = 9999)
#plot 3+chfu
plot3 <-subset(Plantas.s, plot== '3')
d.b.chfu3 <-dist(plot3 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu3 <- vegdist(d.b.chfu3, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c3 <-dist(plot3 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu3, c3,nrepet = 9999)
#plot 4+chfu
plot4 <-subset(Plantas.s, plot== '4')
d.b.chfu4 <-dist(plot4 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu4 <- vegdist(d.b.chfu4, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c4 <-dist(plot4 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu4, c4,nrepet = 9999)
#plot 5+chfu
plot5 <-subset(Plantas.s, plot== '5')
d.b.chfu5 <-dist(plot5 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu5 <- vegdist(d.b.chfu5, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c5 <-dist(plot5 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu5, c5,nrepet = 9999)
#plot 6+chfu
plot6 <-subset(Plantas.s, plot== '6')
d.b.chfu6 <-dist(plot6 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu6 <- vegdist(d.b.chfu6, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c6 <-dist(plot6 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu6, c6,nrepet = 9999)
#plot 7+chfu
plot7 <-subset(Plantas.s, plot== '7')
d.b.chfu7 <-dist(plot7 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu7 <- vegdist(d.b.chfu7, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c7 <-dist(plot7 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu7, c7,nrepet = 9999)
#plot 8+chfu
plot8 <-subset(Plantas.s, plot== '8')
d.b.chfu8 <-dist(plot8 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu8 <- vegdist(d.b.chfu8, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c8 <-dist(plot8 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu8, c8,nrepet = 9999)
#plot 9+chfu
plot9 <-subset(Plantas.s, plot== '9')
d.b.chfu9 <-dist(plot9 [,c(8)], method= "euclidean", diag =T, upper =T)
m.b.chfu9 <- vegdist(d.b.chfu9, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
c9 <-dist(plot9[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.chfu9, c9,nrepet = 9999)
#Lema ----

#plot 1 +lema
d.b.lema1 <-dist(plot1 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema1 <- vegdist(d.b.lema1, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l1 <-dist(plot1[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema1, l1,nrepet = 9999)
#plot 2 +lema
d.b.lema2 <-dist(plot2 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema2 <- vegdist(d.b.lema2, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l2 <-dist(plot2[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema2, l2,nrepet = 9999)
#plot 3 +lema
d.b.lema3 <-dist(plot3 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema3 <- vegdist(d.b.lema3, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l3 <-dist(plot3[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema3, l3,nrepet = 9999)
#plot 4 +lema
d.b.lema4 <-dist(plot4 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema4 <- vegdist(d.b.lema4, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l4 <-dist(plot4[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema4, l4,nrepet = 9999)
#plot 5 +lema
d.b.lema5 <-dist(plot5 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema5 <- vegdist(d.b.lema5, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l5 <-dist(plot5[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema5, l5,nrepet = 9999)
#plot 6 +lema
d.b.lema6 <-dist(plot6 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema6 <- vegdist(d.b.lema6, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l6 <-dist(plot6[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema6, l6,nrepet = 9999)
#plot 7 +lema
d.b.lema7 <-dist(plot7 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema7 <- vegdist(d.b.lema7, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l7 <-dist(plot7[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema7, l7,nrepet = 9999)
#plot 8 +lema
d.b.lema8 <-dist(plot8 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema8 <- vegdist(d.b.lema8, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l8 <-dist(plot8[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema8, l8,nrepet = 9999)
#plot 9 +lema
d.b.lema9 <-dist(plot9 [,c(13)], method= "euclidean", diag =T, upper =T)
m.b.lema9 <- vegdist(d.b.lema9, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
l9 <-dist(plot9[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.lema9, l9,nrepet = 9999)
#pupa
pupa <- subset(Plantas.s, Sp.Focal == "PUPA")
#plot 1 +pupa
d.b.pupa1 <-dist(plot1 [,c(21)], method= "euclidean", diag =T, upper =T)
#plot 2 +pupa
d.b.pupa2 <-dist(plot2 [,c(21)], method= "euclidean", diag =T, upper =T)
m.b.pupa2 <- vegdist(d.b.pupa2, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
p2 <-dist(plot2[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.pupa2, p2,nrepet = 9999)

#plot 3 +pupa
d.b.pupa3 <-dist(plot3 [,c(21)], method= "euclidean", diag =T, upper =T)
m.b.pupa3 <- vegdist(d.b.pupa3, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
#plot 4 +pupa
d.b.pupa4 <-dist(plot4 [,c(21)], method= "euclidean", diag =T, upper =T)
m.b.pupa4 <- vegdist(d.b.pupa4, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
p4 <-dist(plot4[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.pupa4, p4,nrepet = 9999)
#plot 5 +pupa
d.b.pupa5 <-dist(plot5 [,c(21)], method= "euclidean", diag =T, upper =T)
#plot 6 +pupa
d.b.pupa6 <-dist(plot6 [,c(21)], method= "euclidean", diag =T, upper =T)
#plot 7 +pupa
d.b.pupa7 <-dist(plot7 [,c(21)], method= "euclidean", diag =T, upper =T)
m.b.pupa7 <- vegdist(d.b.pupa7, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
p7 <-dist(plot7[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.pupa7, p7,nrepet = 9999)
#plot 8 +pupa
d.b.pupa8 <-dist(plot8 [,c(21)], method= "euclidean", diag =T, upper =T)
m.b.pupa8 <- vegdist(d.b.pupa8, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
p8 <-dist(plot8[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.pupa8, p8,nrepet = 9999)
#plot 9 +pupa
d.b.pupa9 <-dist(plot9 [,c(21)], method= "euclidean", diag =T, upper =T)
#me
#plot 1 + me
d.b.me1 <-dist(plot1 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me1 <- vegdist(d.b.me1, method="morisita", binary=FALSE, diag= T, upper=T,
                     na.rm = FALSE) 
me1 <-dist(plot1[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me1, me1,nrepet = 9999)
#plot 2 + me
d.b.me2 <-dist(plot2 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me2 <- vegdist(d.b.me2, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me2 <-dist(plot2[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me2, me2,nrepet = 9999)
#plot 3 + me
d.b.me3 <-dist(plot3 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me3 <- vegdist(d.b.me3, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me3 <-dist(plot3[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me3, me3,nrepet = 9999)
#plot 4 + me
d.b.me4 <-dist(plot4 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me4 <- vegdist(d.b.me4, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me4 <-dist(plot4[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me4, me4,nrepet = 9999)
#plot 5 + me
d.b.me5 <-dist(plot5 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me5 <- vegdist(d.b.me5, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me5 <-dist(plot5[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me5, me5,nrepet = 9999)
#plot 6 + me
d.b.me6 <-dist(plot6 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me6 <- vegdist(d.b.me6, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
#plot 7 + me
d.b.me7 <-dist(plot7 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me7 <- vegdist(d.b.me7, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me7 <-dist(plot7[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me7, me7,nrepet = 9999)
#plot 8 + me
d.b.me8 <-dist(plot8 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me8 <- vegdist(d.b.me8, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me8 <-dist(plot8[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me8, me8,nrepet = 9999)
#plot 9 + me
d.b.me9 <-dist(plot9 [,c(15)], method= "euclidean", diag =T, upper =T)
m.b.me9 <- vegdist(d.b.me9, method="morisita", binary=FALSE, diag= T, upper=T,
                   na.rm = FALSE) 
me9 <-dist(plot9[,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.b.me9, me9,nrepet = 9999)
#landscape 
#distances between plots
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
dist2<-dis[,c( "plot","cell", "position", "x_coor2", "y_coor2")]#subplots

#dis is at subplot level.

#tapply(dis$x_coor2, INDEX = dis$plot, FUN = mean)
#tapply(dis$y_coor, INDEX = dis$plot, FUN = mean)

dis_plot <- tesaurus[,c(1,4,5)]
#subplot
pol.but9 <- subset(pol.9, Group == "Butterfly")
But.total <- pol.but9 %>% group_by(Plot, Subplot, Species) %>% summarise (visits = sum(num.visitors))
a <- dcast(ab.simple, plot+ subplot ~ Sp.Focal, fun.aggregate = sum, value.var = "num.plantas")
a.s <- a[,c("plot", "subplot", "CHFU","LEMA","PUPA","ME")]
a.s$Subplot <- a.s$subplot
as.sitio <- dplyr::left_join (dist2,a.s)
as.sitio[is.na(as.sitio)] <- 0


chfu.plots <-dist(as.sitio [,c(8)], method= "euclidean", diag =T, upper =T)

m.chfu.plots <- vegdist(chfu.plots, method="morisita", binary=FALSE, diag= T, upper=T,
                       na.rm = FALSE) 
sitios.plantas <-dist(as.sitio [,c(4,5)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.chfu.plots, sitios.plantas,nrepet = 9999)

lema.plots <-dist(as.sitio [,c(9)], method= "euclidean", diag =T, upper =T)

m.lema.plots <- vegdist(lema.plots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
mantel.rtest (m.lema.plots, sitios.plantas,nrepet = 9999)
Pupa.plots <-dist(as.sitio [,c(10)], method= "euclidean", diag =T, upper =T)

m.pupa.plots <- vegdist(Pupa.plots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
mantel.rtest (m.pupa.plots, sitios.plantas,nrepet = 9999)
me.plots <-dist(as.sitio [,c(11)], method= "euclidean", diag =T, upper =T)

m.me.plots <- vegdist(me.plots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
mantel.rtest (m.me.plots, sitios.plantas,nrepet = 9999)
#plot

#
a1 <- dcast(ab.simple, plot ~ Sp.Focal, fun.aggregate = sum, value.var = "num.plantas")
a.sub <- a1[,c("plot", "CHFU","LEMA","PUPA","ME")]
asub.sitio <- dplyr::left_join (dis_plot,a.sub)
asub.sitio[is.na(asub.sitio)] <- 0


chfu.subplots <-dist(asub.sitio [,c(4)], method= "euclidean", diag =T, upper =T)

m.chfu.subplots <- vegdist(chfu.subplots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
sitios.plantas.sub <-dist(asub.sitio [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.chfu.subplots, sitios.plantas.sub,nrepet = 9999)

lema.subplots <-dist(asub.sitio [,c(5)], method= "euclidean", diag =T, upper =T)

m.lema.subplots <- vegdist(lema.subplots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
mantel.rtest (m.lema.subplots, sitios.plantas.sub,nrepet = 9999)
Pupa.subplots <-dist(asub.sitio [,c(6)], method= "euclidean", diag =T, upper =T)

m.pupa.subplots <- vegdist(Pupa.subplots, method="morisita", binary=FALSE, diag= T, upper=T,
                        na.rm = FALSE) 
mantel.rtest (m.pupa.subplots, sitios.plantas.sub,nrepet = 9999)
me.subplots <-dist(asub.sitio [,c(7)], method= "euclidean", diag =T, upper =T)

m.me.subplots <- vegdist(me.subplots, method="morisita", binary=FALSE, diag= T, upper=T,
                      na.rm = FALSE) 
mantel.rtest (m.me.subplots, sitios.plantas.sub,nrepet = 9999)


todo.plantas.junto <-dist(asub.sitio [,c(4:7)], method= "euclidean", diag =T, upper =T)

m.todo <- vegdist(todo.plantas.junto, method="morisita", binary=FALSE, diag= T, upper=T,
                         na.rm = FALSE) 
mantel.rtest (todo.plantas.junto, sitios.plantas.sub,nrepet = 9999)

#subplot pero con todas las sp de plantas juntas
todo.plantas.<-dist(as.sitio [,c(8:11)], method= "euclidean", diag =T, upper =T)

m.todo.sub <- vegdist(todo.plantas., method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
mantel.rtest (m.todo.sub, sitios.plantas,nrepet = 9999)
