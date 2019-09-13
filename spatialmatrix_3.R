library(vegan)
library(tidyverse)
library(ade4)
library(geoR)
library(gstat)
library(aqfig)
library(lattice)
va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
sitios <- read.table("data/caracolesplotposition.csv", header=T, sep= ";")
sitios <-sitios[which(complete.cases(sitios)),]
sitios$Subplot <- sitios$position

head(va)
va19 <- subset(va, Year== "2019")
pol.9 <- va19 %>% group_by(Plot, Subplot, Group) %>% summarise (num.visitors = sum(Visits))
pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")
h <- dist(sitios[,c(4,5)], method= "euclidean", diag=T, upper=T)
#ABUNDANCIAS ----
#Beetles ----
pol.beetle9 <- subset(pol.9, Group == "Beetle")
pol.beetle9$Plot <- as.numeric(as.character(pol.beetle9$Plot))
pol.beetle.B <- pol.beetle9[,c("Plot", "Subplot", "num.visitors")] #datos de plot, subplot, y visitas de BEETLES
BEETLES <- tidyr::spread(pol.beetle.B,key = Plot, value = num.visitors)
BEETLES[is.na(BEETLES)] <- 0


#Ahora vamos a ver la relacion espacial entre la abundancia de beetles en el plot 1
#datos del plot 1 de beetles y posiciones
beetle.plot1 <- BEETLES [,c( "Subplot","1")]
dist.1 <-dist(beetle.plot1 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.1, h,nrepet = 9999) #no es sig el plot 1 con beetles
#plot 2 y beetles
beetle.plot2 <- BEETLES [,c( "Subplot","2")]
dist.2 <-dist(beetle.plot2 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.2, h,nrepet = 9999)
#plot 3 y beetles
beetle.plot3 <- BEETLES [,c( "Subplot","3")]
dist.3 <-dist(beetle.plot3 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.3, h,nrepet = 9999)
#plot 4 y beetles
beetle.plot4 <- BEETLES [,c( "Subplot","4")]
dist.4 <-dist(beetle.plot4 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.4, h,nrepet = 9999)
#plot 5 y beetles
beetle.plot5 <- BEETLES [,c( "Subplot","5")]
dist.5 <-dist(beetle.plot5 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.5, h,nrepet = 9999)
#plot 6 y beetles
beetle.plot6 <- BEETLES [,c( "Subplot","6")]
dist.6 <-dist(beetle.plot6 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.6, h,nrepet = 9999)
#plot 7 y beetles
beetle.plot7 <- BEETLES [,c( "Subplot","7")]
dist.7 <-dist(beetle.plot7 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.7, h,nrepet = 9999)
#plot 8 y beetles
beetle.plot8 <- BEETLES [,c( "Subplot","8")]
dist.8 <-dist(beetle.plot8 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.8, h,nrepet = 9999)
#plot 9 y beetles
beetle.plot9 <- BEETLES [,c( "Subplot","9")]
dist.9 <-dist(beetle.plot9 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (dist.9, h,nrepet = 9999)
#ahora voy a plotear con el mapa de calor aquellos que han dado significativos o casi significativos
#que son: plot 3, plot 5 y plot 6
#plot 3 y beetles
p3 <- subset(pol.beetle.B, Plot== "3")
t3 <- merge(p3, sitios, by= "Subplot", all= T)
t3$Plot[is.na(t3$Plot)] <- 3#en el siguiente paso elijo mi primer grupo
t3[is.na(t3)] <- 0
geo_data3 <- as.geodata(t3[1:36,], coords.col = 7:8, data.col = 3)
vis3 <- likfit(geo_data3, ini = c(1,0.5), fix.nugget = T)
pred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_ph_1133 <- krige.conv(geo_data3, loc = pred.grid3, krige = krige.control(obj.m = vis3))
image(kc_ph_1133, loc = pred.grid3, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetles abundance distribution in plot 3")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_1133$predict),max(kc_ph_1133$predict))) #este es el unico con valores significativos, el resto se aproximan a la significacion
#plot 5 y beetles
p5 <- subset(pol.beetle.B, Plot== "5")
t5 <- merge(p5, sitios, by= "Subplot", all= T)
t5$Plot[is.na(t5$Plot)] <- 5#en el siguiente paso elijo mi primer grupo
t5[is.na(t5)] <- 0
geo_data5 <- as.geodata(t5[1:36,], coords.col = 7:8, data.col = 3)
vis5 <- likfit(geo_data5, ini = c(1,0.5), fix.nugget = T)
pred.grid5 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_ph_113.5 <- krige.conv(geo_data5, loc = pred.grid5, krige = krige.control(obj.m = vis5))
image(kc_ph_113.5, loc = pred.grid5, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetlesabundance distribution in plot 5")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_113.5$predict),max(kc_ph_113.5$predict)))
#plot 6 y beetles
p6 <- subset(pol.beetle.B, Plot== "6")
t6 <- merge(p6, sitios, by= "Subplot", all= T)
t6$Plot[is.na(t6$Plot)] <- 6
t6[is.na(t6)] <- 0
geo_data6 <- as.geodata(t6[1:36,], coords.col = 7:8, data.col = 3)
vis6 <- likfit(geo_data6, ini = c(1,0.5), fix.nugget = T)
pred.grid6 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_ph_113.6 <- krige.conv(geo_data6, loc = pred.grid6, krige = krige.control(obj.m = vis6))
image(kc_ph_113.6, loc = pred.grid6, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Beetles abundance distribution in plot 6")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_113.6$predict),max(kc_ph_113.6$predict)))

#flies ----
f.t <- subset(pol.9, Group == "Fly")
f.t$Plot <- as.numeric(as.character(f$Plot))
f <- f.t[,c("Plot", "Subplot", "num.visitors")] #datos de plot, subplot, y visitas de BEETLES
fly <- tidyr::spread(f,key = Plot, value = num.visitors)
fly[is.na(fly)] <- 0
#plot 1 +flies
f1 <- fly [,c( "Subplot","1")]
d.f1 <-dist(f1 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f1, h,nrepet = 9999) 
#plot 2 +flies
f2 <- fly [,c( "Subplot","2")]
d.f2 <-dist(f2 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f2, h,nrepet = 9999) 
#plot 3 +flies
f3 <- fly [,c( "Subplot","3")]
d.f3 <-dist(f3 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f3, h,nrepet = 9999) 
#plot 4 +flies
f4 <- fly [,c( "Subplot","4")]
d.f4 <-dist(f4 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f4, h,nrepet = 9999) 
#plot 5 +flies
f5 <- fly [,c( "Subplot","5")]
d.f5 <-dist(f5 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f5, h,nrepet = 9999) 
#plot 6 +flies
f6 <- fly [,c( "Subplot","6")]
d.f6 <-dist(f6 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f6, h,nrepet = 9999)
#plot 7 +flies
f7 <- fly [,c( "Subplot","7")]
d.f7 <-dist(f7 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f7, h,nrepet = 9999)
#plot 8 +flies
f8 <- fly [,c( "Subplot","8")]
d.f8 <-dist(f8 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f8, h,nrepet = 9999)
#plot 9 +flies
f9 <- fly [,c( "Subplot","9")]
d.f9 <-dist(f9 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.f9, h,nrepet = 9999)
#mapa de calor de los significativos: plots: 2,5 y 7
#plot 2 +flies
f.t.2 <- subset(f.t, Plot == "2")
tf2 <- merge(f.t.2, sitios, by= "Subplot", all= T)
tf2$Plot[is.na(tf2$Plot)] <- 2
tf2$Group[is.na(tf2$Group)] <- "Fly"
tf2[is.na(tf2)] <- 0
fgeo_data2 <- as.geodata(tf2[1:36,], coords.col = 8:9, data.col = 4)
fvis2 <- likfit(fgeo_data2, ini = c(1,0.5), fix.nugget = T)
fpred.grid2 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_f_2<- krige.conv(fgeo_data2, loc = fpred.grid2, krige = krige.control(obj.m = fvis2))
image(kc_f_2, loc = fpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 2")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_2$predict),max(kc_f_2$predict)))

#plot 5 +flies
f.t.5 <- subset(f.t, Plot == "5")
tf5 <- merge(f.t.5, sitios, by= "Subplot", all= T)
tf5$Plot[is.na(tf5$Plot)] <- 5
tf5$Group[is.na(tf5$Group)] <- "Fly"
tf5[is.na(tf5)] <- 0
fgeo_data5 <- as.geodata(tf5[1:36,], coords.col = 8:9, data.col = 4)
fvis5 <- likfit(fgeo_data5, ini = c(1,0.5), fix.nugget = T)
fpred.grid5 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_f_5<- krige.conv(fgeo_data5, loc = fpred.grid5, krige = krige.control(obj.m = fvis5))
image(kc_f_5, loc = fpred.grid5, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 2")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_5$predict),max(kc_f_5$predict)))

#plot 7 +flies 
f.t.7 <- subset(f.t, Plot == "7")
tf7 <- merge(f.t.7, sitios, by= "Subplot", all= T)
tf7$Plot[is.na(tf7$Plot)] <- 7
tf7$Group[is.na(tf7$Group)] <- "Fly"
tf7[is.na(tf7)] <- 0
fgeo_data7 <- as.geodata(tf7[1:36,], coords.col = 8:9, data.col = 4)
fvis7 <- likfit(fgeo_data7, ini = c(1,0.5), fix.nugget = T)
fpred.grid7 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_f_7<- krige.conv(fgeo_data7, loc = fpred.grid7, krige = krige.control(obj.m = fvis7))
image(kc_f_7, loc = fpred.grid7, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_7$predict),max(kc_f_7$predict)))

#bees ----
b.t <- subset(pol.9, Group == "Bee")
b.t$Plot <- as.numeric(as.character(b.t$Plot))
#plot 1 + bee
uno <- subset(b.t, Plot== "1")
sitios1 <- sitios [,c("Subplot", "x_coor", "y_coor")]
all1 <- dplyr::left_join (sitios1,uno)
all1$Plot[is.na(all1$Plot)] <- 1
all1$Group[is.na(all1$Group)] <- "Bee"
all1[is.na(all1)] <- 0
be1 <- all1 [,c( "Subplot","num.visitors")]
d.be1 <-dist(be1 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be1, h,nrepet = 9999) 
#plot 2+bee
bee.2 <- subset(b.t, Plot== "2")
all2 <- dplyr::left_join (sitios1,bee.2)
all2$Plot[is.na(all2$Plot)] <- 2
all2$Group[is.na(all2$Group)] <- "Bee"
all2[is.na(all2)] <- 0
be2 <- all2 [,c( "Subplot","num.visitors")]
d.be2 <-dist(be2 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be2, h,nrepet = 9999) 

#plot 3+bee
bee.3 <- subset(b.t, Plot== "3")
all3 <- dplyr::left_join (sitios1,bee.3)
all3$Plot[is.na(all3$Plot)] <- 3
all3$Group[is.na(all3$Group)] <- "Bee"
all3[is.na(all3)] <- 0
be3 <- all3 [,c( "Subplot","num.visitors")]
d.be3 <-dist(be3 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be3, h,nrepet = 9999) 

#plot 4+bee
bee.4 <- subset(b.t, Plot== "4")
all4 <- dplyr::left_join (sitios1,bee.4)
all4$Plot[is.na(all4$Plot)] <- 4
all4$Group[is.na(all4$Group)] <- "Bee"
all4[is.na(all4)] <- 0
be4 <- all4 [,c( "Subplot","num.visitors")]
d.be4 <-dist(be4 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be4, h,nrepet = 9999) 

#plot 5+bee
bee.5 <- subset(b.t, Plot== "5")
all5 <- dplyr::left_join (sitios1,bee.5)
all5$Plot[is.na(all5$Plot)] <- 5
all5$Group[is.na(all5$Group)] <- "Bee"
all5[is.na(all5)] <- 0
be5 <- all5 [,c( "Subplot","num.visitors")]
d.be5 <-dist(be5 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be5, h,nrepet = 9999) 
#plot 6+bee
bee.6 <- subset(b.t, Plot== "6")
all6 <- dplyr::left_join (sitios1,bee.6)
all6$Plot[is.na(all6$Plot)] <- 6
all6$Group[is.na(all6$Group)] <- "Bee"
all6[is.na(all6)] <- 0
be6 <- all6 [,c( "Subplot","num.visitors")]
d.be6 <-dist(be6 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be6, h,nrepet = 9999) 
#plot 7+bee
bee.7 <- subset(b.t, Plot== "7")
all7 <- dplyr::left_join (sitios1,bee.7)
all7$Plot[is.na(all7$Plot)] <- 7
all7$Group[is.na(all7$Group)] <- "Bee"
all7[is.na(all7)] <- 0
be7 <- all7 [,c( "Subplot","num.visitors")]
d.be7 <-dist(be7 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be7, h,nrepet = 9999) 

#plot 8+bee
bee.8 <- subset(b.t, Plot== "8")
all8 <- dplyr::left_join (sitios1,bee.8)
all8$Plot[is.na(all8$Plot)] <- 8
all8$Group[is.na(all8$Group)] <- "Bee"
all8[is.na(all8)] <- 0
be8 <- all8 [,c( "Subplot","num.visitors")]
d.be8 <-dist(be8 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be8, h,nrepet = 9999) 
#plot 9+bee
bee.9 <- subset(b.t, Plot== "9")
all9 <- dplyr::left_join (sitios1,bee.9)
all9$Plot[is.na(all9$Plot)] <- 9
all9$Group[is.na(all9$Group)] <- "Bee"
all9[is.na(all9)] <- 0
be9 <- all9 [,c( "Subplot","num.visitors")]
d.be9 <-dist(be9 [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.be9, h,nrepet = 9999) 
# no hay mapas de calor ya que ningun mantel test ha dado significativo. No hay estructura espacial de las abejas (abundancias) 
#butterfly ----
bu.t <- subset(pol.9, Group == "Butterfly")
bu.t$Plot <- as.numeric(as.character(bu.t$Plot))
#solo hay datos de mariposas en los plot 7 y 8, solo voy a analizar esos
#plot 7+butterflies
bu7 <- subset(bu.t, Plot== "7")
bu7t <- dplyr::left_join (sitios1,bu7)
bu7t$Plot[is.na(bu7t$Plot)] <- 7
bu7t$Group[is.na(bu7t$Group)] <- "Butterfly"
bu7t[is.na(bu7t)] <- 0
bu7sim <- bu7t [,c( "Subplot","num.visitors")]
d.bu7 <-dist(bu7sim [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.bu7, h,nrepet = 9999) 
#plot 8+butterflies
bu8 <- subset(bu.t, Plot== "8")
bu8t <- dplyr::left_join (sitios1,bu8)
bu8t$Plot[is.na(bu8t$Plot)] <- 8
bu8t$Group[is.na(bu8t$Group)] <- "Butterfly"
bu8t[is.na(bu8t)] <- 0
bu8sim <- bu8t [,c( "Subplot","num.visitors")]
d.bu8 <-dist(bu8sim [,c(2)], method= "euclidean", diag =T, upper =T)
mantel.rtest (d.bu8, h,nrepet = 9999) 
#no hay mapas de calor porque ninguno ha salido significativo
####
#B-diversidad/composicion ----
p <- va19 %>% group_by(Plot, Subplot, Group,Family, Species) %>% summarise (num.visitors = sum(Visits))
#pol.9 <-pol.9[which(complete.cases(pol.9)),]
p$Family <- as.character(p$Family)
p$Species <- as.character(p$Species)
fct_explicit_na(p$Family, na_level = "(Missing)")
fct_explicit_na(p$Species, na_level = "(Missing)")
p <- subset(p, Plot != "OUT")
# beetles - b-diversity 
#plot 1 + beetles
p.1 <- subset (p, Plot == "1") 
bbe1 <- subset (p.1, Group == "Beetle")

bbe1t <- dplyr::left_join (sitios1,bbe1)

bbe1t$Plot[is.na(bbe1t$Plot)] <- 1
bbe1t$Group[is.na(bbe1t$Group)] <- "Beetle"
bbe1t$num.visitors[is.na(bbe1t$num.visitors)] <- 0
fct_explicit_na(bbe1t$Family, na_level = "(Missing)")
fct_explicit_na(bbe1t$Species, na_level = "(Missing)")
t1 <- bbe1t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 

bbe1sim <- bbe1t [,c( "Subplot","num.visitors")]
d.bbe1 <-dist(bbe1sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe1 <- vegdist(d.bbe1, method="morisita", binary=FALSE, diag= T, upper=T,
        na.rm = FALSE) 

l1<-  dplyr::left_join (sitios1,bbe1sim)
k1 <-dist(l1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe1, k1,nrepet = 9999) #corregido
mean(m.bbe1) #media de b-diversidad para el plot 1 de escarabajos = 0.049
sd(m.bbe1) #plot 1 +beetle = 0.11

#plot 2+beetle
p.2 <- subset (p, Plot == "2") 
bbe2 <- subset (p.2, Group == "Beetle")

bbe2t <- dplyr::left_join (sitios1,bbe2)

bbe2t$Plot[is.na(bbe2t$Plot)] <- 2
bbe2t$Group[is.na(bbe2t$Group)] <- "Beetle"
bbe2t$num.visitors[is.na(bbe2t$num.visitors)] <- 0
fct_explicit_na(bbe2t$Family, na_level = "(Missing)")
fct_explicit_na(bbe2t$Species, na_level = "(Missing)")
t2 <- bbe2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe2sim <- bbe2t [,c( "Subplot","num.visitors")]
d.bbe2 <-dist(bbe2sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe2 <- vegdist(d.bbe2, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l2<-  dplyr::left_join (sitios1,bbe2sim)
k2 <-dist(l2 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe2, k2,nrepet = 9999) #corregido
mean(m.bbe2) #0.26
sd(m.bbe2) # 0.06
#plot 3+beetle
p.3 <- subset (p, Plot == "3") 
bbe3 <- subset (p.3, Group == "Beetle")

bbe3t <- dplyr::left_join (sitios1,bbe3)

bbe3t$Plot[is.na(bbe3t$Plot)] <- 3
bbe3t$Group[is.na(bbe3t$Group)] <- "Beetle"
bbe3t$num.visitors[is.na(bbe3t$num.visitors)] <- 0
fct_explicit_na(bbe3t$Family, na_level = "(Missing)")
fct_explicit_na(bbe3t$Species, na_level = "(Missing)")
t3 <- bbe3t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe3sim <- bbe3t [,c( "Subplot","num.visitors")]
d.bbe3 <-dist(bbe3sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe3 <- vegdist(d.bbe3, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l3<-  dplyr::left_join (sitios1,bbe3sim)
k3 <-dist(l3 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe3, k3,nrepet = 9999) #corregido
mean(m.bbe3) #0.086
sd(m.bbe3) # 0.157
#plot 4+beetle
p.4 <- subset (p, Plot == "4") 
bbe4 <- subset (p.4, Group == "Beetle")

bbe4t <- dplyr::left_join (sitios1,bbe4)

bbe4t$Plot[is.na(bbe4t$Plot)] <- 4
bbe4t$Group[is.na(bbe4t$Group)] <- "Beetle"
bbe4t$num.visitors[is.na(bbe4t$num.visitors)] <- 0
fct_explicit_na(bbe4t$Family, na_level = "(Missing)")
fct_explicit_na(bbe4t$Species, na_level = "(Missing)")
t4 <- bbe4t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe4sim <- bbe4t [,c( "Subplot","num.visitors")]
d.bbe4 <-dist(bbe4sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe4 <- vegdist(d.bbe4, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l4<-  dplyr::left_join (sitios1,bbe4sim)
k4 <-dist(l4 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe4, k4,nrepet = 9999) 
mean(m.bbe4) #0.0819
sd(m.bbe4) # 0.157
#plot 5+beetle
p.5 <- subset (p, Plot == "5") 
bbe5 <- subset (p.5, Group == "Beetle")

bbe5t <- dplyr::left_join (sitios1,bbe5)

bbe5t$Plot[is.na(bbe5t$Plot)] <- 4
bbe5t$Group[is.na(bbe5t$Group)] <- "Beetle"
bbe5t$num.visitors[is.na(bbe5t$num.visitors)] <- 0
fct_explicit_na(bbe5t$Family, na_level = "(Missing)")
fct_explicit_na(bbe5t$Species, na_level = "(Missing)")
t5 <- bbe5t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe5sim <- bbe5t [,c( "Subplot","num.visitors")]
d.bbe5 <-dist(bbe5sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe5 <- vegdist(d.bbe5, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l5<-  dplyr::left_join (sitios1,bbe5sim)
k5 <-dist(l5 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe5, k5,nrepet = 9999) 
mean(m.bbe5) #0.023
sd(m.bbe5) # 0.069
#plot 6+beetle
p.6 <- subset (p, Plot == "6")
p.6$Plot <- as.numeric(p.6$Plot)
bbe6 <- subset (p.6, Group == "Beetle")

bbe6t <- dplyr::left_join (sitios1,bbe6)

bbe6t$Plot[is.na(bbe6t$Plot)] <- 6
bbe6t$Group[is.na(bbe6t$Group)] <- "Beetle"
bbe6t$num.visitors[is.na(bbe6t$num.visitors)] <- 0
fct_explicit_na(bbe6t$Family, na_level = "(Missing)")
fct_explicit_na(bbe6t$Species, na_level = "(Missing)")
t6 <- bbe6t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe6sim <- bbe6t [,c( "Subplot","num.visitors")]
d.bbe6 <-dist(bbe6sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe6 <- vegdist(d.bbe6, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l6<-  dplyr::left_join (sitios1,bbe6sim)
k6 <-dist(l6 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe6, k6,nrepet = 9999) 
mean(m.bbe6) #0.0031
sd(m.bbe6) # 0.016
#plot 7+beetle
p.7 <- subset (p, Plot == "7")
p.7$Plot <- as.numeric(p.7$Plot)
bbe7 <- subset (p.7, Group == "Beetle")

bbe7t <- dplyr::left_join (sitios1,bbe7)

bbe7t$Plot[is.na(bbe7t$Plot)] <- 7
bbe7t$Group[is.na(bbe7t$Group)] <- "Beetle"
bbe7t$num.visitors[is.na(bbe7t$num.visitors)] <- 0
fct_explicit_na(bbe7t$Family, na_level = "(Missing)")
fct_explicit_na(bbe7t$Species, na_level = "(Missing)")
t7 <- bbe7t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe7sim <- bbe7t [,c( "Subplot","num.visitors")]
d.bbe7 <-dist(bbe7sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe7 <- vegdist(d.bbe7, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l7<-  dplyr::left_join (sitios1,bbe7sim)
k7 <-dist(l7 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe7, k7,nrepet = 9999) 
mean(m.bbe7) #0.0788
sd(m.bbe7) # 0.16
#plot 8+beetle
p.8 <- subset (p, Plot == "8")
p.8$Plot <- as.numeric(p.8$Plot)
bbe8 <- subset (p.8, Group == "Beetle")

bbe8t <- dplyr::left_join (sitios1,bbe8)

bbe8t$Plot[is.na(bbe8t$Plot)] <- 8
bbe8t$Group[is.na(bbe8t$Group)] <- "Beetle"
bbe8t$num.visitors[is.na(bbe8t$num.visitors)] <- 0
fct_explicit_na(bbe8t$Family, na_level = "(Missing)")
fct_explicit_na(bbe8t$Species, na_level = "(Missing)")
t8 <- bbe8t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe8sim <- bbe8t [,c( "Subplot","num.visitors")]
d.bbe8 <-dist(bbe8sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe8 <- vegdist(d.bbe8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l8<-  dplyr::left_join (sitios1,bbe8sim)
k8 <-dist(l8 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe8, k8,nrepet = 9999) 
mean(m.bbe8) #0.086
sd(m.bbe8) # 0.19
#plot 9+beetle
p.9 <- subset (p, Plot == "9")
p.9$Plot <- as.numeric(p.9$Plot)
bbe9 <- subset (p.9, Group == "Beetle")

bbe9t <- dplyr::left_join (sitios1,bbe9)

bbe9t$Plot[is.na(bbe9t$Plot)] <- 9
bbe9t$Group[is.na(bbe9t$Group)] <- "Beetle"
bbe9t$num.visitors[is.na(bbe9t$num.visitors)] <- 0
fct_explicit_na(bbe9t$Family, na_level = "(Missing)")
fct_explicit_na(bbe9t$Species, na_level = "(Missing)")
#t9 <- bbe9t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bbe9sim <- bbe9t [,c( "Subplot","num.visitors")]
d.bbe9 <-dist(bbe9sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bbe9 <- vegdist(d.bbe9, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

l9<-  dplyr::left_join (sitios1,bbe9sim)
k9 <-dist(l9 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe9, k9,nrepet = 9999) 
mean(m.bbe9) #0.14
sd(m.bbe8) # 0.19
#flies ----
#plot 1+flies
bf1 <- subset (p.1, Group == "Fly")
bf1t <- dplyr::left_join (sitios1,bf1)

bf1t$Plot[is.na(bf1t$Plot)] <- 1
bf1t$Group[is.na(bf1t$Group)] <- "Fly"
bf1t$num.visitors[is.na(bf1t$num.visitors)] <- 0
fct_explicit_na(bf1t$Family, na_level = "(Missing)")
fct_explicit_na(bf1t$Species, na_level = "(Missing)")
ft1 <- bf1t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf1sim <- bf1t [,c( "Subplot","visits")]
d.df1 <-dist(bf1sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf1 <- vegdist(d.df1, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

u1<-  dplyr::left_join (sitios1,bf1sim)
m1 <-dist(u1 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf1, m1,nrepet = 9999) #no works, me pide valores de true o false ----
#mean(m.bf1) #0.086
#sd(m.bf1) # 0.19

#plot 2+flies
bf2 <- subset (p.2, Group == "Fly")
bf2t <- dplyr::left_join (sitios1,bf2)

bf2t$Plot[is.na(bf2t$Plot)] <- 2
bf2t$Group[is.na(bf2t$Group)] <- "Fly"
bf2t$num.visitors[is.na(bf2t$num.visitors)] <- 0
fct_explicit_na(bf2t$Family, na_level = "(Missing)")
fct_explicit_na(bf2t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf2sim <- bf2t [,c( "Subplot","num.visitors")]
d.df2 <-dist(bf2sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf2 <- vegdist(d.df2, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

u2<-  dplyr::left_join (sitios1,bf2sim)
m2 <-dist(u2 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf2, m2,nrepet = 9999)
mean(m.bf2) #0.025
sd(m.bf2) # 0.077
#plot 3+flies
bf3 <- subset (p.3, Group == "Fly")
bf3t <- dplyr::left_join (sitios1,bf3)

bf3t$Plot[is.na(bf3t$Plot)] <- 2
bf3t$Group[is.na(bf3t$Group)] <- "Fly"
bf3t$num.visitors[is.na(bf3t$num.visitors)] <- 0
fct_explicit_na(bf3t$Family, na_level = "(Missing)")
fct_explicit_na(bf3t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf3sim <- bf3t [,c( "Subplot","num.visitors")]
d.df3 <-dist(bf3sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf3 <- vegdist(d.df3, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

u3<-  dplyr::left_join (sitios1,bf3sim)
m3 <-dist(u3 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf3, m3,nrepet = 9999)
mean(m.bf3) #0.036
sd(m.bf3) # 0.094

#plot 4+flies
bf4 <- subset (p.4, Group == "Fly")
bf4t <- dplyr::left_join (sitios1,bf4)

bf4t$Plot[is.na(bf4t$Plot)] <- 4
bf4t$Group[is.na(bf4t$Group)] <- "Fly"
bf4t$num.visitors[is.na(bf4t$num.visitors)] <- 0
fct_explicit_na(bf4t$Family, na_level = "(Missing)")
fct_explicit_na(bf4t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf4sim <- bf4t [,c( "Subplot","num.visitors")]
d.df4 <-dist(bf4sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf4 <- vegdist(d.df4, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) #sale toda la matriz de 0

u4<-  dplyr::left_join (sitios1,bf4sim)
m4 <-dist(u4 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf4, m4,nrepet = 9999) # no works, me pide values de true/false ----
#mean(m.bf4) 
#sd(m.bf4) 
#plot 5+flies
bf5 <- subset (p.5, Group == "Fly")
bf5t <- dplyr::left_join (sitios1,bf5)

bf5t$Plot[is.na(bf5t$Plot)] <- 5
bf5t$Group[is.na(bf5t$Group)] <- "Fly"
bf5t$num.visitors[is.na(bf5t$num.visitors)] <- 0
fct_explicit_na(bf5t$Family, na_level = "(Missing)")
fct_explicit_na(bf5t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf5sim <- bf5t [,c( "Subplot","num.visitors")]
d.df5 <-dist(bf5sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf5 <- vegdist(d.df5, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

u5<-  dplyr::left_join (sitios1,bf5sim)
m5 <-dist(u5 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf5, m5,nrepet = 9999) # no works, me pide values de true/false ----
mean(m.bf5) 
sd(m.bf5) 
#plot 6+flies
bf6 <- subset (p.6, Group == "Fly")
bf6t <- dplyr::left_join (sitios1,bf6)

bf6t$Plot[is.na(bf6t$Plot)] <- 6
bf6t$Group[is.na(bf6t$Group)] <- "Fly"
bf6t$num.visitors[is.na(bf6t$num.visitors)] <- 0
fct_explicit_na(bf6t$Family, na_level = "(Missing)")
fct_explicit_na(bf6t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf6sim <- bf6t [,c( "Subplot","num.visitors")]
d.df6 <-dist(bf6sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf6 <- vegdist(d.df6, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
u6<-  dplyr::left_join (sitios1,bf6sim)
m6 <-dist(u6 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf6, m6,nrepet = 9999) 
mean(m.bf6)#0.0095 
sd(m.bf6) #0.027

#plot 7+flies
bf7 <- subset (p.7, Group == "Fly")
bf7t <- dplyr::left_join (sitios1,bf7)
bf7t$Plot[is.na(bf7t$Plot)] <- 7
bf7t$Group[is.na(bf7t$Group)] <- "Fly"
bf7t$num.visitors[is.na(bf7t$num.visitors)] <- 0
fct_explicit_na(bf7t$Family, na_level = "(Missing)")
fct_explicit_na(bf7t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf7sim <- bf7t [,c( "Subplot","num.visitors")]
d.df7 <-dist(bf7sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf7 <- vegdist(d.df7, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
u7<-  dplyr::left_join (sitios1,bf7sim)
m7 <-dist(u7 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf7, m7,nrepet = 9999) 
mean(m.bf7)#0.11 
sd(m.bf7) #0.21
#plot 8+flies
bf8 <- subset (p.8, Group == "Fly")
bf8t <- dplyr::left_join (sitios1,bf8)
bf8t$Plot[is.na(bf8t$Plot)] <- 8
bf8t$Group[is.na(bf8t$Group)] <- "Fly"
bf8t$num.visitors[is.na(bf8t$num.visitors)] <- 0
fct_explicit_na(bf8t$Family, na_level = "(Missing)")
fct_explicit_na(bf8t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf8sim <- bf8t [,c( "Subplot","num.visitors")]
d.df8 <-dist(bf8sim [,c(2)], method= "euclidean", diag =T, upper =T)

m.bf8 <- vegdist(d.df8, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
u8<-  dplyr::left_join (sitios1,bf8sim)
m8 <-dist(u8 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf8, m8,nrepet = 9999) 
mean(m.bf8)#0.12 
sd(m.bf8) #0.22

#plot 9+flies
bf9 <- subset (p.9, Group == "Fly")
bf9t <- dplyr::left_join (sitios1,bf9)
bf9t$Plot[is.na(bf9t$Plot)] <- 9
bf9t$Group[is.na(bf9t$Group)] <- "Fly"
bf9t$num.visitors[is.na(bf9t$num.visitors)] <- 0
fct_explicit_na(bf9t$Family, na_level = "(Missing)")
fct_explicit_na(bf9t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf9sim <- bf9t [,c( "Subplot","num.visitors")]
d.df9 <-dist(bf9sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.bf9 <- vegdist(d.df9, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
u9<-  dplyr::left_join (sitios1,bf9sim)
m9 <-dist(u9 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf9, m9,nrepet = 9999) 
mean(m.bf9)#0.089 
sd(m.bf9) #0.18

#plot +flies
bf8 <- subset (p.8, Group == "Fly")
bf8t <- dplyr::left_join (sitios1,bf8)
bf8t$Plot[is.na(bf8t$Plot)] <- 7
bf8t$Group[is.na(bf8t$Group)] <- "Fly"
bf8t$num.visitors[is.na(bf8t$num.visitors)] <- 0
fct_explicit_na(bf8t$Family, na_level = "(Missing)")
fct_explicit_na(bf8t$Species, na_level = "(Missing)")
#ft2 <- bf2t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 
bf8sim <- bf8t [,c( "Subplot","num.visitors")]
d.df8 <-dist(bf8sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.bf8 <- vegdist(d.df8, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
u8<-  dplyr::left_join (sitios1,bf8sim)
m8 <-dist(u8 [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf8, m8,nrepet = 9999) 
mean(m.bf8)#0.12 
sd(m.bf8) #0.22

###
#bees ----
#plot 1+bees
bbees1 <- subset (p.1, Group == "Bee")
bbees1t <- dplyr::left_join (sitios1,bbees1)
bbees1t$Plot[is.na(bbees1t$Plot)] <- 1
bbees1t$Group[is.na(bbees1t$Group)] <- "Bee"
bbees1t$num.visitors[is.na(bbees1t$num.visitors)] <- 0
fct_explicit_na(bbees1t$Family, na_level = "(Missing)")
fct_explicit_na(bbees1t$Species, na_level = "(Missing)")
bbees1sim <- bbees1t [,c( "Subplot","num.visitors")]
d.dbees1 <-dist(bbees1sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees1 <- vegdist(d.dbees1, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

p.1.bee<-  dplyr::left_join (sitios1,bbees1sim)
dis.bee1 <-dist(bbees1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees1, dis.bee1,nrepet = 9999) #corregido
mean(m.dbees1) #0.1
sd(m.dbees1) # 0.29

#plot 2+bees
bbees2 <- subset (p.2, Group == "Bee")
bbees2t <- dplyr::left_join (sitios1,bbees2)
bbees2t$Plot[is.na(bbees2t$Plot)] <- 2
bbees2t$Group[is.na(bbees2t$Group)] <- "Bee"
bbees2t$num.visitors[is.na(bbees2t$num.visitors)] <- 0
fct_explicit_na(bbees2t$Family, na_level = "(Missing)")
fct_explicit_na(bbees2t$Species, na_level = "(Missing)")
bbees2sim <- bbees2t [,c( "Subplot","num.visitors")]
d.dbees2 <-dist(bbees2sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees2 <- vegdist(d.dbees2, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) #problem <- matriz con NAs ----

p.2.bee<-  dplyr::left_join (sitios1,bbees2sim)
dis.bee2 <-dist(p.2.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees2, dis.bee2,nrepet = 9999) #no works ----
#mean(m.dbees2) 
#sd(m.dbees2) 
#plot 3+bees
bbees3 <- subset (p.3, Group == "Bee")
bbees3t <- dplyr::left_join (sitios1,bbees3)
bbees3t$Plot[is.na(bbees3t$Plot)] <- 3
bbees3t$Group[is.na(bbees3t$Group)] <- "Bee"
bbees3t$num.visitors[is.na(bbees3t$num.visitors)] <- 0
fct_explicit_na(bbees3t$Family, na_level = "(Missing)")
fct_explicit_na(bbees3t$Species, na_level = "(Missing)")
bbees3sim <- bbees3t [,c( "Subplot","num.visitors")]
d.dbees3 <-dist(bbees3sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees3 <- vegdist(d.dbees3, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE)
p.3.bee<-  dplyr::left_join (sitios1,bbees3sim)
dis.bee3 <-dist(p.3.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees3, dis.bee3,nrepet = 9999) 
mean(m.dbees3) 
sd(m.dbees3) 
#plot 4+bees
bbees4 <- subset (p.4, Group == "Bee")
bbees4t <- dplyr::left_join (sitios1,bbees4)
bbees4t$Plot[is.na(bbees4t$Plot)] <- 4
bbees4t$Group[is.na(bbees4t$Group)] <- "Bee"
bbees4t$num.visitors[is.na(bbees4t$num.visitors)] <- 0
fct_explicit_na(bbees4t$Family, na_level = "(Missing)")
fct_explicit_na(bbees4t$Species, na_level = "(Missing)")
bbees4sim <- bbees4t [,c( "Subplot","num.visitors")]
d.dbees4 <-dist(bbees4sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees4 <- vegdist(d.dbees4, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 

p.4.bee<-  dplyr::left_join (sitios1,bbees4sim)
dis.bee4 <-dist(p.4.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees4, dis.bee4,nrepet = 9999) 
mean(m.dbees4) 
sd(m.dbees4) 
#plot 5+bees
bbees5 <- subset (p.5, Group == "Bee") #solo hay un dato
bbees5t <- dplyr::left_join (sitios1,bbees5)
bbees5t$Plot[is.na(bbees5t$Plot)] <- 5
bbees5t$Group[is.na(bbees5t$Group)] <- "Bee"
bbees5t$num.visitors[is.na(bbees5t$num.visitors)] <- 0
fct_explicit_na(bbees5t$Family, na_level = "(Missing)")
fct_explicit_na(bbees5t$Species, na_level = "(Missing)")
bbees5sim <- bbees5t [,c( "Subplot","num.visitors")]
d.dbees5 <-dist(bbees5sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees5 <- vegdist(d.dbees5, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) #matriz de Nas , no works ----
p.5.bee<-  dplyr::left_join (sitios1,bbees5sim)
dis.bee5 <-dist(p.5.bee [,c(4)], method= "euclidean", diag =T, upper =T)
#mantel.rtest (m.dbees5, dis.bee5,nrepet = 9999) #no works ----
#mean(m.dbees5) 
#sd(m.dbees5) 
#plot 6+bees
bbees6 <- subset (p.6, Group == "Bee") 
bbees6t <- dplyr::left_join (sitios1,bbees6)
bbees6t$Plot[is.na(bbees6t$Plot)] <- 6
bbees6t$Group[is.na(bbees6t$Group)] <- "Bee"
bbees6t$num.visitors[is.na(bbees6t$num.visitors)] <- 0
fct_explicit_na(bbees6t$Family, na_level = "(Missing)")
fct_explicit_na(bbees6t$Species, na_level = "(Missing)")
bbees6sim <- bbees6t [,c( "Subplot","num.visitors")]
d.dbees6 <-dist(bbees6sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees6 <- vegdist(d.dbees6, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
p.6.bee<-  dplyr::left_join (sitios1,bbees6sim)
dis.bee6 <-dist(p.6.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees6, dis.bee6,nrepet = 9999) 
mean(m.dbees6) #0.17
sd(m.dbees6) #0.30
#plot 7+bees
bbees7 <- subset (p.7, Group == "Bee") 
bbees7t <- dplyr::left_join (sitios1,bbees7)
bbees7t$Plot[is.na(bbees7t$Plot)] <- 7
bbees7t$Group[is.na(bbees7t$Group)] <- "Bee"
bbees7t$num.visitors[is.na(bbees7t$num.visitors)] <- 0
fct_explicit_na(bbees7t$Family, na_level = "(Missing)")
fct_explicit_na(bbees7t$Species, na_level = "(Missing)")
bbees7sim <- bbees7t [,c( "Subplot","num.visitors")]
d.dbees7 <-dist(bbees7sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees7 <- vegdist(d.dbees7, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
p.7.bee<-  dplyr::left_join (sitios1,bbees7sim)
dis.bee7 <-dist(p.7.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees7, dis.bee7,nrepet = 9999) 
mean(m.dbees7) #0.17
sd(m.dbees7) #0.27
#plot 8+bees
bbees8 <- subset (p.8, Group == "Bee") 
bbees8t <- dplyr::left_join (sitios1,bbees8)
bbees8t$Plot[is.na(bbees8t$Plot)] <- 8
bbees8t$Group[is.na(bbees8t$Group)] <- "Bee"
bbees8t$num.visitors[is.na(bbees8t$num.visitors)] <- 0
fct_explicit_na(bbees8t$Family, na_level = "(Missing)")
fct_explicit_na(bbees8t$Species, na_level = "(Missing)")
bbees8sim <- bbees8t [,c( "Subplot","num.visitors")]
d.dbees8 <-dist(bbees8sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees8 <- vegdist(d.dbees8, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
p.8.bee<-  dplyr::left_join (sitios1,bbees8sim)
dis.bee8 <-dist(p.8.bee [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees8, dis.bee8,nrepet = 9999) 
mean(m.dbees8) #0.39
sd(m.dbees8) #0.09
#plot 9+bees
bbees9 <- subset (p.9, Group == "Bee") #solo dos datos
bbees9t <- dplyr::left_join (sitios1,bbees9)
bbees9t$Plot[is.na(bbees9t$Plot)] <- 8
bbees9t$Group[is.na(bbees9t$Group)] <- "Bee"
bbees9t$num.visitors[is.na(bbees9t$num.visitors)] <- 0
fct_explicit_na(bbees9t$Family, na_level = "(Missing)")
fct_explicit_na(bbees9t$Species, na_level = "(Missing)")
bbees9sim <- bbees9t [,c( "Subplot","num.visitors")]
d.dbees9 <-dist(bbees9sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbees9 <- vegdist(d.dbees9, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) #no works, ceros y NAs ----
p.9.bee<-  dplyr::left_join (sitios1,bbees9sim)
dis.bee9 <-dist(p.9.bee [,c(4)], method= "euclidean", diag =T, upper =T)
#mantel.rtest (m.dbees9, dis.bee9,nrepet = 9999) 
#mean(m.dbees9) 
#sd(m.dbees9) 
####
#butterflies----
#plot 7 + butterflies, solo mariposas en plot 7 y 8
bbu7 <- subset (p.7, Group == "Butterfly")
bbu7t <- dplyr::left_join (sitios1,bbu7)
bbu7t$Plot[is.na(bbu7t$Plot)] <- 7
bbu7t$Group[is.na(bbu7t$Group)] <- "Bee"
bbu7t$num.visitors[is.na(bbu7t$num.visitors)] <- 0
fct_explicit_na(bbu7t$Family, na_level = "(Missing)")
fct_explicit_na(bbu7t$Species, na_level = "(Missing)")
bbu7sim <- bbu7t [,c( "Subplot","num.visitors")]
d.dbu7 <-dist(bbu7sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbu7 <- vegdist(d.dbu7, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) #me salen NAs ----
p.7.bu<-  dplyr::left_join (sitios1,bbu7sim)
d.dbu7 <-dist(p.7.bu [,c(4)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbu7, d.dbu7,nrepet = 9999) #no works
#mean(m.dbu7) 
#sd(m.dbu7) 
#plot 8 + butterflies, solo mariposas en plot 7 y 8
bbu8 <- subset (p.8, Group == "Butterfly")
bbu8t <- dplyr::left_join (sitios1,bbu8)
bbu8t$Plot[is.na(bbu8t$Plot)] <- 8
bbu8t$Group[is.na(bbu8t$Group)] <- "Bee"
bbu8t$num.visitors[is.na(bbu8t$num.visitors)] <- 0
fct_explicit_na(bbu8t$Family, na_level = "(Missing)")
fct_explicit_na(bbu8t$Species, na_level = "(Missing)")
bbu8sim <- bbu8t [,c( "Subplot","num.visitors")]
d.dbu8 <-dist(bbu8sim [,c(2)], method= "euclidean", diag =T, upper =T)
m.dbu8 <- vegdist(d.dbu8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) #me salen NAs ----
p.8.bu<-  dplyr::left_join (sitios1,bbu8sim)
d.dbu8 <-dist(p.8.bu [,c(4)], method= "euclidean", diag =T, upper =T)
 i <-mantel.rtest (m.dbu8, d.dbu8,nrepet = 9999) 
mean(m.dbu8) 
sd(m.dbu8) 

###nivel de paisaje <- hacer ----
j <-subset(pol.9,Group %in% c("Bee"))
y <- j [,c( "Subplot", "Group","num.visitors")]
y1 <-dplyr::left_join (sitios1,y)
y1$num.visitors[is.na(y1$num.visitors)] <- 0
d.abeja <- dist(y1 [,c(5)], method= "euclidean", diag =T, upper =T) 
m.abeja <- vegdist(d.abeja, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE)

d.sitios <-dist(y1 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.abeja, d.sitios,nrepet = 9999) 
mean(m.abeja) 
sd(m.abeja) 
