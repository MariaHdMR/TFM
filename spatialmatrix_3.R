#LIBRERIAS
library(vegan) #me sirve para bioenv
library(tidyverse)
library(ade4) #este es el paquete para el mantel test que he usado
library(geoRmantel)
library(gstat)
library(aqfig)
library(lattice)
library(lme4) #para el glmm
library(reshape2)
#datos
va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
sitios <- read.table("data/caracolesplotposition.csv", header=T, sep= ";")
sitios <-sitios[which(complete.cases(sitios)),]
sitios$Subplot <- sitios$position
head(va)
plantas <- read.table("data/Abun_19.csv", header=T, sep= ";")

va19 <- subset(va, Year== "2019")
pol.9 <- va19 %>% group_by(Plot, Subplot, Group) %>% summarise (num.visitors = sum(Visits))
pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")
h <- dist(sitios[,c(4,5)], method= "euclidean", diag=T, upper=T)

#analisis
#ABUNDANCIAS BICHOS ----
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
#mantel(dist.1, h, method="pearson", permutations=999, strata = NULL,na.rm = FALSE, parallel = getOption("mc.cores"))
#vale, comprobado que los dos mantel test dan lo mismo, esta bien hecho mantel.rtest, con paquete ADE4
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
#que son: plot 3
#plot 3 y beetles
p3 <- subset(pol.beetle.B, Plot== "3")
t3 <- merge(p3, sitios, by= "Subplot", all= T)
t3$Plot[is.na(t3$Plot)] <- 3#en el siguiente paso elijo mi primer grupo
t3[is.na(t3)] <- 0
geo_data3 <- as.geodata(t3[1:36,], coords.col = 7:8, data.col = 3)
vis3 <- likfit(geo_data3, ini = c(1,0.5), fix.nugget = T)
pred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_ph_1133 <- krige.conv(geo_data3, loc = pred.grid3, krige = krige.control(obj.m = vis3))
image(kc_ph_1133, loc = pred.grid3, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of beetle abundances in plot 3")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_1133$predict), max(kc_ph_1133$predict))) #este es el unico con valores significativos, el resto se aproximan a la significacion

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
image(kc_f_2, loc = fpred.grid2, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of fly abundances in plot 2")
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
image(kc_f_5, loc = fpred.grid5, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Distribution of fly abundances in plot 5")
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
image(kc_f_7, loc = fpred.grid7, col=rainbow(15), xlab=" Coordinate X (m)", ylab="Coordinate Y (m)", main="Distribution of fly abundances plot 7")
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
#B-diversidad/composicion bichos----
p <- va19 %>% group_by(Plot, Subplot, Group,Family, Species) %>% summarise (num.visitors = sum(Visits))
#pol.9 <-pol.9[which(complete.cases(pol.9)),]
p$Family <- as.character(p$Family)
p$Species <- as.character(p$Species)
#fct_explicit_na(p$Family, na_level = "(Missing)")
#fct_explicit_na(p$Species, na_level = "(Missing)")
p <- subset(p, Plot != "OUT")
# beetles - b-diversity 
#plot 1 + beetles
p.1 <- subset (p, Plot == "1") 
bbet1 <- subset (p.1, Group == "Beetle")
columnsbe1 <- dcast(bbet1, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet1t <- dplyr::left_join (sitios1,columnsbe1)
bet1t[is.na(bet1t)] <- 0


#bbe1t$Plot[is.na(bbe1t$Plot)] <- 1
#bbe1t$Group[is.na(bbe1t$Group)] <- "Beetle"
#bbe1t$num.visitors[is.na(bbe1t$num.visitors)] <- 0
#fct_explicit_na(bbe1t$Family, na_level = "(Missing)")
#fct_explicit_na(bbe1t$Species, na_level = "(Missing)")
#t1 <- bbe1t %>% group_by(Subplot, Group, Family, Species) %>% summarise (visits = sum(num.visitors)) 

#bbe1sim <- bbe1t [,c( "Subplot","num.visitors")]
d.bbe1 <-dist(bet1t [,c(5:7)], method= "euclidean", diag =T, upper =T)

m.bbe1 <- vegdist(d.bbe1, method="morisita", binary=FALSE, diag= T, upper=T,
        na.rm = FALSE) 

#l1<-  dplyr::left_join (sitios1,bbe1sim)
k1 <-dist(bet1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe1, k1,nrepet = 9999) #corregido1
mean(m.bbe1) #media de b-diversidad para el plot 1 de escarabajos = 0.049
sd(m.bbe1) #plot 1 +beetle = 0.11

beetgeo_data1 <- as.geodata(bet1t[1:36,], coords.col = 2:3, data.col = 4:6)
beetvis1 <- likfit(beetgeo_data1, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema, dice que las localizaciones no se ajustan a los datos
beetpred.grid1 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_beet_1<- krige.conv(beetgeo_data2, loc = beetpred.grid1, krige = krige.control(obj.m = beetvis1)) #no works mapa de calor ----
image(kc_beet_1, loc = beetpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_1$predict),max(kc_f_1$predict)))

#plot 2+beetle
p.2 <- subset (p, Plot == "2") 
bbet2 <- subset (p.2, Group == "Beetle") 


columnsbet2 <- dcast(bbet2, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet2t <- dplyr::left_join (sitios1,columnsbet2)
bet2t[is.na(bet2t)] <- 0

bbe2.bueno <- dplyr::left_join (sitios1,bbet2)
bbe2.bueno$num.visitors[is.na(bbe2.bueno$num.visitors)] <- 0

d.bbe2 <-dist(bet2t [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bbe2 <- vegdist(d.bbe2, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l2<-  dplyr::left_join (sitios1,bbe2sim)
k2 <-dist(bet2t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe2, k2,nrepet = 9999) 
mean(m.bbe2) #0.26
sd(m.bbe2) # 0.06


#plot 3+beetle
p.3 <- subset (p, Plot == "3") 
bbet3 <- subset (p.3, Group == "Beetle")
sitios.bbet3 <- dplyr::left_join(sitios1, bbet3)
sitios.bbet3$num.visitors[is.na(sitios.bbet3$num.visitors)] <- 0

columnsbet3 <- dcast(bbet3, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet3t <- dplyr::left_join (sitios1,columnsbet3)
bet3t[is.na(bet3t)] <- 0
d.bbe3 <-dist(bet3t [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bbe3 <- vegdist(d.bbe3, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l3<-  dplyr::left_join (sitios1,bbe3sim)
k3 <-dist(bet3t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe3, k3,nrepet = 9999) #corregido
mean(m.bbe3) #0.086
sd(m.bbe3) # 0.157


B_beetgeo_data3 <- as.geodata(sitios.bbet3[1:44,], coords.col = 2:3, data.col = 9)
b_beetvis3 <- likfit(B_beetgeo_data3, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema----
b_beetpred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
kc_beet_2<- krige.conv(beetgeo_data2, loc = beetpred.grid2, krige = krige.control(obj.m = beetvis2)) #no works mapa de calor ----
image(kc_beet_2, loc = beetpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_7$predict),max(kc_f_7$predict)))

#plot 4+beetle
p.4 <- subset (p, Plot == "4") 
bbe4 <- subset (p.4, Group == "Beetle")


columnsbet4 <- dcast(bbe4, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet4t <- dplyr::left_join (sitios1,columnsbet4)
bet4t[is.na(bet4t)] <- 0
d.bbe4 <-dist(bet4t [,c(5:7)], method= "euclidean", diag =T, upper =T)

m.bbe4 <- vegdist(d.bbe4, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l4<-  dplyr::left_join (sitios1,bbe4sim)
k4 <-dist(bet4t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe4, k4,nrepet = 9999) #corregido
mean(m.bbe4) #0.0819
sd(m.bbe4) # 0.157
#plot 5+beetle
p.5 <- subset (p, Plot == "5") 
bbe5 <- subset (p.5, Group == "Beetle")
columnsbet5 <- dcast(bbe5, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet5t <- dplyr::left_join (sitios1,columnsbet5)
bet5t[is.na(bet5t)] <- 0
d.bbe5 <-dist(bet5t [,c(5:8)], method= "euclidean", diag =T, upper =T)

m.bbe5 <- vegdist(d.bbe5, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l5<-  dplyr::left_join (sitios1,bbe5sim)
k5 <-dist(bet5t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe5, k5,nrepet = 9999) #corregido
mean(m.bbe5) #0.023
sd(m.bbe5) # 0.069
#plot 6+beetle
p.6 <- subset (p, Plot == "6")
p.6$Plot <- as.numeric(p.6$Plot)
bbe6 <- subset (p.6, Group == "Beetle")

columnsbet6 <- dcast(bbe6, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet6t <- dplyr::left_join (sitios1,columnsbet6)
bet6t[is.na(bet6t)] <- 0

d.bbe6 <-dist(bet6t [,c(5:7)], method= "euclidean", diag =T, upper =T)

m.bbe6 <- vegdist(d.bbe6, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l6<-  dplyr::left_join (sitios1,bbe6sim)
k6 <-dist(bet6t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe6, k6,nrepet = 9999) 
mean(m.bbe6) #0.0031
sd(m.bbe6) # 0.016
#plot 7+beetle
p.7 <- subset (p, Plot == "7")
p.7$Plot <- as.numeric(p.7$Plot)
bbe7 <- subset (p.7, Group == "Beetle")

columnsbet7 <- dcast(bbe7, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet7t <- dplyr::left_join (sitios1,columnsbet7)
bet7t[is.na(bet7t)] <- 0
d.bbe7 <-dist(bet7t [,c(5:7)], method= "euclidean", diag =T, upper =T)

m.bbe7 <- vegdist(d.bbe7, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l7<-  dplyr::left_join (sitios1,bbe7sim)
k7 <-dist(bet7t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe7, k7,nrepet = 9999) #corregido
#mean(m.bbe7) #0.0788
#sd(m.bbe7) # 0.16
#plot 8+beetle
p.8 <- subset (p, Plot == "8")
p.8$Plot <- as.numeric(p.8$Plot)
bbe8 <- subset (p.8, Group == "Beetle")

columnsbet8 <- dcast(bbe8, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet8t <- dplyr::left_join (sitios1,columnsbet8)
bet8t[is.na(bet8t)] <- 0

d.bbe8 <-dist(bet8t [,c(5:8)], method= "euclidean", diag =T, upper =T)

m.bbe8 <- vegdist(d.bbe8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l8<-  dplyr::left_join (sitios1,bbe8sim)
k8 <-dist(bet8t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe8, k8,nrepet = 9999) 
#mean(m.bbe8) #0.086
#sd(m.bbe8) # 0.19
#plot 9+beetle
p.9 <- subset (p, Plot == "9")
p.9$Plot <- as.numeric(p.9$Plot)
bbe9 <- subset (p.9, Group == "Beetle")

columnsbet9 <- dcast(bbe9, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet9t <- dplyr::left_join (sitios1,columnsbet9)
bet9t[is.na(bet9t)] <- 0

d.bbe9 <-dist(bet9t [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bbe9 <- vegdist(d.bbe9, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 

#l9<-  dplyr::left_join (sitios1,bbe9sim)
k9 <-dist(bet9t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe9, k9,nrepet = 9999) 
#mean(m.bbe9) #0.14
#sd(m.bbe8) # 0.19
#flies ----
#plot 1+flies
library(reshape2)
bf1 <- subset (p.1, Group == "Fly")

columnsbf1 <- dcast(bf1, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bf1t <- dplyr::left_join (sitios1,columnsbf1)
bf1t[is.na(bf1t)] <- 0
d.df1 <-dist(bf1t [,c(5:13)], method= "euclidean", diag =T, upper =T)

m.bf1 <- vegdist(d.df1, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) #matriz de 0

#u1<-  dplyr::left_join (sitios1,bf1sim)
m1 <-dist(bf1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf1, m1,nrepet = 9999) 
#mean(m.bf1) 
#sd(m.bf1) 

#plot 2+flies
bf2 <- subset (p.2, Group == "Fly")
bf2t <- dplyr::left_join (sitios1,bf2)

columnsfl2 <- dcast(bf2t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl2t <- dplyr::left_join (sitios1,columnsfl2)
fl2t[is.na(fl2t)] <- 0
d.df2 <-dist(fl2t [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bf2 <- vegdist(d.df2, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

#u2<-  dplyr::left_join (sitios1,bf2sim)
m2 <-dist(fl2t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf2, m2,nrepet = 9999)
mean(m.bf2) #0.025
sd(m.bf2) # 0.077
#plot 3+flies
bf3 <- subset (p.3, Group == "Fly")
bf3t <- dplyr::left_join (sitios1,bf3)

columnsfl3 <- dcast(bf3t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl3t <- dplyr::left_join (sitios1,columnsfl3)
fl3t[is.na(fl3t)] <- 0

d.df3 <-dist(fl3t [,c(5:9)], method= "euclidean", diag =T, upper =T)

m.bf3 <- vegdist(d.df3, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

#u3<-  dplyr::left_join (sitios1,bf3sim)
m3 <-dist(fl3t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf3, m3,nrepet = 9999)
mean(m.bf3) #0.036
sd(m.bf3) # 0.094

#plot 4+flies
bf4 <- subset (p.4, Group == "Fly")
bf4t <- dplyr::left_join (sitios1,bf4)

columnsfl4 <- dcast(bf4t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl4t <- dplyr::left_join (sitios1,columnsfl4)
fl4t[is.na(fl4t)] <- 0
d.df4 <-dist(fl4t [,c(5:11)], method= "euclidean", diag =T, upper =T)

m.bf4 <- vegdist(d.df4, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

#u4<-  dplyr::left_join (sitios1,bf4sim)
m4 <-dist(fl4t [,c(2:3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf4, m4,nrepet = 9999) 
#mean(m.bf4) 
#sd(m.bf4) 
#plot 5+flies
bf5 <- subset (p.5, Group == "Fly")
bf5t <- dplyr::left_join (sitios1,bf5)

columnsfl5 <- dcast(bf5t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl5t <- dplyr::left_join (sitios1,columnsfl5)
fl5t[is.na(fl5t)] <- 0

d.df5 <-dist(fl5t [,c(5:12)], method= "euclidean", diag =T, upper =T)

m.bf5 <- vegdist(d.df5, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

#u5<-  dplyr::left_join (sitios1,bf5sim)
m5 <-dist(fl5t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf5, m5,nrepet = 9999) 
mean(m.bf5) #0.041
sd(m.bf5) #0.1
#plot 6+flies
bf6 <- subset (p.6, Group == "Fly")
bf6t <- dplyr::left_join (sitios1,bf6)

columnsfl6 <- dcast(bf6t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl6t <- dplyr::left_join (sitios1,columnsfl6)
fl6t[is.na(fl6t)] <- 0

d.df6 <-dist(fl6t [,c(5:13)], method= "euclidean", diag =T, upper =T)

m.bf6 <- vegdist(d.df6, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
#u6<-  dplyr::left_join (sitios1,bf6sim)
m6 <-dist(fl6t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf6, m6,nrepet = 9999) 
mean(m.bf6)#0.0095 
sd(m.bf6) #0.027

#plot 7+flies
bf7 <- subset (p.7, Group == "Fly")
bf7t <- dplyr::left_join (sitios1,bf7)

columnsfl7 <- dcast(bf7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl7t <- dplyr::left_join (sitios1,columnsfl7)
fl7t[is.na(fl7t)] <- 0


d.df7 <-dist(fl7t [,c(5:11)], method= "euclidean", diag =T, upper =T)

m.bf7 <- vegdist(d.df7, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
#u7<-  dplyr::left_join (sitios1,bf7sim)
m7 <-dist(fl7t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf7, m7,nrepet = 9999) 
mean(m.bf7)#0.11 
sd(m.bf7) #0.21
#plot 8+flies
bf8 <- subset (p.8, Group == "Fly")
bf8t <- dplyr::left_join (sitios1,bf8)

columnsfl8 <- dcast(bf8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl8t <- dplyr::left_join (sitios1,columnsfl8)
fl8t[is.na(fl8t)] <- 0

d.df8 <-dist(fl8t [,c(5:15)], method= "euclidean", diag =T, upper =T)

m.bf8 <- vegdist(d.df8, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
#u8<-  dplyr::left_join (sitios1,bf8sim)
m8 <-dist(fl8t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf8, m8,nrepet = 9999) 
mean(m.bf8)#0.12 
sd(m.bf8) #0.22

#plot 9+flies
bf9 <- subset (p.9, Group == "Fly") #aqui ----
bf9t <- dplyr::left_join (sitios1,bf9)

columnsfl9 <- dcast(bf9t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl9t <- dplyr::left_join (sitios1,columnsfl9)
fl9t[is.na(fl9t)] <- 0
d.df9 <-dist(fl9t [,c(5:12)], method= "euclidean", diag =T, upper =T)
m.bf9 <- vegdist(d.df9, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
#u9<-  dplyr::left_join (sitios1,bf9sim)
m9 <-dist(fl9t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf9, m9,nrepet = 9999) 
mean(m.bf9)#0.089 
sd(m.bf9) #0.18


###
#bees ----
#plot 1+bees
bbees1 <- subset (p.1, Group == "Bee")
bbees1t <- dplyr::left_join (sitios1,bbees1)

columnsbees1t <- dcast(bbees1t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbees1t <- dplyr::left_join (sitios1,columnsbees1t)
bbees1t[is.na(bbees1t)] <- 0

d.dbees1 <-dist(bbees1t [,c(5)], method= "euclidean", diag =T, upper =T)
m.dbees1 <- vegdist(d.dbees1, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 

#p.1.bee<-  dplyr::left_join (sitios1,bbees1sim)
dis.bee1 <-dist(bbees1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees1, dis.bee1,nrepet = 9999) #corregido
mean(m.dbees1) #0.1
sd(m.dbees1) # 0.29

#plot 2+bees
bbees2 <- subset (p.2, Group == "Bee")
bbees2t <- dplyr::left_join (sitios1,bbees2)

columnsbees2t <- dcast(bbees2t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbees2 <- dplyr::left_join (sitios1,columnsbees1t)
bbees2[is.na(bbees2)] <- 0

d.dbees2 <-dist(bbees2 [,c(5)], method= "euclidean", diag =T, upper =T)
m.dbees2 <- vegdist(d.dbees2, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 

#p.2.bee<-  dplyr::left_join (sitios1,bbees2sim)
dis.bee2 <-dist(bbees2 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees2, dis.bee2,nrepet = 9999) 
#mean(m.dbees2) 
#sd(m.dbees2) 
#plot 3+bees
bbees3 <- subset (p.3, Group == "Bee")
bbees3t <- dplyr::left_join (sitios1,bbees3)

columnsbees3t <- dcast(bbees3t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess3 <- dplyr::left_join (sitios1,columnsbees3t)
bbeeess3[is.na(bbeeess3)] <- 0


d.dbees3 <-dist(bbeeess3 [,c(5)], method= "euclidean", diag =T, upper =T)
m.dbees3 <- vegdist(d.dbees3, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE)
#p.3.bee<-  dplyr::left_join (sitios1,bbees3sim)
dis.bee3 <-dist(bbeeess3 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees3, dis.bee3,nrepet = 9999) 
#mean(m.dbees3) #0.14
#sd(m.dbees3) #0.29
#plot 4+bees
bbees4 <- subset (p.4, Group == "Bee")
bbees4t <- dplyr::left_join (sitios1,bbees4)


columnsbees4t <- dcast(bbees4t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess4 <- dplyr::left_join (sitios1,columnsbees4t)
bbeeess4[is.na(bbeeess4)] <- 0

d.dbees4 <-dist(bbeeess4 [,c(5)], method= "euclidean", diag =T, upper =T)
m.dbees4 <- vegdist(d.dbees4, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 

#p.4.bee<-  dplyr::left_join (sitios1,bbees4sim)
dis.bee4 <-dist(bbeeess4 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees4, dis.bee4,nrepet = 9999) 
#mean(m.dbees4) #0.17
#sd(m.dbees4) # 0.28
#plot 5+bees
bbees5 <- subset (p.5, Group == "Bee") #solo hay un dato
bbees5t <- dplyr::left_join (sitios1,bbees5)
columnsbees5t <- dcast(bbees5t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess5 <- dplyr::left_join (sitios1,columnsbees5t)
bbeeess5[is.na(bbeeess5)] <- 0
d.dbees5 <-dist(bbeeess5 [,c(5)], method= "euclidean", diag =T, upper =T)
m.dbees5 <- vegdist(d.dbees5, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.5.bee<-  dplyr::left_join (sitios1,bbees5sim)
dis.bee5 <-dist(bbeeess5 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees5, dis.bee5,nrepet = 9999) 
#mean(m.dbees5) 
#sd(m.dbees5) 
#plot 6+bees
bbees6 <- subset (p.6, Group == "Bee") 
bbees6t <- dplyr::left_join (sitios1,bbees6)


columnsbees6t <- dcast(bbees6t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess6 <- dplyr::left_join (sitios1,columnsbees6t)
bbeeess6[is.na(bbeeess6)] <- 0


d.dbees6 <-dist(bbeeess6 [,c(5:6)], method= "euclidean", diag =T, upper =T)
m.dbees6 <- vegdist(d.dbees6, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.6.bee<-  dplyr::left_join (sitios1,bbees6sim)
dis.bee6 <-dist(bbeeess6 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees6, dis.bee6,nrepet = 9999) 
#mean(m.dbees6) #0.17
#sd(m.dbees6) #0.30
#plot 7+bees
bbees7 <- subset (p.7, Group == "Bee") 
bbees7t <- dplyr::left_join (sitios1,bbees7)

columnsbees7t <- dcast(bbees7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess7 <- dplyr::left_join (sitios1,columnsbees7t)
bbeeess7[is.na(bbeeess7)] <- 0

d.dbees7 <-dist(bbeeess7 [,c(5:8)], method= "euclidean", diag =T, upper =T)
m.dbees7 <- vegdist(d.dbees7, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.7.bee<-  dplyr::left_join (sitios1,bbees7sim)
dis.bee7 <-dist(bbeeess7 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees7, dis.bee7,nrepet = 9999) 
#mean(m.dbees7) #0.17
#sd(m.dbees7) #0.27
#plot 8+bees
bbees8 <- subset (p.8, Group == "Bee") 
bbees8t <- dplyr::left_join (sitios1,bbees8)

columnsbees8t <- dcast(bbees8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess8 <- dplyr::left_join (sitios1,columnsbees8t)
bbeeess8[is.na(bbeeess8)] <- 0



d.dbees8 <-dist(bbeeess8 [,c(5:7)], method= "euclidean", diag =T, upper =T)
m.dbees8 <- vegdist(d.dbees8, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.8.bee<-  dplyr::left_join (sitios1,bbees8sim)
dis.bee8 <-dist(bbeeess8 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees8, dis.bee8,nrepet = 9999) 
#mean(m.dbees8) #0.39
#sd(m.dbees8) #0.09
#plot 9+bees
bbees9 <- subset (p.9, Group == "Bee") #solo dos datos
bbees9t <- dplyr::left_join (sitios1,bbees9)

columnsbees9t <- dcast(bbees9t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess9 <- dplyr::left_join (sitios1,columnsbees9t)
bbeeess9[is.na(bbeeess9)] <- 0

d.dbees9 <-dist(bbeeess9 [,c(5:7)], method= "euclidean", diag =T, upper =T)
m.dbees9 <- vegdist(d.dbees9, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.9.bee<-  dplyr::left_join (sitios1,bbees9sim)
dis.bee9 <-dist(bbeeess9 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees9, dis.bee9,nrepet = 9999) 
#mean(m.dbees9) 
#sd(m.dbees9) 
####
#butterflies----
#plot 7 + butterflies, solo mariposas en plot 7 y 8
bbu7 <- subset (p.7, Group == "Butterfly")
bbu7t <- dplyr::left_join (sitios1,bbu7)


columnsbutter7t <- dcast(bbu7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbutt7 <- dplyr::left_join (sitios1,columnsbutter7t)
bbutt7[is.na(bbutt7)] <- 0

d.dbu7 <-dist(bbutt7 [,c(5:7)], method= "euclidean", diag =T, upper =T)
m.dbu7 <- vegdist(d.dbu7, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
#p.7.bu<-  dplyr::left_join (sitios1,bbu7sim)
d.dbu7 <-dist(bbutt7 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbu7, d.dbu7,nrepet = 9999) 
#mean(m.dbu7) 
#sd(m.dbu7) 
#plot 8 + butterflies, solo mariposas en plot 7 y 8
bbu8 <- subset (p.8, Group == "Butterfly")
bbu8t <- dplyr::left_join (sitios1,bbu8)

columnsbutter8t <- dcast(bbu8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbutt8 <- dplyr::left_join (sitios1,columnsbutter8t)
bbutt8[is.na(bbutt8)] <- 0


d.dbu8 <-dist(bbutt8 [,c(5:7)], method= "euclidean", diag =T, upper =T)
m.dbu8 <- vegdist(d.dbu8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
#p.8.bu<-  dplyr::left_join (sitios1,bbu8sim)
d.dbu8 <-dist(bbutt8 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbu8, d.dbu8,nrepet = 9999) 
#mean(m.dbu8) 
#sd(m.dbu8) 

#mapas de calor de b-diversidad de los bichos ----
#beetles plot 2 

#f.t.2 <- subset(f.t, Plot == "2")
#completo <- merge(bbet2, sitios, by= "Subplot", all= T)
#completo$Plot[is.na(tf2$Plot)] <- 2
completo$num.visitors[is.na(completo$num.visitors)] <- 0
#tf2[is.na(tf2)] <- 0
geo2 <- as.geodata(bet2t[1:36,], coords.col = 2:3, data.col = 4:8)
BETvis2 <- likfit(geo2, ini = c(1,0.5), fix.nugget = T)# no works -----
#BETpred.grid2 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_BET_2<- krige.conv(BETgeo_data2, loc = BETpred.grid2, krige = krige.control(obj.m = BETvis2))
#image(kc_BET_2, loc = BETpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 2")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_BET_2$predict),max(kc_BET_2$predict)))

#beetles plot 3
geo3 <- as.geodata(bet3t[1:36,], coords.col = 2:3, data.col = 4:8)
BETvis3 <- likfit(geo3, ini = c(1,0.5), fix.nugget = T)# no works -----
#BETpred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_BET_3<- krige.conv(BETgeo_data3, loc = BETpred.grid3, krige = krige.control(obj.m = BETvis3))
#image(kc_BET_3, loc = BETpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 2")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_BET_3$predict),max(kc_BET_3$predict)))

#flies plot 1 
geo4 <- as.geodata(bf1t[1:36,], coords.col = 2:3, data.col = 4:12)
Flvis4 <- likfit(geo4, ini = c(1,0.5), fix.nugget = T)# no works -----
#BETpred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_BET_3<- krige.conv(BETgeo_data3, loc = BETpred.grid3, krige = krige.control(obj.m = BETvis3))
#image(kc_BET_3, loc = BETpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 2")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_BET_3$predict),max(kc_BET_3$predict)))


#####
####bioenv : quiero comparar las matrices de distancias de visitantes florales y las de B-diversidad de los mismos visitantes
# florales con los vectores de las abundancias de las spp de plantas. 
#bioenv con B-diversidad----
#beetles en el plot 1 
t.1 <- subset(t, Plot == "1")
r <- bioenv(m.bbe1 ~ t.1$CHFU +t.1$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(r)
#beetles en el Plot 2 
t.2<- subset(t, Plot== "2")  
r2 <- bioenv(m.bbe2 ~ t.2$CHFU +t.2$LEMA+t.2$PUPA +t.2$ME, method= "spearman" )
summary(r2)
#beetles plot 3
t.3<- subset(t, Plot== "3")  
r3 <- bioenv(m.bbe3 ~ t.3$CHFU +t.3$LEMA+t.3$PUPA +t.3$ME, method= "spearman" )
summary(r3)
#beetles plot 4
t.4<- subset(t, Plot== "4")  
r4 <- bioenv(m.bbe4 ~ t.4$CHFU +t.4$LEMA+t.4$PUPA +t.4$ME, method= "spearman" )
summary(r4)
#beetles plot 5
t.5<- subset(t, Plot== "5")  
r5 <- bioenv(m.bbe5 ~ t.5$CHFU +t.5$LEMA+t.5$PUPA +t.5$ME, method= "spearman" )
summary(r5)
#beetles plot 6
t.6<- subset(t, Plot== "6")  
r6 <- bioenv(m.bbe6 ~ t.6$CHFU +t.6$LEMA+t.6$PUPA +t.6$ME, method= "spearman" )
summary(r6)
#beetles plot 7
t.7<- subset(t, Plot== "7")  
r7 <- bioenv(m.bbe7 ~ t.7$CHFU +t.7$LEMA+t.7$PUPA +t.7$ME, method= "spearman" )
summary(r7)
#beetles plot 8
t.8<- subset(t, Plot== "8")  
r8 <- bioenv(m.bbe8 ~ t.8$CHFU +t.8$LEMA+t.8$PUPA +t.8$ME, method= "spearman" )
summary(r8)
#beetles plot 9
t.9<- subset(t, Plot== "9")  
r9 <- bioenv(m.bbe9 ~ t.9$CHFU +t.9$LEMA+t.9$PUPA +t.9$ME, method= "spearman" )
summary(r9)
#flies ----
#flies y plot 1
f1 <- bioenv(m.bf1 ~ t.1$CHFU +t.1$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(f1)
#flies y plot 2
f2 <- bioenv(m.bf2 ~ t.2$CHFU +t.2$LEMA+t.2$PUPA +t.2$ME, method= "spearman" )
summary(f2)##alta correlacion 
#flies y plot 3
f3 <- bioenv(m.bf3 ~ t.3$CHFU +t.3$LEMA+t.3$PUPA +t.3$ME, method= "spearman" )
summary(f3)
#flies y plot 4 
f4 <- bioenv(m.bf4 ~ t.4$CHFU +t.4$LEMA+t.4$PUPA +t.4$ME, method= "spearman" )
summary(f4) #casi sig 
#flies y plot 5
f5 <- bioenv(m.bf5 ~ t.5$CHFU +t.5$LEMA+t.5$PUPA +t.5$ME, method= "spearman" )
summary(f5)
#flies y plot 6
f6 <- bioenv(m.bf6 ~ t.6$CHFU +t.6$LEMA+t.6$PUPA +t.6$ME, method= "spearman" )
summary(f6)
#flies y plot 7
f7 <- bioenv(m.bf7 ~ t.7$CHFU +t.7$LEMA+t.7$PUPA +t.7$ME, method= "spearman" )
summary(f7) #alta correlacion
#flies y plot 8
f8 <- bioenv(m.bf8 ~ t.8$CHFU +t.8$LEMA+t.8$PUPA +t.8$ME, method= "spearman" )
summary(f8) #alta correlacion, mas de 0.16 ---
#flies y plot 9
f9 <- bioenv(m.bf9 ~ t.9$CHFU +t.9$LEMA+t.9$PUPA +t.9$ME, method= "spearman" )
summary(f9) #alta correlacion
#bees y plot 1
bees1 <- bioenv(m.dbees1 ~ t.1$CHFU +t.1$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(bees1) 
#bees y plot 2
bees1 <- bioenv(m.dbees2 ~ t.2$CHFU +t.2$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(bees1) 
#bees y plot 3
bees3 <- bioenv(m.dbees3 ~ t.3$CHFU +t.3$LEMA+t.3$PUPA +t.3$ME, method= "spearman" )
summary(bees3) #0.23 correlacion
#bees y plot 4
bees4 <- bioenv(m.dbees4 ~ t.4$CHFU +t.4$LEMA+t.4$PUPA +t.4$ME, method= "spearman" )
summary(bees4) #0.4 correlacion
#bees y plot 5
#bees5 <- bioenv(m.dbees5 ~ t.5$CHFU +t.5$LEMA+t.5$PUPA +t.5$ME, method= "spearman" )#matrid de b-diversidad con NA, no se puede
#summary(bees5) 
#bees y plot 6
bees6 <- bioenv(m.dbees6 ~ t.6$CHFU +t.6$LEMA+t.6$PUPA +t.6$ME, method= "spearman" )
summary(bees6) 
#bees y plot 7
bees7 <- bioenv(m.dbees7 ~ t.7$CHFU +t.7$LEMA+t.7$PUPA +t.7$ME, method= "spearman" )
summary(bees7) #altas correlaciones
#bees y plot 8
bees8 <- bioenv(m.dbees8 ~ t.8$CHFU +t.8$LEMA+t.8$PUPA +t.8$ME, method= "spearman" )
summary(bees8)
#bees y plot 9
#bees9 <- bioenv(m.dbees9 ~ t.9$CHFU +t.9$LEMA+t.9$PUPA +t.9$ME, method= "spearman" ) no se puede
#summary(bees9)
#butterflies y plot 7
#buter7 <- bioenv(m.dbu7 ~ t.7$CHFU +t.7$LEMA+t.7$PUPA +t.7$ME, method= "spearman" ) no se puede
#summary(buter7) 
#butterflies y plot 8
buter8 <- bioenv(m.dbu8 ~ t.8$CHFU +t.8$LEMA+t.8$PUPA +t.8$ME, method= "spearman" )
summary(buter8)

####        ###             ###
#bioenv con abundancias de visitors ----
#analizar las abundancias de especies de visitantes por plot, frente los vectores de abundancias de las plantas
#para ello se puede hacer un glmm normal
m <-glmer(dist.1~t.1$CHFU+t.1$LEMA+t.1$PUPA +t.1$ME, family="poisson") #no funciona ----
                                                                   #me dice que difieren de longitudes
#como no me funciona el glmm probaré con bioenv
#beetles plot 1
A.bet1<- bioenv(dist.1 ~ t.1$CHFU +t.1$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(A.bet1)
#beetles plot 2
A.bet2<- bioenv(dist.2 ~ t.2$CHFU +t.2$LEMA+t.2$PUPA +t.2$ME, method= "spearman" )
summary(A.bet2)
#beetles plot 3
A.bet3<- bioenv(dist.3 ~ t.3$CHFU +t.3$LEMA+t.3$PUPA +t.3$ME, method= "spearman" )
summary(A.bet3)
#beetles plot 4
A.bet4<- bioenv(dist.4 ~ t.4$CHFU +t.4$LEMA+t.4$PUPA +t.4$ME, method= "spearman" )
summary(A.bet4) #muuucha correlacion ~0.4
#beetles plot 5
A.bet5<- bioenv(dist.5 ~ t.5$CHFU +t.5$LEMA+t.5$PUPA +t.5$ME, method= "spearman" )
summary(A.bet5) #mucha correlacion ~0.36

