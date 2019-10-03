#en este script se va a testar si la composicion de visitors depende de las abundancias de las especies de plantas

#libraries
library(tidyverse)
library(vegan) #para bioenv
library(reshape2)
library(ade4)

#datos

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

plantas <- read.table("data/Abun_19.csv", header=T, sep= ";")
p <- va19 %>% group_by(Plot, Subplot, Group,Family, Species) %>% summarise (num.visitors = sum(Visits))
sitios1 <- sitios [,c("Subplot", "x_coor", "y_coor")]
#analisis
#primero voy a obtener las matrices de b-diversidad
p$Family <- as.character(p$Family)
p$Species <- as.character(p$Species)
p <- subset(p, Plot != "OUT")
# beetles - b-diversity 
#plot 1 + beetles
p.1 <- subset (p, Plot == "1") 
bbet1 <- subset (p.1, Group == "Beetle")
columnsbe1 <- dcast(bbet1, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet1t <- dplyr::left_join (sitios1,columnsbe1)
bet1t[is.na(bet1t)] <- 0
d.bbe1 <-dist(bet1t [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.bbe1 <- vegdist(d.bbe1, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k1 <-dist(bet1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe1, k1,nrepet = 9999) #corregido1
mean(m.bbe1) 
sd(m.bbe1) 

#mapas de calor pero que con las matrices de b-diversidad no funcionan
#beetgeo_data1 <- as.geodata(bet1t[1:36,], coords.col = 2:3, data.col = 4:6)
#beetvis1 <- likfit(beetgeo_data1, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema, dice que las localizaciones no se ajustan a los datos
#beetpred.grid1 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
#kc_beet_1<- krige.conv(beetgeo_data2, loc = beetpred.grid1, krige = krige.control(obj.m = beetvis1)) #no works mapa de calor ----
#image(kc_beet_1, loc = beetpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_1$predict),max(kc_f_1$predict)))

#plot 2+beetle
p.2 <- subset (p, Plot == "2") 
bbet2 <- subset (p.2, Group == "Beetle") 
columnsbet2 <- dcast(bbet2, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet2t <- dplyr::left_join (sitios1,columnsbet2)
bet2t[is.na(bet2t)] <- 0
d.bbe2 <-dist(bet2t [,c(4:8)], method= "euclidean", diag =T, upper =T)
m.bbe2 <- vegdist(d.bbe2, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k2 <-dist(bet2t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe2, k2,nrepet = 9999) 
mean(m.bbe2) 
sd(m.bbe2) 

#plot 3+beetle
p.3 <- subset (p, Plot == "3") 
bbet3 <- subset (p.3, Group == "Beetle")
sitios.bbet3 <- dplyr::left_join(sitios1, bbet3)
sitios.bbet3$num.visitors[is.na(sitios.bbet3$num.visitors)] <- 0
columnsbet3 <- dcast(bbet3, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet3t <- dplyr::left_join (sitios1,columnsbet3)
bet3t[is.na(bet3t)] <- 0
d.bbe3 <-dist(bet3t [,c(4:8)], method= "euclidean", diag =T, upper =T)
m.bbe3 <- vegdist(d.bbe3, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k3 <-dist(bet3t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe3, k3,nrepet = 9999) 
mean(m.bbe3) 
sd(m.bbe3) 
#mapa de calor, no funcionan con matrices b-diversidad
#B_beetgeo_data3 <- as.geodata(sitios.bbet3[1:44,], coords.col = 2:3, data.col = 9)
#b_beetvis3 <- likfit(B_beetgeo_data3, ini = c(1,0.5), fix.nugget = T)#aqui hay un problema----
##b_beetpred.grid3 <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))
##kc_beet_2<- krige.conv(beetgeo_data2, loc = beetpred.grid2, krige = krige.control(obj.m = beetvis2)) #no works mapa de calor ----
##image(kc_beet_2, loc = beetpred.grid2, col=rainbow(15), xlab=" Coordenadas X (m)", ylab="Coordenadas Y (m)", main="Flies abundance distribution in plot 7")
#vertical.image.legend(col=rainbow(15),zlim=c(min(kc_f_7$predict),max(kc_f_7$predict)))

#plot 4+beetle
p.4 <- subset (p, Plot == "4") 
bbe4 <- subset (p.4, Group == "Beetle")
columnsbet4 <- dcast(bbe4, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet4t <- dplyr::left_join (sitios1,columnsbet4)
bet4t[is.na(bet4t)] <- 0
d.bbe4 <-dist(bet4t [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.bbe4 <- vegdist(d.bbe4, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k4 <-dist(bet4t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe4, k4,nrepet = 9999) 
mean(m.bbe4) 
sd(m.bbe4) 

#plot 5+beetle
p.5 <- subset (p, Plot == "5") 
bbe5 <- subset (p.5, Group == "Beetle")
columnsbet5 <- dcast(bbe5, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet5t <- dplyr::left_join (sitios1,columnsbet5)
bet5t[is.na(bet5t)] <- 0
d.bbe5 <-dist(bet5t [,c(4:7)], method= "euclidean", diag =T, upper =T)
m.bbe5 <- vegdist(d.bbe5, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k5 <-dist(bet5t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe5, k5,nrepet = 9999) 
mean(m.bbe5) 
sd(m.bbe5) 

#plot 6+beetle
p.6 <- subset (p, Plot == "6")
p.6$Plot <- as.numeric(p.6$Plot)
bbe6 <- subset (p.6, Group == "Beetle")
columnsbet6 <- dcast(bbe6, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet6t <- dplyr::left_join (sitios1,columnsbet6)
bet6t[is.na(bet6t)] <- 0
d.bbe6 <-dist(bet6t [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.bbe6 <- vegdist(d.bbe6, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k6 <-dist(bet6t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe6, k6,nrepet = 9999) 
mean(m.bbe6) 
sd(m.bbe6) 

#plot 7+beetle
p.7 <- subset (p, Plot == "7")
p.7$Plot <- as.numeric(p.7$Plot)
bbe7 <- subset (p.7, Group == "Beetle")
columnsbet7 <- dcast(bbe7, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet7t <- dplyr::left_join (sitios1,columnsbet7)
bet7t[is.na(bet7t)] <- 0
d.bbe7 <-dist(bet7t [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.bbe7 <- vegdist(d.bbe7, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k7 <-dist(bet7t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe7, k7,nrepet = 9999) 
mean(m.bbe7) 
sd(m.bbe7) 

#plot8
p.8 <- subset (p, Plot == "8")
p.8$Plot <- as.numeric(p.8$Plot)
bbe8 <- subset (p.8, Group == "Beetle")
columnsbet8 <- dcast(bbe8, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet8t <- dplyr::left_join (sitios1,columnsbet8)
bet8t[is.na(bet8t)] <- 0
d.bbe8 <-dist(bet8t [,c(4:7)], method= "euclidean", diag =T, upper =T)

m.bbe8 <- vegdist(d.bbe8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k8 <-dist(bet8t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe8, k8,nrepet = 9999) 
mean(m.bbe8) 
sd(m.bbe8) 

#plot 9+beetle
p.9 <- subset (p, Plot == "9")
p.9$Plot <- as.numeric(p.9$Plot)
bbe9 <- subset (p.9, Group == "Beetle")
columnsbet9 <- dcast(bbe9, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bet9t <- dplyr::left_join (sitios1,columnsbet9)
bet9t[is.na(bet9t)] <- 0
d.bbe9 <-dist(bet9t [,c(4:8)], method= "euclidean", diag =T, upper =T)
m.bbe9 <- vegdist(d.bbe9, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
k9 <-dist(bet9t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bbe9, k9,nrepet = 9999) 
mean(m.bbe9) 
sd(m.bbe9) 

#flies ----
#plot 1+flies
bf1 <- subset (p.1, Group == "Fly")
columnsbf1 <- dcast(bf1, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bf1t <- dplyr::left_join (sitios1,columnsbf1)
bf1t[is.na(bf1t)] <- 0
d.df1 <-dist(bf1t [,c(4:12)], method= "euclidean", diag =T, upper =T)
m.bf1 <- vegdist(d.df1, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m1 <-dist(bf1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf1, m1,nrepet = 9999) 
mean(m.bf1) 
sd(m.bf1) 

#plot 2+flies
bf2 <- subset (p.2, Group == "Fly")
bf2t <- dplyr::left_join (sitios1,bf2)
columnsfl2 <- dcast(bf2t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl2t <- dplyr::left_join (sitios1,columnsfl2)
fl2t[is.na(fl2t)] <- 0
d.df2 <-dist(fl2t [,c(4:8)], method= "euclidean", diag =T, upper =T)
m.bf2 <- vegdist(d.df2, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m2 <-dist(fl2t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf2, m2,nrepet = 9999)
mean(m.bf2) 
sd(m.bf2) 

#plot 3+flies
bf3 <- subset (p.3, Group == "Fly")
bf3t <- dplyr::left_join (sitios1,bf3)
columnsfl3 <- dcast(bf3t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl3t <- dplyr::left_join (sitios1,columnsfl3)
fl3t[is.na(fl3t)] <- 0
d.df3 <-dist(fl3t [,c(4:8)], method= "euclidean", diag =T, upper =T)
m.bf3 <- vegdist(d.df3, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m3 <-dist(fl3t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf3, m3,nrepet = 9999)
mean(m.bf3) 
sd(m.bf3) 

#plot 4+flies
bf4 <- subset (p.4, Group == "Fly")
bf4t <- dplyr::left_join (sitios1,bf4)
columnsfl4 <- dcast(bf4t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl4t <- dplyr::left_join (sitios1,columnsfl4)
fl4t[is.na(fl4t)] <- 0
d.df4 <-dist(fl4t [,c(4:10)], method= "euclidean", diag =T, upper =T)
m.bf4 <- vegdist(d.df4, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m4 <-dist(fl4t [,c(2:3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf4, m4,nrepet = 9999) 
mean(m.bf4) 
sd(m.bf4) 

#plot 5+flies
bf5 <- subset (p.5, Group == "Fly")
bf5t <- dplyr::left_join (sitios1,bf5)
columnsfl5 <- dcast(bf5t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl5t <- dplyr::left_join (sitios1,columnsfl5)
fl5t[is.na(fl5t)] <- 0
d.df5 <-dist(fl5t [,c(4:11)], method= "euclidean", diag =T, upper =T)
m.bf5 <- vegdist(d.df5, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m5 <-dist(fl5t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf5, m5,nrepet = 9999) 
mean(m.bf5) 
sd(m.bf5) 

#plot 6+flies
bf6 <- subset (p.6, Group == "Fly")
bf6t <- dplyr::left_join (sitios1,bf6)
columnsfl6 <- dcast(bf6t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl6t <- dplyr::left_join (sitios1,columnsfl6)
fl6t[is.na(fl6t)] <- 0
d.df6 <-dist(fl6t [,c(4:12)], method= "euclidean", diag =T, upper =T)
m.bf6 <- vegdist(d.df6, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m6 <-dist(fl6t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf6, m6,nrepet = 9999) 
mean(m.bf6)
sd(m.bf6) 

#plot 7+flies
bf7 <- subset (p.7, Group == "Fly")
bf7t <- dplyr::left_join (sitios1,bf7)
columnsfl7 <- dcast(bf7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl7t <- dplyr::left_join (sitios1,columnsfl7)
fl7t[is.na(fl7t)] <- 0
d.df7 <-dist(fl7t [,c(4:10)], method= "euclidean", diag =T, upper =T)
m.bf7 <- vegdist(d.df7, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m7 <-dist(fl7t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf7, m7,nrepet = 9999) 
mean(m.bf7)
sd(m.bf7) 

#plot 8+flies
bf8 <- subset (p.8, Group == "Fly")
bf8t <- dplyr::left_join (sitios1,bf8)
columnsfl8 <- dcast(bf8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl8t <- dplyr::left_join (sitios1,columnsfl8)
fl8t[is.na(fl8t)] <- 0
d.df8 <-dist(fl8t [,c(4:14)], method= "euclidean", diag =T, upper =T)
m.bf8 <- vegdist(d.df8, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m8 <-dist(fl8t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf8, m8,nrepet = 9999) 
mean(m.bf8) 
sd(m.bf8) 

#plot 9+flies
bf9 <- subset (p.9, Group == "Fly") 
bf9t <- dplyr::left_join (sitios1,bf9)
columnsfl9 <- dcast(bf9t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
fl9t <- dplyr::left_join (sitios1,columnsfl9)
fl9t[is.na(fl9t)] <- 0
d.df9 <-dist(fl9t [,c(4:11)], method= "euclidean", diag =T, upper =T)
m.bf9 <- vegdist(d.df9, method="morisita", binary=FALSE, diag= T, upper=T,
                 na.rm = FALSE) 
m9 <-dist(fl9t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.bf9, m9,nrepet = 9999) 
mean(m.bf9) 
sd(m.bf9) 


###
#bees ----
#plot 1+bees
bbees1 <- subset (p.1, Group == "Bee")
bbees1t <- dplyr::left_join (sitios1,bbees1)
columnsbees1t <- dcast(bbees1t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbees1t <- dplyr::left_join (sitios1,columnsbees1t)
bbees1t[is.na(bbees1t)] <- 0
d.dbees1 <-dist(bbees1t [,c(4)], method= "euclidean", diag =T, upper =T)
m.dbees1 <- vegdist(d.dbees1, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee1 <-dist(bbees1t [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees1, dis.bee1,nrepet = 9999) #corregido
mean(m.dbees1) 
sd(m.dbees1) 

#plot 2+bees
bbees2 <- subset (p.2, Group == "Bee")
bbees2t <- dplyr::left_join (sitios1,bbees2)
columnsbees2t <- dcast(bbees2t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbees2 <- dplyr::left_join (sitios1,columnsbees1t)
bbees2[is.na(bbees2)] <- 0
d.dbees2 <-dist(bbees2 [,c(4)], method= "euclidean", diag =T, upper =T)
m.dbees2 <- vegdist(d.dbees2, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee2 <-dist(bbees2 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees2, dis.bee2,nrepet = 9999) 
mean(m.dbees2) 
sd(m.dbees2) 

#plot 3+bees
bbees3 <- subset (p.3, Group == "Bee")
bbees3t <- dplyr::left_join (sitios1,bbees3)
columnsbees3t <- dcast(bbees3t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess3 <- dplyr::left_join (sitios1,columnsbees3t)
bbeeess3[is.na(bbeeess3)] <- 0
d.dbees3 <-dist(bbeeess3 [,c(4)], method= "euclidean", diag =T, upper =T)
m.dbees3 <- vegdist(d.dbees3, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE)
dis.bee3 <-dist(bbeeess3 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees3, dis.bee3,nrepet = 9999) 
mean(m.dbees3) 
sd(m.dbees3)

#plot 4+bees
bbees4 <- subset (p.4, Group == "Bee")
bbees4t <- dplyr::left_join (sitios1,bbees4)
columnsbees4t <- dcast(bbees4t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess4 <- dplyr::left_join (sitios1,columnsbees4t)
bbeeess4[is.na(bbeeess4)] <- 0
d.dbees4 <-dist(bbeeess4 [,c(4)], method= "euclidean", diag =T, upper =T)
m.dbees4 <- vegdist(d.dbees4, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee4 <-dist(bbeeess4 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees4, dis.bee4,nrepet = 9999) 
mean(m.dbees4) 
sd(m.dbees4)

#plot 5+bees
bbees5 <- subset (p.5, Group == "Bee") #solo hay un dato
bbees5t <- dplyr::left_join (sitios1,bbees5)
columnsbees5t <- dcast(bbees5t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess5 <- dplyr::left_join (sitios1,columnsbees5t)
bbeeess5[is.na(bbeeess5)] <- 0
d.dbees5 <-dist(bbeeess5 [,c(4)], method= "euclidean", diag =T, upper =T)
m.dbees5 <- vegdist(d.dbees5, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee5 <-dist(bbeeess5 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees5, dis.bee5,nrepet = 9999) 
mean(m.dbees5) 
sd(m.dbees5) 

#plot 6+bees
bbees6 <- subset (p.6, Group == "Bee") 
bbees6t <- dplyr::left_join (sitios1,bbees6)
columnsbees6t <- dcast(bbees6t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess6 <- dplyr::left_join (sitios1,columnsbees6t)
bbeeess6[is.na(bbeeess6)] <- 0
d.dbees6 <-dist(bbeeess6 [,c(4:5)], method= "euclidean", diag =T, upper =T)
m.dbees6 <- vegdist(d.dbees6, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee6 <-dist(bbeeess6 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees6, dis.bee6,nrepet = 9999) 
mean(m.dbees6) 
sd(m.dbees6) 

#plot 7+bees
bbees7 <- subset (p.7, Group == "Bee") 
bbees7t <- dplyr::left_join (sitios1,bbees7)
columnsbees7t <- dcast(bbees7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess7 <- dplyr::left_join (sitios1,columnsbees7t)
bbeeess7[is.na(bbeeess7)] <- 0
d.dbees7 <-dist(bbeeess7 [,c(4:7)], method= "euclidean", diag =T, upper =T)
m.dbees7 <- vegdist(d.dbees7, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee7 <-dist(bbeeess7 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees7, dis.bee7,nrepet = 9999) 
mean(m.dbees7) 
sd(m.dbees7) 

#plot 8+bees
bbees8 <- subset (p.8, Group == "Bee") 
bbees8t <- dplyr::left_join (sitios1,bbees8)
columnsbees8t <- dcast(bbees8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess8 <- dplyr::left_join (sitios1,columnsbees8t)
bbeeess8[is.na(bbeeess8)] <- 0
d.dbees8 <-dist(bbeeess8 [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.dbees8 <- vegdist(d.dbees8, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee8 <-dist(bbeeess8 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees8, dis.bee8,nrepet = 9999) 
mean(m.dbees8) 
sd(m.dbees8) 

#plot 9+bees
bbees9 <- subset (p.9, Group == "Bee") #solo dos datos
bbees9t <- dplyr::left_join (sitios1,bbees9)
columnsbees9t <- dcast(bbees9t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbeeess9 <- dplyr::left_join (sitios1,columnsbees9t)
bbeeess9[is.na(bbeeess9)] <- 0
d.dbees9 <-dist(bbeeess9 [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.dbees9 <- vegdist(d.dbees9, method="morisita", binary=FALSE, diag= T, upper=T,
                    na.rm = FALSE) 
dis.bee9 <-dist(bbeeess9 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbees9, dis.bee9,nrepet = 9999) 
mean(m.dbees9) 
sd(m.dbees9) 

####
#butterflies----
#plot 7 + butterflies, solo mariposas en plot 7 y 8
bbu7 <- subset (p.7, Group == "Butterfly")
bbu7t <- dplyr::left_join (sitios1,bbu7)
columnsbutter7t <- dcast(bbu7t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbutt7 <- dplyr::left_join (sitios1,columnsbutter7t)
bbutt7[is.na(bbutt7)] <- 0
d.dbu7 <-dist(bbutt7 [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.dbu7 <- vegdist(d.dbu7, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
d.dbu7 <-dist(bbutt7 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbu7, d.dbu7,nrepet = 9999) 
mean(m.dbu7) 
sd(m.dbu7) 

#plot 8 + butterflies, solo mariposas en plot 7 y 8
bbu8 <- subset (p.8, Group == "Butterfly")
bbu8t <- dplyr::left_join (sitios1,bbu8)
columnsbutter8t <- dcast(bbu8t, Subplot ~ Species, fun.aggregate = sum, value.var = "num.visitors")
bbutt8 <- dplyr::left_join (sitios1,columnsbutter8t)
bbutt8[is.na(bbutt8)] <- 0
d.dbu8 <-dist(bbutt8 [,c(4:6)], method= "euclidean", diag =T, upper =T)
m.dbu8 <- vegdist(d.dbu8, method="morisita", binary=FALSE, diag= T, upper=T,
                  na.rm = FALSE) 
d.dbu8 <-dist(bbutt8 [,c(2,3)], method= "euclidean", diag =T, upper =T)
mantel.rtest (m.dbu8, d.dbu8,nrepet = 9999) 
mean(m.dbu8) 
sd(m.dbu8) 
###
#en esta parte voy a usar las matrices de b-diversidad para obtener las correlaciones con las abundancias
#    de plantas 
simple.plantas <- subset(plantas, Sp.Focal %in% c("LEMA","CHFU","PUPA","ME"))
s.plantas <- simple.plantas[,c("plot","subplot","Sp.Focal", "Plantas")]
t.plantas <- s.plantas %>% group_by(plot, subplot,Sp.Focal) %>% summarise ( num.plantas = sum(Plantas))
tabla.plantas <- tidyr::spread(t.plantas,key = Sp.Focal, value = num.plantas)
t <- tabla.plantas

#beetles en el plot 1 
t.1 <- subset(t, plot == "1")
r <- bioenv(m.bbe1 ~ t.1$CHFU +t.1$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(r)
#beetles en el Plot 2 
t.2<- subset(t, plot== "2")  
r2 <- bioenv(m.bbe2 ~ t.2$CHFU +t.2$LEMA+t.2$PUPA +t.2$ME, method= "spearman" )
summary(r2)
#beetles plot 3
t.3<- subset(t, plot== "3")  
r3 <- bioenv(m.bbe3 ~ t.3$CHFU +t.3$LEMA+t.3$PUPA +t.3$ME, method= "spearman" )
summary(r3)
#beetles plot 4
t.4<- subset(t, plot== "4")  
r4 <- bioenv(m.bbe4 ~ t.4$CHFU +t.4$LEMA+t.4$PUPA +t.4$ME, method= "spearman" )
summary(r4)
#beetles plot 5
t.5<- subset(t, plot== "5")  
r5 <- bioenv(m.bbe5 ~ t.5$CHFU +t.5$LEMA+t.5$PUPA +t.5$ME, method= "spearman" )
summary(r5)
#beetles plot 6
t.6<- subset(t, plot== "6")  
r6 <- bioenv(m.bbe6 ~ t.6$CHFU +t.6$LEMA+t.6$PUPA +t.6$ME, method= "spearman" )
summary(r6)
#beetles plot 7
t.7<- subset(t, plot== "7")  
r7 <- bioenv(m.bbe7 ~ t.7$CHFU +t.7$LEMA+t.7$PUPA +t.7$ME, method= "spearman" )
summary(r7)
#beetles plot 8
t.8<- subset(t, plot== "8")  
r8 <- bioenv(m.bbe8 ~ t.8$CHFU +t.8$LEMA+t.8$PUPA +t.8$ME, method= "spearman" )
summary(r8)
#beetles plot 9
t.9<- subset(t, plot== "9")  
r9 <- bioenv(m.bbe9 ~ t.9$CHFU +t.9$LEMA+t.9$PUPA +t.9$ME, method= "spearman" )
summary(r9)

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
bees2 <- bioenv(m.dbees2 ~ t.2$CHFU +t.2$LEMA+t.1$PUPA +t.1$ME, method= "spearman" )
summary(bees2) 
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

citation(package= "DHARMa")
