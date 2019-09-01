library(vegan)
library(tidyverse)
library(ade4)
library(geoR)
library(gstat)
library(aqfig)
library(lattice)
#load data
visitor.abun <- read.table("data/FV_16_19.csv", header=T, sep= ";")#aunque ponga visitor abundance, son los datos de visitas
sitios <- read.table("data/caracolesplotposition.csv", header=T, sep= ";")#hay un error en el csv pero que se corrige en el siguiente paso
sitios <-sitios[which(complete.cases(sitios)),]

#preparacion del data
head(visitor.abun)
v.abun.19 <- subset(visitor.abun, Year== "2019")

#escarabajos ----
#quiero sacar la B-diversidad de los escarabajos por plots, y luego hacer el mantel test. 
pol.9 <- v.abun.19 %>% group_by(Plot, Subplot, Group) %>% summarise (num.visitors = sum(Visits))
pol.9 <-pol.9[which(complete.cases(pol.9)),]
pol.9 <- subset(pol.9, Plot != "OUT")
pol.9 <- subset(pol.9, Subplot != "OUT")
pol.beetle9 <- subset(pol.9, Group == "Beetle")
pol.beetle9$Plot <- as.numeric(as.character(pol.beetle9$Plot))
pol.beetle.B <- pol.beetle9[,c("Plot", "Subplot", "num.visitors")] #total
BEETLES <- tidyr::spread(pol.beetle.B,key = Plot, value = num.visitors)
BEETLES[is.na(BEETLES)] <- 0
#beetles.matrix <- as.matix(BEETLES)

d.beetles.0 <- dist(BEETLES [,c(2:10)], method = "euclidean", diag = T, upper = T)

nombres1 <- list(BEETLES$Subplot, names(BEETLES[,1:length(BEETLES)]))
BEETLE.matrix <- as.matrix(BEETLES[,2:length(BEETLES)], dimnames = nombres1)
vegdist(BEETLE.matrix, method="morisita", binary=FALSE, diag= F, upper=T,
        na.rm = FALSE) #esta es la B-diversidad de cada plot a su vez dividida en subplots de BEETLES (total)

h <- dist(sitios[,c(2,3)], method= "euclidean", diag=T, upper=T)#matriz de las distancias entre los subplots de un plot

#plot1 -> saco la b-diversidad de los escarabajos en plot 1

beetle.plot1 <- BEETLES [,c( "Subplot","1")]
m.beetle.plot1 <- as.matrix(beetle.plot1)
dist.1 <-dist(beetle.plot1 [,c(2)], method= "euclidean", diag =T, upper =T)


nombres.beetle.plot1 <- list(beetle.plot1$Subplot, names(beetle.plot1[,2:length(beetle.plot1)]))
BEETLE1.matrix <- as.matrix(beetle.plot1[,1:length(beetle.plot1)], dimnames = nombres.beetle.plot1)

vegdist(dist.1, method="morisita", binary=FALSE, diag= T, upper=T,
        na.rm = FALSE) 

str(beetle.plot1)
mantel.rtest (dist.1, h, nrepet = 9999, graph = TRUE)
sitios$Subplot <- sitios$position
sitios$Plot <- sitios$plot
all <- dplyr::left_join(pol.beetle.B, sitios) #DUDA, no me sale la matriz. quiero tener todo en el mismo data para graficarlo



#hacer ggplot para graficarlo (algo así)
# ggplot(all, aes(x_coor, y_coor )) +
#geom_tile(aes(fill = num.visitors), color = "white") +
 #   scale_fill_gradient(low = "white", high = "steelblue") +
  #  ylab("y") +
   # xlab("x")

#plot 2 Beetles
Beetle2 <- BEETLES[,c("Subplot", "2")]
d.beetles.2 <- dist( Beetle2[,c(2)], method = "euclidean", diag = T, upper = T)
mantel.rtest(d.beetles.2, h,nrepet = 9999) 

########## script oscar para mapa de calor ----


#escarabajos ----
#escarabajos total plots
pol.beetle9$Plot <- as.numeric(as.character(pol.beetle9$Plot))
pol.beetle.B <- pol.beetle9[,c("Plot", "Subplot", "num.visitors")]
BEETLES <- tidyr::spread(pol.beetle.B,key = Plot, value = num.visitors)
BEETLES[is.na(BEETLES)] <- 0

#Analysis
BEETLES$'1'<-as.numeric(BEETLES$'1')
BEETLES$'2'<-as.numeric(BEETLES$'2')
BEETLES$'3'<-as.numeric(BEETLES$'3')
BEETLES$'4'<-as.numeric(BEETLES$'4')
BEETLES$'5'<-as.numeric(BEETLES$'5')
sitios$plot <- as.numeric(sitios$plot)
sitios$x_coor <- as.numeric(sitios$x_coor)
sitios$y_coor <- as.numeric(sitios$y_coor)
sitios$position <- as.character(sitios$position)
sitios$cell <- as.numeric(sitios$cell)
sitios$Subplot <- sitios$position
completa.beetles <- dplyr::left_join(BEETLES, sitios) #todo el rato me aparece NA¿¿??, esto es para tener las coordenadas dentro de mismo data que el num de visitors

geo_data_BEETLES.1 <-BEETLES[,c(1,3)] #aqui selecciono solo un plot (pero tendre que usar completa.beetles)
row.names(geo_data_BEETLES.1) <- geo_data_BEETLES.1[,1] #no me deja ponerle los nombres de los subplots (usar completa.beetles)
geo_data1 <-geo_data_BEETLES.1[,-1] #(usar completa.beetles)


#resto del script 
ph_113 <-as.geodata(geo_data[1:36,], coords.col = 1:2, data.col = 3)

#estimación de parametros
ml_ph_113 <- likfit(ph_113, ini = c(1,0.5), fix.nugget = T)

# definir la malla de análisis
pred.grid <-  expand.grid(seq(0,8.5, l=40), seq(0,8.5, l=40))

# cálculos de interpolación.
kc_ph_113 <- krige.conv(ph_113, loc = pred.grid, krige = krige.control(obj.m = ml_ph_113))

# representacion de los datos de pH

image(kc_ph_113, loc = pred.grid, col=rainbow(15), xlab=NA, ylab="Coordenadas Y (m)", main="pH P113")
vertical.image.legend(col=rainbow(15),zlim=c(min(kc_ph_113$predict),max(kc_ph_113$predict)))



