library(tidyverse)
library(ape)
library(ncf)
#install.packages("spdep") -> for Moran's I
library(spdep)
library(reshape2)
library(vegan)

#I de Moran. Valores entre -1 y 1. Si es positivo esta positivamente correlacionado, y si es negativo no hay
#   correlacion. Lugares más proximos se parecen mas(+ correlacion) que a los lugares lejanos. 

#cargar datos, juntar bases de datos de polinizadores, abundancias plantas, fruits y seeds, y limpiar
FV <- read.table("data/Metadata_Pollinators_2019_2016_bueno.csv", header=T, sep=";")
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
competencia <- read.table("data/simplex_competencia_SEEDS_2019_new.csv", header=T, sep=";")
FV_19 <- subset(FV, Year == 2019) 
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
ab.2 <- subset(ab.19, Plant_Simple%in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
FV_19$Plot<- as.factor(FV_19$Plot)
ab.2$Plot<- as.factor(ab.2$Plot)
c <-FV_19%>% group_by(Plot, Subplot, Plant_Simple, Group,G_F, ID_Simple, Plant_Simple) %>% summarise (num.visits = sum(Visits))
c1 <- subset(c, Plant_Simple %in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
FINAL <- full_join(c1, ab.2) #polinizadores+abun plantas
FINAL$Plot <- as.numeric(FINAL$Plot)
competencia$Plot <- as.numeric(competencia$Plot)
competencia1 <- competencia[,c("Plot","Subplot","Plant_Simple","Fruit","Seed")]
plantfruit <- subset(competencia1, Plant_Simple%in% c("LEMA","CHFU","ME","PUPA", "CHMI"))
FINAL.seeds <- full_join(FINAL, plantfruit, by= c("Plot", "Subplot", "Plant_Simple"))
Total.f1 <- FINAL.seeds[,c("Plot","Subplot", "Group","G_F", "ID_Simple", "num.visits","Plant_Simple", "num.plantas","Fruit","Seed")]
Total.f1$Fruit <- as.numeric(Total.f1$Fruit)
Total.f1$Seed <- as.numeric(Total.f1$Seed)
Total.f1$Fruit[is.na(Total.f1$Fruit)] <- 0
Total.f1$Seed[is.na(Total.f1$Seed)] <- 0
a2 <- subset(Total.f1, Plot != "OUT")
a4 <- subset(a2, Subplot != "OUT")

# now I select the IDs that we want to exclude
clean <- subset(a4, G_F != "Ants")
clean1 <- subset(clean, ID_Simple != "Coccinella_septempunctata") 
clean2 <- subset(clean1, ID_Simple != "Larva")
clean3 <- subset(clean2, ID_Simple != "Chrysididae")
clean4 <- subset(clean3, ID_Simple != "Diplazon_sp.")
clean5 <- subset(clean4, G_F != "Mosquitoes")
casi <- clean5[,c("Plot","Subplot", "Group", "num.visits","Plant_Simple", "num.plantas","Fruit","Seed")]

#I change the 0 abundance of plants to 1 (at least we might have 1 plant)
casi$num.plantas[casi$num.plantas == "0"] <- "1"
casi$num.visits <-as.numeric(casi$num.visits)
casi$num.plantas <- as.numeric(casi$num.plantas)
casi$visitas_indv <- casi$num.visits/ casi$num.plantas
casi$visitas_indv_hora <- (casi$visitas_indv*60)/30 #de esta manera ya tengo las visitas por individuo y por hora
pol <- casi #aqui tengo ya polinizadores, semillas, individuos y frutos --> pol - base de datos final

#voy a ver si mis datos son normales -> no lo son. 
hist(pol$visitas_indv_hora)

#ahora construllo los datos de coordenadas across plots
distances <- read.csv("data/caracolesplotposition.csv", sep = ";") #aqui solamente esta la informacion de 1 
                                            # plot, se midieron las distancias entre los plots y se añadieron
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
head(dis)
disfinal <- dis[,c("plot", "position","x_coor2", "y_coor2")]
disfinal$Plot <- disfinal$plot
disfinal$Subplot <- disfinal$position
disfinal1 <- disfinal[,c("Plot", "Subplot","x_coor2", "y_coor2")]
distancias.matriz <-as.matrix(dist(cbind(disfinal1$x_coor2, disfinal1$y_coor2)))

    #aqui hago la matriz inversa y la diagonal con 0, para la I de Moran de luego
plots.dists.inv <- 1/distancias.matriz
diag(plots.dists.inv) <- 0
plots.dists.inv[1:5,1:5]

# preparacion de datos para analizar por los 8 vecinos más cercanos --> w5. Asi vemos si se parecen mas 
# entre los vecinos que al resto en la I de Moran
disfinal1$x_coor2 <- as.numeric(disfinal1$x_coor2)
disfinal1$y_coor2 <- as.numeric(disfinal1$y_coor2)
w5 <- knn2nb(knearneigh(coordinates(disfinal1[,3:4]), k=8))

#analisis por grupos

##### ##############################POLLINATORS#######################################
#beetles ----
pol.beetle <- subset(pol, Group == "Beetle")
pol.beetle$Plot <- as.numeric(as.character(pol.beetle$Plot))
pol.beetle.B <- pol.beetle[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas de BEETLES
b <-pol.beetle.B %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
beetle <- left_join(disfinal1, b, by= c("Plot", "Subplot"))
beetle$visits[is.na(beetle$visits)] <- 0

        #grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle$x_coor2, y=beetle$y_coor2,
                           z=beetle$visits, resamp=100, quiet=TRUE)
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")

    #test de moran. Aquí quiero obtener El estadistico de Moran y p.value 
moran.test(beetle$visits,mat2listw(plots.dists.inv)) # I= 0.129
moran.test(beetle$visits, nb2listw(w5)) # vecinos, I= 0.17
moran.plot(beetle$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")

#flies----
flies <- subset(pol, Group == "Fly")
flies$Plot <- as.numeric(as.character(flies$Plot))
flies.B <- flies[,c("Plot", "Subplot","visitas_indv_hora")] 
f <-flies.B %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
flies.t <- left_join(disfinal1,f, by= c("Plot", "Subplot"))
flies.t$visits[is.na(flies.t$visits)] <- 0

flies.corr <- spline.correlog(x=flies.t$x_coor2, y=flies.t$y_coor2,
                           z=flies.t$visits, resamp=100, quiet=TRUE)
plot(flies.corr, main= "Spatial autocorrelation flies across plots")

moran.test(flies.t$visits,mat2listw(plots.dists.inv)) # I= 0.11
moran.test(flies.t$visits, nb2listw(w5)) # vecinos, I= 0.178
moran.plot(beetle$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation flies across plots")

#butterflies----
but <- subset(pol, Group == "Butterfly")
but$Plot <- as.numeric(as.character(but$Plot))
but.B <- but[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
butt <-but.B %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
but.t <- left_join(disfinal1,butt , by= c("Plot", "Subplot"))
but.t$visits[is.na(but.t$visits)] <- 0

but.corr <- spline.correlog(x=but.t$x_coor2, y=but.t$y_coor2,
                                 z=but.t$visits, resamp=100, quiet=TRUE)
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")

moran.test(but.t$visits,mat2listw(plots.dists.inv)) # I= 0.067
moran.test(but.t$visits, nb2listw(w5)) # vecinos, I= 0.047
moran.plot(but.t$visits,mat2listw(plots.dists.inv),  main= "Spatial autocorrelation butterflies across plots")

#bees----
bee <- subset(pol, Group == "Bee")
bee$Plot <- as.numeric(as.character(bee$Plot))
bee.B <- bee[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
bee1 <-bee.B %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
bee.t <- left_join(disfinal1, bee1, by= c("Plot", "Subplot"))
bee.t$visits[is.na(bee.t$visits)] <- 0

bee.corr <- spline.correlog(x=bee.t$x_coor2, y=bee.t$y_coor2,
                               z=bee.t$visits, resamp=100, quiet=TRUE)
plot(bee.corr, main= "Spatial autocorrelation bees across plots")

moran.test(bee.t$visits,mat2listw(plots.dists.inv))# I = 0.0585
moran.test(bee.t$visits, nb2listw(w5))# I= 0.09
moran.plot(bee.t$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation bees across plots")


#polinizadores general ----
junto <- pol[,c("Plot", "Subplot","visitas_indv_hora")]
polinizadores <- junto %>% group_by(Plot, Subplot) %>% summarise (visit = sum(visitas_indv_hora))
polinizadores$Plot <- as.numeric(polinizadores$Plot)
final <- left_join(disfinal1, polinizadores, by= c("Plot", "Subplot"))
final.1 <- subset(final, Subplot != "OUT")
final.1$visit[is.na(final.1$visit)] <- 0

total.corr <- spline.correlog(x=final.1$x_coor2, y=final.1$y_coor2,
                                z=final.1$visit, resamp=100, quiet=TRUE)
plot(total.corr, main= "Spatial Autocorrelation pollinators across plots")

moran.test(final.1$visit,mat2listw(plots.dists.inv)) # I= 0.1648
moran.test(final.1$visit, nb2listw(w5)) # vecinos, I= 0.249
moran.plot(final.1$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")


############################################PLANTS########################################
plantas <- pol[,c("Plot", "Subplot", "Plant_Simple", "num.plantas", "Fruit", "Seed")]
#CHFU ----
CHFU <- subset(plantas, Plant_Simple == "CHFU")
CHFU$num.plantas <- as.numeric(CHFU$num.plantas)
CHFU2 <- CHFU %>% group_by(Plot, Subplot) %>% summarise (planta = sum(num.plantas))
CHFU1 <- left_join(disfinal1, CHFU2,by= c("Plot", "Subplot"))
CHFU1$planta[is.na(CHFU1$planta)] <- 0

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU1$x_coor2, y=CHFU1$y_coor2,
                            z=CHFU1$planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")

moran.test(CHFU1$planta,mat2listw(plots.dists.inv)) # I= 0.2468
moran.test(CHFU1$planta, nb2listw(w5)) # vecinos, I= 0.387
moran.plot(CHFU1$planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")
#LEMA----
LEMA <- subset(plantas, Plant_Simple == "LEMA")
LEMA$num.plantas <- as.numeric(LEMA$num.plantas)
prueba1 <- LEMA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
lema.sub <- left_join(disfinal1, prueba1, by= c("Plot", "Subplot"))
LEMA1<- lema.sub
LEMA1$num.plantas <- LEMA1$num.pla
LEMA1$num.plantas[is.na(LEMA1$num.plantas)] <- 0

lema.corr <- spline.correlog(x=LEMA1$x_coor2, y=LEMA1$y_coor2,
                             z=LEMA1$num.plantas, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

moran.test(LEMA1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.16
moran.test(LEMA1$num.plantas, nb2listw(w5)) # vecinos, I= 0.23
moran.plot(LEMA1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")
#PUPA ----
PUPA <- subset(plantas, Plant_Simple == "PUPA")
PUPA$num.plantas <- as.numeric(PUPA$num.plantas)
prueba2 <- PUPA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
pupa.sub <- left_join(disfinal1, prueba2, by= c("Plot", "Subplot"))
PUPA1<- pupa.sub
PUPA1$num.plantas <- PUPA1$num.pla
PUPA1$num.plantas[is.na(PUPA1$num.plantas)] <- 0

pupa.corr <- spline.correlog(x=PUPA1$x_coor2, y=PUPA1$y_coor2,
                             z=PUPA1$num.plantas, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

moran.test(PUPA1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.2737
moran.test(PUPA1$num.plantas, nb2listw(w5)) # vecinos, I= 0.416
moran.plot(PUPA1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#ME
ME <- subset(plantas, Plant_Simple == "ME")
ME$num.plantas <- as.numeric(ME$num.plantas)
prueba3 <- ME %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
me.sub <- left_join(disfinal1, prueba3, by= c("Plot", "Subplot"))
ME1<- me.sub
ME1$num.plantas <- ME1$num.pla
ME1$num.plantas[is.na(ME1$num.plantas)] <- 0

me.corr <- spline.correlog(x=ME1$x_coor2, y=ME1$y_coor2,
                             z=ME1$num.plantas, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation ME across plots")

moran.test(ME1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.01725 
moran.test(ME1$num.plantas, nb2listw(w5)) # vecinos, I= 0.0143, pero un p.value 0.037
moran.plot(ME1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation ME across plots")

#CHMI ----
CHMI <- subset(plantas, Plant_Simple == "CHMI") # solo 5 entradas

#plantas juntas ----
juntas.plantas <- plantas[,c("Plot", "Subplot","num.plantas")]
juntas.plantas$num.plantas <- as.numeric(juntas.plantas$num.plantas)
pl <- juntas.plantas %>% group_by(Plot, Subplot) %>% summarise (plantas = sum(num.plantas))
pl$Plot <- as.numeric(pl$Plot)
final.pl <- left_join(disfinal1, pl, by= c("Plot", "Subplot"))
final.pl$plantas[is.na(final.pl$plantas)] <- 0

total.corr.pl <- spline.correlog(x=final.pl$x_coor2, y=final.pl$y_coor2,
                              z=final.pl$plantas, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation pollinators across plots")

moran.test(final.pl$plantas,mat2listw(plots.dists.inv)) # I= 0.15
moran.test(final.pl$plantas, nb2listw(w5)) # vecinos, I= 0.25
moran.plot(final.pl$plantas,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")

######################## 2. GLMs #######################################
#Para incluir la variables espacio es necesario generar una auto-covariable. 
# Ver: https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210X.12402 y https://rpubs.com/corey_sparks/111362
# funcion a usar autocov_dist del paquete 'spdep'. Style= "B" y type = 'one'para que me de una matriz simetrica
#mi modelo glm = semillas ~ visitas/group/subplot/indiv/hour . De esta manera sacaremos qué polinizador es el más importante 
# para el fitness
# beetle. 
head(pol)#tengo que añadir una columna dividiendo las visitas por 
CHFU.vis <- subset(pol, Plant_Simple == "CHFU")
visitas.chfu <- CHFU.vis[,c("Seed")]

CHFU.vis$Seed <- as.numeric(CHFU.vis$Seed)
xy1 <-- st_coordinates(st_centroid(st_geometry(xy),
                           of_largest_polygon=TRUE))
m <-autocov_dist(CHFU.vis$Seed,st_sf(xy), type = '1', style= "B" )
0
xy <- (cbind(disfinal1$x_coor2, disfinal1$y_coor2))
