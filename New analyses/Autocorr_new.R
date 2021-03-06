library(tidyverse)
library(ape)
library(ncf)
#install.packages("spdep") -> for Moran's I
library(spdep)
library(reshape2)
library(vegan)
library(nlme)

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

############################ problems 
#me gustaria probar esto, que es coger los 8 vecinos de un dato y ver cuanto se parecen estos 8 vecinos a mi
#   dato, pero me da error. Seria por comprobar los analisis anteriores. 
disfinal1$x_coor2 <- as.numeric(as.character(disfinal1$x_coor2))
disfinal1$y_coor2 <- as.numeric(as.character(disfinal1$y_coor2))
w5 <- knn2nb(knearneigh(coordinates(disfinal1[,3:4]), k=8))
moran.test(bird$nSpecies, nb2listw(w5))
###########################

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
new_df <- plantas[-which(duplicated(plantas)), ]

#CHFU ----
CHFU <- subset(new_df, Plant_Simple == "CHFU")
CHFU$num.plantas <- as.numeric(CHFU$num.plantas)
CHFU2 <- CHFU %>% group_by(Plot, Subplot) %>% summarise (planta = sum(num.plantas))
CHFU1 <- left_join(disfinal1, CHFU2,by= c("Plot", "Subplot"))
CHFU1$planta[is.na(CHFU1$planta)] <- 0

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU1$x_coor2, y=CHFU1$y_coor2,
                            z=CHFU1$planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")

moran.test(CHFU1$planta,mat2listw(plots.dists.inv)) # I= 0.3
moran.test(CHFU1$planta, nb2listw(w5)) # vecinos, I= 0.45
moran.plot(CHFU1$planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")
#LEMA----
LEMA <- subset(new_df, Plant_Simple == "LEMA")
LEMA$num.plantas <- as.numeric(LEMA$num.plantas)
prueba1 <- LEMA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
lema.sub <- left_join(disfinal1, prueba1, by= c("Plot", "Subplot"))
LEMA1<- lema.sub
LEMA1$num.plantas <- LEMA1$num.pla
LEMA1$num.plantas[is.na(LEMA1$num.plantas)] <- 0

lema.corr <- spline.correlog(x=LEMA1$x_coor2, y=LEMA1$y_coor2,
                             z=LEMA1$num.plantas, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

moran.test(LEMA1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.26
moran.test(LEMA1$num.plantas, nb2listw(w5)) # vecinos, I= 0.386
moran.plot(LEMA1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")
#PUPA ----
PUPA <- subset(new_df, Plant_Simple == "PUPA")
PUPA$num.plantas <- as.numeric(PUPA$num.plantas)
prueba2 <- PUPA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
pupa.sub <- left_join(disfinal1, prueba2, by= c("Plot", "Subplot"))
PUPA1<- pupa.sub
PUPA1$num.plantas <- PUPA1$num.pla
PUPA1$num.plantas[is.na(PUPA1$num.plantas)] <- 0

pupa.corr <- spline.correlog(x=PUPA1$x_coor2, y=PUPA1$y_coor2,
                             z=PUPA1$num.plantas, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

moran.test(PUPA1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.41
moran.test(PUPA1$num.plantas, nb2listw(w5)) # vecinos, I= 0.63
moran.plot(PUPA1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")
#ME
ME <- subset(new_df, Plant_Simple == "ME")
ME$num.plantas <- as.numeric(ME$num.plantas)
prueba3 <- ME %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
me.sub <- left_join(disfinal1, prueba3, by= c("Plot", "Subplot"))
ME1<- me.sub
ME1$num.plantas <- ME1$num.pla
ME1$num.plantas[is.na(ME1$num.plantas)] <- 0

me.corr <- spline.correlog(x=ME1$x_coor2, y=ME1$y_coor2,
                             z=ME1$num.plantas, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation ME across plots")

moran.test(ME1$num.plantas,mat2listw(plots.dists.inv)) # I= 0.0437
moran.test(ME1$num.plantas, nb2listw(w5)) # vecinos, I= 0.055
moran.plot(ME1$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation ME across plots")

#CHMI ----
CHMI <- subset(new_df, Plant_Simple == "CHMI") # solo 4 entradas

#plantas juntas ----
juntas.plantas <- new_df[,c("Plot", "Subplot","num.plantas")]
juntas.plantas$num.plantas <- as.numeric(juntas.plantas$num.plantas)
pl <- juntas.plantas %>% group_by(Plot, Subplot) %>% summarise (plantas = sum(num.plantas))
pl$Plot <- as.numeric(pl$Plot)
final.pl <- left_join(disfinal1, pl, by= c("Plot", "Subplot"))
final.pl$plantas[is.na(final.pl$plantas)] <- 0

total.corr.pl <- spline.correlog(x=final.pl$x_coor2, y=final.pl$y_coor2,
                              z=final.pl$plantas, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation pollinators across plots")

moran.test(final.pl$plantas,mat2listw(plots.dists.inv)) # I= 0.21
moran.test(final.pl$plantas, nb2listw(w5)) # vecinos, I= 0.34
moran.plot(final.pl$plantas,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")

######################## 2. GLMs #######################################
#Para incluir la variables espacio es necesario generar una auto-covariable. 
# Ver: https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210X.12402 y https://rpubs.com/corey_sparks/111362
# funcion a usar autocov_dist del paquete 'spdep'. Style= "B" y type = 'one'para que me de una matriz simetrica
#mi modelo glm = semillas ~ visitas/group/subplot/indiv/hour + space . De esta manera sacaremos qué polinizador es el más importante 
# para el fitness
# https://www.ufz.de/export/data/2/92427_Dormann_et_al_Methods_appendix.pdf 
# beetle. 
#head(pol)#tengo que añadir una columna dividiendo las visitas por 
CHFU.vis <- subset(pol, Plant_Simple == "CHFU")

#visitas.chfu <- as.vector(CHFU.vis[,"Seed"])

#CHFU.vis$Seed <- as.numeric(CHFU.vis$Seed)

#xy1 <-- st_coordinates(st_centroid(st_geometry(xy),
                   #        of_largest_polygon=TRUE))

#xy <- (cbind(disfinal1$x_coor2, disfinal1$y_coor2))
#xy <- as.matrix(xy)
#str(xy)

nb.list <- dnearneigh(as.matrix(disfinal1[,c("x_coor2", "y_coor2")]), 0, 5)
nb.weights <- nb2listw(nb.list)
#Make a matrix of coordinates
coords<-as.matrix(cbind(data$x_coor2,data$y_coor2))

m <-autocov_dist(CHFU.vis$Seed,xy, type = '1', style= "B" )
ac <- autocov_dist(CHFU.vis$Seed, coords,type = "inverse", nbs = 4, zero.policy=TRUE)

########################################NEIGHBORS##################################33
#phenology
comp <- read.csv2("data/competition_wide.csv",stringsAsFactors = FALSE)
abund <- read.csv2("data/abundances.csv",stringsAsFactors = FALSE)
abund <- dplyr::arrange(abund,year,month,day,plot,subplot,species)
abund$date <- paste(abund$year,"-",abund$month,"-",abund$day,sep="")
abund$day <- as.numeric(abund$day)
abund$month <- as.numeric(abund$month)
abund$year <- as.numeric(abund$day)
abund$date <- as.numeric(abund$date)
abund$week <- strftime(abund$date,format = "%V")
abund$week <- strftime(abund$date,format = "%V")
phenology.color1 <- ggplot(abund, aes(x= week, y = species))+
    geom_point(aes(color = week))+
    ggtitle ("Phenology 2019")+
    xlab ("weeks")+
    ylab ("spp_Plants")+
    NULL
phenology.color1

#FIRST- I'm making the neighbors data base for LEMA and CHFU, that they are the erliers at the season with RAPE. 
#   we are going to include RAPE in the neighbors but not in the models. 

table.chfu.lema <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_chfu_lema_RAPE.csv", header=T, sep=";")
table.19 <- subset(table.chfu.lema, year == 2019) 
table.sinedge <- subset(table.19,edge %in% c("FALSE"))
table.sinedge$Plant_Simple<- table.sinedge$focal 
table.sinedge$Subplot <- table.sinedge$subplot
table.sinedge$Plot <- table.sinedge$plot
table.sinedge$Subplot <- table.sinedge$subplot
table.sinedge$Plant_Simple <- table.sinedge$focal
table <- table.sinedge[,c("Plot", "Subplot","Plant_Simple", "distance", "neigh_intra", "neigh_inter", "edge")]


c2 <- dplyr::full_join(new_df, table, by= c("Plot", "Subplot", "Plant_Simple"))
c2.clean <- subset(c2, Plant_Simple %in% c("LEMA", "CHFU")) #Here appears a lot of NAs, and it is because the NAs corresponds
# with the edges. We are not having As, Fs, 6s and 1s. Next step eliminate these NAs. 


c2.7.5 <- subset(c2.clean, distance %in% c("d1")) 
c2.7.5$distance7.5 <- c2.7.5$distance
c2.7.5$neigh_intra.7.5 <- c2.7.5$neigh_intra
c2.7.5$neigh_inter.7.5<- c2.7.5$neigh_inter

c2.1m <- subset(c2.clean, distance %in% c("d2")) 
c2.1m$distances.1m <- c2.1m$distance
c2.1m$neigh_intra.1m <- c2.1m$neigh_intra
c2.1m$neigh_inter.1m<- c2.1m$neigh_inter
c2.3m <- subset(c2.clean, distance %in% c("d3")) 
c2.3m$distances.3m <- c2.3m$distance
c2.3m$neigh_intra.3m <- c2.3m$neigh_intra
c2.3m$neigh_inter.3m<- c2.3m$neigh_inter
c2.plot <- subset(c2.clean, distance %in% c("d4")) 
c2.plot$distances.plot <- c2.plot$distance
c2.plot$neigh_intra.plot <- c2.plot$neigh_intra
c2.plot$neigh_inter.plot <- c2.plot$neigh_inter

c2.1 <- dplyr::full_join(c2.7.5, c2.1m, by= c("Plot", "Seed","num.plantas", "Subplot",  "Plant_Simple", "edge" ,"Seed", "Fruit"))
c2.2 <- dplyr::full_join(c2.3m, c2.plot, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge","Seed", "Fruit"))

c2.total <- dplyr::full_join(c2.1, c2.2, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge" ,"Seed", "Fruit"))

head(c2.total)

vecinos.chfu.lema <- c2.total[,c("Plot","Subplot","Plant_Simple","num.plantas", "Fruit","Seed", "neigh_inter.plot","neigh_intra.plot",
                                 "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]
        # this is the final data of Lema and CHFU Neighbors. 

###HERE IS ONE OPTION FOLLOWING NLME.

#FIRST FIT THE RANDOM STRUCTURE. #we are gonna work with to types of spatial autcorrelation types exponential and poisson. 
# Do first example with CHFU (Maria you do the rest)

library(MuMIn)
library(multcomp)
library(emmeans)
library(sjPlot)
library(sjmisc)


CHFU.vis$unique_id <- paste(CHFU.vis$Plot, CHFU.vis$Subplot, sep="_")
CHFU1$unique_id <- paste(CHFU1$Plot, CHFU1$Subplot, sep="_")

chfu <- full_join(CHFU.vis, CHFU1, by=c("Plot", "Subplot", "unique_id")) %>%
    group_by(unique_id)%>%
    mutate(seeds = mean(Seed, na.rm=TRUE), visits = mean(visitas_indv_hora, na.rm = TRUE)) %>%
    distinct(unique_id, .keep_all=TRUE) # this is done to have only one measure per subplot as the spatial autocorrelation does
                                        #not like distances matices equal to 0.
chfu <- na.omit(chfu)

#remove number of seeds equal to 0 because actually they make the model to behave badly. 
chfu <- subset(chfu, seeds>0)

#different models

lCtr <- lmeControl(maxIter = 500, msMaxIter = 500, tolerance = 1e-6, niterEM = 250, msMaxEval = 200)

m1 <- lme(log(seeds) ~ 1, data= chfu,random = ~1 |Plot, control=lCtr,
          method = "ML")

m2 <- lme(log(seeds) ~ 1, data= chfu, random = ~1 |Plot, control=lCtr,
             corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m3 <- lme(log(seeds) ~ 1, data= chfu, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4 <- lme(log(seeds) ~ 1, data= chfu, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")

AIC(m1, m2, m3, m4)
#no spatial autocorrelation fit best

#Now we go with the fixed part MARIA REMEMBER THAT WE NEED TO INCLUDE HERE THE NUMBER OF CONSPECIFIC AND HETEROSPECIFIC PLANT INDIVIDUALS
#WITHIN THE SUBPLOT. 
options(na.action = "na.fail")

model <- lme(log(seeds + 1) ~ visits*Group, data= chfu, random = ~1 |Plot, control=lCtr,
                method = "ML")
model_sec <- dredge(model, trace = TRUE, rank = "AICc", REML = FALSE)

(attr(model_sec, "rank.call"))
fmList <- get.models(model_sec, 1:4)
summary(model.avg(fmList))

#Best model ###This is not the best model but I am doing all this code so you can see how to proceed and how to know the model is correct.
#by checking the residuals. 
model_best <- lme(log(seeds + 1) ~ visits*Group, data= chfu, random = ~1 |Plot, control=lCtr,
                  method = "ML")
summary(model_best)

#Model checking plot 
residuals <- resid(model_best)
hist(residuals, prob=TRUE, col="darkgray")
Range = seq(min(residuals), max(residuals), length = length(residuals))
Norm = dnorm(Range, mean = mean(residuals), sd = sd(residuals))
lines(Range, Norm, col = "blue", lwd = 2)

plot(model_best)
plot(model_best, resid(., type = "p") ~ fitted(.) | Group, abline = 0)
plot(model_best,  visits ~ resid(.))
qqnorm(model_best, ~ranef(., level=1))
qqnorm(residuals)
qqline(residuals)

#Perform tukey test 
summary(glht(model_best, linfct = mcp(Group = "Tukey"))) #esto no me funciona
lsmeans(model_best, list(pairwise ~ visits*Group), adjust = "tukey") #tukey for an interaction
# all the SITES CN1, CN2, and CN3 are differet.

#plot the main results.
plot_model(model_best, type = "pred", terms = c("Group", "visits"))
#Values of SST.C, corresponds to 1quartile, median, and third quartile. 

#OK MARIA this is the basis, you need to follow this procedure for all species and including the number of neighbours. 
