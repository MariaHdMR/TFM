#load libraries
library(tidyverse)
library(ape)
library(ncf)
#install.packages("spdep") -> for Moran's I
library(spdep)
library(reshape2)
library(vegan)
library(nlme)
library(lme4)
library(DHARMa)


#install.packages("remotes")
#remotes::install_github("nyiuab/NBZIMM")
#install.packages("NBZIMM")
#library("NBZIMM")
#I de Moran. Valores entre -1 y 1. Si es positivo esta positivamente correlacionado, y si es negativo no hay
#   correlacion. Lugares más proximos se parecen mas(+ correlacion) que a los lugares lejanos. 

#cargar datos, juntar bases de datos de polinizadores, abundancias plantas, fruits y seeds, y limpiar
                #visitors
FV <- read.table("C:/Users/Cisco/Documents/TFM/data/Metadata_Pollinators_2019_2016_bueno.csv", header=T, sep=";")
FV_19 <- subset(FV, Year == 2019) 
#I have to change the ME plant to MESU. MOst of the plants were MESU insted of MEEL
FV_19$Plant_Simple <- as.character(FV_19$Plant_Simple)
FV_19$Plant_Simple[FV_19$Plant_Simple == 'ME'] <- "MESU" # todas las plantas como MESU
FV_19$Plot<- as.factor(FV_19$Plot)
FV_19 <-FV_19%>% group_by(Plot, Subplot, Plant_Simple, Group,G_F, ID_Simple, Plant_Simple) %>% summarise (num.visits = sum(Visits))%>% ungroup()
FV_19 <- subset(FV_19, Plant_Simple %in% c("LEMA","CHFU","MESU","PUPA", "CHMI"))

                #abundances plants
Abun <-read.table("C:/Users/Cisco/Documents/TFM/data/abundances.csv", header=T, sep=";")
Abun_19 <- subset(Abun, year == 2019) 
Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
Abun_19$Plantas <- Abun_19$individuals
Abun_19$Plant_Simple <- Abun_19$species
Abun_19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))%>% ungroup()
Abun_19.sp <- subset(Abun_19, Plant_Simple%in% c("LEMA","CHFU","MESU","PUPA", "CHMI"))
Abun_19.sp$Plot<- as.factor(Abun_19.sp$Plot)

                #competition plants
competencia <- read.table("C:/Users/Cisco/Documents/TFM/data/competition_wide.csv", header=T, sep=";")
competencia <- subset(competencia, year == 2019)
competencia$Plant_Simple <- competencia$focal
competencia$Plot <- competencia$plot
competencia$Subplot <- competencia$subplot
competencia$Fruit <- competencia$fruit
competencia$Seed <- competencia$seed
competencia$Plot <- as.numeric(competencia$Plot)
competencia1 <- competencia[,c("Plot","Subplot","Plant_Simple","Fruit","Seed")]
plantfruit <- subset(competencia1, Plant_Simple%in% c("LEMA","CHFU","MESU","PUPA", "CHMI"))

            #neighbors data
table.chfu.lema <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours_chfu_lema_RAPE.csv", header=T, sep=";")
table.mesu <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours_MESU_GOOD.csv", header=T, sep=";")
table.pupa <- read.table("C:/Users/Cisco/Documents/TFM/data/focal_neighbours_PUPA_good.csv", header=T, sep=";")


            #plants abundances+pollinators
FINAL <- full_join(FV_19, Abun_19.sp) #polinizadores+abun plantas
FINAL$Plot <- as.numeric(FINAL$Plot)
            #plants abundances+pollinators+seeds
FINAL.seeds <- full_join(FINAL, plantfruit, by= c("Plot", "Subplot", "Plant_Simple"))
Total <- FINAL.seeds[,c("Plot","Subplot", "Group","G_F", "ID_Simple", "num.visits","Plant_Simple", "num.plantas","Fruit","Seed")]
Total$Fruit <- as.numeric(Total$Fruit)
Total$Seed <- as.numeric(Total$Seed)
Total$Fruit[is.na(Total$Fruit)] <- 0
Total$Seed[is.na(Total$Seed)] <- 0
Total <- subset(Total, Plot != "OUT")
Total <- subset(Total, Subplot != "OUT")

# now I select the IDs that we want to exclude
Total <- subset(Total, G_F != "Ants")
Total <- subset(Total, ID_Simple != "Coccinella_septempunctata") 
Total <- subset(Total, ID_Simple != "Larva")
Total <- subset(Total, ID_Simple != "Chrysididae")
Total <- subset(Total, ID_Simple != "Diplazon_sp.")
Total <- subset(Total, G_F != "Mosquitoes")
Total <- Total[,c("Plot","Subplot", "Group", "num.visits","Plant_Simple", "num.plantas","Fruit","Seed")]
Total$Group <- as.character(Total$Group)
Total <- subset(Total, Group %in% c("Bee","Beetle","Butterfly","Fly"))


#I change the 0 abundance of plants to 1 (at least we might have 1 plant)
Total$num.plantas[Total$num.plantas == "0"] <- "1"
Total$num.visits <-as.numeric(Total$num.visits)
Total$num.plantas <- as.numeric(Total$num.plantas)
Total$visitas_indv <- Total$num.visits/ Total$num.plantas
Total$visitas_indv_hora <- (Total$visitas_indv*60)/30


#de esta manera ya tengo las visitas por individuo y por hora
pol <- Total #aqui tengo ya polinizadores, semillas, individuos y frutos --> pol - base de datos final

#voy a ver si mis datos son normales -> no lo son. 
hist(pol$visitas_indv_hora)

##########################################DISTANCES PLOTS#############################################
distances <- read.csv("C:/Users/Cisco/Documents/TFM/data/caracolesplotposition.csv", sep = ";") #aqui solamente esta la informacion de 1 
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
disfinal <- disfinal[,c("Plot", "Subplot","x_coor2", "y_coor2")]
distancias.matriz <-as.matrix(dist(cbind(disfinal$x_coor2, disfinal$y_coor2)))
#now, I make the inverse matrix and the diagonal equal to 0
plots.dists.inv <- 1/distancias.matriz
diag(plots.dists.inv) <- 0
#plots.dists.inv[1:5,1:5]

# preparacion de datos para analizar por los 8 vecinos más cercanos --> w5. Asi vemos si se parecen mas 
# entre los vecinos que al resto en la I de Moran
disfinal$x_coor2 <- as.numeric(disfinal$x_coor2)
disfinal$y_coor2 <- as.numeric(disfinal$y_coor2)
w5 <- knn2nb(knearneigh(coordinates(disfinal[,3:4]), k=8))

################################################ 1.Moran's I####################################################

##### ##############################>Pollinators#######################################
#beetles 
beetle <- subset(pol, Group == "Beetle")
beetle$Plot <- as.numeric(as.character(beetle$Plot))
beetle.v <- beetle[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas de BEETLES
beetle.v <-beetle.v %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
beetle.v <- left_join(disfinal, beetle.v, by= c("Plot", "Subplot"))
beetle.v$visits[is.na(beetle.v$visits)] <- 0

#grafico de la correlacion espacial
bet.corr <- spline.correlog(x=beetle.v$x_coor2, y=beetle.v$y_coor2,
                            z=beetle.v$visits, resamp=100, quiet=TRUE)
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")

#test de moran. Aquí quiero obtener El estadistico de Moran y p.value 
moran.test(beetle.v$visits,mat2listw(plots.dists.inv)) # I= 0.129
moran.test(beetle.v$visits, nb2listw(w5)) # vecinos, I= 0.17
moran.plot(beetle.v$visits,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation Beetles across plots")

#flies
flies <- subset(pol, Group == "Fly")
flies$Plot <- as.numeric(as.character(flies$Plot))
flies.v <- flies[,c("Plot", "Subplot","visitas_indv_hora")] 
flies.v <-flies.v %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
flies.v <- left_join(disfinal,flies.v, by= c("Plot", "Subplot"))
flies.v$visits[is.na(flies.v$visits)] <- 0

flies.corr <- spline.correlog(x=flies.v$x_coor2, y=flies.v$y_coor2,
                              z=flies.v$visits, resamp=100, quiet=TRUE)
plot(flies.corr, main= "Spatial autocorrelation flies across plots")

moran.test(flies.v$visits,mat2listw(plots.dists.inv)) # I= 0.11
moran.test(flies.v$visits, nb2listw(w5)) # vecinos, I= 0.178
moran.plot(flies.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation flies across plots")

#butterflies
but <- subset(pol, Group == "Butterfly")
but$Plot <- as.numeric(as.character(but$Plot))
but.v <- but[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
but.v <-but.v %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
but.v <- left_join(disfinal,but.v , by= c("Plot", "Subplot"))
but.v$visits[is.na(but.v$visits)] <- 0

but.corr <- spline.correlog(x=but.v$x_coor2, y=but.v$y_coor2,
                            z=but.v$visits, resamp=100, quiet=TRUE)
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")

moran.test(but.v$visits,mat2listw(plots.dists.inv)) # I= 0.067
moran.test(but.v$visits, nb2listw(w5)) # vecinos, I= 0.047
moran.plot(but.v$visits,mat2listw(plots.dists.inv),  main= "Spatial autocorrelation butterflies across plots")

#bees
bee <- subset(pol, Group == "Bee")
bee$Plot <- as.numeric(as.character(bee$Plot))
bee.v <- bee[,c("Plot", "Subplot","visitas_indv_hora")] #datos de plot, subplot, y visitas 
bee.v <-bee.v %>% group_by(Plot, Subplot) %>% summarise (visits = sum(visitas_indv_hora))
bee.v <- left_join(disfinal, bee.v, by= c("Plot", "Subplot"))
bee.v$visits[is.na(bee.v$visits)] <- 0

bee.corr <- spline.correlog(x=bee.v$x_coor2, y=bee.v$y_coor2,
                            z=bee.v$visits, resamp=100, quiet=TRUE)
plot(bee.corr, main= "Spatial autocorrelation bees across plots")

moran.test(bee.v$visits,mat2listw(plots.dists.inv))# I = 0.0585
moran.test(bee.v$visits, nb2listw(w5))# I= 0.09
moran.plot(bee.v$visits,mat2listw(plots.dists.inv), main= "Spatial autocorrelation bees across plots")

#polinizadores general 
junto <- pol[,c("Plot", "Subplot","visitas_indv_hora")]
junto <- junto %>% group_by(Plot, Subplot) %>% summarise (visit = sum(visitas_indv_hora))
junto$Plot <- as.numeric(junto$Plot)
junto <- left_join(disfinal, junto, by= c("Plot", "Subplot"))
junto <- subset(junto, Subplot != "OUT")
junto$visit[is.na(junto$visit)] <- 0

total.corr <- spline.correlog(x=junto$x_coor2, y=junto$y_coor2,
                              z=junto$visit, resamp=100, quiet=TRUE)
plot(total.corr, main= "Spatial Autocorrelation visitors across plots")

moran.test(junto$visit,mat2listw(plots.dists.inv)) # I= 0.1648
moran.test(junto$visit, nb2listw(w5)) # vecinos, I= 0.249
moran.plot(junto$visit,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation pollinators across plots")

par(mfrow=c(2,3))
plot(bet.corr, main= " Spatial Autocorrelation beetles across plots")
plot(flies.corr, main= "Spatial autocorrelation flies across plots")
plot(but.corr, main= "Spatial autocorrelation butterflies across plots")
plot(bee.corr, main= "Spatial autocorrelation bees across plots")
plot(total.corr, main= "Spatial Autocorrelation visitors across plots")

############################################ >Plants########################################
plantas <- pol[,c("Plot", "Subplot", "Plant_Simple", "num.plantas", "Fruit", "Seed")]
plantas <- plantas[-which(duplicated(plantas)), ] #It appears in the data set a lot of duplicated rows, because the previus data set was with the pollinators
#                                                   so we have to eliminate the duplicates, and I did it with this step
        #CHFU
CHFU <- subset(plantas, Plant_Simple == "CHFU")
CHFU$num.plantas <- as.numeric(CHFU$num.plantas)
CHFU.p <- CHFU %>% group_by(Plot, Subplot) %>% summarise (planta = sum(num.plantas))
CHFU.p <- left_join(disfinal, CHFU.p,by= c("Plot", "Subplot"))
CHFU.p$planta[is.na(CHFU.p$planta)] <- 0

#grafico de la correlacion espacial
chfu.corr <- spline.correlog(x=CHFU.p$x_coor2, y=CHFU.p$y_coor2,
                             z=CHFU.p$planta, resamp=100, quiet=TRUE)
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")

moran.test(CHFU.p$planta,mat2listw(plots.dists.inv)) # I= 0.3
moran.test(CHFU.p$planta, nb2listw(w5)) # vecinos, I= 0.45
moran.plot(CHFU.p$planta,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU across plots")

        #LEMA
LEMA <- subset(plantas, Plant_Simple == "LEMA")
LEMA$num.plantas <- as.numeric(LEMA$num.plantas)
LEMA.p <- LEMA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
LEMA.p <- left_join(disfinal, LEMA.p, by= c("Plot", "Subplot"))
LEMA.p$num.plantas <- LEMA.p$num.pla
LEMA.p$num.plantas[is.na(LEMA.p$num.plantas)] <- 0

lema.corr <- spline.correlog(x=LEMA.p$x_coor2, y=LEMA.p$y_coor2,
                             z=LEMA.p$num.plantas, resamp=100, quiet=TRUE)
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")

moran.test(LEMA.p$num.plantas,mat2listw(plots.dists.inv)) # I= 0.26
moran.test(LEMA.p$num.plantas, nb2listw(w5)) # vecinos, I= 0.386
moran.plot(LEMA.p$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA across plots")

        #PUPA 
PUPA <- subset(plantas, Plant_Simple == "PUPA")
PUPA$num.plantas <- as.numeric(PUPA$num.plantas)
PUPA.p <- PUPA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
PUPA.p <- left_join(disfinal, PUPA.p, by= c("Plot", "Subplot"))
PUPA.p$num.plantas <- PUPA.p$num.pla
PUPA.p$num.plantas[is.na(PUPA.p$num.plantas)] <- 0

pupa.corr <- spline.correlog(x=PUPA.p$x_coor2, y=PUPA.p$y_coor2,
                             z=PUPA.p$num.plantas, resamp=100, quiet=TRUE)
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")

moran.test(PUPA.p$num.plantas,mat2listw(plots.dists.inv)) # I= 0.41
moran.test(PUPA.p$num.plantas, nb2listw(w5)) # vecinos, I= 0.63
moran.plot(PUPA.p$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")

        #MESU
MESU <- subset(plantas, Plant_Simple == "MESU")
MESU$num.plantas <- as.numeric(MESU$num.plantas)
MESU.p <- MESU %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.pla = sum(num.plantas))
MESU.p <- left_join(disfinal, MESU.p, by= c("Plot", "Subplot"))
MESU.p$num.plantas <- MESU.p$num.pla
MESU.p$num.plantas[is.na(MESU.p$num.plantas)] <- 0

me.corr <- spline.correlog(x=MESU.p$x_coor2, y=MESU.p$y_coor2,
                           z=MESU.p$num.plantas, resamp=100, quiet=TRUE)
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")

moran.test(MESU.p$num.plantas,mat2listw(plots.dists.inv)) # I= 0.0437
moran.test(MESU.p$num.plantas, nb2listw(w5)) # vecinos, I= 0.055
moran.plot(MESU.p$num.plantas,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU across plots")

        #CHMI 
CHMI <- subset(plantas, Plant_Simple == "CHMI") # solo 4 entradas

#plantas juntas ----
juntas.plantas <- plantas[,c("Plot", "Subplot","num.plantas")]
juntas.plantas$num.plantas <- as.numeric(juntas.plantas$num.plantas)
num.plant <- juntas.plantas %>% group_by(Plot, Subplot) %>% summarise (plantas = sum(num.plantas))
num.plant$Plot <- as.numeric(num.plant$Plot)
num.plant <- left_join(disfinal, num.plant, by= c("Plot", "Subplot"))
num.plant$plantas[is.na(num.plant$plantas)] <- 0

total.corr.pl <- spline.correlog(x=num.plant$x_coor2, y=num.plant$y_coor2,
                                 z=num.plant$plantas, resamp=100, quiet=TRUE)
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")

moran.test(num.plant$plantas,mat2listw(plots.dists.inv)) # I= 0.21
moran.test(num.plant$plantas, nb2listw(w5)) # vecinos, I= 0.34
moran.plot(num.plant$plantas,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants across plots")


par(mfrow=c(3,2))
plot(chfu.corr, main= " Spatial Autocorrelation CHFU across plots")
plot(lema.corr, main= " Spatial Autocorrelation LEMA across plots")
plot(pupa.corr, main= " Spatial Autocorrelation PUPA across plots")
plot(me.corr, main= " Spatial Autocorrelation MESU across plots")
plot(total.corr.pl, main= "Spatial Autocorrelation plants across plots")

##########################################>Fitness###################################################

#CHFU 
CHFU.fit <- CHFU %>% group_by(Plot, Subplot) %>% summarise (seeds = sum(Seed))
CHFU.fit <- left_join(disfinal, CHFU.fit,by= c("Plot", "Subplot"))
CHFU.fit$seeds[is.na(CHFU.fit$seeds)] <- 0

chfu.corr.s <- spline.correlog(x=CHFU.fit$x_coor2, y=CHFU.fit$y_coor2,
                               z=CHFU.fit$seeds, resamp=100, quiet=TRUE)
plot(chfu.corr.s, main= " Spatial Autocorrelation CHFU fitness across plots")

moran.test(CHFU.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.294
moran.test(CHFU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.38
moran.plot(CHFU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation CHFU fitness across plots")

#LEMA
LEMA.fit <- LEMA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (seeds = sum(Seed))
LEMA.fit <- left_join(disfinal, LEMA.fit, by= c("Plot", "Subplot"))
LEMA.fit$seeds[is.na(LEMA.fit$seeds)] <- 0

lema.corr.s <- spline.correlog(x=LEMA.fit$x_coor2, y=LEMA.fit$y_coor2,
                               z=LEMA.fit$seeds, resamp=100, quiet=TRUE)
plot(lema.corr.s, main= " Spatial Autocorrelation LEMA fitness across plots")

moran.test(LEMA.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.12
moran.test(LEMA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.19
moran.plot(LEMA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation LEMA fitness across plots")

#PUPA 
PUPA.fit <- PUPA %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (seeds = sum(Seed))
PUPA.fit <- left_join(disfinal, PUPA.fit, by= c("Plot", "Subplot"))
PUPA.fit$seeds <- PUPA.fit$seeds
PUPA.fit$seeds [is.na(PUPA.fit$seeds )] <- 0

pupa.corr.s <- spline.correlog(x=PUPA.fit$x_coor2, y=PUPA.fit$y_coor2,
                               z=PUPA.fit$seeds, resamp=100, quiet=TRUE)
plot(pupa.corr.s, main= " Spatial Autocorrelation PUPA fitness across plots")

moran.test(PUPA.fit$seeds,mat2listw(plots.dists.inv)) # 0.35
moran.test(PUPA.fit$seeds, nb2listw(w5)) # vecinos, I= 0.476
moran.plot(PUPA.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation PUPA across plots")

#MESU
MESU.fit <- MESU %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (seeds = sum(Seed))
MESU.fit <- left_join(disfinal, MESU.fit, by= c("Plot", "Subplot"))
MESU.fit$seeds[is.na(MESU.fit$seeds)] <- 0

me.corr.s <- spline.correlog(x=MESU.fit$x_coor2, y=MESU.fit$y_coor2,
                             z=MESU.fit$seeds, resamp=100, quiet=TRUE)
plot(me.corr.s, main= " Spatial Autocorrelation MESU fitness across plots")

moran.test(MESU.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.027
moran.test(MESU.fit$seeds, nb2listw(w5)) # vecinos, I= 0.0305
moran.plot(MESU.fit$seeds,mat2listw(plots.dists.inv), main= " Spatial Autocorrelation MESU fitness across plots")

#plantas juntas 
juntas.plantas.fit <- plantas[,c("Plot", "Subplot","Seed")]
juntas.plantas.fit$Seed <- as.numeric(juntas.plantas.fit$Seed)
juntas.plantas.fit <- juntas.plantas.fit %>% group_by(Plot, Subplot) %>% summarise (seeds = sum(Seed))
juntas.plantas.fit$Plot <- as.numeric(juntas.plantas.fit$Plot)
juntas.plantas.fit <- left_join(disfinal, juntas.plantas.fit, by= c("Plot", "Subplot"))
juntas.plantas.fit$seeds[is.na(juntas.plantas.fit$seeds)] <- 0

total.corr.pl.s <- spline.correlog(x=juntas.plantas.fit$x_coor2, y=juntas.plantas.fit$y_coor2,
                                   z=juntas.plantas.fit$seeds, resamp=100, quiet=TRUE)
plot(total.corr.pl.s, main= "Spatial Autocorrelation plant fitness across plots")

moran.test(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv)) # I= 0.42
moran.test(juntas.plantas.fit$seeds, nb2listw(w5)) # vecinos, I= 0.57
moran.plot(juntas.plantas.fit$seeds,mat2listw(plots.dists.inv),main= "Spatial Autocorrelation plants fitness across plots")

par(mfrow=c(2,3))
plot(chfu.corr.s, main= " Spatial Autocorrelation CHFU fitness across plots")
plot(lema.corr.s, main= " Spatial Autocorrelation LEMA fitness across plots")
plot(pupa.corr.s, main= " Spatial Autocorrelation PUPA fitness across plots")
plot(me.corr.s, main= " Spatial Autocorrelation MESU fitness across plots")
plot(total.corr.pl.s, main= "Spatial Autocorrelation plant fitness across plots")
######################################################### 2. NEIGHBORS####################################################################
                                                  
#I ned to get the neighbors to include the intra and inter neighbors in the glm.

#FIRST- I'm making the neighbors data base for LEMA and CHFU, that they are the erliers at the season with RAPE. 
#   we are going to include RAPE in the neighbors but not in the models. 

        #>chfu and lema----
table.chfu.lema <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_chfu_lema_RAPE.csv", header=T, sep=";")
table.chfu.lema <- subset(table.chfu.lema, year == 2019) 
table.chfu.lema <- subset(table.chfu.lema,edge %in% c("FALSE")) #elimino los que estén en el borde
table.chfu.lema$Plant_Simple<- table.chfu.lema$focal 
table.chfu.lema$Subplot <- table.chfu.lema$subplot
table.chfu.lema$Plot <- table.chfu.lema$plot
table.chfu.lema$Subplot <- table.chfu.lema$subplot
table.chfu.lema$Plant_Simple <- table.chfu.lema$focal
table.chfu.lema <- table.chfu.lema[,c("Plot", "Subplot","Plant_Simple", "distance", "neigh_intra", "neigh_inter", "edge")]

#datos de fittness sacados de base de datos brutos de abundancia y de seed+fruits
fitness <- dplyr::full_join(plantfruit, Abun_19, by= c("Plot", "Subplot", "Plant_Simple"))

N.chfu.lema <- dplyr::full_join(fitness, table.chfu.lema, by= c("Plot", "Subplot", "Plant_Simple"))
N.chfu.lema <- subset(N.chfu.lema, Plant_Simple %in% c("LEMA", "CHFU")) #Here appears a lot of NAs, and it is because the NAs corresponds
# with the edges. We are not having As, Fs, 6s and 1s. Next step eliminate these NAs. 

chfulema.7.5 <- subset(N.chfu.lema, distance %in% c("d1")) 
chfulema.7.5$distance7.5 <- chfulema.7.5$distance
chfulema.7.5$neigh_intra.7.5 <- chfulema.7.5$neigh_intra
chfulema.7.5$neigh_inter.7.5<- chfulema.7.5$neigh_inter
chfulema.1m <- subset(N.chfu.lema, distance %in% c("d2")) 
chfulema.1m$distances.1m <- chfulema.1m$distance
chfulema.1m$neigh_intra.1m <- chfulema.1m$neigh_intra
chfulema.1m$neigh_inter.1m<- chfulema.1m$neigh_inter
chfulema.3m <- subset(N.chfu.lema, distance %in% c("d3")) 
chfulema.3m$distances.3m <- chfulema.3m$distance
chfulema.3m$neigh_intra.3m <- chfulema.3m$neigh_intra
chfulema.3m$neigh_inter.3m<- chfulema.3m$neigh_inter
chfulema.plot <- subset(N.chfu.lema, distance %in% c("d4")) 
chfulema.plot$distances.plot <- chfulema.plot$distance
chfulema.plot$neigh_intra.plot <- chfulema.plot$neigh_intra
chfulema.plot$neigh_inter.plot <- chfulema.plot$neigh_inter

chfulema.1 <- dplyr::full_join(chfulema.7.5, chfulema.1m, by= c("Plot", "Seed","num.plantas", "Subplot",  "Plant_Simple", "edge" ,"Seed", "Fruit"))
chfulema.2 <- dplyr::full_join(chfulema.3m, chfulema.plot, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge","Seed", "Fruit"))

chfulema.total <- dplyr::full_join(chfulema.1, chfulema.2, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge" ,"Seed", "Fruit"))
head(chfulema.total)
vecinos.chfu.lema <- chfulema.total[,c("Plot","Subplot","Plant_Simple","num.plantas", "Fruit","Seed", "neigh_inter.plot","neigh_intra.plot",
                                 "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]
                                        # this is the final data of Lema and CHFU Neighbors. 
            #>MESU----
table.mesu <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_MESU_GOOD.csv", header=T, sep=";")
table.mesu$unique_id <- paste(table.mesu$plot, table.mesu$subplot, table.mesu$focal,table.mesu$distance, sep="_")
table.mesu$unique_id <- as.factor(table.mesu$unique_id)

table.mesu <- subset(table.mesu,edge %in% c("FALSE"))
table.mesu$Plant_Simple<- table.mesu$focal 
table.mesu$Subplot <- table.mesu$subplot
table.mesu$Plot <- table.mesu$plot
table.mesu$Subplot <- table.mesu$subplot
table.mesu$Plant_Simple <- table.mesu$focal
table.mesu <- table.mesu[,c("Plot", "Subplot","Plant_Simple", "distance", "neigh_intra", "neigh_inter", "edge", "unique_id")] 
N.mesu <- dplyr::full_join(fitness, table.mesu, by= c("Plot", "Subplot", "Plant_Simple")) %>%
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup(unique_id) 

N.mesu <- subset(N.mesu, Plant_Simple %in% c("MESU")) #solo hay 36 datos con semillas y vecinos completos

mesu.7.5 <- subset(N.mesu, distance %in% c("d1")) 
mesu.7.5$distance7.5 <- mesu.7.5$distance
mesu.7.5$neigh_intra.7.5 <- mesu.7.5$neigh_intra
mesu.7.5$neigh_inter.7.5<- mesu.7.5$neigh_inter

mesu.1m <- subset(N.mesu, distance %in% c("d2")) 
mesu.1m$distances.1m <- mesu.1m$distance
mesu.1m$neigh_intra.1m <- mesu.1m$neigh_intra
mesu.1m$neigh_inter.1m<- mesu.1m$neigh_inter
mesu.3m <- subset(N.mesu, distance %in% c("d3")) 
mesu.3m$distances.3m <- mesu.3m$distance
mesu.3m$neigh_intra.3m <- mesu.3m$neigh_intra
mesu.3m$neigh_inter.3m<- mesu.3m$neigh_inter
mesu.plot <- subset(N.mesu, distance %in% c("d4")) 
mesu.plot$distances.plot <- mesu.plot$distance
mesu.plot$neigh_intra.plot <- mesu.plot$neigh_intra
mesu.plot$neigh_inter.plot <- mesu.plot$neigh_inter

mesu.1 <- dplyr::full_join(mesu.7.5, mesu.1m, by= c("Plot", "Seed","num.plantas", "Subplot",  "Plant_Simple", "edge" , "Fruit"))

mesu.2 <- dplyr::full_join(mesu.3m, mesu.plot, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge", "Fruit"))

mesu.total <- dplyr::full_join(mesu.1, mesu.2, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge" , "Fruit"))
head(mesu.total)

vecinos.mesu <- mesu.total[,c("Plot","Subplot","Plant_Simple","num.plantas", "Fruit","Seed", "neigh_inter.plot","neigh_intra.plot",
                              "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]

                             # this is the final data of MESU Neighbors. 
            #>PUPA----
table.pupa <- read.table("C:/Users/Cisco/Documents/TFM/focal_neighbours_PUPA_good.csv", header=T, sep=";")
table.pupa$unique_id <- paste(table.pupa$plot, table.pupa$subplot, table.pupa$focal,table.pupa$distance, sep="_")
table.pupa$unique_id <- as.factor(table.pupa$unique_id)

table.pupa <- subset(table.pupa,edge %in% c("FALSE"))
table.pupa$Plant_Simple<- table.pupa$focal 
table.pupa$Subplot <- table.pupa$subplot
table.pupa$Plot <- table.pupa$plot
table.pupa$Subplot <- table.pupa$subplot
table.pupa$Plant_Simple <- table.pupa$focal
table.pupa <- table.pupa[,c("Plot", "Subplot","Plant_Simple", "distance", "neigh_intra", "neigh_inter", "edge", "unique_id")]
N.pupa <- dplyr::full_join(fitness, table.pupa, by= c("Plot", "Subplot", "Plant_Simple")) %>% 
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
    ungroup(unique_id) 
N.pupa <- subset(N.pupa, Plant_Simple %in% c("PUPA"))

pupa.7.5 <- subset(N.pupa, distance %in% c("d1")) 
pupa.7.5$distance7.5 <- pupa.7.5$distance
pupa.7.5$neigh_intra.7.5 <- pupa.7.5$neigh_intra
pupa.7.5$neigh_inter.7.5<- pupa.7.5$neigh_inter

pupa.1m <- subset(N.pupa, distance %in% c("d2")) 
pupa.1m$distances.1m <- pupa.1m$distance
pupa.1m$neigh_intra.1m <- pupa.1m$neigh_intra
pupa.1m$neigh_inter.1m<- pupa.1m$neigh_inter
pupa.3m <- subset(N.pupa, distance %in% c("d3")) 
pupa.3m$distances.3m <- pupa.3m$distance
pupa.3m$neigh_intra.3m <- pupa.3m$neigh_intra
pupa.3m$neigh_inter.3m<- pupa.3m$neigh_inter
pupa.plot <- subset(N.pupa, distance %in% c("d4")) 
pupa.plot$distances.plot <- pupa.plot$distance
pupa.plot$neigh_intra.plot <- pupa.plot$neigh_intra
pupa.plot$neigh_inter.plot <- pupa.plot$neigh_inter

pupa.1 <- dplyr::full_join(pupa.7.5, pupa.1m, by= c("Plot", "Seed","num.plantas", "Subplot",  "Plant_Simple", "edge" , "Fruit"))

pupa.2 <- dplyr::full_join(pupa.3m, pupa.plot, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge", "Fruit"))

pupa.total <- dplyr::full_join(pupa.1, pupa.2, by= c("Plot", "Seed","num.plantas", "Subplot", "Plant_Simple", "edge" , "Fruit"))
head(pupa.total) 

vecinos.pupa <- pupa.total[,c("Plot","Subplot","Plant_Simple","num.plantas", "Fruit","Seed", "neigh_inter.plot","neigh_intra.plot",
                              "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 

##################################################3. GLM#######################################################################

###HERE IS ONE OPTION FOLLOWING NLME.

#FIRST FIT THE RANDOM STRUCTURE. #we are gonna work with to types of spatial autcorrelation types exponential and poisson. 
# Do first example with CHFU (Maria you do the rest)

library(MuMIn)
library(multcomp)
library(emmeans)
library(sjPlot)
library(sjmisc)
#>CHFU----
CHFU.vis <- subset(pol, Plant_Simple == "CHFU")
CHFU.vis$unique_id <- paste(CHFU.vis$Plot, CHFU.vis$Subplot, sep="_")
CHFU$unique_id <- paste(CHFU$Plot, CHFU$Subplot, sep="_")
solochfu <- subset(vecinos.chfu.lema, Plant_Simple == 'CHFU')
solochfu$unique_id <- paste(solochfu$Plot, solochfu$Subplot, sep="_")
CHFU.dis <- full_join(CHFU, disfinal) 
CHFU.dis$unique_id <- paste(CHFU.dis$Plot, CHFU.dis$Subplot, sep="_")

chfu$unique_id <- paste(chfu$Plot, chfu$Subplot, sep="_")

chfu <- full_join(CHFU.vis,CHFU.dis, by=c("Plot", "Subplot", "unique_id", "Seed", "Fruit", "Plant_Simple", "num.plantas")) %>%
    group_by(unique_id)%>%
    mutate(seeds = mean(Seed, na.rm=TRUE), visits = mean(visitas_indv_hora, na.rm = TRUE))%>%
  ungroup(unique_id) %>% 
  distinct(unique_id, .keep_all=TRUE) 

chfu.test<-  full_join(chfu, solochfu, by=c("Plot", "Subplot", "Plant_Simple","Seed", "Fruit", "num.plantas","unique_id")) %>%
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
    ungroup(unique_id) 
#not like distances matices equal to 0.
chfu.1 <- na.omit(chfu.test)

chfu.test.2<-  left_join(solochfu, chfu ,by=c("Plot", "Subplot", "Plant_Simple","Seed", "Fruit", "num.plantas","unique_id")) %>%
  group_by(unique_id)%>%
  distinct(unique_id, .keep_all=TRUE)%>%
  ungroup(unique_id) 

chfu.test.2$visits[is.na(chfu.test.2$visits)] <- 0
chfu.test.2$num.visits[is.na(chfu.test.2$num.visits)] <- 0
chfu.test.2$visitas_indv_hora[is.na(chfu.test.2$visitas_indv_hora)] <- 0
chfu.test.2$seeds[is.na(chfu.test.2$seeds)] <- 0
chfu.test.2$Group <- as.character(chfu.test.2$Group)
#chfu.test.2$Group[is.na(chfu.test.2$Group)] <- 'NONE'


chfu.1.simple<- chfu.test.2[,c("Plot", "Subplot","Group", "num.visits","visitas_indv_hora", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", 
                    "neigh_inter.plot","neigh_intra.plot",
                    "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
                #data of CHFU with visits, fitness and neighbors
chfufu.final.fitness <- left_join(chfu.1.simple, disfinal, by=c("Plot", "Subplot"))
chfufu.final.fitness <- na.omit(chfufu.final.fitness)#en el grupo de polinizadores aparecen NAs que corresponden a las semillas que 
#                                                     no tienen polinizadores
chfu.simple <- chfu.1[,c("visits", "num.plantas" , "Fruit", "Seed",  "x_coor2", "y_coor2", 
                        "neigh_inter.1m", "neigh_intra.1m")]
graf.chfu <- cor(chfu.simple)
symnum(graf.chfu, legend = TRUE)

#different models
lCtr <- lmeControl(maxIter = 5000, msMaxIter = 5000, tolerance = 1e-9, niterEM = 250, msMaxEval = 200)
chfufu.final.fitness$Seed <-  as.numeric(chfufu.final.fitness$Seed)
#con ceros 
chufu.without0 <-subset(chfufu.final.fitness, Seed>0)

m1<- lme(log(Seed) ~ 1, data= chufu.without0,random = ~1 |Plot, control=lCtr,
           method = "ML")
m2 <- lme(log(Seed) ~ 1, data= chufu.without0, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m3 <- lme(log(Seed) ~ 1, data= chufu.without0, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4 <- lme(log(Seed) ~ 1, data= chufu.without0, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1, m2, m3, m4) #best model is m2

options(na.action = "na.fail")

chufu.without0$neigh_inter.1m <- as.numeric(chufu.without0$neigh_inter.1m)
chufu.without0$neigh_intra.1m <- as.numeric(chufu.without0$neigh_intra.1m)
m.prueba.chufu.2 <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= chufu.without0, random = ~1 |Plot, control=lCtr,
                corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m.prueba_sec.chufu.2 <- dredge(m.prueba.chufu.2, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec.chufu.2, "rank.call"))
fmList.prueba.chufu.2 <- get.models(m.prueba_sec.chufu.2, 1:4) 
summary(model.avg(fmList.prueba.chufu.2))#neigh inter 1m
plot(fitted(m.prueba.chufu.2), resid(m.prueba.chufu.2, type = "pearson"), main= "chfu fitness")
abline(0,0, col="red")
r.squaredGLMM(m.prueba.chufu.2)




    #different groups of pollinators
    #>>Fly----
chfu.fly <- subset(chufu.without0, Group== "Fly")%>% ungroup()
m1.f <- lme(log(Seed) ~ 1, data= chfu.fly,random = ~1 |Plot, control=lCtr,
          method = "ML")
m2.f <- lme(log(Seed) ~ 1, data= chfu.fly, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
m3.f <- lme(log(Seed) ~ 1, data= chfu.fly, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
m4.f <- lme(log(Seed) ~ 1, data= chfu.fly, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1.f, m2.f, m3.f, m4.f) # fit best, model m2.f

chfu.fly$neigh_intra.1m <- as.numeric(chfu.fly$neigh_intra.1m)
chfu.fly$neigh_inter.1m <- as.numeric(chfu.fly$neigh_inter.1m)
model2.fly.1 <- lme(log(Seed) ~ visitas_indv_hora +neigh_inter.1m*neigh_intra.1m, data= chfu.fly, random = ~1 |Plot, control=lCtr,
                  corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
model_sec2.fly.1 <- dredge(model2.fly.1, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_sec2.fly.1, "rank.call"))
fmList2.fly.1 <- get.models(model_sec2.fly.1, 1:4)
summary(model.avg(fmList2.fly.1)) #nothing important, no aparece si le afectan las visitas-----
plot(fitted(model2.fly.1), resid(model2.fly.1, type = "pearson"), main=" chfu fitness with fly visits")
abline(0,0, col="red")
r.squaredGLMM(model2.fly.1)

    #beetle
chfu.bet <- subset(chfufu.final.fitness, Group== "Beetle") #only 5 entries 
    #bee
chfu.be <- subset(chfufu.final.fitness, Group== "Bee") #only 5 entries

#visits 

m1.v<- lme(log(visits) ~ 1, data= chfu.1,random = ~1 |Plot, control=lCtr,
         method = "ML")
m2.v <- lme(log(visits) ~ 1, data= chfu.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

m3.v <- lme(log(visits) ~ 1, data= chfu.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4.v <- lme(log(visits) ~ 1, data= chfu.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1.v, m2.v, m3.v, m4.v) #best model is m2.v

model.chfu.vis <- lmer(log(visits) ~ Group + neigh_inter.1m *neigh_intra.1m + (1 |Plot), data= chfu.1,
                       corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") #funciona sin el control----
model_c.v <- dredge(model.chfu.vis, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_c.v, "rank.call"))
fmList.c.v <- get.models(model_c.v, 1:6) 
summary(model.avg(fmList.c.v))
isSingular(model.chfu.vis, tol = 1e-05)#tengo singularidad, varianzas cercanas a 0. No se explica muy bien el modelo. 
plot(fitted(model.chfu.vis), resid(model.chfu.vis, type = "pearson"),main= "fitness chfu visits")
abline(0,0, col="red")
r.squaredGLMM(model.chfu.vis)

#>LEMA---- 
LEMA.vis <- subset(pol, Plant_Simple == "LEMA")
LEMA.vis$unique_id <- paste(LEMA.vis$Plot, LEMA.vis$Subplot, sep="_")
LEMA$unique_id <- paste(LEMA$Plot, LEMA$Subplot, sep="_")
sololema <- subset(vecinos.chfu.lema, Plant_Simple == 'LEMA')
sololema$unique_id <- paste(sololema$Plot, sololema$Subplot, sep="_")

LEMA.dis <- full_join(LEMA, disfinal)
LEMA.dis$unique_id <- paste(LEMA.dis$Plot, LEMA.dis$Subplot, sep="_")
lema <- full_join(LEMA.vis,LEMA.dis, by=c("Plot", "Subplot", "unique_id", "Seed", "Fruit", "Plant_Simple", "num.plantas")) %>%
    group_by(unique_id)%>%
    mutate(seeds = mean(Seed, na.rm=TRUE), visits = mean(visitas_indv_hora, na.rm = TRUE)) %>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

lema.test<-  full_join(lema, sololema, by=c("Plot", "Subplot", "unique_id", "Plant_Simple", "Fruit", "Seed", "num.plantas", "Fruit", "Seed")) %>%
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

lema.test.2 <- left_join(sololema,lema,  by=c("Plot", "Subplot", "unique_id", "Plant_Simple", "Fruit", "Seed", "num.plantas", "Fruit", "Seed")) %>%
  group_by(unique_id)%>%
  distinct(unique_id, .keep_all=TRUE) %>%#this data base is for fitness. 
  ungroup()

lema.test.2$visits[is.na(lema.test.2$visits)] <- 0
lema.test.2$visitas_indv_hora[is.na(lema.test.2$visitas_indv_hora)] <- 0
lema.test.2$seeds[is.na(lema.test.2$seeds)] <- 0
lema.test.2$Plant_Simple[is.na(lema.test.2$Plant_Simple)] <- 'NONE'
lema.test.2$Group <- as.character(lema.test.2$Group)
#lema.test.2$Group[is.na(lema.test.2$Group)] <- 'NONE'

lema.bis <- lema.test.2[,c("Plot", "Subplot","Group", "visits", "Plant_Simple", "num.plantas" , "Fruit", "Seed", 
          "neigh_inter.plot","neigh_intra.plot",
          "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
lema.fitness <- left_join(lema.bis, disfinal, by= c("Plot", "Subplot"))
lema.1 <- na.omit(lema.test)
lema.1 <- lema.1[,c("Plot", "Subplot","Group", "visits", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", "x_coor2", "y_coor2", 
                    "neigh_inter.plot","neigh_intra.plot",
                    "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
lema.t <- subset(lema.1, Seed>0) 
lema.simple <- lema.t[,c("visits", "num.plantas" , "Fruit", "Seed",  "x_coor2", "y_coor2", 
                       "neigh_inter.1m", "neigh_intra.1m")]
graf.lema <- cor(lema.simple)
symnum(graf.lema, legend = TRUE)

#fitness
la <- lme(log(Seed) ~ 1, data= lema.1,random = ~1 |Plot, control=lCtr,
          method = "ML")
le <- lme(log(Seed) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
li <- lme(log(Seed) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
lo <- lme(log(Seed) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(la, le, li, lo) # best model l1. No space
lema.fitness$Group <- as.factor(lema.fitness$Group)
model3 <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= lema.fitness, random = ~1 |Plot, control=lCtr,
          method = "ML")
model_sec3 <- dredge(model3, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec3, "rank.call"))
fmList3 <- get.models(model_sec3, 1:4) 
summary(model.avg(fmList3)) #neighbors intra at 1m has relevance

plot(fitted(model3), resid(model3, type = "pearson"), main= "fitness lema")
abline(0,0, col="red")
r.squaredGLMM(model3)

#con ceros 
lema.without0 <-subset(lema.fitness, Seed>0)
l1.1 <- lme(log(Seed) ~ 1, data= lema.without0,random = ~1 |Plot, control=lCtr,
          method = "ML")
l2.1 <- lme(log(Seed) ~ 1, data= lema.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
l3.1 <- lme(log(Seed) ~ 1, data= lema.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.1 <- lme(log(Seed) ~ 1, data= lema.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.1, l2.1, l3.1, l4.1) # best model l1. No space
m.prueba <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= lema.without0, random = ~1 |Plot, control=lCtr,
                method = "ML") #Dont work
m.prueba_sec <- dredge(m.prueba, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec, "rank.call"))
fmList.prueba <- get.models(m.prueba_sec, 1:4) 
summary(model.avg(fmList.prueba))#neighbors intra 1m *** , me salen los mismos estimates que en el anterior, por lo que seran iguales
#sin ceros
m.prueba.1 <- lme(log(Seed) ~ visits*Group+neigh_inter.1m*neigh_intra.1m, data= lema.1, random = ~1 |Plot, control=lCtr,
                   method = "ML") #sin 0 funciona bien
m.prueba_sec1 <- dredge(m.prueba.1, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(m.prueba_sec1, "rank.call"))
fmList.prueba1 <- get.models(m.prueba_sec1, 1:4) 
summary(model.avg(fmList.prueba1))#sin ceros, neigh intra ***

            #different groups of pollinators
    #>>beetle----
lema.bet <- subset(lema.without0, Group== "Beetle")
l1.bet <- lme(log(Seed) ~ 1, data= lema.bet,random = ~1 |Plot, control=lCtr,
          method = "ML")
l2.bet <- lme(log(Seed) ~ 1, data= lema.bet, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
l3.bet <- lme(log(Seed) ~ 1, data= lema.bet, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.bet <- lme(log(Seed) ~ 1, data= lema.bet, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.bet, l2.bet, l3.bet, l4.bet) # best model without space. l1.bet
model3.bet <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= lema.bet, random = ~1 |Plot, control=lCtr,
               method = "ML")
model_sec3.bet <- dredge(model3.bet, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_sec3.bet, "rank.call"))
fmList3.bet <- get.models(model_sec3.bet, 1:4)
summary(model.avg(fmList3.bet)) #relevance: neigh_intra.1m**
plot(fitted(model3.bet), resid(model3.bet, type = "pearson"), main= "fitness lema with beetle visits")
abline(0,0, col="red")
r.squaredGLMM(model3.bet)


    #>>fly---- 
lema.fly <- subset(lema.without0, Group== "Fly")
l1.f <- lme(log(Seed) ~ 1, data= lema.fly,random = ~1 |Plot, control=lCtr, method = "ML")
l2.f <- lme(log(Seed) ~ 1, data= lema.fly, random = ~1 |Plot, control=lCtr,
              corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
l3.f <- lme(log(Seed) ~ 1, data= lema.fly, random = ~1 |Plot, control=lCtr,
              corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.f <- lme(log(Seed) ~ 1, data= lema.fly, random = ~1 |Plot, control=lCtr,
              corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.f, l2.f, l3.f, l4.f) # best model l2.f
model3.fly <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= lema.fly, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
model_sec3.fly <- dredge(model3.fly, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_sec3.fly, "rank.call"))
fmList3.fly <- get.models(model_sec3.fly, 1:4) #best model: Seed ~ 1
summary(model.avg(fmList3.fly)) #neigh intra 1m **
plot(fitted(model3.fly), resid(model3.fly, type = "pearson"), main= "fitness lema with fly visits")
abline(0,0, col="red")
r.squaredGLMM(model3.fly)

    #>>bee----
lema.bee <- subset(lema.without0, Group== "Bee")
l1.bee <- lme(log(Seed) ~ 1, data= lema.bee,random = ~1 |Plot, control=lCtr, method = "ML")
l2.bee <- lme(log(Seed) ~ 1, data= lema.bee, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
l3.bee <- lme(log(Seed) ~ 1, data= lema.bee, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.bee <- lme(log(Seed) ~ 1, data= lema.bee, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.bee, l2.bee, l3.bee, l4.bee) # best model without spatial correlation
model3.fly <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= lema.bee, random = ~1 |Plot, control=lCtr,
                  method = "ML")
model_sec3.fly <- dredge(model3.fly, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_sec3.fly, "rank.call"))
fmList3.fly <- get.models(model_sec3.fly, 1:4) 
summary(model.avg(fmList3.fly))#nothing significant



#visits 

l1.v <- lme(log(visits) ~ 1, data= lema.1,random = ~1 |Plot, control=lCtr,
          method = "ML")
l2.v <- lme(log(visits) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
l3.v <- lme(log(visits) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.v <- lme(log(visits) ~ 1, data= lema.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.v, l2.v, l3.v, l4.v)   #the best model is the type gaussian

model3.v <- lme(log(visits) ~ Group+neigh_inter.1m*neigh_intra.1m, data= lema.1, random = ~1 |Plot, control=lCtr,
              corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
model_sec3.v <- dredge(model3.v, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec3.v, "rank.call"))
fmList3.v <- get.models(model_sec3.v, 1:4) #best model: visits ~ neigh_intra.1m + 1 
summary(model.avg(fmList3.v)) #the neighbors intra*** at 1m affects strong negatively to the visits of LEMA, and the group fly**
#  also affects negatively (less strong that the nieghbors)
plot(fitted(model3.v), resid(model3.v, type = "pearson"), main= "LEMA visits")
abline(0,0, col="red")
r.squaredGLMM(model3.v)
#>MESU----
MESU.vis <- subset(pol, Plant_Simple == "MESU")
MESU.vis$unique_id <- paste(MESU.vis$Plot, MESU.vis$Subplot, sep="_")
MESU$unique_id <- paste(MESU$Plot, MESU$Subplot, sep="_")
solomesu <- subset(vecinos.mesu, Plant_Simple == 'MESU')
solomesu$unique_id <- paste(solomesu$Plot, solomesu$Subplot, sep="_")
MESU.dis <- full_join(MESU, disfinal)
MESU.dis$unique_id <- paste(MESU.dis$Plot, MESU.dis$Subplot, sep="_")
mesu <- full_join(MESU.vis,MESU.dis, by=c("Plot", "Subplot", "unique_id", "Seed", "Fruit", "Plant_Simple", "num.plantas")) %>%
    group_by(unique_id)%>%
    mutate(seeds = mean(Seed, na.rm=TRUE), visits = mean(visitas_indv_hora, na.rm = TRUE)) %>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

mesu.test<-  full_join(mesu, solomesu, by=c("Plot", "Subplot", "unique_id", "Plant_Simple", "Fruit", "Seed", "num.plantas", "Fruit", "Seed")) %>%
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()
mesu.1 <- na.omit(mesu.test)
mesu.1 <- mesu.1[,c("Plot", "Subplot","Group", "visits", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", "x_coor2", "y_coor2", 
                    "neigh_inter.plot","neigh_intra.plot",
                    "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
mesu.t <- subset(mesu.1, Seed>0)
mesu.simple <- mesu.t[,c("visits", "num.plantas" , "Fruit", "Seed",  "x_coor2", "y_coor2", 
                         "neigh_inter.plot","neigh_intra.plot",
                         "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]
                            #only 9 entries!!
graf.mesu <- cor(mesu.simple)
symnum(graf.mesu, legend = TRUE)

mesu.test.2<-  left_join(solomesu, mesu ,by=c("Plot", "Subplot", "Plant_Simple","Seed", "Fruit", "num.plantas","unique_id")) %>%
  group_by(unique_id)%>%
  distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

mesu.test.2$visits[is.na(mesu.test.2$visits)] <- 0
mesu.test.2$visitas_indv_hora[is.na(mesu.test.2$visitas_indv_hora)] <- 0
mesu.test.2$seeds[is.na(mesu.test.2$seeds)] <- 0
mesu.test.2$Group <- as.character(mesu.test.2$Group)
#mesu.test.2$Group[is.na(mesu.test.2$Group)] <- 'NONE'
mesu.1.simple<- mesu.test.2[,c("Plot", "Subplot","Group", "visitas_indv_hora", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", 
                               "neigh_inter.plot","neigh_intra.plot",
                               "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
#data of CHFU with visits, fitness and neighbors
mesu.final.fitness <- left_join(mesu.1.simple, disfinal, by=c("Plot", "Subplot"))
#mesu.final.fitness <- na.omit(mesu.final.fitness)


    #models fitness
me1 <- lme(log(Seed) ~ 1, data= mesu.final.fitness,random = ~1 |Plot, control=lCtr,
          method = "ML")
me2 <- lme(log(Seed) ~ 1, data= mesu.final.fitness, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
me3 <- lme(log(Seed) ~ 1, data= mesu.final.fitness, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
me4 <- lme(log(Seed) ~ 1, data= mesu.final.fitness, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(me1, me2, me3, me4) # best model me1, sin estructura espacial
    
model4 <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= mesu.final.fitness, random = ~1 |Plot, control=lCtr,
            method = "ML") #no works
model_sec4.me <- dredge(model4, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(model_sec4.me, "rank.call"))
fmList4.me <- get.models(model_sec4.me, 1:4) #best model: Seed ~ neigh_inter.1m + 1 
summary(model.avg(fmList4.me))#could be important neigh inter 1m
plot(fitted(model4), resid(model4, type = "pearson"), main= "MESU fitness")
abline(0,0, col="red")
r.squaredGLMM(model4)

#con ceros 
mesu.without0 <-subset(mesu.final.fitness, Seed>0)
mesu.prueba <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= mesu.without0, random = ~1 |Plot, control=lCtr,
                corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")

    #only 9 entries, no works 


#sin ceros
mesu.prueba.1 <- lme(Seed ~ visitsGroup+neigh_inter.1m*neigh_intra.1m, data= mesu.final.fitness, random = ~1 |Plot, control=lCtr,
                  corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")#no works

#models visits
mesu.final.visits <- na.omit(mesu.final.fitness)
me1.v <- lme(log(visitas_indv_hora) ~ 1, data= mesu.final.visits,random = ~1 |Plot, control=lCtr,
           method = "ML")
me2.v <- lme(log(visitas_indv_hora) ~ 1, data= mesu.final.visits, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
me3.v <- lme(log(visitas_indv_hora) ~ 1, data= mesu.final.visits, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
me4.v <- lme(log(visitas_indv_hora) ~ 1, data= mesu.final.visits, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(me1.v, me2.v, me3.v, me4.v) # best model me1, sin estructura espacial

model4.v <- lme(log(visitas_indv_hora) ~ Group+neigh_inter.1m*neigh_intra.1m, data= mesu.final.visits, random = ~1 |Plot, control=lCtr,
              method = "ML") #no works
plot(fitted(model4.v), resid(model4.v, type = "pearson"), main= "MESU visits")
abline(0,0, col="red")
r.squaredGLMM(model4.v)

#>PUPA----
PUPA.vis <- subset(pol, Plant_Simple == "PUPA")

PUPA.vis$unique_id <- paste(PUPA.vis$Plot, PUPA.vis$Subplot, sep="_")

PUPA$unique_id <- paste(PUPA$Plot, PUPA$Subplot, sep="_")
solopupa <- subset(vecinos.pupa, Plant_Simple == 'PUPA')
solopupa$unique_id <- paste(solopupa$Plot, solopupa$Subplot, sep="_")

PUPA.dis <- full_join(PUPA, disfinal)
PUPA.dis$unique_id <- paste(PUPA.dis$Plot, PUPA.dis$Subplot, sep="_")
pupa <- full_join(PUPA.vis,PUPA.dis, by=c("Plot", "Subplot", "unique_id", "Seed", "Fruit", "Plant_Simple", "num.plantas")) %>%
    group_by(unique_id)%>%
    mutate(seeds = mean(Seed, na.rm=TRUE), visits = mean(visitas_indv_hora, na.rm = TRUE)) %>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

pupa.test<-  full_join(pupa, solopupa, by=c("Plot", "Subplot", "unique_id", "Plant_Simple", "Fruit", "Seed", "num.plantas", "Fruit", "Seed")) %>%
    group_by(unique_id)%>%
    distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()
pupa.1 <- na.omit(pupa.test)
pupa.1 <- pupa.1[,c("Plot", "Subplot","Group", "visits", "num.visits", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", "x_coor2", "y_coor2", 
                    "neigh_inter.plot","neigh_intra.plot",
                    "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
pupa.t <- subset(pupa.1, Seed>0)
pupa.simple <- pupa.t[,c("visits", "num.plantas" , "Fruit", "Seed",  "x_coor2", "y_coor2", 
                         "neigh_inter.plot","neigh_intra.plot",
                         "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]
                            

pupa.test.2<-  left_join(solopupa, pupa ,by=c("Plot", "Subplot", "Plant_Simple","Seed", "Fruit", "num.plantas","unique_id")) %>%
  group_by(unique_id)%>%
  distinct(unique_id, .keep_all=TRUE)%>%
  ungroup()

pupa.test.2$visits[is.na(pupa.test.2$visits)] <- 0
pupa.test.2$visitas_indv_hora[is.na(pupa.test.2$visitas_indv_hora)] <- 0
pupa.test.2$seeds[is.na(pupa.test.2$seeds)] <- 0
pupa.test.2$Group <- as.character(pupa.test.2$Group)
#pupa.test.2$Group[is.na(pupa.test.2$Group)] <- 'NONE'
pupa.1.simple<- pupa.test.2[,c("Plot", "Subplot","Group", "visitas_indv_hora", "num.visits", "Plant_Simple", "num.plantas" , "Fruit", "Seed", "unique_id", 
                               "neigh_inter.plot","neigh_intra.plot",
                               "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
#data of CHFU with visits, fitness and neighbors
pupa.final.fitness <- left_join(pupa.1.simple, disfinal, by=c("Plot", "Subplot"))
pupa.final.fitness <- na.omit(pupa.final.fitness)

graf.pupa <- cor(pupa.simple)
symnum(graf.pupa, legend = TRUE)
pupa.without0 <- subset(pupa.final.fitness, Seed>0)

p1 <- lme(log(Seed) ~ 1, data= pupa.1,random = ~1 |Plot, control=lCtr,
           method = "ML")
p2 <- lme(log(Seed) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
p3 <- lme(log(Seed) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4 <- lme(log(Seed) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
           corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1, p2, p3, p4) # best model p1, without space

model5.1.c <- lme(log(Seed) ~ visitas_indv_hora*Group + neigh_inter.1m*neigh_intra.1m, data= pupa.final.fitness, random = ~1 |Plot, control=lCtr,
           method = "ML") #interaction of the model dont appears ----
model_sec5.1.c <- dredge(model5.1, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec5.1.c, "rank.call"))
fmList5.1.c <- get.models(model_sec5.1.c, 1:8) 
summary(model.avg(fmList5.1.c))# nothing significant
plot(fitted(model5.1.c), resid(model5.1.c, type = "pearson"), main= "PUPA fitness")
abline(0,0, col="red")
r.squaredGLMM(model5.1.c)
#con ceros 
pupa.without0 <- subset(pupa.final.fitness, Seed>0)
p1.1 <- lme(log(Seed) ~ 1, data= pupa.without0,random = ~1 |Plot, control=lCtr,
          method = "ML")
p2.1 <- lme(log(Seed) ~ 1, data= pupa.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
p3.1 <- lme(log(Seed) ~ 1, data= pupa.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4.1 <- lme(log(Seed) ~ 1, data= pupa.without0, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1.1, p2.1, p3.1, p4.1) #no space

pupa.prueba <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= pupa.without0, random = ~1 |Plot, control=lCtr,
                    method = "ML")
pupa.prueba_sec <- dredge(pupa.prueba, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(pupa.prueba_sec, "rank.call"))
fmList.pupa <- get.models(pupa.prueba_sec, 1:4) 
summary(model.avg(fmList.pupa))#nothing relevance
plot(fitted(pupa.prueba), resid(pupa.prueba, type = "pearson"), main= "PUPA fitness")
abline(0,0, col="red")
r.squaredGLMM(pupa.prueba)

#sin ceros
pupa.prueba.1 <- lme(log(Seed) ~ visits+neigh_inter.1m*neigh_intra.1m, data= pupa.1, random = ~1 |Plot, control=lCtr,
                     corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
pupa.prueba_sec1 <- dredge(pupa.prueba.1, trace = TRUE, rank = "AICc", REML = FALSE)
(attr(pupa.prueba_sec1, "rank.call"))
fmList.pupa1 <- get.models(pupa.prueba_sec1, 1:4) 
summary(model.avg(fmList.pupa1))#sin ceros, nothing important. 
plot(fitted(pupa.prueba.1), resid(pupa.prueba.1, type = "pearson"), main= "PUPA fitness")
abline(0,0, col="red")
r.squaredGLMM(pupa.prueba.1)
              #pollinators groups
    #>>bee----
pupa.bee <- subset(pupa.without0, Group== "Bee" )
p1.b <- lme(log(Seed) ~ 1, data= pupa.bee,random = ~1 |Plot, control=lCtr,
          method = "ML")
p2.b <- lme(log(Seed) ~ 1, data= pupa.bee, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
p3.b <- lme(log(Seed) ~ 1, data= pupa.bee, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4.b <- lme(log(Seed) ~ 1, data= pupa.bee, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1.b, p2.b, p3.b, p4.b) # best model no space
model.p1.bee <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= pupa.bee, random = ~1 |Plot, control=lCtr,
                      method = "ML")
model_sec5.1.bee <- dredge(model.p1.bee, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec5.1.bee, "rank.call"))
fmList5.1.bee <- get.models(model_sec5.1.bee, 1:4) 
summary(model.avg(fmList5.1.bee))#nothing significant

    #>>butterflies----
pupa.but <- subset(pupa.without0, Group== "Butterfly" )
p1.but <- lme(log(Seed) ~ 1, data= pupa.but,random = ~1 |Plot, control=lCtr,
            method = "ML")
p2.but <- lme(log(Seed) ~ 1, data= pupa.but, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
p3.but <- lme(log(Seed) ~ 1, data= pupa.but, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4.but <- lme(log(Seed) ~ 1, data= pupa.but, random = ~1 |Plot, control=lCtr,
            corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1.but, p2.but, p3.but, p4.but) # best model no space
pupa.but.model <- lme(log(Seed) ~ visitas_indv_hora+neigh_inter.1m*neigh_intra.1m, data= pupa.but, random = ~1 |Plot, control=lCtr,
                       method = "ML")
model_sec5.1.but <- dredge(model.p1.bee, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec5.1.but, "rank.call"))
fmList5.1.but <- get.models(model_sec5.1.but, 1:4) 
summary(model.avg(fmList5.1.but))  #nothing significant          

    #flies only 6 entries,and no beetles

#visits
p1.v <- lme(log(visits) ~ 1, data= pupa.1,random = ~1 |Plot, control=lCtr,
          method = "ML")
p2.v <- lme(log(visits) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
p3.v <- lme(log(visits) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4.v <- lme(log(visits) ~ 1, data= pupa.1, random = ~1 |Plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1.v, p2.v, p3.v, p4.v) # no space

model5.1.v <- lme(log(visits) ~ Group+neigh_inter.1m*neigh_intra.1m, data= pupa.1, random = ~1 |Plot, control=lCtr,
                 method = "ML")
model_sec5.1.v <- dredge(model5.1.v, trace = TRUE, rank = "AICc", REML = FALSE) 
(attr(model_sec5.1.v, "rank.call"))
fmList5.1.v <- get.models(model_sec5.1.v, 1:8)
summary(model.avg(fmList5.1.v))#neigh intra 1m important***. 
plot(fitted(model5.1.v), resid(model5.1.v, type = "pearson"), main= "PUPA visits")
abline(0,0, col="red")
r.squaredGLMM(model5.1.v)


#SEM multigroup ----
#Ahora tengo que hacer un SEM multigroup por especie de planta, en el que tengo que incluir los vecinos inter e intra
# a todas las escalas y los polinizadores que me hayan dado algún tipo de importancia

library(dplyr)
library(corrgram)
library(vegan)
library(varhandle)
#install.packages("semPlot")
#install.packages("matrixcalc")
#install.packages("mi")
#install.packages("corrgram")
library(semPlot)

corrgram (pupa.1, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="PUPA correlations")
#------------------
library(lavaan)
#SEM para lema
#diferentes variables de LEMA: el fitness de lema depende de los vecinos que estan 1m intra (si eso de los vecinos inter plot),
# y las visitas dependen de 1 m inter 
#voy a poner semillas en positivo con el log
library(scales)
chfu.sem.prueba <- chfu.1
chfu.sem.prueba$Seed <- rescale(chfu.sem.prueba$Seed)
chfu.sem.prueba$neigh_inter.plot <- rescale(chfu.sem.prueba$neigh_inter.plot)
chfu.sem.prueba$neigh_intra.plot <- rescale(chfu.sem.prueba$neigh_intra.plot)
chfu.sem.prueba$neigh_intra.3m  <- rescale(chfu.sem.prueba$neigh_intra.3m )
chfu.sem.prueba$neigh_inter.3m <- rescale(chfu.sem.prueba$neigh_inter.3m)
chfu.sem.prueba$neigh_intra.1m <- rescale(chfu.sem.prueba$neigh_intra.1m)
chfu.sem.prueba$neigh_inter.1m <- rescale(chfu.sem.prueba$neigh_inter.1m)
chfu.sem.prueba$neigh_intra.7.5 <- rescale(chfu.sem.prueba$neigh_intra.7.5)
chfu.sem.prueba$neigh_inter.7.5 <- rescale(chfu.sem.prueba$neigh_inter.7.5)
#pupa.sem.prueba$neigh_inter.1m <- as.integrate(pupa.sem.prueba$neigh_inter.1m)
#chfu.sem.prueba$neigh_inter.1m <- log(chfu.sem.prueba$neigh_inter.1m) #muchos NAS, no se puede hacer el log porque hay 0s. 
#pupa.sem.prueba$neigh_inter.7.5 <- log(pupa.sem.prueba$neigh_inter.7.5)
#pupa.sem.prueba$neigh_intra.7.5 <- log(pupa.sem.prueba$neigh_intra.7.5)
chfu.sem.prueba$neigh_inter.7.5 <- as.numeric(chfu.sem.prueba$neigh_inter.7.5)
chfu.sem.prueba$neigh_intra.7.5 <- as.numeric(chfu.sem.prueba$neigh_intra.7.5)
chfu.sem.prueba$neigh_inter.1m <- as.numeric(chfu.sem.prueba$neigh_inter.1m)
chfu.sem.prueba$Group <- as.factor(chfu.sem.prueba$Group)

pupa.sem1 <- pupa.sem.prueba[,c("Group", "num.visits", "Seed", "neigh_inter.plot","neigh_intra.plot",
          "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")]


#los grupos los tengo que cambiar a numeros, para poderlos meter en el modelo

parameterEstimates(pupa.sem1, standardized=TRUE)

modelo <- '
#regresions
#Plant_fitness =
num.visits ~ Seed
Seed ~ neigh_inter.7.5
Seed~neigh_intra.7.5
Seed~neigh_intra.1m
Seed~neigh_inter.1m
Seed~neigh_intra.3m
Seed~neigh_inter.3m
Seed~neigh_inter.plot
Seed~neigh_intra.plot
num.visits~neigh_intra.7.5
num.visits~neigh_inter.7.5
num.visits~neigh_intra.1m
num.visits~neigh_inter.1m
num.visits~neigh_intra.3m
num.visits~neigh_inter.3m
num.visits~neigh_inter.plot
num.visits~neigh_intra.plot
num.visits~Fly
neigh_intra.7.5~Fly
neigh_inter.7.5~Fly
neigh_inter.1m~Fly
neigh_intra.3m~Fly
neigh_inter.3m~Fly
neigh_inter.plot~Fly
neigh_intra.plot~Fly

#intercept
'
modelo1 <- '
Seed =~ num.visits+Group + neigh_intra.1m+neigh_inter.1m+neigh_inter.plot
'
#install.packages('dummies')
library(dummies)
library(tidyverse)
library(sjmisc)
dummy <- to_dummy(chfu.sem.prueba$Group,var.name = "groups")
litter<-cbind(dummy,chfu.sem.prueba)
litter$Fly1 <- litter$groups_3
litter$Beetle1 <- litter$groups_2
litter$Bee1 <- litter$groups_1
litter <-spread(litter, Group, visits, fill = 0, convert = FALSE,
       drop = TRUE, sep = NULL) 


#N.big =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m
#N.small=~neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
#num.visits ~ neigh_inter.plot+neigh_intra.plot+neigh_intra.1m+neigh_inter.1m
#N.small~~N.big

modelo3 <- 'Seed ~ visits+Group + neigh_intra.7.5+neigh_intra.7.5+neigh_intra.1m+neigh_inter.1m
          N.big =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m
          N.small=~neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          num.visits ~ Group+neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          N.small~~N.big'
library(lavaan)
#◙install.packages('corrgram')
#install.packages('seriation')
library(corrgram)
corrgram (chfu.sem.prueba, order=TRUE, lower.panel=panel.conf, upper.panel=panel.pts, text.panel=panel.txt,
          diag.panel=panel.density, pch=16, lty=1, main="Environmental correlations")
litter2<- litter[,c("visits", "Seed", "visitas_indv_hora", 
               "neigh_inter.plot","neigh_intra.plot",
               "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
vif(litter2)
cor(litter2)
fit2 <- cfa(model =modelo3,data= litter,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE, ordered=c("Group")) #me salen mogollon de warnings       
varTable(fit1)
summary(fit1,fit.measures = TRUE, standardized = TRUE)
summary(fit1)
modindices(fit1, sort = TRUE, maximum.number = 10)

multigroup2 <- sem(modelo, litter, group = "group") 

#  c=~neigh_inter.plot+neigh_inter.3m+neigh_intra.7.5
# d=~neigh_intra.plot+neigh_inter.3m+neigh_intra.1m
#  e=~neigh_inter.plot+neigh_inter.3m+neigh_intra.1m
#  f=~neigh_inter.1m+neigh_inter.3m
# h=~neigh_inter.7.5+neigh_intra.1m
install.packages("semPlot")
library(semPlot)
install.packages('arm')
install.packages('Hmisc')
library(arm)
latent_formula2 <- '
#regresions
#Plant_Fitness=
    N =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
    Fly ~ N
   Seed ~ Fly + N
#intercept'
latent_model2 <- sem(latent_formula2, litter)
m22 <- cfa(model =latent_model2,data= litter,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE)
vartable(m22)
summary(latent_model2, standardize = T, rsq = T)
summary(m22, fit.measures=TRUE)
print(modindices(latent_model2))




latent_formula4 <- '
#regresions
#Plant_Fitness=
          Seed ~ Fly+ neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          Fly ~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
          
#intercept'
latent_model4 <- sem(latent_formula4, litter)
m.model4 <- cfa(model =latent_model4,data= litter,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE)
vartable(m.model4)
summary(latent_model4, standardize = T, rsq = T)
print(modindices(latent_model4))




latent_formula6 <- '
#regresions
#Plant_Fitness=
    N =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
    Fly ~ N
   Seed ~ Fly + N
   neigh_intra.3m ~~   neigh_intra.1m
   neigh_inter.3m ~~   neigh_inter.1m
   neigh_intra.plot ~~   neigh_intra.3m
   neigh_intra.plot ~~   neigh_intra.1m 
   #intercept'
latent_model6 <- sem(latent_formula6, litter)
m6 <- cfa(model =latent_model6,data= litter,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE, ordered=c("Group"))
vartable(m6)
summary(latent_model4, standardize = T, rsq = T)
print(modindices(latent_model6))



latent_formula9 <- '
#regresions
#Plant_Fitness=
    N =~neigh_inter.plot+neigh_intra.plot+neigh_intra.3m+neigh_inter.3m+neigh_intra.1m+neigh_inter.1m+neigh_inter.7.5+neigh_intra.7.5
       Seed ~ Fly + N
       neigh_inter.plot ~~   neigh_inter.3m
       neigh_inter.1m ~~ Seed
       neigh_intra.plot ~~   neigh_inter.1m
        neigh_intra.plot ~~   neigh_inter.1m 
   #intercept'
latent_model9 <- sem(latent_formula9, litter)
m9 <- cfa(model =latent_model9,data= litter,check.gradient = FALSE,std.lv=TRUE, orthogonal=TRUE, ordered=c("Group"))
vartable(m9)
summary(latent_model9, standardize = T, rsq = T)
print(modindices(latent_model9))
