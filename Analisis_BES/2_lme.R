#In these analysis I'm going to do lme. The response variable would be two different: one to see the fitness (seeds/one fruit and seed/fruit * fruits)
#and visits (count of visits and visits/flowers).For the predictable variable I'll include the neighbors inter 7.5 for fitness and for
#the visits at 1m
library(lme4)
library(nlme)
library(MuMIn)
library(predictmeans)

#fitness----
data <- read_csv2("Analisis_BES/data/Final_global_data.csv")
data$visits <- as.numeric(data$visits)
data <- as.data.frame(data)

#I'm going to create 2 different dataframe in order to can spread the number of visits (count) and the number of visit
# per flowers. For that, I create the two dataframes, I spread them, and after I join the columns.
data1 <- data %>% group_by(plot, subplot, Plant, Group, individuals, seed,seed.indv,fruit,visits,visits.flower, x_coor2, y_coor2,
                          neigh_inter.plot,neigh_intra.plot,neigh_intra.3m ,neigh_inter.3m, neigh_inter.1m ,neigh_intra.1m ,
                          neigh_inter.7.5,neigh_intra.7.5) %>% summarise (visits.total = sum(visits))%>%
    ungroup()
data2 <- data %>% group_by(plot, subplot, Plant, Group, individuals, seed,seed.indv,fruit,visits.flower, x_coor2, y_coor2,
                          neigh_inter.plot,neigh_intra.plot,neigh_intra.3m ,neigh_inter.3m, neigh_inter.1m ,neigh_intra.1m ,
                          neigh_inter.7.5,neigh_intra.7.5) %>% summarise (visits.total.fl = sum(visits.flower))%>%
    ungroup()


data1$unique_id <- paste(data1$plot, data1$subplot,data1$Plant,data1$Group, sep="_")
data1 <- data1 %>% distinct(unique_id, .keep_all = TRUE)
data2$unique_id <- paste(data2$plot, data2$subplot,data2$Plant,data2$Group, sep="_")
data2 <- data2 %>% distinct(unique_id, .keep_all = TRUE)
data1 <- data1[,c("plot","subplot","Plant", "Group", "individuals","seed","seed.indv","fruit","visits.total", "x_coor2", "y_coor2",
                 "neigh_inter.plot","neigh_intra.plot",
      "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
data2 <- data2[,c("plot","subplot","Plant", "Group", "individuals","seed","seed.indv","fruit","visits.total.fl", "x_coor2", "y_coor2",
                 "neigh_inter.plot","neigh_intra.plot",
                 "neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
data1$seed [is.na(data1$seed )] <- 0

data1<- na.omit(data1)
data.spread1 <- tidyr::spread(data1, Group, visits.total, fill = 0, convert = FALSE,
       drop = TRUE, sep = NULL)
data.spread1 <- na.omit(data.spread1)
data2 <- na.omit(data2)
data.spread2 <- tidyr::spread(data2, Group, visits.total.fl, fill = 0, convert = FALSE,
                              drop = TRUE, sep = NULL)
#i will need to join the columns of data.spread1 and data.spread2
data.spread2$Bee.fl <- data.spread2$Bee
data.spread2$Beetle.fl <- data.spread2$Beetle
data.spread2$Butterfly.fl <- data.spread2$Butterfly
data.spread2$Fly.fl <- data.spread2$Fly
data.spread2 <- data.spread2[,c("plot","subplot","Plant", "individuals","seed","seed.indv","fruit", "x_coor2", "y_coor2",
                                "Bee.fl",   "Beetle.fl",  "Butterfly.fl", "Fly.fl",              
         "neigh_inter.plot","neigh_intra.plot","neigh_intra.3m", "neigh_inter.3m", "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 

todo <- full_join(data.spread1, data.spread2)
head(as.data.frame(todo))


#control for the lme
lCtr <- lmeControl(maxIter = 5000, msMaxIter = 5000, tolerance = 1e-9, niterEM = 250, msMaxEval = 200)#this lmecontrol is
#                   going to be used in all the lme models for all the species
#>CHFU seeds/one fruit
CHFU.vis <- subset(todo, Plant == "CHFU")
hist (log(CHFU.vis$seed))
#fit random structure
m1<- lme(seed ~ 1, data= CHFU.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
m2 <- lme(seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

#m3 <- lme(seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
 #        corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")#no converge

m4 <- lme(seed ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1, m2, m4) #best model is m1, without coordenates

options(na.action = "na.fail")

#final model for CHFU
#head(CHFU.vis)
pairs(CHFU.vis[,c(5,7,10:25)]) #always good to see how it looks like.
m.prueba.chufu.3 <- lme(seed ~ Beetle+Fly+ Bee +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, 
                        data= CHFU.vis, random = ~1 |plot, control=lCtr,
                         method = "ML")
m.prueba.chufu.3.fl <- lme(seed ~ Beetle.fl+Fly.fl+ Bee.fl +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, data= CHFU.vis, random = ~1 |plot, control=lCtr,
                        method = "ML")

residplot(m.prueba.chufu.3)
residplot(m.prueba.chufu.3.fl)#I think that the one that fix better it is that one. 
#In this case, I am not worried by which has better residuals, but more explanatory power.
summary(m.prueba.chufu.3)
summary(m.prueba.chufu.3.fl)

m.prueba_sec.chufu.2 <- dredge(m.prueba.chufu.3.fl, 
                               trace = TRUE, rank = "AICc", 
                               REML = FALSE) #nothing so relevant
#IB: Why AICc and not AIC? Is this considered a low sample size?
(attr(m.prueba_sec.chufu.2, "rank.call"))
fmList.prueba.chufu.2 <- get.models(m.prueba_sec.chufu.2, 1:6) 
importance(m.prueba_sec.chufu.2) #MARIA, this is how you calculate importance. 
importance(fmList.prueba.chufu.2) #MARIA, this is how you calculate importance. 
#I think this is what we want to use to select variables.
summary(model.avg(fmList.prueba.chufu.2))# nothing
r.squaredGLMM(m.prueba.chufu.3)

#CHFU seeds per total fruits
#fit random structure
m1.fr<- lme(seed.indv ~ 1, data= CHFU.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
m2.fr <- lme(seed.indv ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")

#m3.fr <- lme(seed.indv ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
 #         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")

m4.fr <- lme(seed.indv ~ 1, data= CHFU.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(m1.fr, m2.fr, m4.fr) #best model is m1, without coordenates

options(na.action = "na.fail")
m.prueba.chufu.3.fr <- lme(seed.indv ~ Beetle+Fly+ Bee +neigh_inter.7.5+neigh_intra.7.5+neigh_intra.1m+neigh_inter.1m , data= CHFU.vis, random = ~1 |plot, control=lCtr,
                        method = "ML")
m.prueba.chufu.3.fr1 <- lme(seed.indv ~ Beetle.fl+Fly.fl+ Bee.fl +neigh_inter.7.5+neigh_intra.7.5+neigh_intra.1m+neigh_inter.1m , data= CHFU.vis, random = ~1 |plot, control=lCtr,
                           method = "ML")
residplot(m.prueba.chufu.3.fr)
residplot(m.prueba.chufu.3.fr1)#both models seem so similar, so I just try with the visits counts. 

CHFU.vis1 <- CHFU.vis[,c("individuals","seed","seed.indv","fruit","Bee", "Beetle", "Fly", "Butterfly", "x_coor2", "y_coor2",
        "neigh_inter.1m", "neigh_intra.1m" , "neigh_inter.7.5" , "neigh_intra.7.5")] 
cor(CHFU.vis1) #I thought that maybe were  correlation between the neighbors variables, no seems. 

m.prueba_sec.chufu.2.fr <- dredge(m.prueba.chufu.3.fr, trace = TRUE, rank = "AICc", REML = FALSE)#neigh intra 1m important
(attr(m.prueba_sec.chufu.2.fr, "rank.call"))
fmList.prueba.chufu.2.fr <- get.models(m.prueba_sec.chufu.2.fr, 1:6) 
summary(model.avg(fmList.prueba.chufu.2.fr))# neigh intra 1m
importance(fmList.prueba.chufu.2.fr) #MARIA, this is how you calculate importance. 
r.squaredGLMM(m.prueba.chufu.3.fr)

CHFU.vis$fittedvalues <- fitted(m.prueba.chufu.3.fr) #estas usando el modelo completo
g.chfu <- ggplot(CHFU.vis, aes(x = neigh_intra.1m))+
    geom_point(aes(y=seed.indv))+ #Aqui tenias seeds, pero el modelo lo haces con seed.idiv
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("CHFU fitness with neighbors intra at 1m")
g.chfu #no cuadra, duda IB: YA CUADRA.


#LEMA
#>LEMA seed/one fruit
LEMA.vis <- subset(todo, Plant == "LEMA") 
LEMA.vis1 <- LEMA.vis
LEMA.vis1$Bee.fl[LEMA.vis1$Bee.fl== Inf] <- 'NA' #I have problems with the INf numbers, so I change them to NA and
#           I delete them
LEMA.vis1$Beetle.fl[LEMA.vis1$Beetle.fl== Inf] <- 'NA'
LEMA.vis1$Butterfly.fl[LEMA.vis1$Butterfly.fl== Inf] <- 'NA'
LEMA.vis1$Fly.fl[LEMA.vis1$Fly.fl== Inf] <- 'NA'
LEMA.vis1$Beetle.fl <- as.numeric(LEMA.vis1$Beetle.fl)
LEMA.vis1$Butterfly.fl <- as.numeric(LEMA.vis1$Butterfly.fl)
LEMA.vis1$Fly.fl <- as.numeric(LEMA.vis1$Fly.fl)
LEMA.vis1$Bee.fl <- as.numeric(LEMA.vis1$Bee.fl)
LEMA.vis1 <- LEMA.vis1[!is.na(LEMA.vis1$Beetle.fl),]
LEMA.vis1 <- LEMA.vis1[!is.na(LEMA.vis1$Fly.fl),]
LEMA.vis1 <- LEMA.vis1[!is.na(LEMA.vis1$Butterfly.fl),]
LEMA.vis1 <- LEMA.vis1[!is.na(LEMA.vis1$Bee.fl),]#this is the dataframe for the visits/flowers

#random structure
l1<- lme(seed ~ 1, data= LEMA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
l2 <- lme(seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
l3<- lme(seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4 <- lme(seed ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1, l2, l3, l4) #best model is l1

options(na.action = "na.fail")
m.prueba.lema.2 <- lme(seed ~ Beetle+Fly+ Bee+Butterfly +neigh_inter.1m+neigh_intra.1m, data= LEMA.vis, random = ~1 |plot, control=lCtr,
                       method = "ML") 

m.prueba.lema.2.fl <- lme(seed ~ Beetle.fl+Fly.fl+ Bee.fl+Butterfly.fl +neigh_inter.1m+neigh_intra.1m, data= LEMA.vis1, random = ~1 |plot, control=lCtr,
                       method = "ML")

residplot(m.prueba.lema.2)#This one is better. better the count of visits in this case
residplot(m.prueba.lema.2.fl)

m.prueba_sec.lema.2 <- dredge(m.prueba.lema.2, trace = TRUE, rank = "AICc", REML = FALSE)#Maybe neighbors intra 1m and beetles
(attr(m.prueba_sec.lema.2, "rank.call"))
fmList.prueba.lema.2 <- get.models(m.prueba_sec.lema.2, 1:4) 
summary(model.avg(fmList.prueba.lema.2))# 
importance(fmList.prueba.lema.2) #nice... beetles is positive! 


#>LEMA (seed/one fruit)*fruits
#random structure
l1.fr<- lme(seed.indv ~ 1, data= LEMA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
l2.fr <- lme(seed.indv ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
l3.fr<- lme(seed.indv ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
l4.fr <- lme(seed.indv ~ 1, data= LEMA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(l1.fr, l2.fr, l3.fr, l4.fr) #best model is l1

options(na.action = "na.fail")
m.prueba.lema.2.fr <- lme(seed.indv ~ Beetle+Fly+ Bee+Butterfly +neigh_inter.1m+neigh_intra.1m, data= LEMA.vis, random = ~1 |plot, control=lCtr,
                       method = "ML") 
m.prueba.lema.2.fr.fl <- lme(seed.indv ~ Beetle.fl+Fly.fl+ Bee.fl+Butterfly.fl +neigh_inter.1m+neigh_intra.1m, data= LEMA.vis1, random = ~1 |plot, control=lCtr,
                          method = "ML") 
residplot(m.prueba.lema.2.fr)
residplot(m.prueba.lema.2.fr.fl)#both models seem similar, for that reason I chose the first one.The visits count


m.prueba_sec.lema.2.fr <- dredge(m.prueba.lema.2.fr, trace = TRUE, rank = "AICc", REML = FALSE)#nothing
fmList.prueba.lema.2.fr <- get.models(m.prueba_sec.lema.2.fr, 1:4) 
summary(model.avg(fmList.prueba.lema.2.fr))# 
importance(fmList.prueba.lema.2.fr)

#>PUPA
PUPA.vis <- subset(todo, Plant == "PUPA") 

PUPA.vis1 <- PUPA.vis
PUPA.vis1$Bee.fl[PUPA.vis1$Bee.fl== Inf] <- 'NA' #I have problems with the INf numbers, so I change them to NA and
#           I delete them
PUPA.vis1$Beetle.fl[PUPA.vis1$Beetle.fl== Inf] <- 'NA'
PUPA.vis1$Butterfly.fl[PUPA.vis1$Butterfly.fl== Inf] <- 'NA'
PUPA.vis1$Fly.fl[PUPA.vis1$Fly.fl== Inf] <- 'NA'
PUPA.vis1$Beetle.fl <- as.numeric(PUPA.vis1$Beetle.fl)
PUPA.vis1$Butterfly.fl <- as.numeric(PUPA.vis1$Butterfly.fl)
PUPA.vis1$Fly.fl <- as.numeric(PUPA.vis1$Fly.fl)
PUPA.vis1$Bee.fl <- as.numeric(PUPA.vis1$Bee.fl)
PUPA.vis1 <- PUPA.vis1[!is.na(PUPA.vis1$Beetle.fl),]
PUPA.vis1 <- PUPA.vis1[!is.na(PUPA.vis1$Fly.fl),]
PUPA.vis1 <- PUPA.vis1[!is.na(PUPA.vis1$Butterfly.fl),]
PUPA.vis1 <- PUPA.vis1[!is.na(PUPA.vis1$Bee.fl),]


#seed/one fruit
#random structure
p1<- lme(seed ~ 1, data= PUPA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
p2 <- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
p3<- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4 <- lme(seed ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1, p2, p3, p4) #best model is p1

options(na.action = "na.fail")

#I have two models because I'm not sure of which model is best seeing the residuals. The log model or without log?
m.prueba.pupa.2 <- lme(seed ~ Beetle+Fly+ Bee +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, data= PUPA.vis, random = ~1 |plot, control=lCtr,
                       method = "ML")
m.prueba.pupa.2.fl <- lme(seed ~ Beetle.fl+Fly.fl+ Bee.fl+Butterfly.fl +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, data= PUPA.vis1, random = ~1 |plot, control=lCtr,
                       method = "ML")

residplot(m.prueba.pupa.2)#se ajusta mal por la falta de puntos
residplot(m.prueba.pupa.2.fl)#Parece que los residuos por los menos se ajustan un poco mejor, el grafico q-q es parecido en ambas


m.prueba_sec.pupa.2 <- dredge(m.prueba.pupa.2.fl, trace = TRUE, rank = "AICc", REML = FALSE)#maybe neigh intra 7.5
#IMPORTANTE. ELegir bien el modelo, porque si es el de visits counts las bees pueden tener importancia. 
(attr(m.prueba_sec.pupa.2, "rank.call"))
fmList.prueba.pupa.2 <- get.models(m.prueba_sec.pupa.2, 1:4) 
summary(model.avg(fmList.prueba.pupa.2))# 
importance(fmList.prueba.pupa.2)
r.squaredGLMM(m.prueba.pupa.2)

PUPA.vis1$fittedvalues <- fitted(m.prueba.pupa.2.fl)
PUPA.seed.n <- ggplot(PUPA.vis1, aes(x = neigh_intra.7.5))+
    geom_point(aes(y= seed))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("PUPA fitness (seed/fruit) with neighbors intra at 7.5cm")+
    ylab("Number of seed/fruit")+
    xlab("Number of neighbors intra at 7.5cm")+
    theme_light()
PUPA.seed.n #molaria que fuese un efecto indirecto de polinizadores... a ver el SEM

#(seed/one fruit)*fruits
#random structure
p1.fr<- lme(seed.indv ~ 1, data= PUPA.vis,random = ~1 |plot, control=lCtr,
         method = "ML")
p2.fr <- lme(seed.indv ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML") 
p3.fr<- lme(seed.indv ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
p4.fr <- lme(seed.indv ~ 1, data= PUPA.vis, random = ~1 |plot, control=lCtr,
          corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(p1.fr, p2.fr, p3.fr, p4.fr) #best model is p1

options(na.action = "na.fail")


m.prueba.pupa.2.fr <- lme(seed.indv ~ Beetle+Fly+ Bee +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, data= PUPA.vis, random = ~1 |plot, control=lCtr,
                       method = "ML")
m.prueba.pupa.2.fr.fl <- lme(seed.indv ~ Beetle.fl+Fly.fl+ Bee.fl+Butterfly.fl +neigh_inter.1m+neigh_intra.1m+neigh_inter.7.5+neigh_intra.7.5, data= PUPA.vis1, random = ~1 |plot, control=lCtr,
                          method = "ML")

residplot(m.prueba.pupa.2.fr)#se ajusta muy mal porque tengo pocos puntos
residplot(m.prueba.pupa.2.fr.fl)#se ajusta también muy mal. He escogido visits counts. 

m.prueba_sec.pupa.2.fr <- dredge(m.prueba.pupa.2.fr, trace = TRUE, rank = "AICc", REML = FALSE)#Inter 7.5 and bee
(attr(m.prueba_sec.pupa.2.fr, "rank.call"))
fmList.prueba.pupa.2.fr <- get.models(m.prueba_sec.pupa.2.fr, 1:4) 
summary(model.avg(fmList.prueba.pupa.2.fr))#
importance(fmList.prueba.pupa.2.fr) #mola!
r.squaredGLMM(m.prueba.pupa.2.fr)


PUPA.vis$fittedvalues <- fitted(m.prueba.pupa.2.fr)
PUPA.seed.n.bee <- ggplot(PUPA.vis, aes(x = Bee))+
    geom_point(aes(y= seed.indv))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("PUPA fitness (seed/fruit)*fruits with bee visits")+
    ylab("Number of seed/fruit")+
    xlab("Number of bee vistis")+
    theme_light()
PUPA.seed.n.bee #poquisimos datos... IB: Bueno... suficientes.


PUPA.seed.n.7.5 <- ggplot(PUPA.vis, aes(x = neigh_inter.7.5))+
    geom_point(aes(y= seed.indv))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("PUPA fitness (seed/fruit)*fruits with neighbors inter at 7.5cm")+
    ylab("Number of seed/fruit")+
    xlab("Number of neighbors inter at 7.5cm")+
    theme_light()
PUPA.seed.n.7.5 #poquisimos datos. Hay ahi un outlyer.

###################################################################################################################
################                                  VISITS----
##################################################################################################################
#for the variable response visits, I have two posible variables: the visits (count) and the visits per flowers. I'm going
#to do one lme per each, in order to see diferences. Also, these models are going to be done per group of floral visitor
#For that reason, I'm going to use the general dataframe call data. It is more easy to do the subset with it.

#Beetle visits
Bet.vis <- subset(data, Group== "Beetle")
#In Bet.vis I have some coordenates that are not unique, for that I change the y_coord in other to have different coordenates and the model could 
#           converge. The next lines in the code are for changing these y coordenates. 
Bet.vis$y_coor2 <-  jitter(Bet.vis$y_coor2)
#random structure visits count
c.vis.bet.1<- lme(visits ~ 1, data= Bet.vis,random = ~1 |plot, control=lCtr,
                  method = "ML")
c2.vis.bet.1 <- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bet.1<- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bet.1 <- lme(visits ~ 1, data= Bet.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(c.vis.bet.1, c2.vis.bet.1, c3.vis.bet.1, c4.vis.bet.1)#best model 2

k <- lme(visits ~ neigh_inter.1m+ neigh_intra.1m, data= Bet.vis,random = ~ 1|plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T),method = "ML")
#I tried the model writting as random factor ~ 1|plot+ 1|Plant but the model stays more than 45 min running. For that, I decide
#not to include them
#I think this is not correctly specified (according to google) try
k <- lme(visits ~ neigh_inter.1m+ neigh_intra.1m, data= Bet.vis,random = ~ 1|plot/Plant, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T),method = "ML")

m.prueba_sec.k <- dredge(k, trace = TRUE, rank = "AICc", REML = FALSE)#neigh intra 1m
#yes, it's slow, but like 5 minuts in my Mac.
(attr(m.prueba_sec.k, "rank.call"))
fmList.prueba.k <- get.models(m.prueba_sec.k, 1:4) #this one is also slow, but doable 
summary(model.avg(fmList.prueba.k))#neigh intra 1m
importance(fmList.prueba.k)
residplot(k) 

Bet.vis$fittedvalues <- fitted(k)
Bet.plot <- ggplot(Bet.vis, aes(x = neigh_intra.1m))+
    geom_point(aes(y= visits))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("Visits of Beetles with neigh_intra.1m")+
    ylab("Number of Beetle visits")+
    xlab("Number of neighbors intra at 1m")+
    theme_light()
Bet.plot #super nice!!


#random structure visits per flower
Bet.vis$visits.flower <- as.numeric(Bet.vis$visits.flower)
Bet.vis1 <- Bet.vis[!is.na(Bet.vis$visits.flower),]
Bet.vis1$visits.flower[Bet.vis1$visits.flower== Inf] <- 'NA' #I have problems with the INf numbers, so I change them to NA and
#           I delete them
Bet.vis1$visits.flower <- as.numeric(Bet.vis1$visits.flower)
Bet.vis1 <- Bet.vis1[!is.na(Bet.vis1$visits.flower),]

c.vis.bet.1.i<- lme(visits.flower ~ 1, data= Bet.vis1,random = ~1 |plot, control=lCtr,
                  method = "ML")
c2.vis.bet.1.i <- lme(visits.flower ~ 1, data= Bet.vis1, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bet.1.i<- lme(visits.flower ~ 1, data= Bet.vis1, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bet.1.i <- lme(visits.flower ~ 1, data= Bet.vis1, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")
AIC(c.vis.bet.1.i, c2.vis.bet.1.i, c3.vis.bet.1.i, c4.vis.bet.1.i)#best model 3

k.i <- lme(log(visits.flower)~ neigh_inter.1m+ neigh_intra.1m, data= Bet.vis1,random = ~ 1|plot, control=lCtr,
         corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T),method = "ML")#better with log
#I tried the model writting as random factor ~ 1|plot+ 1|Plant but the model stays more than 45 min runnung. For that, I decide
#not to include them

m.prueba_sec.k.i <- dredge(k.i, trace = TRUE, rank = "AICc", REML = FALSE)#nothing relevant, maybe ngh intra 1m
(attr(m.prueba_sec.k.i, "rank.call"))
fmList.prueba.k.i <- get.models(m.prueba_sec.k.i, 1:4) 
summary(model.avg(fmList.prueba.k.i))
residplot(k.i) #better with the log


Bet.vis1$fittedvalues <- fitted(k.i)
Bet.plot.fl <- ggplot(Bet.vis1, aes(x = neigh_intra.1m))+
    geom_point(aes(y= log(visits.flower)))+
    geom_smooth(method = "lm",aes(y=fittedvalues))+
    ggtitle("Visits of Beetles with neigh_intra.1m")+
    ylab("Number of Beetle visits")+
    xlab("Number of neighbors intra at 1m")+
    theme_light()
Bet.plot.fl #Seeing the plot, there is not a huge effect of the neigh intra 1m in the visits/flowers of the beetles


##butterflies
but.vis <- subset(data, Group== "Butterfly") #solo 4 entradas

#bees
bee.vis <- subset(data, Group== "Bee")#I have rows with the same coordenates. 
bee.vis$y_coor2 <-  jitter(bee.vis$y_coor2)

#random structure visits count
c.vis.bee.1<- lme(visits ~ 1, data= bee.vis,random = ~1 |plot, control=lCtr,
                  method = "ML")
c2.vis.bee.1 <- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bee.1<- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bee.1 <- lme(visits ~ 1, data= bee.vis, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")

AIC(c.vis.bee.1, c2.vis.bee.1, c3.vis.bee.1, c4.vis.bee.1)#the 3rd model is the best.

k2.bee <- lme(log(visits) ~ neigh_inter.1m+ neigh_intra.1m, data= bee.vis,random = ~1 |plot, control=lCtr,
              corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
residplot(k2.bee) #the resiudals are better with the log. 

m.prueba_sec.k2.bee <- dredge(k2.bee, trace = TRUE, rank = "AICc", REML = FALSE)#nothing
(attr(m.prueba_sec.k2.bee, "rank.call"))
fmList.prueba.k2.bee <- get.models(m.prueba_sec.k2.bee, 1:4) 
summary(model.avg(fmList.prueba.k2.bee))#

#Bees random structure visits per flower
bee.vis$visits.flower <- as.numeric(bee.vis$visits.flower)
bee.vis1 <- bee.vis[!is.na(bee.vis$visits.flower),]
bee.vis1$visits.flower[bee.vis1$visits.flower== Inf] <- 'NA' #I have problems with the INf numbers, so I change them to NA and
#           I delete them
bee.vis1$visits.flower <- as.numeric(bee.vis1$visits.flower)
bee.vis1 <- bee.vis1[!is.na(bee.vis1$visits.flower),]

c.vis.bee.1.i<- lme(visits.flower ~ 1, data= bee.vis1,random = ~1 |plot, control=lCtr,
                  method = "ML")
c2.vis.bee.1.i <- lme(visits.flower ~ 1, data= bee.vis1, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
c3.vis.bee.1.i<- lme(visits.flower ~ 1, data= bee.vis1, random = ~1 |plot, control=lCtr,
                   corr = corSpatial(form = ~x_coor2 + y_coor2, type ="rational", nugget = T), method = "ML")
c4.vis.bee.1.i <- lme(visits.flower ~ 1, data= bee.vis1, random = ~1 |plot, control=lCtr,
                    corr = corSpatial(form = ~x_coor2 + y_coor2, type ="exponential", nugget = T), method = "ML")

AIC(c.vis.bee.1.i, c2.vis.bee.1.i, c3.vis.bee.1.i, c4.vis.bee.1.i)#the 2nd model is the best. 

k2.bee.i <- lme(log(visits.flower) ~ neigh_inter.1m+ neigh_intra.1m, data= bee.vis1,random = ~1 |plot, control=lCtr,
                corr = corSpatial(form = ~x_coor2 + y_coor2, type ="gaussian", nugget = T), method = "ML")
residplot(k2.bee.i) #the resiudals are better with the log. 

m.prueba_sec.k2.bee.i <- dredge(k2.bee.i, trace = TRUE, rank = "AICc", REML = FALSE)#nothing
(attr(m.prueba_sec.k2.bee.i, "rank.call"))
fmList.prueba.k2.bee.i <- get.models(m.prueba_sec.k2.bee.i, 1:4) 
summary(model.avg(fmList.prueba.k2.bee.i))#

####
