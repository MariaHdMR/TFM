#en este script se quiere obtener la relacion de las
library(DHARMa)
library(reshape2)
library(tidyverse)

va <- read.table("data/FV_16_19.csv", header=T, sep= ";")
va19 <- subset(va, Year== "2019")
plantas <- read.table("data/Abun_19.csv", header=T, sep= ";")
head(va19)
pol.9 <- va19 %>% group_by(Plot, Subplot, Group, Plant_Simple) %>% summarise (num.visitors = sum(Visits))
head(pol.9)
head(plantas)
plantas <- subset(plantas, select = c("plot", "subplot", "Sp.Focal", "Plantas"))
colnames(plantas) <- c("Plot", "Subplot", "Plant_Simple", "abundancia")
dat <- merge(pol.9, plantas, by = c("Plot", "Subplot", "Plant_Simple"), all = TRUE)
head(dat)
#add abundance at plot level

plot_abund <- dcast(plantas, Plot + Plant_Simple ~ ., fun.aggregate = sum, value.var = "abundancia")
plot_abund
colnames(plot_abund)[3] <- "abundancia_plot"
#merge
dat2 <- merge(dat, plot_abund, by = c("Plot", "Plant_Simple"), all.x = TRUE)
head(dat2)
#clean empty ones.
dat2[which(dat2$abundancia == 0),]
dat2[which((is.na(dat2$Group) & dat2$abundancia >0)),]
dat2[which((!is.na(dat2$Group) & dat2$abundancia == 0)),"abundancia"] <- 1
#ESTO es una asumpcion bestia. 
#modelo para flies
flyCHUFU <- subset(dat2, Group == "Fly" & Plant_Simple == "CHFU")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple %in% c("LEMA", "ME"))
temp1 <- dcast(flyOthers, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(temp1)[2] <- "abundance_plot_inter"
temp2<- dcast(flyOthers, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(temp2)[3] <- "abundance_inter"
head(temp2)
temp3 <- merge(temp2, temp1)
head(flyCHUFU)
flyCHUFU2 <- merge(flyCHUFU, temp3)
head(flyCHUFU2)

m1 <- lm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, data = flyCHUFU2)
plot(m1)#no es bueno, hay que fijarse en la primera gráfica de los residuos
summary(m1)
m2 <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
          data = flyCHUFU2, family = "poisson")
hist(flyCHUFU2$num.visitors)
simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
plot(simulationOutput)
#performance::check_overdispersion(m2) #esto es para ver si hay sobredispersion
summary(m2)
####
#datos fenología: hay 3 fases
# 1) CHFU
# 2) LEMA, ME, CHFU
# 3) PUPA, LEMA
#modelo para beetles
beetleCHUFU <- subset(dat2, Group == "Beetle" & Plant_Simple == "CHFU")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
beetleOthers <- subset(dat2, Group == "Beetle" & Plant_Simple %in% c("LEMA", "ME"))
b.temp1 <- dcast(beetleOthers, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(b.temp1)[2] <- "abundance_plot_inter"
b.temp2<- dcast(beetleOthers, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(b.temp2)[3] <- "abundance_inter"
head(b.temp2)
b.temp3 <- merge(b.temp2, b.temp1)
head(beetleCHUFU)
beetleCHUFU2 <- merge(beetleCHUFU, b.temp3)
head(beetleCHUFU2)
par(mfrow=c(1,1))

b.m2 <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
          data = beetleCHUFU2, family = "poisson")
hist(beetleCHUFU2$num.visitors)
b.simulationOutput <- simulateResiduals(fittedModel = b.m2, n = 250)
plot(b.simulationOutput)
#performance::check_overdispersion(m2) #esto es para ver si hay sobredispersion
summary(b.m2)

#modelo para bees
beesCHUFU <- subset(dat2, Group == "Bees" & Plant_Simple == "CHFU")#0
butCHUFU <- subset(dat2, Group == "Butterfly" & Plant_Simple == "CHFU")#0
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
beetleOthers <- subset(dat2, Group == "Beetle" & Plant_Simple %in% c("LEMA", "ME"))
b.temp1 <- dcast(beetleOthers, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(b.temp1)[2] <- "abundance_plot_inter"
b.temp2<- dcast(beetleOthers, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(b.temp2)[3] <- "abundance_inter"
head(b.temp2)
b.temp3 <- merge(b.temp2, b.temp1)
head(beetleCHUFU)
beetleCHUFU2 <- merge(beetleCHUFU, b.temp3)
head(beetleCHUFU2)
par(mfrow=c(1,1))

b.m2 <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
            data = beetleCHUFU2, family = "poisson")
hist(beetleCHUFU2$num.visitors)
b.simulationOutput <- simulateResiduals(fittedModel = b.m2, n = 250)
plot(b.simulationOutput)
#performance::check_overdispersion(m2) #esto es para ver si hay sobredispersion
summary(b.m2)
###

#modelo para beetles y lema

betLEMA <- subset(dat2, Group == "Beetle" & Plant_Simple == "LEMA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
beetleOthers.1 <- subset(dat2, Group == "Beetle" & Plant_Simple %in% c("CHFU", "ME"))
b.temp1.lema <- dcast(beetleOthers.1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(b.temp1.lema)[2] <- "abundance_plot_inter"
b.temp2.lema<- dcast(beetleOthers.1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(b.temp2.lema)[3] <- "abundance_inter"
head(b.temp2.lema)
b.temp3.lema <- merge(b.temp2.lema, b.temp1.lema)
head(betLEMA)
beetleLEMA2 <- merge(betLEMA, b.temp3.lema)
head(beetleLEMA2)
b.m2.lema <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
            data = beetleLEMA2, family = "poisson")
summary(b.m2.lema)

#modelo para flies y lema

flyLEMA <- subset(dat2, Group == "Fly" & Plant_Simple == "LEMA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
flyOthers.lema <- subset(dat2, Group == "Fly" & Plant_Simple %in% c("CHFU", "ME"))
fly.temp1.lema <- dcast(flyOthers.lema, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(fly.temp1.lema)[2] <- "abundance_plot_inter"
f.temp2.lema<- dcast(flyOthers.lema, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(f.temp2.lema)[3] <- "abundance_inter"
head(f.temp2.lema)
f.temp3.lema <- merge(f.temp2.lema, fly.temp1.lema)
head(flyLEMA)
flyLEMA2.1 <- merge(flyLEMA, f.temp3.lema)
head(flyLEMA2.1)
f.m2.lema <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                 data = flyLEMA2.1, family = "poisson")
summary(f.m2.lema)

#modelo bee + lema

beeLEMA <- subset(dat2, Group == "Bee" & Plant_Simple == "LEMA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
beeOthers.lema <- subset(dat2, Group == "Bee" & Plant_Simple %in% c("CHFU", "ME"))
beeOthers.lema1 <-beeOthers.lema[which(complete.cases(beeOthers.lema)),]
bee.temp1.lema <- dcast(beeOthers.lema1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(bee.temp1.lema)[2] <- "abundance_plot_inter"
bee.temp2.lema<- dcast(beeOthers.lema1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(bee.temp2.lema)[3] <- "abundance_inter"
head(bee.temp2.lema)
bee.temp3.lema <- merge(bee.temp2.lema, bee.temp1.lema)
head(beeLEMA)
beeLEMA2.1 <- merge(beeLEMA, bee.temp3.lema)
head(beeLEMA2.1)
bee.m2.lema <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                 data = beeLEMA2.1, family = "poisson")
summary(bee.m2.lema)

#modelo butterfly + lema

butLEMA <- subset(dat2, Group == "Butterfly" & Plant_Simple == "LEMA")


#modelo beetle + me

betME <- subset(dat2, Group == "Bettle" & Plant_Simple == "ME")
#flies+ me

flyME <- subset(dat2, Group == "Fly" & Plant_Simple == "ME")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
fly.Others.me <- subset(dat2, Group == "Fly" & Plant_Simple %in% c("CHFU", "LEMA"))
fly.temp1.me <- dcast(fly.Others.me, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(fly.temp1.me)[2] <- "abundance_plot_inter"
fly.temp2.me<- dcast(fly.Others.me, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(fly.temp2.me)[3] <- "abundance_inter"
head(fly.temp2.me)
fly.temp3.me <- merge(fly.temp2.me, fly.temp1.me)
head(flyME)
flyME2 <- merge(flyME, fly.temp3.me)
head(flyME2)
fly.m2.me <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                   data = flyME2, family = "poisson")
summary(fly.m2.me)

# bees y me
beeME <- subset(dat2, Group == "Bee" & Plant_Simple == "ME")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
bee.Others.me <- subset(dat2, Group == "Bee" & Plant_Simple %in% c("CHFU", "LEMA"))
bee.Others.me1 <- bee.Others.me[which(complete.cases(bee.Others.me)),]
bee.temp1.me <- dcast(bee.Others.me1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(bee.temp1.me)[2] <- "abundance_plot_inter"
bee.temp2.me<- dcast(bee.Others.me1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(bee.temp2.me)[3] <- "abundance_inter"
head(bee.temp2.me)
bee.temp3.me <- merge(bee.temp2.me, bee.temp1.me)
head(beeME)
beeME2 <- merge(beeME, bee.temp3.me)
head(beeME2)
bee.m2.me <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                 data = beeME2, family = "poisson")
summary(bee.m2.me)

# butterflies y me
butME <- subset(dat2, Group == "Butterfly" & Plant_Simple == "ME")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
bee.Others.me <- subset(dat2, Group == "Bee" & Plant_Simple %in% c("CHFU", "LEMA"))
bee.Others.me1 <- bee.Others.me[which(complete.cases(bee.Others.me)),]
bee.temp1.me <- dcast(bee.Others.me1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(bee.temp1.me)[2] <- "abundance_plot_inter"
bee.temp2.me<- dcast(bee.Others.me1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(bee.temp2.me)[3] <- "abundance_inter"
head(bee.temp2.me)
bee.temp3.me <- merge(bee.temp2.me, bee.temp1.me)
head(beeME)
beeME2 <- merge(beeME, bee.temp3.me)
head(beeME2)
bee.m2.me <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                 data = beeME2, family = "poisson")
summary(bee.m2.me)

# ahora la combinacion lema y pupa. pupa como sp focal
bepupa <- subset(dat2, Group == "Beetle" & Plant_Simple == "PUPA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
bet.Others.pupa <- subset(dat2, Group == "Beetle" & Plant_Simple %in% c("LEMA"))
bet.Others.pupa1 <- bet.Others.pupa[which(complete.cases(bet.Others.pupa)),]
bet.temp1.pupa <- dcast(bet.Others.pupa1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(bet.temp1.pupa)[2] <- "abundance_plot_inter"
bet.temp2.pupa<- dcast(bet.Others.pupa1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(bet.temp2.pupa)[3] <- "abundance_inter"
head(bet.temp2.pupa)
bet.temp3.pupa <- merge(bet.temp2.pupa, bet.temp1.pupa)
head(bepupa)
bepupa2 <- merge(bepupa, bet.temp3.pupa)
head(bepupa2)
bet.m2.pupa <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                 data = bepupa2, family = "poisson")
summary(bet.m2.pupa)

#flies
flypupa <- subset(dat2, Group == "Fly" & Plant_Simple == "PUPA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
fly.Others.pupa <- subset(dat2, Group == "Fly" & Plant_Simple %in% c("LEMA"))
fly.Others.pupa1 <- fly.Others.pupa[which(complete.cases(fly.Others.pupa)),]
fly.temp1.pupa <- dcast(fly.Others.pupa1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(fly.temp1.pupa)[2] <- "abundance_plot_inter"
fly.temp2.pupa<- dcast(fly.Others.pupa1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(fly.temp2.pupa)[3] <- "abundance_inter"
head(fly.temp2.pupa)
fly.temp3.pupa <- merge(fly.temp2.pupa, fly.temp1.pupa)
head(flypupa)
flypupa2 <- merge(flypupa, fly.temp3.pupa)
head(flypupa2)
fly.m2.pupa <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                   data = flypupa2, family = "poisson")
summary(fly.m2.pupa)

#bees

beespupa <- subset(dat2, Group == "Bee" & Plant_Simple == "PUPA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
bee.Others.pupa <- subset(dat2, Group == "Bee" & Plant_Simple %in% c("LEMA"))
bee.Others.pupa1 <- bee.Others.pupa[which(complete.cases(bee.Others.pupa)),]
bee.temp1.pupa <- dcast(bee.Others.pupa1, Plot ~ ., fun.aggregate = sum, value.var = "abundancia_plot") #7 has NA, REMOVE OR FIX:
colnames(bee.temp1.pupa)[2] <- "abundance_plot_inter"
bee.temp2.pupa<- dcast(bee.Others.pupa1, Plot + Subplot ~ ., fun.aggregate = sum, value.var = "abundancia")
colnames(bee.temp2.pupa)[3] <- "abundance_inter"
head(bee.temp2.pupa)
bee.temp3.pupa <- merge(bee.temp2.pupa, bee.temp1.pupa)
head(beespupa)
beespupa2 <- merge(beespupa, bee.temp3.pupa)
head(beespupa2)
bee.m2.pupa <- glm(num.visitors ~ abundancia + abundance_inter + abundancia_plot + abundance_plot_inter, 
                   data = beespupa2, family = "poisson")
summary(bee.m2.pupa)

#bUTTER

buterpupa <- subset(dat2, Group == "Butterfly" & Plant_Simple == "PUPA")
#flyOthers <- subset(dat2, Group == "Fly" & Plant_Simple != "CHFU")
#flyOthers$Plant_Simple #eliminar HOMA, y no sincronicas.
but.Others.pupa <- subset(dat2, Group == "Butterfly" & Plant_Simple %in% c("LEMA"))

