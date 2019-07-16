#random factor
#para sacar los R squared: intalar MuMLn
library(MuMIn)

#tengo que poner como random factor PLOT.  tabla.PUPA.19
tabla.PUPA.19
library(nlme)
library(tidyverse)
#PUPA ----
model.pupa = lme(num.indv~ num.visits, random=~1|Plot,
               data=tabla.PUPA.19,
               method="REML")   
summary(model.pupa)
plot(model.pupa)
r.squaredGLMM(model.pupa)

#el plot de abajo no funciona, buscar otro. 
#lattice::xyplot(num.indv~num.visits | Plot, groups= Group_Floralvisitor, data=tabla.PUPA.19, type=c('p','r'), auto.key=F)

    #ahora pupa y coleopteros
    tabla.P.Cosubset19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Coleoptera')
    model.pupa.co = lme(num.indv~ num.visits, random=~1|Plot,
                 data=tabla.P.Cosubset19,
                 method="REML")   
    summary(model.pupa.co)
    #segun esto parece que una abundancia mayor de Pupa no se corresponde de un mayor numero de visitas
    r.squaredGLMM(model.pupa.co)
    
    #pupa y dipteros
    tabla.P.D19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Diptera')
    model.pupa.d = lme(num.indv~ num.visits, random=~1|Plot,
                    data=tabla.P.D19,
                    method="REML")   
    summary(model.pupa.d)
    r.squaredGLMM(model.pupa.d)
    
    #pupa e hymenopteros
    tabla.P.H19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Hymenoptera')
    model.pupa.H = lme(num.indv~ num.visits, random=~1|Plot,
                   data=tabla.P.H19,
                   method="REML")   
    summary(model.pupa.H)
    r.squaredGLMM(model.pupa.H)
    
    #pupa y lepidoptera
    tabla.P.L <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Lepidoptera')
    model.pupa.L = lme(num.indv~ num.visits, random=~1|Plot,
                       data=tabla.P.L,
                       method="REML")   
    summary(model.pupa.L)
    r.squaredGLMM(model.pupa.L)

#CHFU----
tabla.CHFU.19 <- subset(tabla.completa.19, Plant == 'CHFU'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))

tabla.CHFU.19.1 <-tabla.CHFU.19[which(!is.na(tabla.CHFU.19$num.indv)),]    
model.chfu = lme(num.indv~ num.visits, random=~1|Plot,
                   data=tabla.CHFU.19.1,
                   method="REML")   
summary(model.chfu)
r.squaredGLMM(model.chfu)

    #chfu y diptera
    tabla.chfu.19.d <- subset(tabla.CHFU.19.1, Group_Floralvisitor == 'Diptera')
    model.chfu.d = lme(num.indv~ num.visits, random=~1|Plot,
                     data=tabla.chfu.19.d,
                     method="REML")   
    summary(model.chfu.d)
    r.squaredGLMM(model.chfu.d)
    #chfu e hymenoptera
    tabla.chfu.19.h <- subset(tabla.CHFU.19.1, Group_Floralvisitor == 'Hymenoptera')
    model.chfu.h = lme(num.indv~ num.visits, random=~1|Plot,
                       data=tabla.chfu.19.h,
                       method="REML")   
    summary(model.chfu.h)
    r.squaredGLMM(model.chfu.h)
    #chfu y coleoptera
    tabla.chfu.19.c <- subset(tabla.CHFU.19.1, Group_Floralvisitor == 'Coleoptera')
    model.chfu.c = lme(num.indv~ num.visits, random=~1|Plot,
                       data=tabla.chfu.19.c,
                       method="REML")   
    summary(model.chfu.c)
    r.squaredGLMM(model.chfu.c)
    #chfu y hemiptera -> no sale bien creo que porque solo hay dos visitas de hemiptera
    tabla.chfu.19.he <- subset(tabla.CHFU.19.1, Group_Floralvisitor == 'Hemiptera')
    model.chfu.he = lme(num.indv~ num.visits, random=~1|Plot,
                       data=tabla.chfu.19.he,
                       method="REML")   
    summary(model.chfu.he)
  

#pupa ----
tabla.PUPA.19 <- subset(tabla.completa.19, Plant == 'PUPA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
    tabla.PUPA.19.1 <-tabla.PUPA.19[which(!is.na(tabla.PUPA.19$num.indv)),]  
     
model.pupa = lme(num.indv~ num.visits, random=~1|Plot,
                     data=tabla.PUPA.19.1,
                     method="REML")   
summary(model.pupa)
r.squaredGLMM(model.pupa)
    
    #pupa+diptera    
    tabla.pupa.19.d <- subset(tabla.PUPA.19.1, Group_Floralvisitor == 'Diptera')
    model.pupa.d = lme(num.indv~ num.visits, random=~1|Plot,
                        data= tabla.pupa.19.d,
                        method="REML")   
    summary(model.pupa.d)
    r.squaredGLMM(model.pupa.d)
    #pupa+lepidoptera    
    tabla.pupa.19.L <- subset(tabla.PUPA.19.1, Group_Floralvisitor == 'Lepidoptera')
    model.pupa.L = lme(num.indv~ num.visits, random=~1|Plot,
                       data= tabla.pupa.19.L,
                       method="REML")   
    summary(model.pupa.L)
    r.squaredGLMM(model.pupa.L)
    #pupa+hymenoptera
    tabla.pupa.19.H <- subset(tabla.PUPA.19.1, Group_Floralvisitor == 'Hymenoptera')
    model.pupa.H = lme(num.indv~ num.visits, random=~1|Plot,
                       data= tabla.pupa.19.H,
                       method="REML")   
    summary(model.pupa.H)
    r.squaredGLMM(model.pupa.H)
    #pupa+coleoptera
    tabla.pupa.19.C <- subset(tabla.PUPA.19.1, Group_Floralvisitor == 'Coleoptera')
    model.pupa.C = lme(num.indv~ num.visits, random=~1|Plot,
                       data= tabla.pupa.19.C,
                       method="REML")   
    summary(model.pupa.C)
    r.squaredGLMM(model.pupa.C)
    

    
    
#chmi #solo tengo 5 visitas en todo 2019, no se si es relevante.   
tabla.chmi.19 <- subset(tabla.completa.19, Plant == 'CHMI'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))

#ME ---- 
#NO ME SALE
library(tidyverse)

# Parte dAVID quitar na's antes de subset----
data19 <- droplevels(tabla.completa.19.1[which(complete.cases(tabla.completa.19)),])
data19$Plot <- as.numeric(as.character(data19$Plot))
data19$Subplot <- as.character(data19$Subplot)

data.me <- subset(tabla.completa.19.1, Sp.Focal.Simpe %in% c("MEEL","MEPO","MESU"))
data.me$Sp.Focal.Simpe <- "MESP"
data.aggr <- data.me %>% group_by(Plot,Subplot) %>% summarise(num.indiv.aggr = sum(num.indiv))

# cambiar nombre a columna num.indiv.aggr

data.new <- left_join(data.me,orig)
data.new <- subset(data.new, !Sp.Focal.Simpe %in% c("MEEL","MEPO","MESU"))

#de esta manera he sacado el numero de individuos por subplots de ME,MEEL,MEPO,MESU (he agrupado todo en ME)
TABLA.ME <- subset(data19, Sp.Focal.Simpe %in% c("ME", "MEEL", "MEPO", "MESU"))
                    t.ME <- TABLA.ME %>%group_by(Subplot) %>% summarise(num.indvs = sum(num.visits))
                    tabla.me.19 <- dplyr::left_join (TABLA.ME,t.ME)


                       tabla.me.19.1 <-tabla.me.19[which(!is.na(tabla.me.19$num.indvs)),]  

                    model.me = lme(num.indv~ num.visits, random=~1|Plot,
                                                    data=tabla.me.19.1,
                                                     method="REML")   
                   summary(model.me)

                        #me+coleoptera
                    #    tabla.me.19.1.c <- subset(tabla.me.19.1, Group_Floralvisitor == 'Coleoptera')
                     #   model.me.C = lme(num.indv~ num.visits, random=~1|Plot,
                      #                     data= tabla.me.19.1.c,
                       #                    method="REML")   
                        #summary(model.me.C)
    


tabla.RAPE.19 <- subset(tabla.completa.19, Plant == 'RAPE'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
    #SOLO TENGO 9 VISITAS, LO METO?
                        
                        
                        
#con esto veo si ls indiv dependen del numero de visitas

datos16
str(datos16)
library(nlme)
model.16 = lme(num.indv~ num.visits, random=~1|Plot,
                  data=datos16,
                  method="REML")
summary(model.16)
plot(model.pupa)

######
model.fixed = gls(num.indv ~ num.visits,
                  data=tabla.PUPA.19,
                  method="REML")
summary(model.fixed)

tabla.PUPA.19 <- subset(tabla.completa.19, Plant == 'PUPA'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
str(tabla.PUPA.19)

install.packages("lme4") #estos resultados no los entiendo muy bien
library(lme4)
otro<- lmer(num.indv ~ num.visits  |Plot , data=tabla.PUPA.19)
summary(otro)
