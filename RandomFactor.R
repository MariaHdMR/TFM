#random factor

#tengo que poner como random factor PLOT.  tabla.PUPA.19
tabla.PUPA.19
library(nlme)
#PUPA ----
model.pupa = lme(num.indv~ num.visits, random=~1|Plot,
               data=tabla.PUPA.19,
               method="REML")   
summary(model.pupa)
plot(model.pupa)


#ahora pupa y coleopteros
tabla.P.Cosubset19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Coleoptera')
model.pupa.co = lme(num.indv~ num.visits, random=~1|Plot,
                 data=tabla.P.Cosubset19,
                 method="REML")   
summary(model.pupa.co)
#segun esto parece que una abundancia mayor de Pupa no se corresponde de un mayor numero de visitas

#pupa y dipteros
tabla.P.D19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Diptera')
model.pupa.d = lme(num.indv~ num.visits, random=~1|Plot,
                    data=tabla.P.D19,
                    method="REML")   
summary(model.pupa.d)
#es no significativo por poco, pero el AIC es mucho mayor que con los coleopteros por ejemplo
tabla.P.H19 <- subset(tabla.PUPA.19, Group_Floralvisitor == 'Hymenoptera')
model.pupa.H = lme(num.indv~ num.visits, random=~1|Plot,
                   data=tabla.P.H19,
                   method="REML")   
summary(model.pupa.H)

#CHFU----
tabla.CHFU.19 <- subset(tabla.completa.19, Plant == 'CHFU'& Group_Floralvisitor %in% c("Coleoptera", "Hymenoptera", "Diptera", "Lepidoptera", "Hemiptera"))
model.chfu = lme(num.indv~ num.visits, random=~1|Plot,
                   data=tabla.CHFU.19,
                   method="REML")   
summary(model.chfu)

tabla.chfu.19.d <- subset(tabla.CHFU.19, Group_Floralvisitor == 'Diptera')
model.chfu.d = lme(num.indv~ num.visits, random=~1|Plot,
                 data=tabla.chfu.19.d,
                 method="REML")   
summary(model.chfu.d)

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
