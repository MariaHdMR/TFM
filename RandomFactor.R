#random factor

#tengo que poner como random factor PLOT.  tabla.PUPA.19
tabla.PUPA.19
library(nlme)
model.pupa = lme( num.indv~ num.visits, random=~1|Plot,
            data=tabla.PUPA.19,
            method="REML")
summary(model.pupa)
plot(model.pupa)

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
