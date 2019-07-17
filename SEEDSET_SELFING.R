# seed set --> selfing 
library(tidyverse)
#SF_2019 es el archivo de selfing de pupa, lema y chfu
boxplot(SF_2019$VIABLES_SEEDS~ SF_2019$SELFING, xlab= ('selfing'), col= c(123, 456,234), ylab = ('viable seeds'),main = ('Viable seeds vs selfing: LEMA, PUPA Y CHFU'))
t.test(SF_2019$VIABLES_SEEDS~SF_2019$SELFING, data= SF_2019)
aov(VIABLES_SEEDS ~ SELFING, data = SF_2019)
k <- lm(VIABLES_SEEDS ~ SELFING, data = SF_2019)
summary(k)

SF_2019$SELFING <- as.factor(SF_2019$SELFING)
str(SF_2019)
SF_2019$VIABLES_SEEDS <- as.numeric(SF_2019$VIABLES_SEEDS)
par(mfrow=c(1,1))

SF <- ggplot(SF_2019, aes(x= SELFING, y = VIABLES_SEEDS))+
    geom_point(aes(color=PLANT))+
    xlab('SELFING')+
    ylab('SEMILLAS VIABLES')+
    ggtitle ('SEMILLAS VIABLES')+
NULL
SF #hacerlo con cajas estaría mejor


#BUscar soluciones, no me sale este barplot
#PLANT<-factor(SF_2019$PLANT, levels = c ("CHFU","LEMA"))
#barplot(table(SF_2019$VIABLES_SEEDS, PLANT), beside = T, legend.text = T, col= c(123, 345))


#chfu----
SF_CHFU <- subset(SF_2019, PLANT== 'CHFU')
boxplot(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: CHFU'))
m <- lm(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)
summary(m)
aov(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)

#LEMA ----
SF_LEMA <- subset(SF_2019, PLANT== 'LEMA')
boxplot(SF_LEMA$VIABLES_SEEDS~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: LEMA'))
n <- lm(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)
summary(n)
aov(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)

#pupa ----
SF_PUPA <- subset(SF_2019, PLANT== 'PUPA')
boxplot(SF_PUPA$VIABLES_SEEDS~SF_PUPA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: PUPA'))
aov(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
O <- lm(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
summary(O)
plot(O)

#según los analisis anteriores se podría decir que a CHFU los polinizadores le producen un mayor fitness 
    #que a LEMA y PUPA, que no dependen tanto de los polinizadores. LEMa tiene altas tasas de autofecundación
################
# seed set --> selfing 
library(tidyverse)
boxplot(SF_2019$VIABLES_SEEDS~ SF_2019$SELFING, xlab= ('selfing'), col= c(123, 456), ylab = ('viable seeds'),main = ('Viable seeds vs selfing: LEMA, PUPA y CHFU'))
t.test(SF_2019$VIABLES_SEEDS~SF_2019$SELFING, data= SF_2019)
SF_2019$SELFING <- as.factor(SF_2019$SELFING)
str(SF_2019)
SF_2019$VIABLES_SEEDS <- as.numeric(SF_2019$VIABLES_SEEDS)
par(mfrow=c(1,1))

SF <- ggplot(SF_2019, aes(x= SELFING, y = VIABLES_SEEDS))+
    geom_point(aes(color=PLANT))+
    xlab('SELFING')+
    ylab('SEMILLAS VIABLES')+
    ggtitle ('SEMILLAS VIABLES')+
NULL
SF #estaría mejor si consigo hacerlo con cajas, 2 por especie (1 de no viables y otra de viables)

#BUscar soluciones, no me sale este barplot
#PLANT= factor(SF_2019$PLANT, levels = c ("CHFU","LEMA"))
#barplot(table(SF_2029$VIABLES_SEEDS, PLANT), beside = TRUE, legend.text = T, col= c(123, 345))




