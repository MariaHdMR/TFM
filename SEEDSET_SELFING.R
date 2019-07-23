# seed set --> selfing 

#load libraries:
library(tidyverse)

#load data:
#Please load it here, or is hard to guess which file you are using.
# In fact, there is no data in the repo!! Add it as csv's.
# It means I am reading over the code, but without actually beeing able to run it.
SF_2019 <- read.csv("data/SF_19.csv", sep = ";")
head(SF_2019)

#SF_2019 es el archivo de selfing de pupa, lema y chfu
boxplot(SF_2019$VIABLES_SEEDS ~ SF_2019$SELFING, xlab= ('selfing'), col= c(123, 456,234), ylab = ('viable seeds'),main = ('Viable seeds vs selfing: LEMA, PUPA Y CHFU'))
t.test(SF_2019$VIABLES_SEEDS~SF_2019$SELFING, data= SF_2019)
aov(VIABLES_SEEDS ~ SELFING, data = SF_2019)
k <- lm(VIABLES_SEEDS ~ SELFING, data = SF_2019)
summary(k)
plot(k)
#I would not test all species at once, and if so, use random factors. 
#I would check model assumptions using plot(k) 
#and looking at the residuals distribution.
#I am ignoring this joint test for now.

SF_2019$SELFING <- as.factor(SF_2019$SELFING)
str(SF_2019)
SF_2019$VIABLES_SEEDS <- as.numeric(SF_2019$VIABLES_SEEDS) #alreadi integer in my dataset, so this is not needed.

par(mfrow=c(1,1)) 

SF <- ggplot(SF_2019, aes(x= SELFING, y = VIABLES_SEEDS))+
    geom_point(aes(color=PLANT))+
    xlab('SELFING')+
    ylab('SEMILLAS VIABLES')+
    ggtitle ('SEMILLAS VIABLES')+
NULL
SF #hacerlo con cajas estaría mejor


#Buscar soluciones, no me sale este barplot
#PLANT<-factor(SF_2019$PLANT, levels = c ("CHFU","LEMA"))
#barplot(table(SF_2019$VIABLES_SEEDS, PLANT), beside = T, legend.text = T, col= c(123, 345))
#IB: Con los datos me lo miro. 

#chfu----
SF_CHFU <- subset(SF_2019, PLANT== 'CHFU')
boxplot(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: CHFU'))
m <- lm(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)
summary(m)
plot(m) #check residuals. The model is not perfect, but is good enough. 
aov(VIABLES_SEEDS ~ SELFING, data = SF_CHFU) #I wuold report in text the summary results only.

#LEMA ----
SF_LEMA <- subset(SF_2019, PLANT== 'LEMA')
boxplot(SF_LEMA$VIABLES_SEEDS~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: LEMA'))
n <- lm(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)
summary(n)
plot(n) #this is completely fine.
aov(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)

#alternative analysis
SF_LEMA$TOTAL_SEEDS <- as.numeric(as.character(SF_LEMA$TOTAL_SEEDS))
boxplot((SF_LEMA$VIABLES_SEEDS/SF_LEMA$TOTAL_SEEDS)~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('% of viable seeds'), main = ('Viable seeds vs selfing: LEMA'), las = 1)
n1 <- glm(cbind(SF_LEMA$VIABLES_SEEDS,SF_LEMA$NO_VIABLES_SEEDS) ~ SF_LEMA$SELFING, family = "binomial")
summary(n1)
#SURPRISE, selfing has more viable seeds! This is super cool, because it match our results showing coleopterans damaging seed development.
#we do not use plot here, because the family is not gaussian. binomial is very robust to model assumptions, but if you want, you can test it with packahe dharma.
#Can you repeat this analysis for the other two plants?

#pupa ----
SF_PUPA <- subset(SF_2019, PLANT== 'PUPA')
boxplot(SF_PUPA$VIABLES_SEEDS~SF_PUPA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: PUPA'))
# I would double check the 2 outlyers (> 10 seeds). Maybe plot it without them for clarity?
boxplot(SF_PUPA$VIABLES_SEEDS~SF_PUPA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: PUPA'),
        ylim = c(0,5), las = 1) #las = 1 is also nice.
aov(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
O <- lm(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
summary(O)
plot(O) #good model.

#según los analisis anteriores se podría decir que a CHFU los polinizadores le producen un mayor fitness 
    #que a LEMA y PUPA, que no dependen tanto de los polinizadores. LEMa tiene altas tasas de autofecundación
#Gran primer resultado!

