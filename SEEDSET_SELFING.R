# seed set --> selfing 

SF_2019 <- read.table("SF_19.csv", header= T, sep= ";")


library(tidyverse)

boxplot(SF_2019$VIABLES_SEEDS~ SF_2019$SELFING, xlab= ('selfing'), col= c(123, 456,234), ylab = ('viable seeds'),main = ('Viable seeds vs selfing: LEMA, PUPA Y CHFU'))
t.test(SF_2019$VIABLES_SEEDS~SF_2019$SELFING, data= SF_2019)

aov(VIABLES_SEEDS ~ SELFING, data = SF_2019)
k <- lm(VIABLES_SEEDS ~ SELFING, data = SF_2019)
summary(k)

SF_2019$SELFING <- as.factor(SF_2019$SELFING)
str(SF_2019)
SF_2019$VIABLES_SEEDS <- as.numeric(SF_2019$VIABLES_SEEDS)

#chfu----
SF_CHFU <- subset(SF_2019, PLANT== 'CHFU')
boxplot(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: CHFU'),col=c("blue4", "chartreuse"))
m <- lm(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)
summary(m)
aov(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)

#otra manera
SF_CHFU$TOTAL_SEEDS <- as.numeric(as.character(SF_CHFU$TOTAL_SEEDS))
boxplot((SF_CHFU$VIABLES_SEEDS/SF_CHFU$TOTAL_SEEDS)~SF_CHFU$SELFING, xlab= ('selfing'), ylab = ('% of viable seeds'), main = ('Viable seeds vs selfing: CHFU'), las = 1, col=c("blue4", "chartreuse"))
n3 <- glm(cbind(SF_CHFU$VIABLES_SEEDS,SF_CHFU$NO_VIABLES_SEEDS) ~ SF_CHFU$SELFING, family = "binomial") #no funciona
summary(n3)

#El primer boxplot(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING) sale significiativo, mientras que el segundo no (SF_CHFU$VIABLES_SEEDS/SF_CHFU$TOTAL_SEEDS)~SF_CHFU$SELFING). 
#Esto puede ser porque el selfing en CHFU parece que afecta al numero total de semillas, y no tanto en la viabilidad de estas.

#LEMA ----
SF_LEMA <- subset(SF_2019, PLANT== 'LEMA')
boxplot(SF_LEMA$VIABLES_SEEDS~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: LEMA'),col=c("blue4", "chartreuse"))
n <- lm(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)
summary(n)
aov(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)

#otra manera
SF_LEMA$TOTAL_SEEDS <- as.numeric(as.character(SF_LEMA$TOTAL_SEEDS))
boxplot((SF_LEMA$VIABLES_SEEDS/SF_LEMA$TOTAL_SEEDS)~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('% of viable seeds'), main = ('Viable seeds vs selfing: LEMA'), las = 1, col=c("blue4", "chartreuse"))
n1 <- glm(cbind(SF_LEMA$VIABLES_SEEDS,SF_LEMA$NO_VIABLES_SEEDS) ~ SF_LEMA$SELFING, family = "binomial")
summary(n1)
#SURPRISE, selfing has more viable seeds! This is super cool, because it match our results showing coleopterans damaging seed development.
#we do not use plot here, because the family is not gaussian. binomial is very robust to model assumptions, but if you want, you can test it with packahe dharma.
#Can you repeat this analysis for the other two plants?

#pupa ----
SF_PUPA <- subset(SF_2019, PLANT== 'PUPA')
boxplot(SF_PUPA$VIABLES_SEEDS~SF_PUPA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), outline= F, las=1, main = ('Viable seeds vs selfing: PUPA'),col=c("blue4", "chartreuse"))
aov(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
O <- lm(VIABLES_SEEDS ~ SELFING, data = SF_PUPA)
summary(O)
plot(O)#good model.
#otra manera

SF_PUPA$TOTAL_SEEDS <- as.numeric(as.character(SF_PUPA$TOTAL_SEEDS))
boxplot((SF_PUPA$VIABLES_SEEDS/SF_PUPA$TOTAL_SEEDS)~SF_PUPA$SELFING, xlab= ('selfing'), ylab = ('% of viable seeds'), main = ('Viable seeds vs selfing: PUPA'), las = 1, col=c("blue4", "chartreuse"), outline=F)
n2 <- glm(cbind(SF_PUPA$VIABLES_SEEDS,SF_PUPA$NO_VIABLES_SEEDS) ~ SF_PUPA$SELFING, family = "binomial")
summary(n2)


#según los analisis anteriores se podría decir que a CHFU los polinizadores le producen un mayor fitness 
    #que a LEMA y PUPA, que no dependen tanto de los polinizadores. LEMa tiene altas tasas de autofecundación
