# seed set --> selfing 
library(tidyverse)
boxplot(SF_2019$VIABLES_SEEDS~ SF_2019$SELFING, xlab= ('selfing'), col= c(123, 456), ylab = ('viable seeds'),main = ('Viable seeds vs selfing: LEMA Y CHFU'))
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
SF
#ANOVAS
m <- lm(VIABLES_SEEDS ~ SELFING, data = SF_CHFU)
summary(m)
plot(m)
aov(VIABLES_SEEDS ~ SELFING, data = SF_LEMA)

#BUscar soluciones, no me sale este barplot
PLANT= factor(SF_2019$PLANT, levels = c ("CHFU","LEMA"))
barplot(table(SF_2029$VIABLES_SEEDS, PLANT), beside = TRUE, legend.text = T, col= c(123, 345))




#chfu
SF_CHFU <- subset(SF_2019, PLANT== 'CHFU')
boxplot(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: CHFU'))
CHFU <- t.test(SF_CHFU$VIABLES_SEEDS~SF_CHFU$SELFING, data= SF_CHFU)
#LEMA
SF_LEMA <- subset(SF_2019, PLANT== 'LEMA')
boxplot(SF_LEMA$VIABLES_SEEDS~SF_LEMA$SELFING, xlab= ('selfing'), ylab = ('viable seeds'), main = ('Viable seeds vs selfing: LEMA'))
LEMA <- t.test(SF_LEMA$VIABLES_SEEDS~SF_LEMA$SELFING, data= SF_LEMA)



