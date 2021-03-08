library(dplyr)
library(corrgram)
library(vegan)
library(varhandle)
library(semPlot)
library(lavaan)
library(grid)
library(gridExtra)
library(ggplot2)


data <- read.table("Analisis_BES/CHFUnew.csv", header= T , sep= ";") 
data <- as.data.frame(data) 
head(data) #Todos los valores entre 0 y 1, no hace falta modificar nada
data$distance <- as.factor(data$distance)

#CHUFU con seed_fruit
chfu.1 <- subset(data, group== "seed_fruit")
chfu.1 <- chfu.1[,c( "distance", "bee","inter" ,"intra")] 
chfu.1$distance <- factor(chfu.1$distance, levels = c('7.5cm', '1m', '3m','6m'))
#chfu.1 <- subset(chfu.1, distance != "7.5cm")
str(chfu.1)
chfu.1$distance <- as.factor(chfu.1$distance)
chfu.1$bee <- as.numeric(chfu.1$bee)
chfu.1$inter <- as.numeric(chfu.1$inter)
chfu.1$intra <- as.numeric(chfu.1$intra)


fitness.bee<-ggplot(data=chfu.1, aes(x=distance, y=bee,fill=distance)) +
    geom_bar(stat="identity")

fitness.bee1<-fitness.bee+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Bee to seeds per fruit", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)


#chfu con total seeds
chfu.4 <- subset(data, group== "seed_total")
chfu.4 <- chfu.4[,c( "distance", "bee","inter" ,"intra")] 
chfu.4$distance <- factor(chfu.4$distance, levels = c('7.5cm', '1m', '3m','6m'))
str(chfu.4)
chfu.4$distance <- as.factor(chfu.4$distance)
chfu.4$bee <- as.numeric(chfu.4$bee)
chfu.4$inter <- as.numeric(chfu.4$inter)
chfu.4$intra <- as.numeric(chfu.4$intra)


fitness.intra.1<-ggplot(data=chfu.4, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")

fitness.intra.1<-fitness.intra.1+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Intraspecific to total seeds", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)



#CHUFU con bee
chfu.2 <- subset(data, group== "bee")
chfu.2 <- chfu.2[,c( "distance","inter")] 
chfu.2$distance <- factor(chfu.2$distance, levels = c('7.5cm', '1m', '3m','6m'))

bee.inter<-ggplot(data=chfu.2, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

bee.inter2<-bee.inter+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Interspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)



#CHUFU con flowers
chfu.3 <- subset(data, group== "flowers")
chfu.3$distance <- factor(chfu.3$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.3 <- chfu.3[,c("distance","inter", "intra")] 
chfu.3$intra <- as.numeric(chfu.3$intra)
flower.inter<-ggplot(data=chfu.3, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

flower.inter2<-flower.inter+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flower.intra<-ggplot(data=chfu.3, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")

flower.intra2<-flower.intra+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(fitness.bee1,fitness.intra.1,bee.inter2,flower.inter2, flower.intra2, ncol=4,
              top = textGrob(expression(paste(italic("Chamaemelum fuscatum"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
              left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
              bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25))) #This is the barplot for CHFU with the 
            # seeds per fruit




#lema----

data.lema <- read.table("Analisis_BES/LEMAnew.csv", header= T , sep= ";")
head(data.lema)
str(data.lema)
data.lema2 <- data.lema

#Para dibujar el barplot sin problemas dentro de la escala entre -1 y 1, lo que voy a hacer es cambiar
# los valores mayores de 1 por 1, simplemente para ver la tendencia de los gráficos. Para ello voy a crear
# otra base de datos donde esten estos valores modificados. 

data.lema2[,3:6][data.lema2 [,3:6] >1 ] <- 1

lema.1 <- subset(data.lema2, group== "seed_fruit")
lema.1 <- lema.1[,c("distance", "beetle", "fly")] 
lema.1$distance <- factor(lema.1$distance, levels = c('7.5cm', '1m', '3m','6m'))



seed.bet.lema<-ggplot(data=lema.1, aes(x=distance, y=beetle,fill=distance)) +
    geom_bar(stat="identity")
seed.bet.lema1<-seed.bet.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = -0.5, label = "Beetle to seeds per fruit", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.fly.lema3<-ggplot(data=lema.1, aes(x=distance, y=fly, fill=distance)) +
    geom_bar(stat="identity")
seed.fly.lema1<-seed.fly.lema3+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = -0.5, label = "Fly to seeds per fruit", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.5 <- subset(data.lema, group== "seed_total")
lema.5 <- lema.5[,c("distance", "intra","inter")] 
lema.5$distance <- factor(lema.5$distance, levels = c('7.5cm', '1m', '3m','6m'))

seed.t.lema.intra<-ggplot(data=lema.5, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seed.t.lema1.intra<-seed.t.lema.intra+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Intraspecific to \n total seeds", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.t.lema.inter<-ggplot(data=lema.5, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seed.t.lema1.inter<-seed.t.lema.inter+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Interspecific to \n total seeds", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 



lema.2 <- subset(data.lema, group== "beetle")
lema.2 <- lema.2[,c("distance", "intra","inter")] 
lema.2$distance <- factor(lema.2$distance, levels = c('7.5cm', '1m', '3m','6m'))


bet.intra.lema<-ggplot(data=lema.2, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
bet.intra.lema1<-bet.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Intraspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

bet.inter.lema<-ggplot(data=lema.2, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bet.inter.lema1<-bet.inter.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Interspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.3 <- subset(data.lema, group== "fly")
lema.3 <- lema.3[,c("distance", "intra")] 
lema.3$distance <- factor(lema.3$distance, levels = c('7.5cm', '1m', '3m','6m'))

fly.intra.lema<-ggplot(data=lema.3, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")
fly.intra.lema1<-fly.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.4 <- subset(data.lema, group== "flower")
lema.4 <- lema.4[,c("distance", "inter", "intra")] 
lema.4$distance <- factor(lema.4$distance, levels = c('7.5cm', '1m', '3m','6m'))

flo.intra.lema<-ggplot(data=lema.4, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
flo.intra.lema1<-flo.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flo.inter.lema<-ggplot(data=lema.4, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
flo.inter.lema1<-flo.inter.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.6, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(seed.bet.lema1, seed.fly.lema1, seed.t.lema1.intra,seed.t.lema1.inter, bet.intra.lema1, bet.inter.lema1, 
             fly.intra.lema1,flo.intra.lema1, flo.inter.lema1 ,ncol=4,
             top = textGrob(expression(paste(italic("Leontodon maroccanus"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))


#pupa----

data.pupa <- read.table("Analisis_BES/PUPAnew.csv", header= T , sep= ";")
head(data.pupa)
str(data.pupa)
pupa.1 <- subset(data.pupa, group== "seed_fruit")
pupa.1 <- pupa.1[,c("distance", "fly")] 
pupa.1$distance <- factor(pupa.1$distance, levels = c('7.5cm', '1m', '3m','6m'))

seed.fly.pupa<-ggplot(data=pupa.1, aes(x=distance, y=fly,fill=distance)) +
    geom_bar(stat="identity")
seed.fly.pupa1<-seed.fly.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Fly to seeds per fruit", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 




pupa.4 <- subset(data.pupa, group== "seed_total")
pupa.4 <- pupa.4[,c("distance", "intra", "inter")] 
pupa.4$distance <- factor(pupa.4$distance, levels = c('7.5cm', '1m', '3m','6m'))


seedt.inter.pupa<-ggplot(data=pupa.4, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seedt.inter.pupa1<-seedt.inter.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Interspecific to total seeds", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seedt.intra.pupa<-ggplot(data=pupa.4, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seedt.intra.pupa1<-seedt.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Intraspecific to total seeds", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 



pupa.2 <- subset(data.pupa, group== "fly")
pupa.2 <- pupa.2[,c("distance", "intra")] 
pupa.2$distance <- factor(pupa.2$distance, levels = c('7.5cm', '1m', '3m','6m'))


fly.intra.pupa<-ggplot(data=pupa.2, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
fly.intra.pupa1<-fly.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 




pupa.3 <- subset(data.pupa, group== "beetle")
pupa.3 <- pupa.3[,c("distance", "intra", "inter")] 
pupa.3$distance <- factor(pupa.3$distance, levels = c('7.5cm', '1m', '3m','6m'))


bet.intra.pupa<-ggplot(data=pupa.3, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
bet.intra.pupa1<-bet.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Intraspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


bet.inter.pupa<-ggplot(data=pupa.3, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bet.inter.pupa1<-bet.inter.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2.5, y = 0.8, label = "Interspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

grid.arrange(seed.fly.pupa1, seedt.inter.pupa1,seedt.intra.pupa1,fly.intra.pupa1, 
             bet.intra.pupa1,bet.inter.pupa1 ,ncol=4,
             top = textGrob(expression(paste(italic("Pulicaria paludosa"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))#This is
#       the barplot for PUPA with the seeds per fruit


