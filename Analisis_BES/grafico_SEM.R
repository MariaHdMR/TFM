library(dplyr)
library(corrgram)
library(vegan)
library(varhandle)
library(semPlot)
library(lavaan)
library(grid)
library(gridExtra)
library(ggplot2)


data <- read.table("Analisis_BES/CHFU.csv", header= T , sep= ";") 
data <- as.data.frame(data) 
head(data)
data$distance <- as.factor(data$distance)

#CHUFU con fitness
chfu.1 <- subset(data, group== "fitness")
chfu.1 <- chfu.1[,c( "distance", "bee","inter" ,"intra")] 
chfu.1$distance <- factor(chfu.1$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.1 <- subset(chfu.1, distance != "7.5cm")
str(chfu.1)
chfu.1$distance <- as.factor(chfu.1$distance)
chfu.1$bee <- as.numeric(chfu.1$bee)
chfu.1$inter <- as.numeric(chfu.1$inter)
chfu.1$intra <- as.numeric(chfu.1$intra)


fitness.bee<-ggplot(data=chfu.1, aes(x=distance, y=bee,fill=distance)) +
    geom_bar(stat="identity")

fitness.bee1<-fitness.bee+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Bee to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)


fitness.inter<-ggplot(data=chfu.1, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

fitness.inter1<-fitness.inter+labs(x = NULL, y= NULL,size=200)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) #noworks

fitness.intra<-ggplot(data=chfu.1, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")

fitness.intra1<-fitness.intra+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)




#CHUFU con bee
chfu.2 <- subset(data, group== "bee")
chfu.2 <- chfu.2[,c( "distance","inter", "flower")] 
chfu.2$distance <- factor(chfu.2$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.2 <- subset(chfu.2, distance != "7.5cm")
bee.inter<-ggplot(data=chfu.2, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

bee.inter2<-bee.inter+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) # no works

bee.flower<-ggplot(data=chfu.2, aes(x=distance, y=flower,fill=distance)) +
    geom_bar(stat="identity")

bee.flower2<-bee.flower+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to bee", size=4)+
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
chfu.3 <- subset(chfu.3, distance != "7.5cm")
chfu.3$intra <- as.numeric(chfu.3$intra)
flower.inter<-ggplot(data=chfu.3, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

flower.inter2<-flower.inter+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flower.intra<-ggplot(data=chfu.3, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")

   

flower.intra2<-flower.intra+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(fitness.bee1,fitness.inter1,fitness.intra1,bee.inter2,bee.flower2,flower.inter2,flower.intra2, ncol=4,
              top = textGrob(expression(paste(italic("Chamaemelum fuscatum"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
              left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
              bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25))) #This is the barplot for CHFU with the 
            # seeds per fruit



#now the barplot with the total seeds per individual.
#CHUFU con fitness
chfu.1.indv <- subset(data, group== "fitness.indv")
chfu.1.indv <- chfu.1.indv[,c( "distance", "bee","inter" ,"intra")] 
chfu.1.indv$distance <- factor(chfu.1.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.1.indv <- subset(chfu.1, distance != "7.5cm")
str(chfu.1)
chfu.1.indv$distance <- as.factor(chfu.1.indv$distance)
chfu.1.indv$bee <- as.numeric(chfu.1.indv$bee)
chfu.1.indv$inter <- as.numeric(chfu.1.indv$inter)
chfu.1.indv$intra <- as.numeric(chfu.1.indv$intra)


fitness.bee.indv<-ggplot(data=chfu.1.indv, aes(x=distance, y=bee,fill=distance)) +
    geom_bar(stat="identity")

fitness.bee1.indv<-fitness.bee.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Bee to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)


fitness.inter.indv<-ggplot(data=chfu.1.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

fitness.inter1.indv<-fitness.inter.indv+labs(x = NULL, y= NULL,size=200)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) #noworks

fitness.intra.indv<-ggplot(data=chfu.1.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")

fitness.intra1.indv<-fitness.intra.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0)




#CHUFU con bee
chfu.2.indv <- subset(data, group== "bee.indv")
chfu.2.indv <- chfu.2.indv[,c( "distance","inter", "flower")] 
chfu.2.indv$distance <- factor(chfu.2.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.2.indv <- subset(chfu.2.indv, distance != "7.5cm")
bee.inter.indv<-ggplot(data=chfu.2.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

bee.inter2.indv<-bee.inter.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) # no works

bee.flower.indv<-ggplot(data=chfu.2.indv, aes(x=distance, y=flower,fill=distance)) +
    geom_bar(stat="identity")

bee.flower2.indv<-bee.flower.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

#CHUFU con flowers
chfu.3.indv <- subset(data, group== "flowers.indv")
chfu.3.indv$distance <- factor(chfu.3.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
chfu.3.indv <- chfu.3.indv[,c("distance","inter", "intra")] 
chfu.3.indv <- subset(chfu.3.indv, distance != "7.5cm")
chfu.3.indv$intra <- as.numeric(chfu.3.indv$intra)
flower.inter.indv<-ggplot(data=chfu.3.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")

flower.inter2.indv<-flower.inter.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flower.intra.indv<-ggplot(data=chfu.3.indv, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")



flower.intra2.indv<-flower.intra.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(fitness.bee1.indv,fitness.inter1.indv,fitness.intra1.indv,bee.inter2.indv,bee.flower2.indv,flower.inter2.indv,flower.intra2.indv, ncol=4,
             top = textGrob(expression(paste(italic("Chamaemelum fuscatum"), " with total seeds/individual")), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))





#lema----

data.lema <- read.table("Analisis_BES/LEMA.csv", header= T , sep= ";")
head(data.lema)
str(data.lema)
lema.1 <- subset(data.lema, group== "fitness")
lema.1 <- lema.1[,c("distance", "beetle","intra", "inter")] 
lema.1$distance <- factor(lema.1$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.1 <- subset(lema.1, distance != "7.5cm")

seed.bet.lema<-ggplot(data=lema.1, aes(x=distance, y=beetle,fill=distance)) +
    geom_bar(stat="identity")
seed.bet.lema1<-seed.bet.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Beetle to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.inter.lema<-ggplot(data=lema.1, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seed.inter.lema1<-seed.inter.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.intra.lema<-ggplot(data=lema.1, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seed.intra.lema1<-seed.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


lema.2 <- subset(data.lema, group== "beetle")
lema.2 <- lema.2[,c("distance", "flowers","inter")] 
lema.2$distance <- factor(lema.2$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.2 <- subset(lema.2, distance != "7.5cm")

bet.flo.lema<-ggplot(data=lema.2, aes(x=distance, y=flowers,fill=distance)) +
    geom_bar(stat="identity")
bet.flo.lema1<-bet.flo.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

bet.inter.lema<-ggplot(data=lema.2, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bet.inter.lema1<-bet.inter.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.3 <- subset(data.lema, group== "fly")
lema.3 <- lema.3[,c("distance", "intra")] 
lema.3$distance <- factor(lema.3$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.3 <- subset(lema.3, distance != "7.5cm")

fly.intra.lema<-ggplot(data=lema.3, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")
fly.intra.lema1<-fly.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.4 <- subset(data.lema, group== "flower")
lema.4 <- lema.4[,c("distance", "inter", "intra")] 
lema.4$distance <- factor(lema.4$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.4 <- subset(lema.4, distance != "7.5cm")

flo.intra.lema<-ggplot(data=lema.4, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
flo.intra.lema1<-flo.intra.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flo.inter.lema<-ggplot(data=lema.4, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
flo.inter.lema1<-flo.inter.lema+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(seed.bet.lema1,seed.inter.lema1,seed.intra.lema1,bet.flo.lema1,bet.inter.lema1, 
             fly.intra.lema1,flo.intra.lema1,flo.inter.lema1 ,ncol=4,
             top = textGrob(expression(paste(italic("Leontodon maroccanus"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))#this is the
#       barplot of LEMA with the seeds/fruit


#Now, I'm going to do the same barplot for Lema but, with the total seeds per individual. 

lema.1.indv <- subset(data.lema, group== "fitness.indv")
lema.1.indv <- lema.1.indv[,c("distance", "beetle","intra", "inter")] 
lema.1.indv$distance <- factor(lema.1.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.1.indv <- subset(lema.1.indv, distance != "7.5cm")

seed.bet.lema.indv<-ggplot(data=lema.1.indv, aes(x=distance, y=beetle,fill=distance)) +
    geom_bar(stat="identity")
seed.bet.lema1.indv<-seed.bet.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Beetle to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.inter.lema.indv<-ggplot(data=lema.1.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seed.inter.lema1.indv<-seed.inter.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.intra.lema.indv<-ggplot(data=lema.1.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seed.intra.lema1.indv<-seed.intra.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


lema.2.indv <- subset(data.lema, group== "beetle.indv")
lema.2.indv <- lema.2.indv[,c("distance", "flowers","inter")] 
lema.2.indv$distance <- factor(lema.2.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.2.indv <- subset(lema.2.indv, distance != "7.5cm")

bet.flo.lema.indv<-ggplot(data=lema.2.indv, aes(x=distance, y=flowers,fill=distance)) +
    geom_bar(stat="identity")
bet.flo.lema1.indv<-bet.flo.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

bet.inter.lema.indv<-ggplot(data=lema.2.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bet.inter.lema1.indv<-bet.inter.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to beetle", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.3.indv <- subset(data.lema, group== "fly.indv")
lema.3.indv <- lema.3.indv[,c("distance", "intra")] 
lema.3.indv$distance <- factor(lema.3.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.3.indv <- subset(lema.3.indv, distance != "7.5cm")

fly.intra.lema.indv<-ggplot(data=lema.3.indv, aes(x=distance, y=intra, fill=distance)) +
    geom_bar(stat="identity")
fly.intra.lema1.indv<-fly.intra.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

lema.4.indv <- subset(data.lema, group== "flower.indv")
lema.4.indv <- lema.4.indv[,c("distance", "inter", "intra")] 
lema.4.indv$distance <- factor(lema.4.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
lema.4.indv <- subset(lema.4.indv, distance != "7.5cm")

flo.intra.lema.indv<-ggplot(data=lema.4.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
flo.intra.lema1.indv<-flo.intra.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

flo.inter.lema.indv<-ggplot(data=lema.4.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
flo.inter.lema1.indv<-flo.inter.lema.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to flower", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(seed.bet.lema1.indv,seed.inter.lema1.indv,seed.intra.lema1.indv,bet.flo.lema1.indv,bet.inter.lema1.indv, 
             fly.intra.lema1.indv,flo.intra.lema1.indv,flo.inter.lema1.indv,ncol=4,
             top = textGrob(expression(paste(italic("Leontodon maroccanus"), " with total seeds/individual")), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))



#pupa----

data.pupa <- read.table("Analisis_BES/pupa.csv", header= T , sep= ";")
head(data.pupa)
str(data.pupa)
pupa.1 <- subset(data.pupa, group== "fitness")
pupa.1 <- pupa.1[,c("distance", "bee","intra", "inter")] 
pupa.1$distance <- factor(pupa.1$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.1 <- subset(pupa.1, distance != "7.5cm")

seed.bee.pupa<-ggplot(data=pupa.1, aes(x=distance, y=bee,fill=distance)) +
    geom_bar(stat="identity")
seed.bee.pupa1<-seed.bee.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Bee to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.inter.pupa<-ggplot(data=pupa.1, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seed.inter.pupa1<-seed.inter.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.intra.pupa<-ggplot(data=pupa.1, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seed.intra.pupa1<-seed.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

pupa.2 <- subset(data.pupa, group== "bee")
pupa.2 <- pupa.2[,c("distance", "intra", "inter", "flower")] 
pupa.2$distance <- factor(pupa.2$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.2 <- subset(pupa.2, distance != "7.5cm")

bee.inter.pupa<-ggplot(data=pupa.2, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bee.inter.pupa1<-bee.inter.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


bee.flower.pupa<-ggplot(data=pupa.2, aes(x=distance, y=flower,fill=distance)) +
    geom_bar(stat="identity")
bee.flower.pupa1<-bee.flower.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

bee.intra.pupa<-ggplot(data=pupa.2, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
bee.intra.pupa1<-bee.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

pupa.3 <- subset(data.pupa, group== "fly")
pupa.3 <- pupa.3[,c("distance", "intra")] 
pupa.3$distance <- factor(pupa.3$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.3 <- subset(pupa.3, distance != "7.5cm")

fly.intra.pupa<-ggplot(data=pupa.3, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
fly.intra.pupa1<-fly.intra.pupa+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(seed.bee.pupa1,seed.inter.pupa1,seed.intra.pupa1,bee.inter.pupa1,bee.flower.pupa1,
             bee.intra.pupa1,fly.intra.pupa1 ,ncol=4,
             top = textGrob(expression(paste(italic("Pulicaria paludosa"))), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))#This is
#       the barplot for PUPA with the seeds per fruit


#Now, i'm going to do the same barplot but, with the number of seeds per individual. 
pupa.1.indv <- subset(data.pupa, group== "fitness.indv")
pupa.1.indv <- pupa.1.indv[,c("distance", "bee","intra", "inter")] 
pupa.1.indv$distance <- factor(pupa.1.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.1.indv <- subset(pupa.1.indv, distance != "7.5cm")

seed.bee.pupa.indv<-ggplot(data=pupa.1.indv, aes(x=distance, y=bee,fill=distance)) +
    geom_bar(stat="identity")
seed.bee.pupa1.indv<-seed.bee.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Bee to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.inter.pupa.indv<-ggplot(data=pupa.1.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
seed.inter.pupa1.indv<-seed.inter.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

seed.intra.pupa.indv<-ggplot(data=pupa.1.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
seed.intra.pupa1.indv<-seed.intra.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fitness", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

pupa.2.indv <- subset(data.pupa, group== "bee.indv")
pupa.2.indv <- pupa.2.indv[,c("distance", "intra", "inter", "flower")] 
pupa.2.indv$distance <- factor(pupa.2.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.2.indv <- subset(pupa.2.indv, distance != "7.5cm")

bee.inter.pupa.indv<-ggplot(data=pupa.2.indv, aes(x=distance, y=inter,fill=distance)) +
    geom_bar(stat="identity")
bee.inter.pupa1.indv<-bee.inter.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Interspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


bee.flower.pupa.indv<-ggplot(data=pupa.2.indv, aes(x=distance, y=flower,fill=distance)) +
    geom_bar(stat="identity")
bee.flower.pupa1.indv<-bee.flower.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Flower to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

bee.intra.pupa.indv<-ggplot(data=pupa.2.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
bee.intra.pupa1.indv<-bee.intra.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to bee", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 

pupa.3.indv <- subset(data.pupa, group== "fly.indv")
pupa.3.indv <- pupa.3.indv[,c("distance", "intra")] 
pupa.3.indv$distance <- factor(pupa.3.indv$distance, levels = c('7.5cm', '1m', '3m','6m'))
pupa.3.indv <- subset(pupa.3.indv, distance != "7.5cm")

fly.intra.pupa.indv<-ggplot(data=pupa.3.indv, aes(x=distance, y=intra,fill=distance)) +
    geom_bar(stat="identity")
fly.intra.pupa1.indv<-fly.intra.pupa.indv+labs(x = NULL, y= NULL,size=30)+ 
    annotate("text", x = 2, y = 0.8, label = "Intraspecific to fly", size=4)+
    theme(axis.title.y = element_text(size=15))+
    theme(axis.title.x = element_text(size=15))+
    theme(axis.line = element_line(colour="black"))+
    scale_y_continuous(limits=c(-1,1), breaks=seq(-1,1, by = .25), expand=c(0,0))+
    theme(     panel.background  = element_blank())+
    geom_hline(yintercept = 0) 


grid.arrange(seed.bee.pupa1.indv,seed.inter.pupa1.indv,seed.intra.pupa1.indv,bee.inter.pupa1.indv,bee.flower.pupa1.indv,
             bee.intra.pupa1.indv,fly.intra.pupa1.indv,ncol=4,
             top = textGrob(expression(paste(italic("Pulicaria paludosa"), " with total seeds/individual")), vjust = 0.4, gp = gpar(fontface = "bold", cex = 2)),
             left = textGrob("Standarized total effects", rot = 90, vjust = 0.4, gp=gpar(cex=1.25)), 
             bottom = textGrob("Aggregation level of plants", vjust = 0.4, gp = gpar( cex = 1.25)))

