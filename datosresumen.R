#load libraries
library(tidyverse)

V_a_16_19 <-read.table("data/V_a_16_19.csv", header=T, sep=";")
FV_19 <- subset(V_a_16_19, Year == 2019) 
Abun_19 <-read.table("data/Abun_19.csv", header=T, sep=";")
FV_visits <-read.table("data/FV_16_19.csv", header=T, sep=";")
FV_visits <- subset(FV_visits, Year == 2019) 
FV.1 <- subset(FV_visits,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
FV.1$Plot <- as.numeric (FV.1$Plot)

Abun_19$Plot <- Abun_19$plot
Abun_19$Subplot <- Abun_19$subplot
Abun_19$Plant_Simple <- Abun_19$Sp.Focal
ab.19 <-Abun_19 %>% group_by(Plot, Subplot, Plant_Simple) %>% summarise (num.plantas = sum(Plantas))
FINAL <- dplyr::left_join(FV_19, ab.19)
FINAL1 <- subset(FINAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetles", "Flies", "Butterflies","Bees"))
FINAL2 <- FINAL1[,c("Plot","Subplot","Plant_Simple","Group","Abundances","num.plantas")] #970
FINAL3 <- FINAL2[which(complete.cases(FINAL2)),]
FINAL3 <- subset(FINAL3, Plot != "OUT") #969 datos
C1 <- FINAL3[,c("Plant_Simple","num.plantas")]
PLANTAS <- C1 %>% group_by(Plant_Simple)%>% summarise (n.pla = sum(num.plantas))
#write.table(PLANTAS, file= "C:/Users/Cisco/Documents/TFM/results/PlantasconABUNDANCIAS_POL.csv",  sep= ";", row.names = F)
C2 <- FINAL3[,c("Group","Abundances")]
VISITANTES <- C2 %>% group_by(Group)%>% summarise (n.poli = sum(Abundances))
#write.table(VISITANTES, file= "C:/Users/Cisco/Documents/TFM/results/Visitantes_conABUNDANCIAS_POL.csv",  sep= ";", row.names = F)

#############
TOTAL <- dplyr::left_join(ab.19, FV.1)
TOTAL1 <- subset(TOTAL,Plant_Simple %in% c("LEMA","CHFU","RAPE","ME", "HOMA","PUPA", "CHMI") & Group %in% c("Beetle", "Fly", "Butterfly","Bee"))
TOTAL2 <- TOTAL1[,c("Plot","Subplot","Plant_Simple","Group","Visits","num.plantas")] #970
TOTAL3 <- TOTAL2[which(complete.cases(TOTAL2)),]
TOTAL3 <- subset(TOTAL3, Plot != "OUT") #1023
C6 <- TOTAL3[,c("Group","Visits")]
visitas.poli <- C6 %>% group_by(Group)%>% summarise (n.visits = sum(Visits))
#write.table(visitas.poli, file= "C:/Users/Cisco/Documents/TFM/results/Visitantes_conVISITAS_POL.csv",  sep= ";", row.names = F)
C7 <- TOTAL3[,c("Plant_Simple","num.plantas")]
plantas.con.visits <- C7 %>% group_by(Plant_Simple)%>% summarise (n.plantas = sum(num.plantas))
#write.table(plantas.con.visits, file= "C:/Users/Cisco/Documents/TFM/results/Plantas_conVISITAS_POL.csv",  sep= ";", row.names = F)
