# neighbours of each focal at different distances:
# d1- 7.5 cm, using competition data
# d2- 1m^2, subplot data, using abundances data
# d3- 8 most nearest subplots, using abundances data
# d4- whole plot

# NOTE: subplots on the edges of the plot
# do not have 8 neighbouring subplots
# perhaps it's better to remove them:
# A1,B1,C1,D1,E1,F1
# A1,A2,A3,A4,A5,A6
# A6,B6,C6,D6,E6,F6
# F1,F2,F3,F4,F5,F6

# load packages
library(tidyverse)

#source("R/CaracolesNeighbours.R")
source("C:/Users/Cisco/Documents/TFM/CaracolesNeighbours.R")
# read data
comp <- read.csv2("C:/Users/Cisco/Documents/TFM/data/Data_2020/competition_caracoles2020.csv",stringsAsFactors = FALSE)
comp<- subset(comp, plot != 4)
str(comp)
#antes de empezar m?s voy a seleccionar las plantas qu equiero para esta primera fase de la fenologia
#comp <- comp[,c("year","month", "day", "plot", "subplot","focal", "fruit", "seed", "CHFU", "LEMA", "SOAS", "SCLA", "CHMI", "HOMA")] 
#comp <- subset(comp,  focal%in% c("CHFU", "LEMA", "SOAS", "SCLA", "CHMI", "HOMA"))


abund <- read.csv2("C:/Users/Cisco/Documents/TFM/data/Data_2020/Abundances_2020.csv",stringsAsFactors = FALSE)
#abund <- subset(abund,  species %in% c("CHFU", "LEMA", "SOAS", "SCLA", "CHMI", "HOMA"))
abund <- dplyr::arrange(abund,year,month,day,plot,subplot,species)
#antes de empezar m?s voy a seleccionar las plantas qu equiero para esta primera fase de la fenologia


library(dplyr)

abund$unique_id <- paste(abund$plot, abund$subplot, abund$species, sep="_")
abund <- abund %>% distinct(unique_id,.keep_all = TRUE)
abund2 <- abund %>% group_by(year, plot, subplot,species, unique_id) %>% summarise (indv = sum(individuals))%>%
  ungroup()
abund2<- subset(abund2, plot != 4)
abund.1 <- abund2[,c("year","plot", "subplot","species", "indv")]

# convert abundances to wide format
abund.wide <- abund.1 %>% spread(key = species,value = indv
                                 ,fill = 0)


#comp <- comp[,c("year","month", "day", "plot", "subplot","focal", "fruit", "seed" , "CHFU", "CHMI", "HOMA", "LEMA", "SCLA", "SOAS")]

#abund.wide <- abund.wide[,c("year","month", "day", "plot", "subplot","CHFU", "CHMI", "HOMA", "LEMA", "SCLA", "SOAS")]

abund.wide <- na.omit(abund.wide)



# set of species that are sampled in both datasets
all.sp <- sort(intersect(unique(comp$focal),unique(abund$species)))


# create results dataframe
neigh <- data.frame(comp[,c("year","plot","subplot","focal")])

# neighbours of the same species

neigh$d1.intra <- 0
neigh$d2.intra <- 0
neigh$d3.intra <- 0
neigh$d4.intra <- 0
# neighbours of different species
neigh$d1.inter <- 0
neigh$d2.inter <- 0
neigh$d3.inter <- 0
neigh$d4.inter <- 0

# go through each individual
for(i.focal in 1:nrow(comp)){
  # identity of this focal
  my.sp <- comp$focal[i.focal]
  other.sp <- all.sp[which(all.sp != my.sp)]
  
  # neighbours at 7.5cm
  neigh$d1.intra[i.focal] <- comp[i.focal,which(names(comp) == my.sp)]
  neigh$d1.inter[i.focal] <- sum(comp[i.focal,which(names(comp) %in% other.sp)])
  if(length(which(abund.wide$plot == neigh$plot[i.focal])) == 0) next
  if (length(which(abund.wide$subplot == neigh$subplot[i.focal])) == 0) next
  if (length(which(abund.wide$year == neigh$year[i.focal])) == 0) next
  
  # neighbours in subplot using abundances
  my.subplot <- which(abund.wide$year == neigh$year[i.focal] & 
                        abund.wide$plot == neigh$plot[i.focal] &
                        abund.wide$subplot == neigh$subplot[i.focal])
  if (length(my.subplot) == 0) next
  neigh$d2.intra[i.focal] <- sum(abund.wide[my.subplot,which(names(abund.wide) == my.sp)])
  neigh$d2.inter[i.focal] <- sum(abund.wide[my.subplot,which(names(abund.wide) %in% other.sp)])

  # neighbours in 8 subplots
  # if subplot is on a side/corner, there are less neighbours
  my.neigh_ID <- CaracolesNeighbours(comp$subplot[i.focal],"M")
  # 
  
  my.neigh <- which(abund.wide$year == neigh$year[i.focal] & 
                      abund.wide$plot == neigh$plot[i.focal] &
                      abund.wide$subplot %in% my.neigh_ID)
  neigh$d3.intra[i.focal] <- sum(abund.wide[my.neigh,which(names(abund.wide) == my.sp)])
  neigh$d3.inter[i.focal] <- sum(abund.wide[my.neigh,which(names(abund.wide) %in% other.sp)])
  
  # neighbours in plot
  
  my.plot <- which(abund.wide$year == neigh$year[i.focal] & 
                     abund.wide$plot == neigh$plot[i.focal])
  
  neigh$d4.intra[i.focal] <- sum(abund.wide[my.plot,which(names(abund.wide) == my.sp)])
  neigh$d4.inter[i.focal] <- sum(abund.wide[my.plot,which(names(abund.wide) %in% other.sp)])
}

# convert to long format
# it's easier to do it separately 
# for intra and inter neighbours
neigh.long.intra <- gather(neigh[,c("year","plot",
                                    "subplot","focal",
                                    "d1.intra","d2.intra",
                                    "d3.intra","d4.intra")],
                           key = "distance",
                           value = "neigh_intra",
                           d1.intra,d2.intra,d3.intra,d4.intra)
neigh.long.intra$distance <- sub(".intra","",neigh.long.intra$distance)

neigh.long.inter <- gather(neigh[,c("year","plot",
                                    "subplot","focal",
                                    "d1.inter","d2.inter",
                                    "d3.inter","d4.inter")],
                           key = "distance",
                           value = "neigh_inter",
                           d1.inter,d2.inter,d3.inter,d4.inter)
neigh.long.inter$distance <- sub(".inter","",neigh.long.inter$distance)

# join again intra and inter
neigh.long <- dplyr::left_join(neigh.long.intra,neigh.long.inter)

# add a column to know if this subplot is an edge subplot
edges <- c("A1","B1","C1","D1","E1","F1",
           "A2","A3","A4","A5","A6",
           "B6","C6","D6","E6","F6",
           "F2","F3","F4","F5")
neigh.long$edge <- neigh.long$subplot %in% edges

# small test
tt <- neigh.long %>% 
  group_by(subplot) %>% 
  mutate(sum_intra = sum(neigh_intra),sum_inter = sum(neigh_inter))

ggplot(tt,aes(x = subplot,y = sum_intra)) + geom_point(aes(color = edge)) 

write.csv2(neigh.long,file = "C:/Users/Cisco/Documents/TFM/data/focal_neighbours.2020_2nphenology.csv",row.names = FALSE)

