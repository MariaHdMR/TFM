# script to generate a dataset including the sum of flower visits to all neighbours of a subplot
# 

library(tidyverse)

# function ----
# neighbours function, accepts a subplot ID and returns the ID of all neighbours
# considering either the "von Neumann neighbourhood" (4 neighbours) or the "Moore neighbourhood" (8 neigbours)
# the default is 8 neighbours (M), but you can change to 4 neighbours if you want by setting neigh = "VN"

CaracolesNeighbours <- function(ID, neigh = "M"){
  subplot.names <- paste(sort(rep(LETTERS[1:6],6)),rep(1:6,6),sep="")
  my.neigh <- NA
  if(ID %in% subplot.names){
    # border.positions <- subplot.names[grep("1|6|A|F",subplot.names)]
    subplot.matrix <- matrix(subplot.names,nrow = 6)
    my.position <- which(subplot.matrix == ID,arr.ind = T)
    low.border <- ifelse(my.position[1] == 1,T,F)
    up.border <- ifelse(my.position[1] == 6,T,F)
    left.border <- ifelse(my.position[2] == 1,T,F)
    right.border <- ifelse(my.position[2] == 6,T,F)
    
    if(neigh == "VN"){
      
      if(!low.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]])
      }
      if(!up.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]])
      }
      if(!left.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1],my.position[2]-1])
      }
      if(!right.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1],my.position[2]+1])
      }
      
    }else if(neigh == "M"){
      if(!low.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]])
        if(!right.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]+1])
        }
        if(!left.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]-1])
        }
        
      }
      if(!up.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]])
        if(!right.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]+1])
        }
        if(!left.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]-1])
        }
      }
      
      if(!left.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1],my.position[2]-1])
        if(!low.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]-1])
        }
        if(!up.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]-1])
        }
      }
      
      if(!right.border){
        my.neigh <- c(my.neigh,subplot.matrix[my.position[1],my.position[2]+1])
        if(!low.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]-1,my.position[2]+1])
        }
        if(!up.border){
          my.neigh <- c(my.neigh,subplot.matrix[my.position[1]+1,my.position[2]+1])
        }
      }
    }# if-else von-neumann or moore neigh

    # clean
    my.neigh <- sort(unique(my.neigh[!is.na(my.neigh)]))
    
  }# if valid ID
  
  # return
  my.neigh
}

# now, the data ----

# here, change to read you data. I am using 2016 data as uploaded to caracoles github

pollinators <- read.table("V_a_16_19.csv",header = T,sep = ";",stringsAsFactors = F)
#### DGC: asegúrate de que lee la tabla correctamente, mirando los datos
head(pollinators)
summary(pollinators)


p19 <- subset(pollinators, Year=='2019')
# only the columns we need
p.solo <- p19[,c("Plot","Subplot","Plant_Simple","Group","Abundances")]
pollinators.subset <- subset(p.solo, Plant_Simple %in% c("LEMA", "CHFU", "RAPE","ME","HOMA","PUPA","CHMI") & Group %in% c("Bees","Beetles","Butterflies", "Flies"))

# remove NAs and clean a little bit
pollinators.clean <- pollinators.subset[complete.cases(pollinators),]

##### DGC: a mí me funcionan estas dos lineas así
pollinators.clean2 <- pollinators.clean[pollinators.clean$Plot %in% 1:9,] #M:if I do that i have no data, It says that I have 0 rows.
pollinators.clean3 <- pollinators.clean2[pollinators.clean2$Subplot %in% paste(sort(rep(LETTERS[1:6],6)),rep(1:6,6),sep=""),] # M:and here, it happens the same with this script line. 

#### DGC: he ido cambiando el nombre después de cada cambio para ver si funciona bien
# cuando los datos ya están preparados, es más fácil ponerle un nombre sencillo :)
pol <- pollinators.clean3

# group by plant species and pollinator group
pol$visits <-pol$Abundances
plant.visits <- pol %>% group_by(Plant_Simple, Group, Plot, Subplot) %>% summarise(visits = sum(Abundances))

# this column will keep the number of visits to the neighbours
plant.visits$neigh.visits <- 0

# there are combinations of plants, pollinators, plots, and subplots that are not present in the data
# but if there are zero visits we also want to know it. This code includes these zeroes
plots <- unique(plant.visits$Plot)
subplots <- unique(plant.visits$Subplot)
plants <- unique(plant.visits$Plant_Simple)
pollinator.names <- unique(plant.visits$Group)

zero.visits <- expand.grid(plants,pollinator.names,plots,subplots)
names(zero.visits) <- c("Plant_Simple","Group","Plot","Subplot")

# join the observed visits and the zero visits
full.visits <- left_join(zero.visits,plant.visits)
full.visits[is.na(full.visits)] <- 0

# now, go for every plot, subplot, plant, and pollinator species and sum the neighbour visits
for(i.Plant_Simple in 1:length(plants)){
  for(i.Group in 1:length(pollinator.names)){
    for(i.Plot in 1:length(plots)){
      for(i.Subplot in 1:length(subplots)){
        
        # the neighbours of this particular subplot
        my.neigh <- CaracolesNeighbours(subplots[i.Subplot])
        
        # get the sum of the visits to the neighbour subplots, but only 
        # for the i.plant plant species and in the i.plot plot
        my.visits <- sum(full.visits$visits[full.visits$Plant_Simple == plants[i.Plant_Simple] & 
                                              full.visits$Group == pollinator.names[i.Group] &
                                              full.visits$Plot == plots[i.Plot] & 
                                              full.visits$Subplot %in% my.neigh])
        # add this sum to the data
        full.visits$neigh.visits[full.visits$Plant_Simple == plants[i.Plant_Simple] &
                                   full.visits$Group == pollinator.names[i.Group] &
                                   full.visits$Plot == plots[i.Plot] & 
                                   full.visits$Subplot == subplots[i.Subplot]] <- my.visits
      }# for each subplot
    }# for each plot
  }# for each pollinator
  
}# for each plant

# note that neigh.visits does not include the visits to the central subplot, only the neighbours!
# write the results
# again, change the path of the file
write.table(full.visits,file = "C:/Users/Cisco/Documents/TFM/results/neighbour_visits5.csv",sep=";",row.names = F)






