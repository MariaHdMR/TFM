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