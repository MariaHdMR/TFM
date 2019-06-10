#Play with simulations


#load package
#devtools::install_github("ibartomeus/cxr")
library("cxr")
#other packages
library(tidyverse)


?cxr_optimize

data("competition")
head(competition)
?competition

# 1) Parametrizar los modelos----

# Caracoles competition data
competition.data <- competition
head(competition.data)

# spread the data from long to wide format
#competition.data <- spread(data = competition.data, key = competitor,
  #                         value = number, fill = 0)
library(reshape2)
competition.data <- dcast(data = competition.data, plot + subplot + focal + fruit + seed ~ competitor, 
      fun.aggregate = sum, value.var = "number")
head(competition.data)

# how many focal species
focal.sp <- unique(competition.data$focal)

# competition matrix
comp.matrix <- as.matrix(competition.data[,10:ncol(competition.data)])

# covariate: salinity
data("salinity")
head(salinity)

# one observation per row of competition.data
full.data <- left_join(competition.data,salinity)

#############################
# simulation parameters

# models to parameterize
# be aware of including 4 and 5 ONLY if there are covariates
# otherwise it makes no sense (see equations in Lanuza et al. 2018)
models <- 3:5

# which values do we optimize for each model?
param.list <- list(c("lambda","alpha"),
                   c("lambda","alpha","lambda.cov","alpha.cov"),
                   c("lambda","alpha","lambda.cov","alpha.cov"))

# keep the model definitions in a list, for ease
fitness.models <- list(BH_1 = BH_1,BH_2 = BH_2,BH_3 = BH_3,BH_4 = BH_4,BH_5 = BH_5)

# environmental covariates
covariates <- full.data[,"sum_salinity"]
# if no covariates, comment above and uncomment here
# covariates <- 0

# optimization methods to use
optim.methods <- c("optim_NM")

# from which method are we taking initial estimates for the next model?
# preliminary observations suggest optim_NM or DEoptimR
init.par.method <- optim.methods[1]
init.method.num <- which(optim.methods == init.par.method)

# values for initial estimates of parameters. 
# Overwrite if necessary
# init.lambda is calculated after log.fitness for each focal species
lower.lambda <- 1
upper.lambda <- 1e4
# sigma
lower.sigma <- 0.000001
upper.sigma <- 1
# alpha
init.alpha <- 1e-4
lower.alpha <- 0
upper.alpha <- 1e5
# lambda.cov
init.lambda.cov <- 1
lower.lambda.cov <- 0
upper.lambda.cov <- 1e4
# alpha.cov
init.alpha.cov <- 1
lower.alpha.cov <- 0
upper.alpha.cov <- 1e4

# if we want quicker calculations, we can disable 
# the bootstrapping for the standard errors
generate.errors <- FALSE
bootstrap.samples <- 99

# store results?
write.results <- FALSE

##############################
# initialize data structures
# elements are matrices/vectors within nested lists, of the form matrix[[focal.sp]][[model]][[method]]

param.matrices <- list()
for(i.sp in 1:length(focal.sp)){
    param.matrices[[i.sp]] <- list()
    for(i.model in 1:length(models)){
        param.matrices[[i.sp]][[i.model]] <- list()
        for(i.method in 1:length(optim.methods)){
            param.matrices[[i.sp]][[i.model]][[i.method]] <- list(lambda = 0,
                                                                  lambda.lower.error = 0,
                                                                  lambda.upper.error = 0,
                                                                  sigma = 0,
                                                                  alpha = 0,
                                                                  alpha.lower.error = 0,
                                                                  alpha.upper.error = 0,
                                                                  lambda.cov = 0,
                                                                  lambda.cov.lower.error = 0,
                                                                  lambda.cov.upper.error = 0,
                                                                  alpha.cov = 0,
                                                                  alpha.cov.lower.error = 0,
                                                                  alpha.cov.upper.error = 0,
                                                                  log.likelihood = 0)
        }
        names(param.matrices[[i.sp]][[i.model]]) <- optim.methods
    }
    names(param.matrices[[i.sp]]) <- names(fitness.models)[models]
}
names(param.matrices) <- focal.sp

###############################
# main loop

for(i.sp in 1:length(focal.sp)){
    
    # subset and prepare the data
    
    focal.sp.data <- subset(competition.data, focal == focal.sp[i.sp])
    # current focal species
    focal <- unique(focal.sp.data$focal)
    # fitness metric...
    # subset >0 records, for calculating logarithms
    focal.sp.data <- subset(focal.sp.data, seed > 0)
    fitness <- focal.sp.data$seed #fitness
    log.fitness <- log(fitness)
    # competition matrix: number of competitors
    focal.comp.matrix <- comp.matrix[which(competition.data$focal == focal.sp[i.sp]),]
    # number of competitors
    str(comp.matrix)
    num.competitors <- dim(focal.comp.matrix)[2]
    # number of covariates
    num.covariates <- ifelse(is.null(ncol(covariates)),0,ncol(covariates))
    # covariates for the focal species
    if(num.covariates > 0){
        focal.covariates <- covariates[which(competition.data$focal == focal.sp[i.sp]),,drop = FALSE]
    }else{
        focal.covariates <- 0
    }
    
    # generate initial values for the different parameters
    # or gather them from data if they are not to be optimized
    
    # lambda
    if("lambda" %in% param.list[[i.model]]){
        current.init.lambda <- mean(log.fitness)
    }else{
        current.init.lambda <- init.lambda[i.sp]
    }
    # sigma
    current.init.sigma <- sd(log.fitness)
    if(current.init.sigma > upper.sigma){
        current.init.sigma <- upper.sigma
    }
    # alpha
    if("alpha" %in% param.list[[i.model]]){
        if(models[i.model]<=2){
            alpha.length <- 1
        }else{
            alpha.length <- num.competitors
        }
        if(length(init.alpha) != alpha.length){
            current.init.alpha <- rep(init.alpha[1],alpha.length) 
        }else{
            current.init.alpha <- init.alpha
        }
    }else{
        current.init.alpha <- init.alpha[i.sp,]
    }
    # lambda.cov
    if("lambda.cov" %in% param.list[[i.model]]){
        if(length(init.lambda.cov) != num.covariates){
            current.init.lambda.cov <- rep(init.lambda.cov[1],num.covariates)
        }else{
            current.init.lambda.cov <- init.lambda.cov  
        }
    }else{
        current.init.lambda.cov <- init.lambda.cov[i.sp]  
    }
    # alpha.cov
    if("alpha.cov" %in% param.list[[i.model]]){
        if(models[i.model]<=4){
            length.alpha.cov <- num.covariates
        }else if(models[i.model]>4){
            length.alpha.cov <- num.covariates*num.competitors
        }
        if(length(init.alpha.cov) != length.alpha.cov){
            current.init.alpha.cov <- rep(init.alpha.cov[1],length.alpha.cov)
        }else{
            current.init.alpha.cov <- init.alpha.cov  
        }  
    }else{
        current.init.alpha.cov <- init.alpha.cov[i.sp]  
    }
    
    # model to optimize  
    for(i.model in 1:length(models)){
        
        print("*********************************")
        print(paste(date()," - starting focal sp ",focal.sp[i.sp],", model ",models[i.model],sep=""))
        print("*********************************")
        
        # obtain initial estimates from either previous model, or from given values
        # also, beware if the initial estimates are single values or vectors.
        
        ######################
        # compute each method
        
        for(i.method in 1:length(optim.methods)){
            
            temp.results <- cxr_optimize(fitness.model = fitness.models[[models[i.model]]],
                                         optim.method = optim.methods[i.method],
                                         param.list = param.list[[i.model]],
                                         log.fitness = log.fitness,
                                         init.lambda = current.init.lambda,
                                         lower.lambda = lower.lambda,
                                         upper.lambda = upper.lambda,
                                         init.sigma = current.init.sigma,
                                         lower.sigma = lower.sigma,
                                         upper.sigma = upper.sigma,
                                         init.alpha = current.init.alpha,
                                         lower.alpha = lower.alpha,
                                         upper.alpha = upper.alpha,
                                         init.lambda.cov = current.init.lambda.cov,
                                         lower.lambda.cov = lower.lambda.cov,
                                         upper.lambda.cov = upper.lambda.cov,
                                         init.alpha.cov = current.init.alpha.cov,
                                         lower.alpha.cov = lower.alpha.cov,
                                         upper.alpha.cov = upper.alpha.cov,
                                         focal.comp.matrix = focal.comp.matrix,
                                         focal.covariates = focal.covariates,
                                         generate.errors = generate.errors,
                                         bootstrap.samples = bootstrap.samples)
            ###############
            # clean up results
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda <- temp.results$lambda
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda.lower.error <- temp.results$lambda.lower.error
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda.upper.error <- temp.results$lambda.upper.error
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$sigma <- temp.results$sigma
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha <- temp.results$alpha
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha.upper.error <- temp.results$alpha.upper.error
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha.lower.error <- temp.results$alpha.lower.error
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda.cov <- temp.results$lambda.cov
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda.cov.upper.error <- temp.results$lambda.cov.upper.error
            param.matrices[[i.sp]][[i.model]][[i.method]]$lambda.cov.lower.error <- temp.results$lambda.cov.lower.error
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha.cov <- temp.results$alpha.cov
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha.cov.upper.error <- temp.results$alpha.cov.upper.error
            param.matrices[[i.sp]][[i.model]][[i.method]]$alpha.cov.lower.error <- temp.results$alpha.cov.lower.error
            
            param.matrices[[i.sp]][[i.model]][[i.method]]$log.likelihood <- temp.results$log.likelihood
            
        }# for i.method
        
        #######################
        # update initial values for the different parameters
        
        # lambda
        if("lambda" %in% param.list[[i.model]]){
            if(!is.na(param.matrices[[i.sp]][[i.model]][[init.par.method]]$lambda)){
                current.init.lambda <- param.matrices[[i.sp]][[i.model]][[init.par.method]]$lambda
            }
        }
        # sigma
        if(!is.na(param.matrices[[i.sp]][[i.model]][[init.par.method]]$sigma)){
            current.init.sigma <- param.matrices[[i.sp]][[i.model]][[init.par.method]]$sigma
            if(current.init.sigma > upper.sigma){
                current.init.sigma <- upper.sigma
            }
        }
        # alpha
        if("alpha" %in% param.list[[i.model]]){
            if(sum(is.na(param.matrices[[i.sp]][[i.model]][[init.par.method]]$alpha)) == 0){
                current.init.alpha <- param.matrices[[i.sp]][[i.model]][[init.par.method]]$alpha
                # is the current estimate of the appropriate length?
                if(i.model > 2){
                    if(length(current.init.alpha) == 1){
                        current.init.alpha <- rep(current.init.alpha,num.competitors)
                    }
                }# if model > 2
            }
        }
        # lambda.cov
        if("lambda.cov" %in% param.list[[i.model]]){
            if(sum(is.na(param.matrices[[i.sp]][[i.model]][[init.par.method]]$lambda.cov)) == 0){
                current.init.lambda.cov <- param.matrices[[i.sp]][[i.model]][[init.par.method]]$lambda.cov
            }
        }
        # alpha.cov
        if("alpha.cov" %in% param.list[[i.model]]){
            if(sum(is.na(param.matrices[[i.sp]][[i.model]][[init.par.method]]$alpha.cov)) == 0){
                current.init.alpha.cov <- param.matrices[[i.sp]][[i.model]][[init.par.method]]$alpha.cov
                # is the current estimate of the appropriate length?
                if(i.model > 4){
                    if(length(current.init.alpha.cov) == num.covariates){
                        current.init.alpha.cov <- rep(current.init.alpha.cov,num.competitors)
                    }
                }# if model > 4
            }
        }
        
    }# for i.model
}# for i.sp

if(write.results){
    save(param.matrices,file = "results/param_estimates.Rdata")
}

warnings()
param.matrices[[1]][[2]][[1]] #sp / modelo / optim


# 2) Predict homogenous plots----

# timesteps
timesteps <- 4
covariates <- salinity

##############
#Here you define the number of sites. sites = 36*9 subplots
covariates$site <- paste(covariates$plot,"_",covariates$subplot,sep="")
#Here you atributte salinity values. e.g. all sites = mean(salinity)
covariates <- covariates[,c("year","site","sum_salinity")]
#covariates$value <- mean(salinity$sum_salinity) #o algo asi
covariates <- gather(covariates,key = "covariate",value = "value",-year,-site)

# years are the different timesteps; rescale them
names(covariates)[1] <- "timestep"
covariates$timestep <- covariates$timestep - 2014

# initial abundances
data("abundance")
abundances <- abundance
# complete missing sp-year combinations
abundances <- tidyr::complete(abundances, year,plot,subplot,species, fill = list(individuals = 0, month = 0, day = 0, order = 0))
# Also play with equal abundance species. #Maybe also use only polintor dependant species?

###########
# read lambda,s,g, and alpha values
load("results/param_estimates.Rdata")
##############
# TODO: change when species.rates is part of the package
##############
species.rates <- readr::read_delim("../Caracoles_data/raw_data/seed_germination_survival.txt",delim = "\t")

# only species with germination/survival rates
sp.names <- sort(unique(species.rates$code))

estimates.model <- "BH_3" #Beverton-holt model number 5
estimates.method <- "optim_NM"

# gather lambda from fitted data
species.rates$lambda <- 0
for(i.sp in 1:length(sp.names)){
    if(!is.null(param.matrices[[sp.names[i.sp]]])){
        species.rates$lambda[species.rates$code == sp.names[i.sp]] <- param.matrices[[sp.names[i.sp]]][[estimates.model]][[estimates.method]]$lambda
    }
}

# subset species set to these with valid s,g, and lambda estimates
species.rates <- subset(species.rates,lambda != 0)
# clean up the species rates dataframe
sp.par <- species.rates[,c("lambda","germination","seed survival")]
names(sp.par) <- c("lambda","germ.rate","survival.rate")

# update sp.names
sp.names <- sort(unique(species.rates$code))
num.sp <- length(sp.names)

# gather the complete competition matrix from the fitted data
alpha.matrix <- matrix(0,nrow=num.sp,ncol=num.sp)
rownames(alpha.matrix) <- sp.names
colnames(alpha.matrix) <- sp.names

estimated.names <- names(param.matrices)
valid.positions <- match(estimated.names,sp.names)

for(i.sp in 1:num.sp){
    my.sp.alpha <- rep(0,num.sp)
    
    # including a temporary hack for valid sorting of species positions
    my.sp.alpha <- param.matrices[[sp.names[i.sp]]][[estimates.model]][[estimates.method]]$alpha[valid.positions]
    my.sp.alpha <- my.sp.alpha[!is.na(my.sp.alpha)]
    
    alpha.matrix[sp.names[i.sp],] <- my.sp.alpha
}

#####################

# format initial abundances
init.abund <- abundances %>% filter(species %in% sp.names) %>% group_by(year,plot,subplot,species) %>% summarise(abundance = sum(individuals))
init.abund$site <- paste(init.abund$plot,"_",init.abund$subplot,sep="")
init.abund <- init.abund[,c("year","site","species","abundance")]

# format environmental heterogeneity
num.cov = 1 #for now
if(num.cov > 0){ #WARNING num.cov not declared before.
    
    lambda.cov <- matrix(0,nrow = num.sp,ncol = num.cov)
    # lambda.cov
    for(i.sp in 1:num.sp){
        for(i.cov in 1:num.cov){
            lambda.cov[i.sp,i.cov] <- param.matrices[[sp.names[i.sp]]][[estimates.model]][[estimates.method]]$lambda.cov[i.cov]
        }
    }
    
    alpha.cov <- list()
    for(i.cov in 1:num.cov){
        alpha.cov[[i.cov]] <- matrix(nrow = num.sp,ncol = num.sp)
        for(i.sp in 1:num.sp){
            alpha.cov[[i.cov]][i.sp,] <- param.matrices[[sp.names[i.sp]]][[estimates.model]][[estimates.method]]$alpha.cov[i.sp+(num.sp*(i.cov-1))]
        }
    }
    
}else{
    covariates <- 0
    lambda.cov <- 0
    alpha.cov <- 0
}

###################
# which year to take as a starting point
year.abund <- subset(init.abund, year == 2016)

par <- list(sp.par = sp.par, initial.values = year.abund, 
            covariates = covariates, other.par = list(alpha.matrix = alpha.matrix, 
                                                      lambda.cov.matrix = lambda.cov, 
                                                      alpha.cov.matrix = alpha.cov))


abundance.model <- BH_abundance_3
predicted.abundances <- PredictAbundances(par = par,timesteps = timesteps,
                                          abundance.model = abundance.model)
predicted.abundances$timestep <- as.factor(predicted.abundances$timestep)
predicted.abundances$site <- as.factor(predicted.abundances$site)
predicted.abundances$sp <- as.factor(predicted.abundances$sp)

# some quick summarising for plotting
plot.data <- predicted.abundances %>% group_by(timestep,sp) %>% summarise(mean.abund = mean(abundance), sd.abund = sd(abundance))

# mean predicted abundance 
abund.mean.plot <- ggplot(plot.data,aes(x = timestep,y = mean.abund, group = sp)) + 
    geom_point(aes(color = sp)) + 
    geom_line(aes(color = sp)) +
    # geom_errorbar(aes(ymin = mean.abund - sd.abund, ymax = mean.abund + sd.abund))+
    # facet_wrap(site~.,ncol = 4)+
    # ylim(-10,10)+
    NULL
abund.mean.plot

# prepare data for comparing observed vs predicted
obs.pred <- abundances[,c("year","plot","subplot","species","individuals")]
# timestep to real years
predicted.abundances$timestep <- as.numeric(as.character(predicted.abundances$timestep)) + 2015
# get back plots and subplots
predicted.abundances$plot <- as.numeric(substr(predicted.abundances$site,1,1))
predicted.abundances$subplot <- substr(predicted.abundances$site,3,4)
predicted.abundances <- predicted.abundances[,c("timestep","plot","subplot","sp","abundance")]
names(predicted.abundances) <- c("year","plot","subplot","species","predicted")
obs.pred <- left_join(obs.pred,predicted.abundances)
obs.pred <- subset(obs.pred,year > 2015)
obs.pred <- obs.pred[which(!is.na(obs.pred$predicted)),]
obs.pred$year <- as.factor(obs.pred$year)

# observed and predicted abundances
obs.pred.plot <- ggplot(obs.pred,aes(x = individuals,y = predicted,group = species)) + 
    geom_point(aes(color = species, shape = year)) + 
    geom_abline(slope = 1) +
    facet_wrap(plot~., scales = "free_y") +
    xlim(0,100) + ylim(0,100) +
    NULL
obs.pred.plot




# 3) Predict heterogenus plots----


# 4) plot comparision of with/without heterogeneity



