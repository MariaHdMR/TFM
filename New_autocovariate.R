library(spdep)

# First establish the type of analysis, to chose look at the information of the function " autocov_dist"
wtype="inverse"
wstyle = "B"
#  Merge the data_frame of coordinates and seed ( here for CHFU) in order that the first two element of the "autocov_dist"  
#  have the same length 
#  Important to check that the merge zork correctly and associated the good coordinates with the good plot
test.new.df <- left_join(CHFU.vis, disfinal1, by= c("Plot","Subplot"))
view(test.new.df)

# creation of the matrix of corrdinates,
# here the coordinates are already changed into distance from a point of reference
coords <- as.matrix(cbind(test.new.df$x_coor2, test.new.df$y_coor2)) #Make a matrix of coordinates 
nbsize=max(coords)+1

# do the autocov_dist function 
ac <- autocov_dist(CHFU.vis$Seed,coords,nbs = nbsize, style=wstyle, type = wtype)
# look in the suppleñentqry info why this step is important
CHFU.vis$ac <- ac/sd(ac)
view(ac)

# write your basic model, 
# !! Latter add random effect of plot and subplot ?? 
models = "visitas_indv_hora * Group"
# model 0, with no variables to explain seeds
model.null <- glm("Seed ~ 1",data= CHFU.vis, family = poisson)
summary(model.null)    


# fist model without the effect of the coordinates
model.visitor <- glm(paste("Seed ~ ",models),data= CHFU.vis, family = poisson)
summary(model.visitor)    

elist1=coefficients(model.visitor)[-1]
intercept1=coefficients(model.visitor)[1]
summ1=summary(model.visitor)
p.hat=as.vector(fitted(model.visitor))
R2=cor(CHFU.vis$Seed, p.hat)
AIC.c=AIC(model.visitor)
neg.llik=(AIC(model.visitor)-2*length(coefficients(model.visitor)))/2
delta.AIC.c = AIC(model.visitor) - AIC(model.null)

# second model, with the effect of the coordinates 
model.complet <- glm(paste("Seed ~  ac + ", models),data= CHFU.vis,
                     family = poisson,
                     epsilon = 1e-14,maxit = 100)
summary(model.complet)
elist2.U=coefficients(model.complet)
delta.AIC.c = AICc(model.complet) - AICc(model.null)

delta.AIC.c1 = AICc(model.complet) - AICc(model.visitor) 

intercept2.U=coefficients(model.complet)[1]
summ2.U[[k]]=summary(model.complet)
beta.auto.U.s=coefficients(model.complet)[2];
beta.auto.U=beta.auto.U.s/sd(ac) # recover unstandardized beta.auto.

# construct log(pseudo-likelihood) function
llike=function(coeffs){llambda= coeffs[1] + coeffs[2]*ac + 
    coeffs[3]*CHFU.vis$visitas_indv_hora;
lambda=exp(llambda);
sum(-lambda+obs*llambda-lfac)}

    obs= CHFU.vis$Seed;
    lfac=lfactorial(obs);
    #ac <- autocov_dist(obs,  coords,  nbs = nbsize, style=wstyle, type = wtype)
    npars=length(model.complet$coefficients);
    ctrl.lst=list(fnscale=-1,maxit=1000);
    c(rep(0,npars-1),-1)
    
    fmB=constrOptim(c(0,-1, 0),llike,NULL,ui=rbind(c(0,-1,0)),ci=c(0),
                    mu=0.02,control=ctrl.lst,outer.iterations = 100, outer.eps = 1e-10)
    cmat1=fmB$par
    beta.auto.s=(fmB$par)[2]; 
    beta.auto=beta.auto.s/sd(ac); # recover unstandardized beta.auto.

    # beta.auto must not be > 0; The code below implements this as follows:
    # If unconstrained beta.auto  is not > 0 use unconstrained results (numerically simpler => more accurate)
    # Otherwise {If constrained beta.auto is appreciably less than zero then use constrained results. 
    # If constrained beta.auto is effectively zero (extremely small in magnitude ), then the 
    # auto-poisson model reduces to a poisson model, so use poisson results for covariate coefficients 
    # because the constrained optimisation loses accuracy at the constraint boundary.} 
    

#PUPA----
PUPA.vis <- subset(pol, Plant_Simple == "PUPA")
#  Important to check that the merge zork correctly and associated the good coordinates with the good plot
test.pupa.df <- left_join(PUPA.vis, disfinal1, by= c("Plot","Subplot"))
view(test.pupa.df)

# creation of the matrix of corrdinates, here the coordinates are already changed into distance from a point of reference
coords.pupa <- as.matrix(cbind(test.pupa.df$x_coor2, test.pupa.df$y_coor2)) #Make a matrix of coordinates 

# do the autocov_dist function 
ac.pupa <- autocov_dist(PUPA.vis$Seed,coords.pupa,nbs = nbsize, style=wstyle, type = wtype)
# look in the suppleñentqry info why this step is important
ac2.pupa=ac.pupa/sd(ac.pupa)
view(ac.pupa)

# write your basic model, 
# !! Latter add random effect of plot and subplot ?? 
models = "visitas_indv_hora * Group"
# model 0, with no variables to explain seeds
model.null.pupa <- glm("Seed ~ 1",data= PUPA.vis, family = poisson)
summary(model.null.pupa)    


# fist model without the effect of the coordinates
model.visitor.PUPA <- glm(paste("Seed ~ ",models),data= PUPA.vis, family = poisson)
summary(model.visitor.PUPA)    

elist1=coefficients(model.visitor.PUPA)[-1]
intercept1=coefficients(model.visitor.PUPA)[1]
summ1=summary(model.visitor.PUPA)
p.hat=as.vector(fitted(model.visitor.PUPA))
R2=cor(PUPA.vis$Seed, p.hat)
AIC.c=AIC(model.visitor.PUPA)
neg.llik=(AIC(model.visitor.PUPA)-2*length(coefficients(model.visitor.PUPA)))/2
delta.AIC.c = AIC(model.visitor.PUPA) - AIC(model.null.pupa)

# second model, with the effect of the coordinates 
model.complet.pupa <- glm(PUPA.vis$Seed ~ ac2.pupa + PUPA.vis$visitas_indv_hora*PUPA.vis$Group,family = poisson,
                     epsilon = 1e-14,maxit = 100)
summary(model.complet.pupa)
elist2.U=coefficients(model.complet.pupa)
delta.AIC.c = AICc(model.complet.pupa) - AICc(model.null.pupa)

delta.AIC.c1 = AICc(model.complet.pupa) - AICc(model.visitor.PUPA) 

intercept2.U=coefficients(model.complet.pupa)[1]
summ2.U[[k]]=summary(model.complet.pupa)
beta.auto.U.s=coefficients(model.complet.pupa)[2];
beta.auto.U=beta.auto.U.s/sd(ac.pupa) # recover unstandardized beta.auto.

