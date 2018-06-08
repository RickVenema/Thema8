### dz = step_depth

library(deSolve)

##############
# PARAMETERS #
##############
## IDIOT PARAM VAR
PARAM <-  c(0.01, 0.83, 5.24, 21.60, 0.34, 0.99, 0.94)

## LIMIT_GRASS
limit_grass=1 # the limit from the three where grass still grows (meter)

## STEP_DIST
step_dist=0.1 # the steps by which the model calculates

## DEPTH
depth = 1.95 # the depth of the model

## DZ
dz = step_dist # the diference in depth

## Z
z= as.data.frame(seq(-0.05,-depth,by=-step_dist)) # creates data frame containing all the time steps

## Dt
Dt=PARAM[4]/10000.

## D_SLOW 
D_slow = PARAM[3]/10000.

## V
D = D_slow

## Dmix
Dmix<-rep(0,dim(z)[1])

import_tree_ab=0
import_tree_be=0
import_grass_be=0
import_crop_be= profil_CR_R_SPIN[2:(dim(z)[1]+1),1+1]*Input_CR_SPIN[1]

mr_grass = 1
mr_crop = 1
mr_tree=2.2 # Mortality rate for the tree, crop and grass roots (year) 
# This mr_tree is collected from Germon et al. 2016 Plant and Soil

####
# Root profile
####

#Roots profil of crop roots (% of the total root mass)
profil_CR_R<-matrix(ncol=dim(d)[1]+1,nrow=dim(z)[1]+1)
for (i in 1:dim(d)[1]) {profil_CR_R[1,i+1]<-d[i,]}
for (i in 1:dim(z)[1]) {profil_CR_R[i+1,1]<-z[i,]}

for (j in seq(z[1,1],z[dim(z)[1],1], by=-step_depth)){ 
  for (i in 1:dim(z)[1]+1) {
    if (profil_CR_R[i,1]==as.character(j)){
      for (k in 1:dim(d)[1]+1)     {
        profil_CR_R[i,k]<-26.443*exp((-2.6)*(-z[i-1,1])) 
        profil_CR_R[i,k]<-profil_CR_R[i,k]/100 #Conversion from % to proportion
        if (profil_CR_R[i,1]<limit_root_crop) {profil_CR_R[i,k]<-0} # no more crop roots below 1.5m
      }
    }
  }
}
profil_CR_R_SPIN<-profil_CR_R
for (i in 1:dim(z)[1]+1) {
  for (k in 1:dim(d)[1]+1)     {
    if (profil_CR_R[1,k]<=limit_grass) {profil_CR_R[i,k]<-0} # no crop on the tree line
  }
}

#############
# THE MODEL #
#############
modelp3difft <- function(t, initial_state, parms){
  with (as.list(parms),{
          A <- initial_state[1:dim(z)[1]]
          S <- initial_state[(dim(z)[1]+1):((2*dim(z)[1]))]
          P <- initial_state[((2*dim(z)[1])+1):((3*dim(z)[1]))]  
          
          #Fluxes in z direction
          FluxA <- Dt * (c(0,A)) / dz -D * diff(c(0,A,0)) / dz - c(0,Dmix) * diff(c(0,A,0)) / dz 
          FluxS <- Dt * (c(0,S)) / dz -D_slow * diff(c(0,S,0)) / dz - c(0,Dmix) * diff(c(0,S,0)) / dz
          FluxP <- Dt * (c(0,P)) / dz -D_slow * diff(c(0,P,0)) / dz - c(0,Dmix) * diff(c(0,P,0)) / dz
          FluxA[1]=0.
          FluxS[1]=0.
          FluxP[1]=0.
          
          #Reaction       
          Import<-import_tree_be*mr_tree + import_grass_be*mr_grass + import_crop_be*mr_crop
          dA=-diff(FluxA) + (e*ks*S*frac_SA + e*kp*P - kf* A* clay_func)* mf* tf  + Import
          dS=-diff(FluxS) + (frac_AS*kf*e*A*clay_func - ks*S)* mf* tf
          dP=-diff(FluxP) + ((1-frac_SA)*e*ks*S + (1-frac_AS)*kf*e*A*clay_func - kp*P)* mf* tf
          
          return(list(c(dA=dA,dS=dS,dP=dP)))
        })
}



initial_state<- c(rep(0,dim(z)[1]),rep(0,dim(z)[1]),rep(0,dim(z)[1]))
times <- seq(0,5000,by=1)

ode.2D(y= initial_state, times = 1:1000, func=modelp3difft, dimens= c(1,2), parms = NULL)
       