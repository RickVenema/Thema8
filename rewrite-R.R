### dz = step_depth

library(deSolve)

step_dist=0.1
depth = 1.95
dz = step_dist
z= as.data.frame(seq(-0.05,-depth,by=-step_dist))


modelp3difft <- function(t, initial_state,parms){
  with (as.list(parms),
        {
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
          
          list(c(dA=dA,dS=dS,dP=dP
          ))
        })
}
PARAM <-  c(0.01, 0.83, 5.24, 21.60, 0.34, 0.99, 0.94)
Dt=PARAM[4]/10000.

initial_state<- c(rep(0,dim(z)[1]),rep(0,dim(z)[1]),rep(0,dim(z)[1]))
times <- seq(0,5000,by=1)

ode.2D(y= initial_state, times = 1:1000, func=modelp3difft, dimens= c(1,2), parms = NULL)
       