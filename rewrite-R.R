# Rick Venema
# 368044
# k.f.venema@st.hanze.nl

# Rewritten model of Cardinael et al. 2018

#Do we use priming?
priming="n"

#Do you want to run on the control plot?
control="y"

### dz = step_depth

library(deSolve)
source("Moyano.R")
source("Parameters.R")



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

out_spinup<-matrix(ncol=dim(d)[1],nrow=3*dim(z)[1])
out_intermediate<-matrix(nrow= spin_length,ncol=3*dim(z)[1])

#Calculation of the input
import_tree_ab=0
import_tree_be=0
import_crop_ab= Input_crop_ab_spin[1]
import_crop_be= profil_CR_R_SPIN[2:(dim(z)[1]+1),1+1]*Input_CR_SPIN[1]
import_grass_ab=0
import_grass_be=0

import_crop_be[1]=import_crop_be[1]+ import_crop_ab

#Mixing effect of tillage
Dmix<-rep(0,dim(z)[1])
for (j in 1:dim(z)[1]) 	{ if (abs(z[j,1])<=abs(til_lay)) {Dmix[j]<-50} else {Dmix[j]<-0}}

#Moisture function on decomposition
mf<-mf_ctrl

#Calculation of the soil temperature function
tf<-Q10^((temp-304.15)/10)
for (j in 1:dim(z)[1]) {if (tf[j] > 1) {tf[j]<-1} else {tf[j]<-tf[j]}}

#Calculation of the clay function
if (control=="y") {clay_func<-ta_clay_func} else {clay_func<-af_clay_func}

#Lauching the simulation
kf=((import_tree_be)/(import_tree_be+import_crop_be+import_grass_be))*kt + ((import_crop_be+import_grass_be)/(import_tree_be +import_crop_be+import_grass_be))*kw

kf[1]=((import_tree_ab+import_tree_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kt + ((import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kw

for (j in 1:dim(z)[1]) {if (is.na(kf[j])){kf[j]<-kw}}                                                                                                           

parms <- c(e=e,ks=ks,mr_tree=mr_tree,mr_grass=mr_grass,mr_crop=mr_crop,kf=kf,D=v,cr=c,D_slow=v_slow,dz= step_depth,Dt=Dt, ad=1, mf = mf,Dmix=Dmix, tf=tf,frac_AS=frac_AS,frac_SA=frac_SA,kp=kp)
initial_state<-c(rep(0,dim(z)[1]),rep(0,dim(z)[1]),rep(0,dim(z)[1]))

out<- ode.1D(y=initial_state, time=times, func=modelp3difft,parms=parms,nspec=2)
nb_param<-10


out_spinup[,1]<-out[spin_length,1:((3*dim(z)[1]))+1]        
for (i in 1:dim(d)[1])  {out_spinup[,i]<-out_spinup[,1]	}


out_for_optim<-rep(0,10)
out_for_optim[1]<-out_spinup[21,1]+out_spinup[41,1]
out_for_optim[2]<-out_spinup[22,1]+out_spinup[23,1]+out_spinup[42,1]+out_spinup[43,1]
out_for_optim[3]<-out_spinup[24,1]+out_spinup[25,1]+out_spinup[44,1]+out_spinup[45,1]
out_for_optim[4]<-out_spinup[26,1]+out_spinup[27,1]+out_spinup[46,1]+out_spinup[47,1]
out_for_optim[5]<-out_spinup[28,1]+out_spinup[29,1]+out_spinup[30,1]+out_spinup[48,1]+out_spinup[49,1]+out_spinup[50,1]
out_for_optim[6]<-out_spinup[31,1]+out_spinup[32,1]+out_spinup[51,1]+out_spinup[52,1]
out_for_optim[7]<-out_spinup[33,1]+out_spinup[34,1]+out_spinup[53,1]+out_spinup[54,1]
out_for_optim[8]<-out_spinup[35,1]+out_spinup[36,1]+out_spinup[55,1]+out_spinup[56,1]
out_for_optim[9]<-out_spinup[37,1]+out_spinup[38,1]+out_spinup[57,1]+out_spinup[58,1]
out_for_optim[10]<-out_spinup[39,1]+out_spinup[40,1]+out_spinup[59,1]+out_spinup[60,1]

out_for_optim<-out_for_optim*c(1,1/2,1/2,1/2,1/3,1/2,1/2,1/2,1/2,1/2) #(conversion in kg m3 for the optimization)
print("out_for_optim")
print(out_for_optim)

write.table(out_for_optim,'out_Mik_p0.txt',row.names=FALSE,col.names=FALSE)

print("***************************************")
print("Spinup finished")

#run agroforestery
out_final<-matrix(ncol=dim(d)[1],nrow=3*dim(z)[1])
out_intermediate_Agrof<-matrix(nrow= Agrof_length,ncol=3*dim(z)[1])
out_total <- array(0, dim=c(dim(d)[1], Agrof_length, 3*dim(z)[1]))
for (i in 1:dim(d)[1]) {
  if (i==1){print("***************************************");print("We start the band");print(i)} else {print("We start the band");print(i)}
  for (t in 1: Agrof_length) {
    if (t==1){print("***************************************");print("We start the year");print(t)} else {print("We start the year");print(t)}
    
    #Calculation of the input
    if (control=="y") {
      import_tree_ab=0
      import_tree_be=0
      import_crop_ab= Input_crop_ab_spin[1]
      import_crop_be= profil_CR_R_SPIN[2:(dim(z)[1]+1),1+1]*Input_CR_SPIN[1]
      import_grass_ab=0
      import_grass_be=0
      
      import_crop_be[1]=import_crop_be[1]+ import_crop_ab
    }
    else {
      import_tree_ab= Input_leaves[time_Agrof_length [t]]
      import_tree_be=profil_TR_R[2:(dim(z)[1]+1),i+1]*Input_TR[time_Agrof_length [t],i]
      import_crop_ab= Input_crop_ab[t,i]
      import_crop_be=profil_CR_R[2:(dim(z)[1]+1),i+1]*Input_CR[t,i]
      import_grass_ab=Input_grass_ab[i]
      import_grass_be=profil_GR_R[2:(dim(z)[1]+1),i+1]*Input_GR[i]
      
      import_crop_be[1]=import_crop_be[1]+ import_crop_ab
      import_tree_be[1]=import_tree_be[1]+ import_tree_ab/mr_tree #we divided by mr_tree to take into account that leaves are product only once per year
      import_grass_be[1]=import_grass_be[1]+ import_grass_ab
    }
    
    #Mixing effect of tillage
    Dmix<-rep(0,dim(z)[1])
    for (j in 1:dim(z)[1]) 	{ if (abs(z[j,1])<=abs(til_lay)) {Dmix[j]<-50} else {Dmix[j]<-0}}
    if (control=="n") {    if (d[i,] <= limit_grass) {Dmix[]<-0} }
    
    #Calculation of the soil moisture function
    if (d[i,] <= limit_grass) {mf<-mf_tl} else{mf<-mf_ir}
    if (control=="y") {mf<-mf_ctrl} else {mf<-mf}
    #Calculation of the clay function
    clay_func<-af_clay_func
    
    #Lauching the simulation
    times <- seq(0,Agrof_length,by=1)
    
    kf=((import_tree_be)/(import_tree_be+import_crop_be+import_grass_be))*kt + ((import_crop_be+import_grass_be)/(import_tree_be +import_crop_be+import_grass_be))*kw
    
    kf[1]=((import_tree_ab+import_tree_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kt + ((import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kw
    
    for (j in 1:dim(z)[1]) {if (is.na(kf[j])){kf[j]<-kw}}
    
    parms <- c(e=e,ks=ks,mr_tree=mr_tree,mr_grass=mr_grass,mr_crop=mr_crop,kf=kf,D=v,cr=c,D_slow=v_slow,dz= step_depth,Dt=Dt, ad=1, mf = mf,Dmix=Dmix, tf=tf,frac_AS=frac_AS,frac_SA=frac_SA,kp=kp)
    if (t==1){initial_state<-out_spinup[,i]} else {initial_state<-out_agrof[2,2:(3*dim(z)[1]+1)]}
    
    out_agrof<- ode.1D(y=initial_state, time=times, func=modelp3difft,parms=parms,nspec=2)
    nb_param<-10
    out_intermediate_Agrof[t,]<-out_agrof[2,2:((3*dim(z)[1]+1))]
  }
  out_final[,i]<-out_intermediate_Agrof[Agrof_length,1:((3*dim(z)[1]))]
  out_total[i,,]<-out_intermediate_Agrof[1:Agrof_length,1:((3*dim(z)[1]))]
}
print("***************************************")
print("Simulation finished")