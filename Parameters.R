#### Rick Venema
#### 368044
#### k.f.venema@st.hanze.nl
#### rewritten model parameters based on the model of Cardinael et al.

##############
# PARAMETERS #
##############
## IDIOT PARAM VAR
PARAM <-  c(0.01, 0.83, 5.24, 21.60, 0.34, 0.99, 0.94)

## LIMIT_GRASS
limit_grass=1 # the limit from the three where grass still grows (meter)

## STEP_DIST
step_dist=0.1 # the steps by which the model calculates
step_depth = 0.1

## DEPTH
depth = 1.95 # the depth of the model

#Duration of the agroforestry experiment in year
Agrof_length=18
time_Agrof_length=seq(1, Agrof_length)


## DZ
dz = step_dist # the diference in depth

## Z
z= as.data.frame(seq(-0.05,-depth,by=-step_dist)) # creates data frame containing all the time steps


## Defining d
dist=6.5 # (middle of the agroforestry alley)
step_dist=0.1 # step difference
d=as.data.frame(seq(step_dist,dist, by=step_dist))

####
# Root profile
####

######### NEW CODE ##########
profil_CR_R<-matrix(ncol=dim(d)[1],nrow=dim(z)[1])

## Formula to calculate profile 26.443*exp((-2.6)*(-z[i-1,1]))
test <- function(z){
  return(26.433*exp((-2.6)*(-z))/100)
}

profil_CR_R <- test(z)


profil_CR_R <- as.data.frame(rep(profil_CR_R, dim(d)[1]))
colnames(profil_CR_R) <- as.character(d[,1])
rownames(profil_CR_R) <- as.character(z[,1])
profil_CR_R_SPIN <- profil_CR_R
profil_CR_R[z[,1]<= -1.5,] <- 0


##### INPUT FROM CROP ##########
Input_crop_ab_spin<-d[,1]
Input_crop_ab<-matrix(ncol=dim(d)[1],nrow= Agrof_length)

Input_crop_ab_spin[]<-Yield_ctrl* St_Yi_ratio* Non_exp_straw* C_cont_straw

for (k in 1:dim(d)[1])     {
  if (d[k,1]<=limit_grass)   {
    Input_crop_ab[,k]=0
  }
  else                             {
    Input_crop_ab[,k]= Yield_AF[,k]* St_Yi_ratio* Non_exp_straw* C_cont_straw
  }
}

#Soil temperature (in K)
temp<--0.8931*(-z[,1])+288.24

#The soil moisture profile (in volumetric fraction)
moist_profil=0.0476*(-z[,1])+0.284

#pH (in pH units) profile
ph<-0.1603*log(-z[,1]) + 8.3608

## Dt
Dt=21.60/10000.

## D_SLOW 
D_slow = 5.24/10000.

## V
D = D_slow

## Dmix
Dmix<-rep(0,dim(z)[1])

import_tree_ab=0
import_tree_be=0
import_grass_be=0
import_grass_ab =0
Input_CR_SPIN<-d[,1]
import_crop_be= profil_CR_R_SPIN[2:(dim(z)[1]+1),1+1]*Input_CR_SPIN[1]
import_crop_ab= Input_crop_ab_spin[1]

mr_grass = 1
mr_crop = 1
mr_tree=2.2 # Mortality rate for the tree, crop and grass roots (year) 
# This mr_tree is collected from Germon et al. 2016 Plant and Soil

## e (Yield of decomposed FOM that goes to SOM)
e= 0.34

#Decomposition rate of of slow SOM (1/residence time)
ks<-z[,1]
for (i in 2:(dim(z)[1]+1)) {ks[i-1]<-(0.01/100)*exp(-1.455*(-z[i-1,1]))*365.25}

#Q10 value
Q10=2.0

#Decomposition rate of Walnut tree roots (yr-1)
kt=2.46 

#Yield for wheat in the control plot (t DM ha-1, average over 20 years)
Yield_ctrl<-3.79

#C concentration in wheat roots (g g-1)
C_cont_root=0.3514

#C concentration in wheat straw (g g-1)
C_cont_straw=0.4332

#Decomposition rate of Wheat roots (yr-1)
kw=3.03 

#Non_exported straw (0-1)
Non_exp_straw=0.25

#Straw:yield ratio of wheat (unitless)
St_Yi_ratio=1.03

#Above Ground Biomass:yield ratio of wheat (unitless)
AB_Yi_ratio=2.45

#Root:shoot ratio of wheat (unitless)
Ro_Sh_ratio=0.79

######
# GEEN IDEE
####
kf=((import_tree_be)/(import_tree_be+import_crop_be+import_grass_be))*kt + ((import_crop_be+import_grass_be)/(import_tree_be +import_crop_be+import_grass_be))*kw
kf[1]=((import_tree_ab+import_tree_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kt + ((import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kw


#Yield for wheat in the control plot (t DM ha-1, average over 20 years)
Yield_ctrl<-3.79

#Distribution of mineralized C in the different pools
frac_SA=0.99
frac_AS=0.94

#Decomposition rate of of passive SOM (1/residence time)
kp<-z[,1]
for (i in 2:(dim(z)[1]+1)) {kp[i-1]<-(0.83/100)*exp(-1.455*(-z[i-1,1]))*365.25}



#DBH (m)
DBH<-0.0157* time_Agrof_length - 0.0391
for (k in 1: Agrof_length)     {if (DBH[k]<0) {DBH[k]<-0.01}}

#Yield for wheat in the AF plot (t DM ha-1)
#Here we defined b as the minimum between a and 1 with a being the function of yield reduction
#depending on DBH
a=DBH
b=DBH

Yield_AF<-matrix(ncol=dim(d)[1],nrow= Agrof_length)

#Here, the coefficient were obtained based on data regression, but the function underestimates the 
#yield by 20%, that's why we multiplied by 1.2

for (k in 1:dim(d)[1])     {
  Yield_AF[1: Agrof_length,k]<- 1.2*((4.39*d[k,1]+64.57)/100)*Yield_ctrl*b[1: Agrof_length]
} 
