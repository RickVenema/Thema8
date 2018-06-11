#### Rick Venema
#### 368044
#### k.f.venema@st.hanze.nl
#### rewritten model parameters based on the model of Cardinael et al.

##############
# PARAMETERS #
##############
## IDIOT PARAM VAR
PARAM <-  c(0.01, 0.83, 5.24, 21.60, 0.34, 0.99, 0.94)

## Defining d
dist=6.5 # (middle of the agroforestry alley)
step_dist=0.1 # step difference
d=as.data.frame(seq(step_dist,dist, by=step_dist))


## LIMIT_GRASS
limit_grass=1 # the limit from the three where grass still grows (meter)

#Limit to crop roots development (in the alley)
limit_root_crop=-1.50

#Limit to grass roots development (in the tree row)
limit_root_grass=-1.50

#Duration of the spinup in year
spin_length=5000

## STEP_DIST
step_dist=0.1 # the steps by which the model calculates
step_depth = 0.1

## DEPTH
depth = 1.95 # the depth of the model

#Duration of the agroforestry experiment in year
Agrof_length=18
time_Agrof_length=seq(1, Agrof_length)

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

#Tillage layer (in meter)
til_lay=-0.15


Yield_AF<-matrix(ncol=dim(d)[1],nrow= Agrof_length)
## DZ
dz = step_dist # the diference in depth

## Z
z= as.data.frame(seq(-0.05,-depth,by=-step_dist)) # creates data frame containing all the time steps


###################################
###################################
###                            ####
###     KUT CODE HIERONDER     ####
###                            ####
###################################
###################################
#bulk density (kg m-3) from Cardinael et al., 2015 Geoderma
ta_bd<-z[,1]
for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {ta_bd[i]<-1.41}
  if (z[i,1]==as.character(-0.85)) {ta_bd[i]<-1.74}
  if (z[i,1]==as.character(-0.15)) {ta_bd[i]<-approx   (c(0.05,0.20),c(1.41,1.61), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {ta_bd[i]<-approx   (c(0.20,0.40),c(1.61, 1.73), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {ta_bd[i]<-approx   (c(0.20,0.40),c(1.61, 1.73), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {ta_bd[i]<-approx   (c(0.40,0.60),c(1.73, 1.80), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {ta_bd[i]<-approx   (c(0.40,0.60),c(1.73, 1.80), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {ta_bd[i]<-approx   (c(1.10,1.30),c(1.61, 1.65), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {ta_bd[i]<-approx   (c(1.10,1.30),c(1.61, 1.65), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {ta_bd[i]<-approx   (c(1.30,1.50),c(1.65, 1.65), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {ta_bd[i]<-approx   (c(1.30,1.50),c(1.65, 1.65), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {ta_bd[i]<-approx   (c(1.50,1.70),c(1.65, 1.65), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.65)) {ta_bd[i]<-approx   (c(1.50,1.70),c(1.65, 1.65), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}


zd.bd <- c(-0.05,-0.20,-0.40,-0.60,-0.85,-1.10,-1.30,-1.50,-1.70,-1.90)
ta.bd <- as.numeric(c(1.41,1.61,1.73,1.80,1.74,1.61,1.65,1.65,1.65,1.65))
meet_results <- cbind(zd.bd, ta.bd)


ta_bd_test <- z[,1]

tester <- function(ta_bd_test){
  approx(c(0.05,0.20),c(1.41,1.61), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]
  return(ta_bd_test)
}
ta_bd_test <- tester(ta_bd_test)


ir_bd<-z[,1]
for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {ir_bd[i]<-1.23}
  if (z[i,1]==as.character(-0.85)) {ir_bd[i]<-1.71}
  if (z[i,1]==as.character(-0.15)) {ir_bd[i]<-approx   (c(0.05,0.20),c(1.23,1.60), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {ir_bd[i]<-approx   (c(0.20,0.40),c(1.60, 1.67), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {ir_bd[i]<-approx   (c(0.20,0.40),c(1.60, 1.67), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {ir_bd[i]<-approx   (c(0.40,0.60),c(1.67, 1.77), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {ir_bd[i]<-approx   (c(0.40,0.60),c(1.67, 1.77), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {ir_bd[i]<-approx   (c(0.60,0.85),c(1.77, 1.71), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {ir_bd[i]<-approx   (c(0.60,0.85),c(1.77, 1.71), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {ir_bd[i]<-approx   (c(0.85,1.10),c(1.71, 1.55), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {ir_bd[i]<-approx   (c(0.85,1.10),c(1.71, 1.55), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {ir_bd[i]<-approx   (c(1.10,1.30),c(1.55, 1.64), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {ir_bd[i]<-approx   (c(1.10,1.30),c(1.55, 1.64), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {ir_bd[i]<-approx   (c(1.30,1.50),c(1.64, 1.64), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.40)) {ir_bd[i]<-approx   (c(1.30,1.50),c(1.64, 1.64), 1.40, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {ir_bd[i]<-approx   (c(1.30,1.50),c(1.64, 1.64), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {ir_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.65), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.60)) {ir_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.65), 1.60, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.65)) {ir_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.65), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {ir_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.80)) {ir_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {ir_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {ir_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-2.00)) {ir_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 2.00, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}

tr_bd<-z[,1]
for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {tr_bd[i]<-1.10}
  if (z[i,1]==as.character(-0.20)) {tr_bd[i]<-1.49}
  if (z[i,1]==as.character(-0.40)) {tr_bd[i]<-1.71}
  if (z[i,1]==as.character(-0.60)) {tr_bd[i]<-1.73}
  if (z[i,1]==as.character(-0.85)) {tr_bd[i]<-1.68}
  if (z[i,1]==as.character(-1.10)) {tr_bd[i]<-1.55}
  if (z[i,1]==as.character(-1.30)) {tr_bd[i]<-1.63}
  if (z[i,1]==as.character(-1.50)) {tr_bd[i]<-1.64}
  if (z[i,1]==as.character(-1.70)) {tr_bd[i]<-1.62}
  if (z[i,1]==as.character(-1.90)) {tr_bd[i]<-1.64}
  
  if (z[i,1]==as.character(-0.10)) {tr_bd[i]<-approx   (c(0.05,0.20),c(1.10,1.49), 0.10, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.15)) {tr_bd[i]<-approx   (c(0.05,0.20),c(1.10,1.49), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {tr_bd[i]<-approx   (c(0.20,0.40),c(1.49, 1.71), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.30)) {tr_bd[i]<-approx   (c(0.20,0.40),c(1.49, 1.71), 0.30, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {tr_bd[i]<-approx   (c(0.20,0.40),c(1.49, 1.71), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {tr_bd[i]<-approx   (c(0.40,0.60),c(1.71, 1.73), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.50)) {tr_bd[i]<-approx   (c(0.40,0.60),c(1.71, 1.73), 0.50, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {tr_bd[i]<-approx   (c(0.40,0.60),c(1.71, 1.73), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {tr_bd[i]<-approx   (c(0.60,0.85),c(1.73, 1.68), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.70)) {tr_bd[i]<-approx   (c(0.60,0.85),c(1.73, 1.68), 0.70, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {tr_bd[i]<-approx   (c(0.60,0.85),c(1.73, 1.68), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.80)) {tr_bd[i]<-approx   (c(0.60,0.85),c(1.73, 1.68), 0.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.90)) {tr_bd[i]<-approx   (c(0.85,1.10),c(1.68, 1.55), 0.90, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {tr_bd[i]<-approx   (c(0.85,1.10),c(1.68, 1.55), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.00)) {tr_bd[i]<-approx   (c(0.85,1.10),c(1.68, 1.55), 1.00, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {tr_bd[i]<-approx   (c(0.85,1.10),c(1.68, 1.55), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {tr_bd[i]<-approx   (c(1.10,1.30),c(1.55, 1.63), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.20)) {tr_bd[i]<-approx   (c(1.10,1.30),c(1.55, 1.63), 1.20, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {tr_bd[i]<-approx   (c(1.10,1.30),c(1.55, 1.63), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {tr_bd[i]<-approx   (c(1.30,1.50),c(1.63, 1.64), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.40)) {tr_bd[i]<-approx   (c(1.30,1.50),c(1.63, 1.64), 1.40, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {tr_bd[i]<-approx   (c(1.30,1.50),c(1.63, 1.64), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {tr_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.62), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.60)) {tr_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.62), 1.60, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.65)) {tr_bd[i]<-approx   (c(1.50,1.70),c(1.64, 1.62), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {tr_bd[i]<-approx   (c(1.70,1.90),c(1.62, 1.64), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.80)) {tr_bd[i]<-approx   (c(1.70,1.90),c(1.62, 1.64), 1.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {tr_bd[i]<-approx   (c(1.70,1.90),c(1.62, 1.64), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {tr_bd[i]<-approx   (c(1.70,1.90),c(1.62, 1.64), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-2.00)) {tr_bd[i]<-approx   (c(1.70,1.90),c(1.62, 1.64), 2.00, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}

bd<-matrix(ncol=dim(d)[1],nrow=dim(z)[1])									

for (i in 1:dim(d)[1]) {
  if (d[i,1]<= limit_grass) {bd[,i]<-tr_bd} else {bd[,i]<-ir_bd}
}


#Clay content profile from Cardinael et al., 2015 Geoderma
af_clay<-z[,1]
ta_clay<-z[,1]
for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {af_clay[i]<-0.1754}
  if (z[i,1]==as.character(-0.85)) {af_clay[i]<-0.3092}
  if (z[i,1]==as.character(-0.10)) {af_clay[i]<-approx   (c(0.05,0.20),c(0.1754,0.17029), 0.10, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.15)) {af_clay[i]<-approx   (c(0.05,0.20),c(0.1754,0.17029), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {af_clay[i]<-approx   (c(0.20,0.40),c(0.17029, 0.17762), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.30)) {af_clay[i]<-approx   (c(0.20,0.40),c(0.17029, 0.17762), 0.30, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {af_clay[i]<-approx   (c(0.20,0.40),c(0.17029, 0.17762), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {af_clay[i]<-approx   (c(0.40,0.60),c(0.17762, 0.25004), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.50)) {af_clay[i]<-approx   (c(0.40,0.60),c(0.17762, 0.25004), 0.50, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {af_clay[i]<-approx   (c(0.40,0.60),c(0.17762, 0.25004), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {af_clay[i]<-approx   (c(0.60,0.85),c(0.25004, 0.3092), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.70)) {af_clay[i]<-approx   (c(0.60,0.85),c(0.25004, 0.3092), 0.70, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {af_clay[i]<-approx   (c(0.60,0.85),c(0.25004, 0.3092), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.80)) {af_clay[i]<-approx   (c(0.60,0.85),c(0.25004, 0.3092), 0.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.90)) {af_clay[i]<-approx   (c(0.85,1.10),c(0.3092, 0.32209), 0.90, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {af_clay[i]<-approx   (c(0.85,1.10),c(0.3092, 0.32209), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.00)) {af_clay[i]<-approx   (c(0.85,1.10),c(0.3092, 0.32209), 1.00, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {af_clay[i]<-approx   (c(0.85,1.10),c(0.3092, 0.32209), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {af_clay[i]<-approx   (c(1.10,1.30),c(0.32209, 0.33695), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.20)) {af_clay[i]<-approx   (c(1.10,1.30),c(0.32209, 0.33695), 1.20, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {af_clay[i]<-approx   (c(1.10,1.30),c(0.32209, 0.33695), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {af_clay[i]<-approx   (c(1.30,1.50),c(0.33695, 0.34204), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.40)) {af_clay[i]<-approx   (c(1.30,1.50),c(0.33695, 0.34204), 1.40, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {af_clay[i]<-approx   (c(1.30,1.50),c(0.33695, 0.34204), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {af_clay[i]<-approx   (c(1.50,1.70),c(0.34204, 0.3399394), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.60)) {af_clay[i]<-approx   (c(1.50,1.70),c(0.34204, 0.3399394), 1.60, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.65)) {af_clay[i]<-approx   (c(1.50,1.70),c(0.34204, 0.3399394), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {af_clay[i]<-approx   (c(1.70,1.90),c(0.3399394, 0.3316413), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.80)) {af_clay[i]<-approx   (c(1.70,1.90),c(0.3399394, 0.3316413), 1.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {af_clay[i]<-approx   (c(1.70,1.90),c(0.3399394, 0.3316413), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {af_clay[i]<-approx   (c(1.70,1.90),c(0.3399394, 0.3316413), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-2.00)) {af_clay[i]<-approx   (c(1.70,1.90),c(0.3399394, 0.3316413), 2.00, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}

for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {ta_clay[i]<-0.1785269}
  if (z[i,1]==as.character(-0.85)) {ta_clay[i]<-0.3069355}
  
  if (z[i,1]==as.character(-0.10)) {ta_clay[i]<-approx   (c(0.05,0.20),c(0.1785269,0.1730538), 0.10, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.15)) {ta_clay[i]<-approx   (c(0.05,0.20),c(0.1785269,0.1730538), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {ta_clay[i]<-approx   (c(0.20,0.40),c(0.1730538, 0.1773226), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.30)) {ta_clay[i]<-approx   (c(0.20,0.40),c(0.1730538, 0.1773226), 0.30, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {ta_clay[i]<-approx   (c(0.20,0.40),c(0.1730538, 0.1773226), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {ta_clay[i]<-approx   (c(0.40,0.60),c(0.1773226, 0.2426882), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.50)) {ta_clay[i]<-approx   (c(0.40,0.60),c(0.1773226, 0.2426882), 0.50, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {ta_clay[i]<-approx   (c(0.40,0.60),c(0.1773226, 0.2426882), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {ta_clay[i]<-approx   (c(0.60,0.85),c(0.2426882, 0.3069355), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.70)) {ta_clay[i]<-approx   (c(0.60,0.85),c(0.2426882, 0.3069355), 0.70, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {ta_clay[i]<-approx   (c(0.60,0.85),c(0.2426882, 0.3069355), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.80)) {ta_clay[i]<-approx   (c(0.60,0.85),c(0.2426882, 0.3069355), 0.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.90)) {ta_clay[i]<-approx   (c(0.85,1.10),c(0.3069355, 0.3256022), 0.90, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {ta_clay[i]<-approx   (c(0.85,1.10),c(0.3069355, 0.3256022), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.00)) {ta_clay[i]<-approx   (c(0.85,1.10),c(0.3069355, 0.3256022), 1.00, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {ta_clay[i]<-approx   (c(0.85,1.10),c(0.3069355, 0.3256022), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {ta_clay[i]<-approx   (c(1.10,1.30),c(0.3256022, 0.3305161), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.20)) {ta_clay[i]<-approx   (c(1.10,1.30),c(0.3256022, 0.3305161), 1.20, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {ta_clay[i]<-approx   (c(1.10,1.30),c(0.3256022, 0.3305161), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {ta_clay[i]<-approx   (c(1.30,1.50),c(0.3305161, 0.3286292), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.40)) {ta_clay[i]<-approx   (c(1.30,1.50),c(0.3305161, 0.3286292), 1.40, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {ta_clay[i]<-approx   (c(1.30,1.50),c(0.3305161, 0.3286292), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {ta_clay[i]<-approx   (c(1.50,1.70),c(0.3286292, 0.3283146), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.60)) {ta_clay[i]<-approx   (c(1.50,1.70),c(0.3286292, 0.3283146), 1.60, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.65)) {ta_clay[i]<-approx   (c(1.50,1.70),c(0.3286292, 0.3283146), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {ta_clay[i]<-approx   (c(1.70,1.90),c(0.3283146, 0.3126136), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.80)) {ta_clay[i]<-approx   (c(1.70,1.90),c(0.3283146, 0.3126136), 1.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {ta_clay[i]<-approx   (c(1.70,1.90),c(0.3283146, 0.3126136), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {ta_clay[i]<-approx   (c(1.70,1.90),c(0.3283146, 0.3126136), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-2.00)) {ta_clay[i]<-approx   (c(1.70,1.90),c(0.3283146, 0.3126136), 2.00, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}


#Clay function coming from ORCHIDEE
#For this site we do not take that into account the effect of clay on decomposition since we define the decomposition
#rate of SOM in function of depth. The effect of depth is probably due to clay differences.
ta_clay_func<-1-0.75* ta_clay
af_clay_func<-1-0.75* af_clay


#######                         ######
######      END KUT CODE        ##### 
######                          ######



#Calculation of the clay function
clay_func<-af_clay_func


####
# Root profile
####

#Roots profil of tree (% of the total root mass) - from Cardinael et al. 2015 Pland and Soil
profil_TR_R<-matrix(ncol=dim(d)[1]+1,nrow=dim(z)[1]+1)
for (i in 1:dim(d)[1]) {profil_TR_R[1,i+1]<-d[i,]}
for (i in 1:dim(z)[1]) {profil_TR_R[i+1,1]<-z[i,]}

for (j in seq(z[1,1],z[dim(z)[1],1], by=-step_depth)){ 
  for (i in 1:dim(z)[1]+1) {
    if (profil_TR_R[i,1]==as.character(j)){
      for (k in 1:dim(d)[1]+1)     {
        if (profil_TR_R[1,k]<=limit_grass) {
          profil_TR_R[i,k]<-13.9169*exp((-1.38609)*(-z[i-1,1])) 	
        }
        else 												  {
          profil_TR_R[i,k]<-(10.306589-1.1505*(d[k-1,1]))*exp((-1.10282+0.18586*(d[k-1,1]))*(-z[i-1,1])) 
          
          
        }
        profil_TR_R[i,k]<-profil_TR_R[i,k]/100 #Conversion from % to proportion			
      }
    }
  }
}		                            


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

######### OLD CODE ##########
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


#Roots profil of graminae under the tree line (% of the total root mass)
profil_GR_R<-matrix(ncol=dim(d)[1]+1,nrow=dim(z)[1]+1)
for (i in 1:dim(d)[1]) {profil_GR_R[1,i+1]<-d[i,]}
for (i in 1:dim(z)[1]) {profil_GR_R[i+1,1]<-z[i,]}

for (j in seq(z[1,1],z[dim(z)[1],1], by=-step_depth)){ 
  for (i in 1:dim(z)[1]+1) {
    if (profil_GR_R[i,1]==as.character(j)){
      for (k in 1:dim(d)[1]+1)     {
        if (profil_GR_R[1,k] <= limit_grass) {
          profil_GR_R[i,k]<-0.4365*exp(-3.121*(-z[i-1,1]))
        }													
        else 								{
          profil_GR_R[i,k]<-0
        }
      }
    }
  }
}


for (i in 1:dim(z)[1]+1) {if (profil_GR_R[i,1]<limit_root_grass) {profil_GR_R[i,]<-0}}	
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


#Transport parameters  5.24, 21.60
v=5.24/10000.
v_slow=v

Dt=21.60/10000.

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





######
# GEEN IDEE
####
kf=((import_tree_be)/(import_tree_be+import_crop_be+import_grass_be))*kt + ((import_crop_be+import_grass_be)/(import_tree_be +import_crop_be+import_grass_be))*kw
kf[1]=((import_tree_ab+import_tree_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kt + ((import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1])/(import_tree_ab+import_tree_be[1]+ import_crop_ab+import_crop_be[1]+import_grass_ab+import_grass_be[1]))*kw




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

#Aboveground total inputs (tC ha-1)
Input_leaves= 3.04568*DBH  

#Here, the coefficient were obtained based on data regression, but the function underestimates the 
#yield by 20%, that's why we multiplied by 1.2

for (k in 1:dim(d)[1])     {
  Yield_AF[1: Agrof_length,k]<- 1.2*((4.39*d[k,1]+64.57)/100)*Yield_ctrl*b[1: Agrof_length]
} 

#SOC stocks are from Cardinael et al., 2015 Geoderma
#For control
SOM <- c(9.34,13.23,13.23,11.81,11.81,10.17,10.17,10.11,10.11,10.11,9.65,9.65,10.06,10.06,10.22,10.22,9.69,9.69,8.33,8.33)*0.1/(ta_bd*step_depth)
clay<-ta_clay
moist_f<-moyano(SOM,clay)
hum_soil<-seq(0.02,1.2,0.02)
mf_ctrl<-rep(1,dim(z)[1])

for (j in 1:dim(z)[1]) 	{
  k<-which.min(abs(hum_soil-moist_profil[j]))
  mf_ctrl[j]<-moist_f[j,k]
}
for (j in 1:dim(z)[1]) {if (mf_ctrl[j] > 1) {mf_ctrl[j]<-1} else {mf_ctrl[j]<-mf_ctrl[j]}}

#For tree line
SOM <- c(21.56,15.63,15.63,12.14,12.14,10.47,10.47,10.79,10.79,10.79,9.95,9.95,10.27,10.27,10.10,10.10,9.22,9.22,8.30,8.30)*0.1/(ta_bd*step_depth)
clay<-af_clay
moist_f<-moyano(SOM,clay)
hum_soil<-seq(0.02,1.2,0.02)
mf_tl<-rep(1,dim(z)[1])

for (j in 1:dim(z)[1]) 	{
  k<-which.min(abs(hum_soil-moist_profil[j]))
  mf_tl[j]<-moist_f[j,k]
}
for (j in 1:dim(z)[1]) {if (mf_tl[j] > 1) {mf_tl[j]<-1} else {mf_tl[j]<-mf_tl[j]}}

#For inter row
SOM <- c(9.78,14.04,14.04,12.04,12.04,10.22,10.22,10.43,10.43,10.43,9.72,9.72,9.82,9.82,9.82,9.82,9.05,9.05,7.76,7.76)*0.1/(ta_bd*step_depth)
clay<-af_clay
moist_f<-moyano(SOM,clay)
hum_soil<-seq(0.02,1.2,0.02)
mf_ir<-rep(1,dim(z)[1])

for (j in 1:dim(z)[1]) 	{
  k<-which.min(abs(hum_soil-moist_profil[j]))
  mf_ir[j]<-moist_f[j,k]
}
for (j in 1:dim(z)[1]) {if (mf_ir[j] > 1) {mf_ir[j]<-1} else {mf_ir[j]<-mf_ir[j]}}


#Belowground total inputs (tC ha-1)
Input_TR<-matrix(ncol=dim(d)[1],nrow= Agrof_length)
Input_CR_SPIN<-d[,1]
Input_CR<-matrix(ncol=dim(d)[1],nrow= Agrof_length)
Input_GR<-d[,1]

#To obtain the relationship between belowground inputs and DBH, we just do a linear regression between the inputs in 2012 after 20 years of
#agroforestery and a point of 0 inputs at t0.
for (t in 1: Agrof_length) {
  for (k in 1:dim(d)[1])     {
    if (d[k,1]<=limit_grass) {
      Input_TR[t,k]=3.6886*DBH[t]
    }
    else                           {
      Input_TR[t,k]=4.7005*DBH[t]*exp(-0.27531*(d[k,1]))
    }
  }
}										
for (k in 1:dim(d)[1])     {
  Input_CR_SPIN[k]= Yield_ctrl*AB_Yi_ratio* Ro_Sh_ratio* C_cont_root
  Input_GR[k]=1 #We do not have the data to estimate the total roots input, therefore we fitted a profile on the stock on the first layer and we directly applied this function depending on stock. To facilitate model developpement we artificially fixed our inputs to 1.
}

for (k in 1:dim(d)[1])     {
  if (d[k,1]<=limit_grass)   {
    Input_CR[,k]=0
  }
  else                             {
    Input_CR[,k]= Yield_AF[,k]* AB_Yi_ratio* Ro_Sh_ratio* C_cont_root
  }
}

#Aboveground total inputs (tC ha-1)
Input_leaves= 3.04568*DBH  

Input_grass_ab<-d[,1]
for (k in 1:dim(d)[1])     {
  if (d[k,1]<=limit_grass)   {
    Input_grass_ab[k]=2.13
  }
  else                             {
    Input_grass_ab[k]= 0
  }
}

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


