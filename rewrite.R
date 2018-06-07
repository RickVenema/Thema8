#An agroforestery model designed in March 2015 to simulate the soil organic carbon (SOC) dynamic designed by Remi Cardinael and Bertrand Guenet. 
#Published in Cardinael et al., 2018 Biogoesciences
#Any questions or suggestions please send emails to remi.cardinael@cirad.fr and/or bertrand.guenet@lsce.ipsl.fr.

#The time step of the model is 1year.
#The transport and decomposition models are based on Guenet et al., 2013 Biogeosciences. The moisture function is based on Moyano et al. 2012 Biogeosciences.
setwd('Agroforestry_model/Agroforestry_model/')
rm(list = ls())
print("***************************************")
print("Welcome!")
print("The model is starting")
#Read the parameter files

#Do we use priming?
priming="n"

#Do you want to run on the control plot?
control="n"

#Mean parameters (optimized coefficients)
PARAM=read.table('run_optim_p0_3pools.def',header=FALSE)

#Model parameter

#distance to the three in meter and resolution in m.

dist=6.5 # (middle of the agroforestry alley)
step_dist=0.1
d=as.data.frame(seq(step_dist,dist, by=step_dist))

#Soil depth in meter and resolution of the depth axis in m
depth=1.95
step_depth=0.1
z= as.data.frame(seq(-0.05,-depth,by=-step_depth))

#Tillage layer (in meter)
til_lay=-0.15

#Limit to crop roots development (in the alley)
limit_root_crop=-1.50

#Limit to grass roots development (in the tree row)
limit_root_grass=-1.50

#Duration of the agroforestry experiment in year
Agrof_length=18
time_Agrof_length=seq(1, Agrof_length)

#The limit from the tree where grass still grow (in meter)
limit_grass=1

zd.bd <- as.numeric(c(-0.05,-0.20,-0.40,-0.60,-0.85,-1.10,-1.30,-1.50,-1.70,-1.90))
ta.bd <- as.numeric(c(1.41,1.61,1.73,1.80,1.74,1.61,1.65,1.65,1.65,1.65))

bd.mv <- data.frame(zd.bd,ta.bd)

ap.bd <- c()

counts.bd <- 0

for (i in z[,1]) {
  counts.bd <- counts.bd + 1
  z.plus <- abs(i)
  print(counts.bd)
  if (i )
  ap.bd[counter] <- approx(c(zd.bd[counts.bd],zd.bd[counts.bd+1]),c(ta.bd[counts.bd],ta.bd[counts.bd+1]), i, method="linear",rule = 1, f = 0, ties = mean)$y[1]
  #ap.bd[i]<-approx(as.character(c(zd.bd[i],zd.bd[i+1])),as.character(c(ta.bd[i],ta.bd[i+1])), z[i], method="linear",rule = 1, f = 0, ties = mean)$y[1]
}
b.d.df <- data.frame(zd.bd,ta.bd)

#bulk density (kg m-3) from Cardinael et al., 2015 Geoderma
ta_bd<-z[,1]
for (i in 1:dim(z)[1]) {
  if (z[i,1]==as.character(-0.05)) {ta_bd[i]<-1.41}
  if (z[i,1]==as.character(-0.20)) {ta_bd[i]<-1.61}
  if (z[i,1]==as.character(-0.40)) {ta_bd[i]<-1.73}
  if (z[i,1]==as.character(-0.60)) {ta_bd[i]<-1.80}
  if (z[i,1]==as.character(-0.85)) {ta_bd[i]<-1.74}
  if (z[i,1]==as.character(-1.10)) {ta_bd[i]<-1.61}
  if (z[i,1]==as.character(-1.30)) {ta_bd[i]<-1.65}
  if (z[i,1]==as.character(-1.50)) {ta_bd[i]<-1.65}
  if (z[i,1]==as.character(-1.70)) {ta_bd[i]<-1.65}
  if (z[i,1]==as.character(-1.90)) {ta_bd[i]<-1.65}
  
  if (z[i,1]==as.character(-0.10)) {ta_bd[i]<-approx   (c(0.05,0.20),c(1.41,1.61), 0.10, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.15)) {ta_bd[i]<-approx   (c(0.05,0.20),c(1.41,1.61), 0.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.25)) {ta_bd[i]<-approx   (c(0.20,0.40),c(1.61, 1.73), 0.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.30)) {ta_bd[i]<-approx   (c(0.20,0.40),c(1.61, 1.73), 0.30, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.35)) {ta_bd[i]<-approx   (c(0.20,0.40),c(1.61, 1.73), 0.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.45)) {ta_bd[i]<-approx   (c(0.40,0.60),c(1.73, 1.80), 0.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.50)) {ta_bd[i]<-approx   (c(0.40,0.60),c(1.73, 1.80), 0.50, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.55)) {ta_bd[i]<-approx   (c(0.40,0.60),c(1.73, 1.80), 0.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.65)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.70)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.70, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.75)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.80)) {ta_bd[i]<-approx   (c(0.60,0.85),c(1.80, 1.74), 0.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.90)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 0.90, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-0.95)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 0.95, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.00)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 1.00, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.05)) {ta_bd[i]<-approx   (c(0.85,1.10),c(1.74, 1.61), 1.05, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.15)) {ta_bd[i]<-approx   (c(1.10,1.30),c(1.61, 1.65), 1.15, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.20)) {ta_bd[i]<-approx   (c(1.10,1.30),c(1.61, 1.65), 1.20, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.25)) {ta_bd[i]<-approx   (c(1.10,1.30),c(1.61, 1.65), 1.25, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.35)) {ta_bd[i]<-approx   (c(1.30,1.50),c(1.65, 1.65), 1.35, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.40)) {ta_bd[i]<-approx   (c(1.30,1.50),c(1.65, 1.65), 1.40, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.45)) {ta_bd[i]<-approx   (c(1.30,1.50),c(1.65, 1.65), 1.45, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.55)) {ta_bd[i]<-approx   (c(1.50,1.70),c(1.65, 1.65), 1.55, method="linear",rule = 1, f = 0, ties = mean)$y[1]}	
  if (z[i,1]==as.character(-1.60)) {ta_bd[i]<-approx   (c(1.50,1.70),c(1.65, 1.65), 1.60, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.65)) {ta_bd[i]<-approx   (c(1.50,1.70),c(1.65, 1.65), 1.65, method="linear",rule = 1, f = 0, ties = mean)$y[1]}				
  if (z[i,1]==as.character(-1.75)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.75, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.80)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.80, method="linear",rule = 1, f = 0, ties = mean)$y[1]}
  if (z[i,1]==as.character(-1.85)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.85, method="linear",rule = 1, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-1.95)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 1.95, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
  if (z[i,1]==as.character(-2.00)) {ta_bd[i]<-approx   (c(1.70,1.90),c(1.65, 1.65), 2.00, method="linear",rule = 2, f = 0, ties = mean)$y[1]}			
}