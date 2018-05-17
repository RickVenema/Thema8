# Rick Venema
# 368044
# k.f.venema@st.hanze.nl

########
# Libs #
########
library(deSolve)

#############
# functions #
#############

# Define the parameters
parameters <- c(kd_Rm = 0.612, ks_Rm = 2.90, ks_r = 3.22, kd_R = 0.0572, D=53.409, k_on= 0.00329, IC50_Rm = 26.2,
                k_t = 0.63, k_re = 0.57, Rf = 0.49) 


# Define the model function
volume <- function(t, y, parms){
  with(as.list(c(y, parms)), {
    dmRNA.R_dt <- ks_Rm * (1- (DR_N/ (IC50_Rm + DR_N)))- kd_Rm * mRNA.R 
    dR_dt <- ks_r *  mRNA.R + Rf * k_re * DR_N - k_on * D * R - kd_R * R
    dDR_dt <- k_on * D * R -k_t * DR
    dDR_N_dt <- k_t * DR - k_re* DR_N
    return(list(c(dmRNA.R_dt, dR_dt, dDR_N_dt, dDR_dt)))
  }
  )
}

# Define the state
state <- c(mRNA.R = 4.74, R = 267, DR_N = 0, DR = 0)

# Define time sequence you want to run the model
times <- seq(0, 48,  by = 1)

# Run simulation using continuous approach
out  <- ode(times = times, y = state,   parms = parameters, func = volume, method = "euler")

plot(out[,'mRNA.R'], type="l", xlab="timeunits (hours)", ylab="[mRNA]") 
title(main="Concentration of mRNA in the cell", sub="Graph showing the concentration of mRNA in the cell during
      a 48 hour time period",
      cex.sub = 0.75, cex.main = 1.75, outer = TRUE)
# Makes plots of the results of the model
plot(out)


