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
# parameters <- c(kd_Rm = 0.612, ks_Rm = 2.90, ks_r = 3.22, kd_R = 0.0572, D= 0, k_on= 0.00329) 


# model <- function(t, y, parms){
#   with(as.list(c(parms)), {
#     dmRNA.R_dt <- ks_Rm * 1 - kd_Rm * mRNA.R
#     dR_dt <- ks_r *  mRNA.R - k_on * D * R - kd_R * R
#     return(list(c(dmRNA.R_dt, dR_dt)))
#   }
#   )
# }


parameters <- c(kd_Rm = 0.612, ks_Rm = 2.90, ks_r = 3.22, kd_R = 0.0572, D=53.409, k_on= 0.00329, IC50_Rm = 26.2,
                k_t = 0.63, k_re = 0.57, Rf = 0.49) 



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


state <- c(mRNA.R = 4.74, R = 267, DR_N = 0, DR = 0)

#define time sequence you want to run the model
times <- seq(0, 48,  by = 1)
# run simulation using continuous approach
out  <- ode(times = times, y = state,   parms = parameters, func = volume, method = "euler")


plot(out)


