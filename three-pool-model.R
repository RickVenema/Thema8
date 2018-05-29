# Rick Venema
# 368044
# k.f.venema@st.hanze.nl

########
# Libs #
########
library(deSolve)

##
# dz = diepte
##

parameters <- c(I_tzd = 1, A = 0.0000854, D = 9.64*(10^-4), C = 1, dz = 0.2, h = 0.34, f2 = 0.8, f1 = 0.86) 

state <- c(dec_HSOC2_tzd <- 0.0083, dec_HSOC1_tzd <- 0.0001, dec_FOC_tzd <- 1.01)

times <- seq(0, 20,  by = 1)

three.pool <- function(t, y, parms){
  with(as.list(c(parms)), {
    dF_AD <- (A * C) + (-D * (C/dz))
    dFOC_tzd <- I_tzd + (dF_AD) + h * f2 * dec_HSOC1_tzd + h * dec_HSOC2_tzd - dec_FOC_tzd
    dHSOC1 <- (dF_AD) + h * f1 * dec_FOC_tzd - dec_HSOC1_tzd
    dHSOC2 <- (dF_AD) + h * (1-f1) * dec_FOC_tzd + h * (1-f2) * dec_HSOC1_tzd - dec_HSOC2_tzd
    return(list(c(dFOC_tzd, dHSOC1, dHSOC2)))
  }
  )
}

####
# not in attachment
####
out  <- ode(times = times, y = state,   parms = parameters, func = three.pool, method = "euler")

out <- as.data.frame(out)
colnames(out)<- c("distance from tree", "FOC", "HSOC1", "HSOC2")
plot(out$`distance from tree`, out$FOC)

