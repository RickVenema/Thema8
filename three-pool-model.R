# Rick Venema
# 368044
# k.f.venema@st.hanze.nl

########
# Libs #
########
library(deSolve)

#############
# The model #
#############
three.pool <- function(t, y, parms){
  with(as.list(c(parms)), {
    dF_AD <- (A * C) + (-D * (C/z))
    temp_z <- temp_z + z*0.1
    f_clay <- 1 - 0.75 * Clay_z
    f_temp <- Q ^((temp_z - temp_opt) /10)
    
    k_HSOC1 <- a1 * exp(-b * z)
    k_HSOC2 <- a2 * exp(-b * z)    
    
    dec_HSOC1_tzd <- -k_HSOC1 * HSOC1 * f_moist * f_temp
    dec_HSOC2_tzd <- -k_HSOC2 * HSOC2 * f_moist * f_temp
    dec_FOC_tzd <- -k_FOC * FOC_tzd * f_clay * f_moist * f_temp
    
    dFOC_tzd <- I_tzd + (dF_AD) + h * f2 * dec_HSOC1_tzd + h * dec_HSOC2_tzd - dec_FOC_tzd
    dHSOC1 <- (dF_AD) + h * f1 * dec_FOC_tzd - dec_HSOC1_tzd
    dHSOC2 <- (dF_AD) + h * (1-f1) * dec_FOC_tzd + h * (1-f2) * dec_HSOC1_tzd - dec_HSOC2_tzd
    
    HSOC1 <- HSOC1 + dec_HSOC1_tzd
    HSOC2 <- HSOC2 + dec_HSOC2_tzd
    FOC_tzd <- dec_FOC_tzd + FOC_tzd
    return(list(c(dFOC_tzd, dHSOC1, dHSOC2)))
  }
  )
}




##############
# parameters #
##############
parameters <- c(I_tzd = 0.69,
                A = 21.6*(10^-4),
                D = 9.64*(10^-4),
                C = 2.29,
                h = 0.34,
                f2 = 0.8,
                f1 = 0.86, 
                Clay_z = 0.4,
                f_clay = 0.4,
                f_moist = 0.5,
                Q = 2,
                temp_opt = 304.15, 
                temp_z = 288,
                k_FOC =1.37,
                FOC_tzd = 100,
                a1 = 0.001,
                a2 = 0.0083,
                b = 1,
                HSOC1 = 100,
                HSOC2 = 100,
                z = 0.1)

###################
# Starting Values #
###################
state <- c(dec_HSOC2_tzd <- 0, dec_HSOC1_tzd <- 0, dec_FOC_tzd <- 0)

############
# sequence #
############
times <- seq(0, 20,  by = 1)

out <- ode(times = times, y = state,   parms = parameters, func = three.pool, method = "euler")
out <- as.data.frame(out)


colnames(out)<- c("years", "FOC", "HSOC1", "HSOC2")
plot(out$HSOC1 ~ out$years, type="l", main="Stuff; Red = HSOC2 \n Black = HSOC1 \n Blue = FOC", ylim=c(-150, 350))
points(out$HSOC2 ~ out$years, type="l", col="red")
points(out$FOC ~ out$years, type="l", col="blue")


