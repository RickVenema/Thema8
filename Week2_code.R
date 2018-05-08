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
parameters <- c(kt = 0.63, DR = 0, Kre = 0.57) 


model <- function(t, y, parms){
  with(as.list(c(parms)), {
    dDR.N_dt <- t * DR - Kre * y
    return(list(c(dDR.N_dt)))
  }
  )
}

state <- c(DR.N = 1)
times <- seq(0.63, 2, by = 0.01)

out <- ode(times = times, y = state, parms = parameters, func=model, method="euler")
plot(out)