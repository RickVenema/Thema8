library(deSolve)

LVmod2D <- function (time, state, parms, N, Da, dx, dy){
  P <- matrix(nr = N, nc = N, state[1:NN]) 
  C <- matrix(nr = N, nc = N, state[-(1:NN)])
  with (as.list(parms), {
    dP <- rG * P *(1 - P/K) - rI * P *C  
    dC <- rI * P * C * AE - rM * C
    zero <- numeric(N)
    ## Fluxes in x-direction; zero fluxes near boundaries 
    FluxP <- rbind(zero, -Da * (P[-1,] - P[-N,])/dx, zero) 
    FluxC <- rbind(zero, -Da * (C[-1,] - C[-N,])/dx, zero) 
    dP <- dP - (FluxP[-1,] - FluxP[-(N+1),])/dx 
    dC <- dC - (FluxC[-1,] - FluxC[-(N+1),])/dx  
    ## Fluxes in y-direction
    FluxP <- cbind(zero, -Da * (P[,-1] - P[,-N])/dy, zero) 
    FluxC <- cbind(zero, -Da * (C[,-1] - C[,-N])/dy, zero) 
    dP <- dP - (FluxP[,-1] - FluxP[,-(N+1)])/dy 
    dC <- dC - (FluxC[,-1] - FluxC[,-(N+1)])/dy 
    return(list(c(as.vector(dP), as.vector(dC)))) 
  }) 
}
pars <- c(rI = 0.2, rG = 1.0, rM = 0.2, AE = 0.5, K = 10)
R <- 20
N <- 50
dx <- R/N
dy <- R/N
Da <- 0.05
NN <- N * N
yini <- rep(0, 2 * N * N)
cc <- c((NN/2):(NN/2+1) + N/2, (NN/2):(NN/2 + 1)- N/2)
yini[cc] <- yini[NN + cc] <- 10
times <- seq(0,200, by = 1)
out <- ode.2D(y = yini, times = times, func = LVmod2D, 
              parms = pars, dimens = c(N, N), N = N, 
              dx = dx, dy = dy, Da = Da, ynames = FALSE, lrw = 440000)


t50 <-  matrix(nrow = nx, ncol = ny,
               data = subset(out, select = "C", subset = (time == 50)))