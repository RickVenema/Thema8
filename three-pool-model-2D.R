library(reshape2)
library(rgl)
M <- melt(out_ar)
colors<-rep("black", length(M[,1,]))
colors[M[,1,] > 5 && M[,1,] < 6] <- colorRampPalette(c( "brown"))(sum(M[,,1] > 5 && M[,1,] < 6))
colors[M[,1,] > 7 && M[,1,] < 8] <- colorRampPalette(c( "yellow"))(sum(M[,,1] > 7 && M[,1,] < 8))
colors[M[,1,] > 15 && M[,1,] < 20] <- colorRampPalette(c( "green"))(sum(M[,,1] > 15 && M[,1,] < 20))

# points3d(M, col=colors)
dim(out_ar)
plot3d(M, col=colors)

M[3,1,6]
