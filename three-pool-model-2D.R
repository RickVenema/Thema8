library(reshape2)
library(rgl)
library(colorspace)
library(plot3D)
library(shiny)
out_ar <- array(out_total, dim=c(65,18,60))
M <- melt(out_ar)
colors<-rep("black", length(M[,1,]))
colors[M[,1,] > 5 && M[,1,] < 6] <- colorRampPalette(c( "brown"))(sum(M[,,1] > 5 && M[,1,] < 6))
colors[M[,1,] > 7 && M[,1,] < 8] <- colorRampPalette(c( "yellow"))(sum(M[,,1] > 7 && M[,1,] < 8))
colors[M[,1,] > 15 && M[,1,] < 20] <- colorRampPalette(c( "green"))(sum(M[,,1] > 15 && M[,1,] < 20))

# points3d(M, col=colors)
dim(out_ar)
# plot3d(M, col=colors, type = "s", radius = (M$value / 5) )
# 
# colors<-rep("black", length(M[,1,]))
# colors[M[,1,] > 5 && M[,1,] < 6] <- colorRampPalette(c( "brown"))(sum(M[,,1] > 5 && M[,1,] < 6))
# colors[M[,1,] > 7 && M[,1,] < 8] <- colorRampPalette(c( "yellow"))(sum(M[,,1] > 7 && M[,1,] < 8))
# (colors[M$value > 15 && M$value < 20] <- colorRampPalette(c( "green"))(sum(M$value > 15 && M$value < 20)))
# points3d(M, col=colors)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(out_total[,,2]), col=heat.colors(10000), useRaster = TRUE) 
hist3D(z= out_total[,,4])
image2D(out_total[,,5])



