library(rgl)

setwd("~/R/Analysis/1_Test")
METADATA<-read.csv("metadata.csv")

x<-METADATA[,1] #CHANGE ME
y<-METADATA[,2] #CHANGE ME
z<-METADATA[,3] #CHANGE ME

plot3d(x, y, z, xlab="X", ylab="Y", zlab="Z")
fit <- lm(z ~ x + y)
coefs <- coef(fit)
planes3d(coefs[2], coefs[3], -1, coefs[1], col="blue", alpha=0.5) #Draw the planar
#See https://kohske.github.io/ESTRELA/201501/index.html

snapshot3d("3D.png")
