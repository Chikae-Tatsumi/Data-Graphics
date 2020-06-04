library(lmf)
library(maptools)

setwd("~/R/Analysis/1_Test")
METADATA <- read.csv(file="metadata.csv",header=T)
DESIGN <- read.csv("experimental_design.csv")
pilots.pca <- prcomp(na.omit(METADATA),scale=TRUE, center = TRUE)  #standardized
biplot(pilots.pca)

loading=sweep(pilots.pca$rotation,MARGIN=2,pilots.pca$sdev,FUN="*")
par(las=1)
plot(x=NULL,type="n",xlab="PC1",ylab="PC2",xlim=c(-1,1),ylim=c(-1,1),xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")
axis(side=1,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
axis(side=2,at=seq(-1,1,0.2),tck=1.0,lty="dotted",lwd=0.5,col="#dddddd",labels=expression(-1.0,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0))
for(i in 1:nrow(loading))
{arrows(0,0,loading[i,1],loading[i,2],col="black",length=0.1)}
pointLabel(x=loading[,1],y=loading[,2],labels=rownames(loading),cex=1.5)
box(bty="l")

dev.copy(pdf,file="PCA.pdf")
dev.off()