library(maptools)
library(ggplot2)
library(ggrepel)

# Import files
setwd("~/R/Analysis/1_Test")
METADATA <- read.csv(file="metadata.csv",header=T)

# Make dataset
pilots.pca <- prcomp(na.omit(METADATA),scale=TRUE, center = TRUE)  #standardized
biplot(pilots.pca) # Check the result
loading <- sweep(pilots.pca$rotation,MARGIN=2,pilots.pca$sdev,FUN="*")
loading <- data.frame(loading)

# ggplot
ggplot(loading) + 
geom_segment(aes(xend=PC1, yend=PC2), x=0, y=0, arrow=arrow(length = unit(0.5,"cm"))) + 
geom_text_repel(aes(x=PC1, y=PC2, label=rownames(loading)),  size=8, color='black') +
xlim(-1,1) + 
ylim(-1,1) +
theme_classic()+
theme(text=element_text(size=14,color="black"),
axis.text=element_text(size=12,color="black"))+
coord_fixed()

# Save
ggsave(file = "PCA.png")
