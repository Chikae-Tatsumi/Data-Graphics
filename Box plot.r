library(ggplot2)

setwd("~/R/Analysis/1_Test")
METADATA <- read.csv("metadata.csv",header=T)
DESIGN <- read.csv("experimental_design.csv",header=T)
MD <- cbind (METADATA,DESIGN)

ggplot(MD) +
geom_boxplot(aes(y = Factor1, x = Factor2, fill = Group))+   #Change
# scale_fill_manual(values = c("#C77CFF","#7CAE00","#00BFC4","#F8766D"))+  # if you want to change the colors
theme_classic()+
theme(text=element_text(size=14,color="black"),axis.text=element_text(size=12,color="black"))+
# labs (x="",y="")+ # if you want to change the axis titles
coord_fixed()

ggsave("Box plot.png")