library(tidyverse)
library (dplyr)

setwd("~/R/Analysis/1_Test") # Change
DESIGN <- read.csv("experimental_design.csv",header=T) # Change
setwd("~/R/Analysis/1_Test/ITS") # Change

funguild.table <- read.csv(file="rarefied_ASV_table_funguild.guild.csv",row.names = 1)
ASV <- funguild.table [,1:(ncol(funguild.table)-10)] 
guild <- funguild.table [,(ncol(funguild.table)-9):ncol(funguild.table)] 
percent <- ASV / mean(colSums(ASV)) *100

aggregated <- aggregate(percent, by=list(guild$Guild),FUN = sum,na.rm=F)  # Change "Guild" --> "Trophic_mode"if needed
row.names(aggregated)<-aggregated[,1]
aggregated <- aggregated[,-1]
aggregated <- data.frame(aggregated)
rowMeans <- rowMeans(aggregated)
aggregated <- cbind(aggregated,rowMeans)

rows <- grep ("Ectomycorrhizal", rownames (aggregated)) # Change "Ectomycorrhizal" if needed
subset <- aggregated[rows,]
subset.t <- t(subset)
data <- data.frame(rowSums (subset.t))
colnames (data) [1] <- "Ectomycorrhizal"  # Change "Ectomycorrhizal" if needed
data [nrow(data),1] <- NA
data <- na.omit (data)
data <- cbind (data, DESIGN)

ggplot(data)+
geom_point(aes(x=Factor1, y=Ectomycorrhizal, color=Group))+ # Change 
geom_smooth(method="lm", aes(x=Factor1, y=Ectomycorrhizal, color=Group))+  # Change 
# scale_fill_manual(values = c("#C77CFF","#7CAE00","#00BFC4","#F8766D"))+  # if you want to change the colors
theme_classic()+
theme(text=element_text(size=14,color="black"),axis.text=element_text(size=12,color="black"))+
# labs (y="",x="") # if you want to change the axis titles

ggsave("Plot_Ectomycorrhizal.png")