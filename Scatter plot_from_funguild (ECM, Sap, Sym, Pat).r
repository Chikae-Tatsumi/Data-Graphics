library(tidyverse)
library (dplyr)

setwd("~/R/Analysis/1_Test")
DESIGN <- read.csv("experimental_design.csv",header=T)
setwd("~/R/Analysis/1_Test/ITS")
funguild.table <- read.csv(file="rarefied_ASV_table_funguild.guild.csv",row.names = 1)
ASV <- funguild.table [,1:(ncol(funguild.table)-10)] 
guild <- funguild.table [,(ncol(funguild.table)-9):ncol(funguild.table)] 
percent <- ASV / mean(colSums(ASV)) *100

# For ECM
aggregated <- aggregate(percent, by=list(guild$Guild),FUN = sum,na.rm=F) 
row.names(aggregated)<-aggregated[,1]
aggregated <- aggregated[,-1]
aggregated <- data.frame(aggregated)
rows <- grep ("Ectomycorrhizal", rownames (aggregated)) 
subset <- aggregated[rows,]
subset.t <- t(subset)
ECM <- data.frame(rowSums (subset.t))
colnames (ECM) [1] <- "ECM"

# For Saprotroph
aggregated <- aggregate(percent, by=list(guild$Trophic.Mode),FUN = sum,na.rm=F)  
row.names(aggregated)<-aggregated[,1]
aggregated <- aggregated[,-1]
aggregated <- data.frame(aggregated)
rows <- grep ("Saprotroph", rownames (aggregated)) 
subset <- aggregated[rows,]
subset.t <- t(subset)
Saprotroph <- data.frame(rowSums (subset.t))
colnames (Saprotroph) [1] <- "Saprotroph" 

# For Symbiotroph
rows <- grep ("Symbiotroph", rownames (aggregated)) 
subset <- aggregated[rows,]
subset.t <- t(subset)
Symbiotroph <- data.frame(rowSums (subset.t))
colnames (Symbiotroph) [1] <- "Symbiotroph" 

# For Pathotroph
rows <- grep ("Pathotroph", rownames (aggregated)) 
subset <- aggregated[rows,]
subset.t <- t(subset)
Pathotroph <- data.frame(rowSums (subset.t))
colnames (Pathotroph) [1] <- "Pathotroph"

Result <- cbind (ECM, Saprotroph,Symbiotroph, Pathotroph)
write.csv(Result, "aggregated.funguild.table.csv")

data <- cbind (Result, DESIGN)
for (i in 1:ncol(Result)){
ggplot(data)+
geom_point(aes(x=Factor1, y=data[,i], color=Factor2))+ #Change Factor 1 and 2 
geom_smooth(method="lm", aes(x=Factor1, y=data[,i], group=Factor2, color=Factor2))+  #Change Factor 1 and 2
# scale_fill_manual(values = c("#C77CFF","#7CAE00","#00BFC4","#F8766D"))+  # if you want to change the colors
theme_classic()+
theme(text=element_text(size=14,color="black"),axis.text=element_text(size=12,color="black"))+
labs (y=paste(colnames(data)[i], "(%)",sep = " "), x="") # if you want to change the axis titles

ggsave(paste(colnames(data)[i],".png"),width = 5, height = 4)}
