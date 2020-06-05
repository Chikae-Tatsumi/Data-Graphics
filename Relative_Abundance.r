library(ggplot2)
library(stringr)
library(stringi)

# Import files
setwd("~/R/Analysis/1_Test")
DESIGN <- read.csv("experimental_design.csv",header=T)
setwd("~/R/Analysis/1_Test/ITS")
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)

# % table
ASV <- ASV.table [,1:(ncol(ASV.table)-7)]
taxonomy <- ASV.table [,(ncol(ASV.table)-6):ncol(ASV.table)]
Percent <- ASV / mean(colSums(ASV)) *100
# Remove "p__" before phylum name
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="p__", replacement = "", x)}),stringsAsFactors = FALSE) # Change p__ --> the first alphabet of the level you analyze

# Aggregate
Level <- aggregate(Percent, by=list(taxonomy$Phylum),FUN = sum,na.rm=F) # Change Phylum --> the level you analyze
row.names(Level)<-Level[,1]
Level <- Level[,-1]
Level <- data.frame(Level)
rowMeans <- rowMeans(Level)
Level <- cbind(Level,rowMeans)

# Main + <1% abund 
Majors <- Level[Level[,"rowMeans"] > 1,]
Minors <- Level[Level[,"rowMeans"] < 1,]
Others <- colSums (Minors)
Selected <- rbind (Majors, Others) 
rownames (Selected)[nrow(Selected)] <- "Others"
Selected <- Selected[,-ncol(Selected)] 

# Make dataset
Selected.t <- t (Selected)
Bind <- cbind (Selected.t, DESIGN)
Table <- aggregate(Selected.t, by=list(Bind$Group),FUN = mean) # Change Group --> the category you compare
Group <- as.character(Table[,1])
Table <- data.frame(Table)
Data <- data.frame()
for (i in 2:(ncol(Table))){
Abundance <-Table[,i]
Phylum <- colnames (Table)[i] # Change Phylum --> the level you analyze
Bind <- cbind(Group, Abundance, Phylum) # Change Phylum --> the level you analyze
Data<-rbind (Data,Bind)}
Data$Abundance <- as.numeric(as.character(Data$Abundance))

# ggplot
Order <- Majors[order(Majors$rowMeans),]
rownames <- rownames(Order)
rownames <- c("Others",rownames)
Data$Phylum <- factor(Data$Phylum, levels = rownames)

ggplot (Data,  mapping=aes(x=Group, y=Abundance, fill=Phylum))+ # Change Group --> the category you compare, Change Phylum --> the level you analyze, 
geom_bar(aes(), stat="identity", position="stack",color="black")+
theme_classic()+
theme(text=element_text(size=14,color="black"),
axis.text=element_text(size=12,color="black"))

# Save
ggsave(file = "Rel.Abund.png")