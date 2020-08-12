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
percent <- ASV / mean(colSums(ASV)) *100
# Remove "p__" before phylum name
taxonomy <- data.frame(lapply(taxonomy, function(x){gsub(pattern="p__", replacement = "", x)}),stringsAsFactors = FALSE) # Change p__ --> the first alphabet of the level you analyze

# Aggregate
agrregated <- aggregate(percent, by=list(taxonomy$Phylum),FUN = sum,na.rm=F) # Change Phylum --> the level you analyze
row.names(agrregated)<-agrregated[,1]
agrregated <- agrregated[,-1]
agrregated <- data.frame(agrregated)
rowMeans <- rowMeans(agrregated)
agrregated <- cbind(agrregated,rowMeans)

# Main + <1% abund 
major <- agrregated[agrregated[,"rowMeans"] > 1,]
majors <- majors[order(majors$rowMeans,decreasing = T),]
minors <- agrregated[agrregated[,"rowMeans"] < 1,]
Others <- colSums (minors)
selected <- rbind (majors, others) 
rownames (selected)[nrow(selected)] <- "Others"
selected <- selected[,-ncol(selected)] 

# Make dataset
selected.t <- t (selected)
write.csv(selected.t, "aggregated.family.table.csv") #Change
bind <- cbind (selected.t, DESIGN)
table <- aggregate(selected.t, by=list(bind$Group),FUN = mean) # Change Group --> the category you compare
Group <- as.character(table[,1])
table <- data.frame(table)
data <- data.frame()
for (i in 2:(ncol(table))){
Abundance <-table[,i]
Example <- colnames (table)[i]
bind <- cbind(Group, Abundance, Example) 
data<-rbind (data,bind)}
data$Abundance <- as.numeric(as.character(data$Abundance))

rownames<-colnames(table)[ncol(table):2]
data$Example <- factor(data$Example, levels = rownames)

# ggplot
ggplot (data,  mapping=aes(x=Group, y=Abundance, fill=Example))+ # Change Group --> the category you compare
geom_bar(aes(), stat="identity", position="stack",color="black")+
# scale_fill_manual(values = c("gray","#C77CFF","#7CAE00","#00BFC4","#F8766D"))+  # if you want to change the colors
theme_classic()+
theme(text=element_text(size=14,color="black"),axis.text=element_text(size=12,color="black"))+
labs (y="Abundance (%)")

# Save
ggsave(file = "Rel.Abund.png")
