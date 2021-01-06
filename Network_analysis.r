library(igraph)  
library(Hmisc)  
library(Matrix)  

# Import files
setwd("~/R/Analysis/1_Test/ITS")
ASV.table <- read.table(file="rarefied_ASV_table.txt",header=T)
ASV <- ASV.table [,1:(ncol(ASV.table)-7)]
taxonomy <- ASV.table [,(ncol(ASV.table)-6):ncol(ASV.table)]
percent <- ASV / mean(colSums(ASV)) *100
percent.t <- t(percent)

percent.t.filter <- percent.t[ ,colMeans(percent.t) >= 0.01]
print(c(ncol(percent.t),"versus",ncol(percent.t.filter)))

percent.cor <- rcorr(as.matrix(percent.t.filter), type="spearman")
percent.pval <- forceSymmetric(percent.cor$P) # Self-correlation as NA
#Select only the taxa for the filtered ASVs by using rownames of percent.pval
sel.tax <- taxonomy[rownames(percent.pval),,drop=FALSE]
#Sanity check --> should be "[1] TRUE"
all.equal(rownames(sel.tax), rownames(percent.pval))

p.yes <- percent.cor$P<0.05
r.yes <- percent.cor$r>0
r.high <- percent.cor$r>0.7
r.val = percent.cor$r # select all the correlation values 
p.r.yes = p.yes*r.yes*r.val*r.high
adjm<-p.r.yes 

net.grph=graph.adjacency(adjm,mode="undirected",weighted=TRUE,diag=FALSE)

# hs <- hub_score(net.grph, weights=NA)$vector　
# as <- authority_score(net.grph, weights=NA)$vector
# pr <- page.rank(net.grph,directed=F)$vector　
deg <- degree(net.grph, mode="all")　

col=rainbow(length(levels(sel.tax$Phylum)))
plot.igraph(net.grph, vertex.size=deg*0.15,vertex.label=NA, vertex.color=col[unclass(sel.tax$Phylum)],layout=layout.kamada.kawai)
# plot(net.grph, vertex.size=deg*0.15,vertex.label=NA,vertex.color=col[unclass(sel.tax$Phylum)],layout=layout.random)
# plot(net.grph, vertex.size=deg*0.15,vertex.label=NA,vertex.color=col[unclass(sel.tax$Phylum)],layout=layout.fruchterman.reingold)
legend(x = "bottomleft", legend = levels(sel.tax$Phylum), pch = 19, col = col, bty = "n", pt.cex=2, title = "Color legend")

gsize <- gsize(net.grph)
edge_density <- round(edge_density(net.grph),digit=5)
text(x=1,y=-1,paste("The number of edge = ", gsize))
text(x=1,y=-1.1,paste("edge density = ", edge_density))

# Save as you like
