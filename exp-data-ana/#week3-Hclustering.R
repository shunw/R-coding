#week three lecture samples
set.seed(1234)
par(mar=c(0, 0, 0, 0))
x<-rnorm(12, mean=rep(1:3, each=4), sd=.2)
y<-rnorm(12, mean=rep(c(1, 2, 1), each=4), sd=.2)
plot(x, y, col="blue", pch=19, cex=2)
text(x+.05, y+.05, labels=as.character(1:12))

dataFrame<-data.frame(x=x, y=y)
distxy<-dist(dataFrame)
hClustering<-hclust(distxy)
plot(hClustering)

hClustering
hClustering$merge
str(hClustering)

myplclust<-function(hclust, lab=hclust$labels, lab.col=rep(1, length(hclust$labels)), 
    hang=0.1, ...){
  ##modification of plcluster for plotting hclust objects *in color*! Copyright
  ##hclust: hclust object lab: a character vector of labels of the leaves of the leaves of the 
  ##tree lab.col: color for the labesl;
  y<-rep(hclust$height, 2)
  x<-as.numeric(hclust$merge)
  y<-y[which(x<0)]
  x<-x[which(x<0)]
  x<-abs(x)
  y<-y[order(x)]
  x<-x[order(x)]
  plot(hclust, labels=FALSE, hang=hang, ...)
  text(x=x, y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order],
       col=lab.col[hclust$order], srt=90, adj=c(1, .5), xpd=NA, ...)
}

#heat map()
dataFrame<-data.frame(x=x, y=y)
set.seed(143)
dataMatrix<-as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

myplclust(hClustering, lab=rep(1:3, each=4), lab.col=rep(1:3, each=4))
