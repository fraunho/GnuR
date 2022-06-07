range01 <- function(x){(x-min(x))/(max(x)-min(x))}
rangeMAX <-function(x){x/max(x)}

pdf("rsp.pdf")
table1<-data.frame(read.delim("ArlR SrrA SrrB.csv"))[0:108,]
datatest<-data.frame(table1$ArlR-table1$WT,table1$SrrA-table1$WT,table1$SrrB-table1$WT)
table2<-read.delim("ccpE.csv")[0:108,]
datatest<-data.frame(datatest,table2$ccpE-table2$WT)
table3<-read.delim("codY rsbU.csv")[0:108,]
datatest<-data.frame(datatest,table3$codY-table3$WT,table3$rsbU-table3$WT)
table4<-read.delim("RpiRc.csv")[0:108,]
datatest<-data.frame(datatest,table4$RpiRc-table4$WT)
table5<-read.delim("sigB agrA SaeS.csv")[0:108,]
datatest<-data.frame(datatest,table5$sigB.rsp-table5$WT,table5$agrA.rsp-table5$WT,table5$SaeS-table5$WT)
table6<-read.delim("VfrB.csv")[0:108,]
datatest<-data.frame(datatest,table6$VfrB-table6$WT)
colnames(datatest)<-c("ArlR","SrrA","SrrB","ccpE","codY","rsbU","RpiRc","sigB.rsp","agrA.rsp","SaeS","VfrB")
matplot(datatest,type='l', xlab='time', ylab='Arb. GFP',main="No normalization")
legend('topleft', inset=.005, legend=colnames(datatest), 
       pch=1,bty = "n",horiz = F, col=1:ncol(datatest))
pheatmap::pheatmap(t(datatest),cluster_rows = F,cluster_cols = F,main="No normalization")
summary(datatest)
pheatmap::pheatmap(t(data.frame(apply(datatest,2,range01))),cluster_rows = F,cluster_cols = F,main="normalization 0 to 1 afterwards")


table1<-data.frame(read.delim("ArlR SrrA SrrB.csv"))[0:108,]
table1<-data.frame(apply(table1,2,range01))
datatest<-data.frame(table1$ArlR-table1$WT,table1$SrrA-table1$WT,table1$SrrB-table1$WT)
table2<-read.delim("ccpE.csv")[0:108,]
table2<-data.frame(apply(table2,2,range01))
datatest<-data.frame(datatest,table2$ccpE-table2$WT)
table3<-read.delim("codY rsbU.csv")[0:108,]
table3<-data.frame(apply(table3,2,range01))
datatest<-data.frame(datatest,table3$codY-table3$WT,table3$rsbU-table3$WT)
table4<-read.delim("RpiRc.csv")[0:108,]
table4<-data.frame(apply(table4,2,range01))
datatest<-data.frame(datatest,table4$RpiRc-table4$WT)
table5<-read.delim("sigB agrA SaeS.csv")[0:108,]
table5<-data.frame(apply(table5,2,range01))
datatest<-data.frame(datatest,table5$sigB.rsp-table5$WT,table5$agrA.rsp-table5$WT,table5$SaeS-table5$WT)
table6<-read.delim("VfrB.csv")[0:108,]
table6<-data.frame(apply(table6,2,range01))
datatest<-data.frame(datatest,table6$VfrB-table6$WT)
colnames(datatest)<-c("ArlR","SrrA","SrrB","ccpE","codY","rsbU","RpiRc","sigB.rsp","agrA.rsp","SaeS","VfrB")
matplot(datatest,type='l', xlab='time', ylab='Arb. GFP',main="normalization 0 to 1 before")
legend('topleft', inset=.005, legend=colnames(datatest), 
       pch=1,bty = "n",horiz = F, col=1:ncol(datatest))
pheatmap::pheatmap(t(datatest),cluster_rows = F,cluster_cols = F,main="normalization 0 to 1 before")



table1<-data.frame(read.delim("ArlR SrrA SrrB.csv"))[0:108,]
table1<-data.frame(apply(table1,2,rangeMAX))
datatest<-data.frame(table1$ArlR-table1$WT,table1$SrrA-table1$WT,table1$SrrB-table1$WT)
table2<-read.delim("ccpE.csv")[0:108,]
table2<-data.frame(apply(table2,2,rangeMAX))
datatest<-data.frame(datatest,table2$ccpE-table2$WT)
table3<-read.delim("codY rsbU.csv")[0:108,]
table3<-data.frame(apply(table3,2,rangeMAX))
datatest<-data.frame(datatest,table3$codY-table3$WT,table3$rsbU-table3$WT)
table4<-read.delim("RpiRc.csv")[0:108,]
table4<-data.frame(apply(table4,2,rangeMAX))
datatest<-data.frame(datatest,table4$RpiRc-table4$WT)
table5<-read.delim("sigB agrA SaeS.csv")[0:108,]
table5<-data.frame(apply(table5,2,rangeMAX))
datatest<-data.frame(datatest,table5$sigB.rsp-table5$WT,table5$agrA.rsp-table5$WT,table5$SaeS-table5$WT)
table6<-read.delim("VfrB.csv")[0:108,]
table6<-data.frame(apply(table6,2,rangeMAX))
datatest<-data.frame(datatest,table6$VfrB-table6$WT)
colnames(datatest)<-c("ArlR","SrrA","SrrB","ccpE","codY","rsbU","RpiRc","sigB.rsp","agrA.rsp","SaeS","VfrB")

matplot(datatest,type='l', xlab='time', ylab='Arb. GFP',main="normalization to Max value before")
legend('topleft', inset=.005, legend=colnames(datatest), 
       pch=1,bty = "n",horiz = F, col=1:ncol(datatest))
pheatmap::pheatmap(t(datatest),cluster_rows = F,cluster_cols = F,main="normalization to Max value before")

summary(datatest)


dev.off()