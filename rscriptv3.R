
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
datatest<-data.frame(datatest$sigB.rsp,datatest$rsbU,datatest$codY,datatest$ccpE,datatest$RpiRc,datatest$agrA.rsp,datatest$VfrB,datatest$SrrA,datatest$SrrB,datatest$ArlR,datatest$SaeS)
colnames(datatest)
colnames(datatest)<-gsub("^datatest.","",colnames(datatest))

matplot(datatest,type='l', xlab='time', ylab='Arb. GFP',main="No normalization")
legend('topleft', inset=.005, legend=colnames(datatest), 
       pch=1,bty = "n",horiz = F, col=1:ncol(datatest))
dev.off()
pdf("rspV3.pdf")
plot(y=table1$WT,x=seq(0, 23, 23/107),type = "l",ylab = "rel. fluorescence units",xlab="time [h]",  xlim=c(0, 23),xaxt="n")
axis(side=1, at=c(0:23))
pheatmap::pheatmap(t(datatest),cluster_rows = F,cluster_cols = F,main="No normalization")
dev.off()
