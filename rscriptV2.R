table1<-data.frame(read.delim("ccpe.csv"))[0:108,]
datatest<-data.frame(table1$ccpE-table1$WT)
table2<-data.frame(read.delim("CodY, sigB, rsbU Mutante.csv"))[0:108,]
datatest<-data.frame(datatest,table2$codY-table2$WT,table2$rsbU-table2$WT,table2$sigB-table2$WT)
table3<-data.frame(read.delim("RnaseY pknB SaeS agrA.csv"))[0:108,]
datatest<-data.frame(datatest,table3$pknB-table3$WT,table3$SaeS-table3$WT,table3$agrA-table3$WT,table3$ΔRNase.Y-table3$WT)
table4<-data.frame(read.delim("rpirc.csv"))[0:108,]
datatest<-data.frame(datatest,table4$RpiRc-table4$WT)
table5<-data.frame(read.delim("rshrelprelq.csv"))[0:108,]
datatest<-data.frame(datatest,table5$rsh.relp.relq-table5$WT)
table6<-data.frame(read.delim("rsp Mutante.csv"))[0:108,]
datatest<-data.frame(datatest,table6$rsp-table6$WT)
table7<-data.frame(read.delim("SarA sarT sarU rot.csv"))[0:108,]
datatest<-data.frame(datatest,table7$SarA-table7$WT,table7$SarT-table7$WT,table7$SarU-table7$WT,table7$rot-table7$WT)
table8<-data.frame(read.delim("SigS SaeR.csv"))[0:108,]
datatest<-data.frame(datatest,table8$SigS-table8$WT,table8$SaeR-table8$WT)
table9<-data.frame(read.delim("VfrB.csv"))[0:108,]
datatest<-data.frame(datatest,table9$VfrB -table9$WT)
table10<-data.frame(read.delim("VraR VraS ArlR SrrA SrrB.csv"))[0:108,]
datatest<-data.frame(datatest,table10$VraR-table10$WT,
table10$VraS-table10$WT,table10$ArlR-table10$WT,table10$ArlS-table10$WT,table10$SrrA-table10$WT,table10$SrrB-table10$WT)
colnames(datatest)<-c("ccpE","codY","rsbU","sigB","pknB","SaeS","agrA","ΔRNase.Y","RpiRc","rsh.relp.relq","rsp","SarA","SarT",
                      "SarU","rot","SigS","SaeR","VfrB","VraR","VraS","ArlR","ArlS","SrrA","SrrB")
colnames(datatest)
datatest<-data.frame(datatest$rsp,datatest$sigB,datatest$rsbU,datatest$ccpE,datatest$codY,datatest$RpiRc,datatest$agrA,
                     datatest$SarU,datatest$VfrB,datatest$SaeS,datatest$SaeR,datatest$SigS,datatest$rot,datatest$pknB,datatest$SrrA,datatest$SrrB,
                     datatest$VraS,datatest$VraR,datatest$ArlS,datatest$ArlR,datatest$SarA,datatest$SarT,datatest$rsh.relp.relq,datatest$ΔRNase.Y)
colnames(datatest)<-gsub("^datatest.","",colnames(datatest))
colnames(datatest)
pdf("ssr42v2.pdf")
plot(y=table1$WT,x=seq(0, 23, 23/107),type = "l",ylab = "rel. fluorescence units",xlab="time [h]",  xlim=c(0, 23),xaxt="n")
axis(side=1, at=c(0:23))
pheatmap::pheatmap(t(datatest),cluster_rows = T,cluster_cols = F,main="No normalization")
pheatmap::pheatmap(t(datatest),cluster_rows = F,cluster_cols = F,main="No normalization")
summary(datatest)
dev.off()