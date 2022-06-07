library("pheatmap")

dat<-read.delim("hier_Norm_100_ncRNA_max_3cl.csv",sep=";", header = T)[,13:33]
yMat <- as.matrix(dat)
names(dimnames(yMat)) <- c("ID", "")


#colnames(dat)
colnames(dat)<-gsub("^ID.*?_130_","",colnames(dat))

colnames(dat)<-c("ID", "Fraction_01", 
                      "Fraction_02",  "Fraction_03", 
                      "Fraction_04",  "Fraction_05", 
                      "Fraction_06",  "Fraction_07", 
                      "Fraction_08",  "Fraction_09", 
                      "Fraction_10",  "Fraction_11", 
                      "Fraction_12",  "Fraction_13", 
                      "Fraction_14",  "Fraction_15", 
                      "Fraction_16",  "Fraction_17", 
                      "Fraction_18",  "Fraction_19", 
                      "Fraction_20",  "Pellet")



#colnames(dat)
#pdf("test.pdf")
#dat2<-data.frame[,2:33]
pheatmap(t(dat),cluster_rows = F,cluster_cols = T, main="GradSeq S.a. ncRNAs, clustered")

summary(dat)
#dev.off()
