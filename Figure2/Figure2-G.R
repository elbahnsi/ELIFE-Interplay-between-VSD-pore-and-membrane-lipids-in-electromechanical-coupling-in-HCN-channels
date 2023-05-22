#!/usr/bin/env Rscript 
library(dplyr)

#paths
rest="./data/withHCND/"
noh="./data/noHCND/"

#Reading data
Rs4_s5_hph=read.table(paste(rest,"s4_s5.hph",sep=""),header=F)
Rs4_s1_hph=read.table(paste(rest,"s4_s1.hph",sep=""),header=F)

Ns4_s5_hph=read.table(paste(noh,"s4_s5.hph",sep=""),header=F)
Ns4_s1_hph=read.table(paste(noh,"s4_s1.hph",sep=""),header=F)


chain=c("A","B","C","D")

#Making figure
png("Figure2-G.png",width=900, height=900, res=130)
layout(matrix(c(1,2,2), 3, 1, byrow = TRUE))
par(mar=c(0,6.5,2,1))
plot(Rs4_s5_hph[which(Rs4_s5_hph[,2] == "D"),1]*5,
Rs4_s5_hph[which(Rs4_s5_hph[,2] == "D"),3], type='l', col="snow4",
ylab=expression(paste("CA (",ring(A)^2,")",sep="")),
xlab="",ylim=c(0,150),cex.main=2,cex.lab=2.5,cex=2.5,cex.axis=2.5,lwd=3,
xaxt="n",xlim=c(0,1000),yaxt="n")
lines(Rs4_s1_hph[which(Rs4_s1_hph[,2] == "D"),1]*5,
Rs4_s1_hph[which(Rs4_s1_hph[,2] == "D"),3], type='l', col="chocolate4",lwd=3, cex=2.5)
axis(2,seq(0,100,50),cex.axis=2.5) 	

par(mar=c(19.5,6.5,0,1))
plot(Ns4_s5_hph[which(Ns4_s5_hph[,2] == "A"),1]*5,
Ns4_s5_hph[which(Ns4_s5_hph[,2] == "A"),3], type='l', col="snow4",pch=16,
ylab=expression(paste("CA (",ring(A)^2,")",sep="")),
xlab="Time (ns)",ylim=c(0,150),cex.main=2,cex.lab=2.5,cex=2.5,cex.axis=2.5,lwd=3,
xaxt="n",xlim=c(0,1000),yaxt="n")
lines(Ns4_s1_hph[which(Ns4_s1_hph[,2] == "A"),1]*5,pch=16,
Ns4_s1_hph[which(Ns4_s1_hph[,2] == "A"),3], type='l', col="chocolate4",lwd=3, cex=2.5)
#abline(h=75)
axis(1,seq(0,1000,250),cex.axis=2.5)
axis(2,seq(0,100,50),cex.axis=2.5)

dev.off()


