#!/usr/bin/env Rscript 
library(dplyr)

#paths
rest="./data/Resting/"
act="./data/Activated/"
abmd1="./data/A1/"
abmd2="./data/R1/"
abmd3="./data/A2/"
abmd4="./data/R2/"

#Reading data
rest1=read.table(paste(rest,"Pore1.dat",sep=""),header=F)
act1=read.table(paste(act,"Pore1.dat",sep=""),header=F)
abmd1_1=read.table(paste(abmd1,"Pore1.dat",sep=""),header=F)
abmd2_1=read.table(paste(abmd2,"Pore1.dat",sep=""),header=F)
abmd3_1=read.table(paste(abmd3,"Pore1.dat",sep=""),header=F)
abmd4_1=read.table(paste(abmd4,"Pore1.dat",sep=""),header=F)

rest2=read.table(paste(rest,"Pore2.dat",sep=""),header=F)
act2=read.table(paste(act,"Pore2.dat",sep=""),header=F)
abmd1_2=read.table(paste(abmd1,"Pore2.dat",sep=""),header=F)
abmd2_2=read.table(paste(abmd2,"Pore2.dat",sep=""),header=F)
abmd3_2=read.table(paste(abmd3,"Pore2.dat",sep=""),header=F)
abmd4_2=read.table(paste(abmd4,"Pore2.dat",sep=""),header=F)


#Making the figre
png("Figure4-B.png",width=1000, height=500, res=100)
par(mar=c(5.5,6,2,1))
plot(rest1[,1],rest1[,2], type='l', col="blue2",main="Pore Diameter",
ylab=expression(paste("D (",ring(A),")",sep="")),
xlab="Time (ns)",ylim=c(8,22),cex.main=2.5,cex.lab=2.5,cex=2.5,cex.axis=2.5,lwd=3,
xaxt="n",xlim=c(0,2000),yaxt="n")
lines(rest2[,1],rest2[,2], type='l', col="blue2",lwd=3, cex=2.5)

lines(act1[,1],act1[,2], type='l', col="red",lwd=3, cex=2.5)
lines(act2[,1],act2[,2], type='l', col="red",lwd=3, cex=2.5)

lines(abmd1_1[,1],abmd1_1[,2], type='l', col="purple",lwd=3, cex=2.5)
lines(abmd1_2[,1],abmd1_2[,2], type='l', col="purple",lwd=3, cex=2.5)

lines(abmd2_1[,1],abmd2_1[,2], type='l', col="purple4",lwd=3, cex=2.5)
lines(abmd2_2[,1],abmd2_2[,2], type='l', col="purple4",lwd=3, cex=2.5)

lines(abmd3_1[,1],abmd3_1[,2], type='l', col="orchid",lwd=3, cex=2.5)
lines(abmd3_2[,1],abmd3_2[,2], type='l', col="orchid",lwd=3, cex=2.5)

lines(abmd4_1[,1],abmd4_1[,2], type='l', col="orchid4",lwd=3, cex=2.5)
lines(abmd4_2[,1],abmd4_2[,2], type='l', col="orchid4",lwd=3, cex=2.5)

axis(2,seq(0,20,5),cex.axis=2.5)
axis(1,seq(0,2000,500),seq(0,1000,250),cex.axis=2.5)

dev.off()


