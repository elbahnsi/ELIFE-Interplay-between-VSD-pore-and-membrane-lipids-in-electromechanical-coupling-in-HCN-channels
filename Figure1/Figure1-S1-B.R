#!/usr/bin/env Rscript 

#Paths for systems
rest="./data/resting/"
act="./data/activated/"
int="./data/intermediate/"

#Reading data
rest1=read.table(paste(rest,"W281A_N300A.dat",sep=""),header=F)
rest2=read.table(paste(rest,"W281B_N300B.dat",sep=""),header=F)
rest3=read.table(paste(rest,"W281C_N300C.dat",sep=""),header=F)
rest4=read.table(paste(rest,"W281D_N300D.dat",sep=""),header=F)

act1=read.table(paste(act,"W281A_N300A.dat",sep=""),header=F)
act2=read.table(paste(act,"W281B_N300B.dat",sep=""),header=F)
act3=read.table(paste(act,"W281C_N300C.dat",sep=""),header=F)
act4=read.table(paste(act,"W281D_N300D.dat",sep=""),header=F)

int1=read.table(paste(int,"W281A_N300A.dat",sep=""),header=F)
int2=read.table(paste(int,"W281B_N300B.dat",sep=""),header=F)
int3=read.table(paste(int,"W281C_N300C.dat",sep=""),header=F)
int4=read.table(paste(int,"W281D_N300D.dat",sep=""),header=F)





png("Figure1-S1-B.png",width=1600, height=400, res=100)
par(mfrow = c(1,4))
par(mar=c(5,5.5,2,1))
plot(density(rest1[,2]),type='l', col="blue2",main="W281-N300",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(0,20),ylim=c(0,0.6),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act1[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(int1[,2]), type='l', col="yellow2",lwd=3, cex=2)

plot(density(rest2[,2]),type='l', col="blue2",main="W281-N300",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(0,20),ylim=c(0,0.6),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act2[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(int2[,2]), type='l', col="yellow2",lwd=3, cex=2)

plot(density(rest3[,2]),type='l', col="blue2",main="W281-N300",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(0,20),ylim=c(0,0.6),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act3[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(int3[,2]), type='l', col="yellow2",lwd=3, cex=2)

plot(density(rest4[,2]),type='l', col="blue2",main="W281-N300",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(0,20),ylim=c(0,0.6),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act4[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(int4[,2]), type='l', col="yellow2",lwd=3, cex=2)
dev.off()


