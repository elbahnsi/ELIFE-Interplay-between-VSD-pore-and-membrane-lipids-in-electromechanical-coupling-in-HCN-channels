#!/usr/bin/env Rscript 
library(dplyr)

#path
rest="./data/Resting/"
act="./data/Activated/"
CA="./data/A2/"
TA="./data/A1/"

#Reading the data
rest1=read.table(paste(rest,"H392A_Q398D.dat",sep=""),header=F)
rest2=read.table(paste(rest,"H392B_Q398A.dat",sep=""),header=F)
rest3=read.table(paste(rest,"H392C_Q398B.dat",sep=""),header=F)
rest4=read.table(paste(rest,"H392D_Q398C.dat",sep=""),header=F)

act1=read.table(paste(act,"H392A_Q398D.dat",sep=""),header=F)
act2=read.table(paste(act,"H392B_Q398A.dat",sep=""),header=F)
act3=read.table(paste(act,"H392C_Q398B.dat",sep=""),header=F)
act4=read.table(paste(act,"H392D_Q398C.dat",sep=""),header=F)

CA1=read.table(paste(CA,"H392A_Q398D.dat",sep=""),header=F)
CA2=read.table(paste(CA,"H392B_Q398A.dat",sep=""),header=F)
CA3=read.table(paste(CA,"H392C_Q398B.dat",sep=""),header=F)
CA4=read.table(paste(CA,"H392D_Q398C.dat",sep=""),header=F)

TA1=read.table(paste(TA,"H392A_Q398D.dat",sep=""),header=F)
TA2=read.table(paste(TA,"H392B_Q398A.dat",sep=""),header=F)
TA3=read.table(paste(TA,"H392C_Q398B.dat",sep=""),header=F)
TA4=read.table(paste(TA,"H392D_Q398C.dat",sep=""),header=F)


#Making the figure
png("Figure4-S3-B.png",width=1600, height=400, res=100)
par(mar=c(5,5.5,2,1))
par(mfrow = c(1,4))
par(mar=c(5,5.5,2,1))
plot(density(rest1[,2]),type='l', col="blue2",main="H392-Q398",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(2,15),ylim=c(0,1.2),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act1[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(CA1[,2]), type='l', col="orchid",lwd=3, cex=2)
lines(density(TA1[,2]), type='l', col="purple",lwd=3, cex=2)

plot(density(rest2[,2]),type='l', col="blue2",main="H392-Q398",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(2,15),ylim=c(0,1.2),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act2[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(CA2[,2]), type='l', col="orchid",lwd=3, cex=2)
lines(density(TA2[,2]), type='l', col="purple",lwd=3, cex=2)

plot(density(rest3[,2]),type='l', col="blue2",main="H392-Q398",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(2,15),ylim=c(0,1.2),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act3[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(CA3[,2]), type='l', col="orchid",lwd=3, cex=2)
lines(density(TA3[,2]), type='l', col="purple",lwd=3, cex=2)

plot(density(rest4[,2]),type='l', col="blue2",main="H392-Q398",
xlab=expression(paste("D (",ring(A),")",sep="")), ylab="Density",xlim=c(2,15),ylim=c(0,1.2),
cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
lines(density(act4[,2]), type='l', col="red",lwd=3, cex=2)
lines(density(CA4[,2]), type='l', col="orchid",lwd=3, cex=2)
lines(density(TA4[,2]), type='l', col="purple",lwd=3, cex=2)
dev.off()

