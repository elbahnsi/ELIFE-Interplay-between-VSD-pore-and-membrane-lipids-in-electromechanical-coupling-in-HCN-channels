#!/usr/bin/env Rscript 

#paths
nohcnd="./data/noHCND/lipid/"
rest2="./data/withHCND/lipid/"
rest="./data/Resting/lipidC/"
act="./data/Activated/lipidC/"

#reading the data
s45a=read.table(paste(nohcnd,"s45_A.dat",sep=""),header=F)
s45b=read.table(paste(nohcnd,"s45_B.dat",sep=""),header=F)
s45c=read.table(paste(nohcnd,"s45_C.dat",sep=""),header=F)
s45d=read.table(paste(nohcnd,"s45_D.dat",sep=""),header=F)

Rs45a=read.table(paste(rest,"s45_A.dat",sep=""),header=F)
Rs45b=read.table(paste(rest,"s45_B.dat",sep=""),header=F)
Rs45c=read.table(paste(rest,"s45_C.dat",sep=""),header=F)
Rs45d=read.table(paste(rest,"s45_D.dat",sep=""),header=F)

R2s45a=read.table(paste(rest2,"s45_A.dat",sep=""),header=F)
R2s45b=read.table(paste(rest2,"s45_B.dat",sep=""),header=F)
R2s45c=read.table(paste(rest2,"s45_C.dat",sep=""),header=F)
R2s45d=read.table(paste(rest2,"s45_D.dat",sep=""),header=F)

As45a=read.table(paste(act,"s45_A.dat",sep=""),header=F)
As45b=read.table(paste(act,"s45_B.dat",sep=""),header=F)
As45c=read.table(paste(act,"s45_C.dat",sep=""),header=F)
As45d=read.table(paste(act,"s45_D.dat",sep=""),header=F)

#Average for Activated AND Resting states
a=rep(mean(c(As45a[,1],As45b[,1],As45c[,1],As45d[,1])),
nrow(As45a))
r=rep(mean(c(Rs45a[,1],Rs45b[,1],Rs45c[,1],Rs45d[,1])),
nrow(As45a))


#Making the figure
png("Figure2-H.png",width=900, height=900, res=130)
layout(matrix(c(1,2,2), 3, 1, byrow = TRUE))

par(mar=c(0,6.5,2,1))
plot(1:nrow(Rs45d),r,main="",cex.main=2.5, cex=2.5, col="blue2",
     cex.lab=2.5, cex.axis=2.5, ylab="N count",xlab="",ylim=c(15,55),
     xaxt='n',lwd=3,type='l',pch=16)
lines(1:nrow(As45d),a,col="red",xaxt='n',lwd=3,type='l',pch=16,cex=2.5)
lines(1:nrow(R2s45d),R2s45d[,1],col="pink",xaxt='n',lwd=3,type='l',pch=16,cex=2.5)

par(mar=c(19.5,6.5,0,1))
plot(1:nrow(Rs45a),r,main="",cex.main=2.5, cex=2.5, col="blue2", cex.lab=2.5, 
     cex.axis=2.5, ylab="N count",xlab="Time (ns)",ylim=c(15,55),
     xaxt='n',lwd=3,type='l',pch=16)
lines(1:nrow(As45a),a,col="red",xaxt='n',lwd=3,type='l',pch=16,cex=2.5)
lines(1:nrow(s45a),s45a[,1],col="pink3",xaxt='n',lwd=3,type='l',pch=16,cex=2.5)

axis(1,seq(0,(nrow(Rs45d)),50),labels=seq(0,(nrow(Rs45d))*5,250),cex.axis=2.5)
dev.off()
