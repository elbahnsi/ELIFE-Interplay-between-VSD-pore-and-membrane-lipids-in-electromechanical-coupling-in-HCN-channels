#!/usr/bin/env Rscript 

#Paths for systems
rest="./data/resting/"
act="./data/activated/"
int="./data/intermediate/"

#Reading data
Rs4_s5_elec=read.table(paste(rest,"s4_s5.elec",sep=""),header=F)
Rs4_s5_hph=read.table(paste(rest,"s4_s5.hph",sep=""),header=F)
Rs4_s1_elec=read.table(paste(rest,"s4_s1.elec",sep=""),header=F)
Rs4_s1_hph=read.table(paste(rest,"s4_s1.hph",sep=""),header=F)

As4_s5_elec=read.table(paste(act,"s4_s5.elec",sep=""),header=F)
As4_s5_hph=read.table(paste(act,"s4_s5.hph",sep=""),header=F)
#As4_s1_elec=read.table(paste(act,"s4_s1.elec",sep=""),header=F)
As4_s1_hph=read.table(paste(act,"s4_s1.hph",sep=""),header=F)


Is4_s5_elec=read.table(paste(int,"s4_s5.elec",sep=""),header=F)
Is4_s5_hph=read.table(paste(int,"s4_s5.hph",sep=""),header=F)
Is4_s1_elec=read.table(paste(int,"s4_s1.elec",sep=""),header=F)
Is4_s1_hph=read.table(paste(int,"s4_s1.hph",sep=""),header=F)


chain=c("A","B","C","D")

#Making Figure
png('Figure1-S1-A.png', width=1600, height=800,res=100)
par(mfrow = c(2,4))
par(mar=c(5,5.5,4.5,1))
for (i in 1:4) {
	plot(density(Rs4_s5_hph[which(Rs4_s5_hph[,2] == chain[i]),3]), type='l', col="blue2",pch=16,
	main=paste("Chain",chain[i] ,"\nS4-S5 interface",sep=""),ylab="Density",
	xlab=expression(paste("CA (",ring(A)^2,")",sep="")), ylim=c(0,0.09),xlim=c(0,120),
	cex.main=2,cex.lab=1.8,cex=1.8,cex.axis=1.8,lwd=3)
	lines(density(As4_s5_hph[which(As4_s5_hph[,2] == chain[i]),3]),pch=16, type='l', col="red",lwd=3, cex=1.8)
	lines(density(Is4_s5_hph[which(Is4_s5_hph[,2] == chain[i]),3]),pch=16, type='l', col="yellow2",lwd=3, cex=1.8)
        }

for (i in 1:4) {
        plot(density(Rs4_s1_hph[which(Rs4_s1_hph[,2] == chain[i]),3]), type='l', col="blue2",pch=16,
        main=paste("S4-S1 interface",sep=""),ylab="Density",
        xlab=expression(paste("CA (",ring(A)^2,")",sep="")), ylim=c(0,0.14),xlim=c(0,150),
        cex.main=2,cex.lab=1.8,cex=1.8,cex.axis=1.8,lwd=3)
        lines(density(As4_s1_hph[which(As4_s1_hph[,2] == chain[i]),3]),pch=16, type='l', col="red",lwd=3, cex=1.8)
        lines(density(Is4_s1_hph[which(Is4_s1_hph[,2] == chain[i]),3]),pch=16, type='l', col="yellow2",lwd=3, cex=1.8)
}
dev.off()


