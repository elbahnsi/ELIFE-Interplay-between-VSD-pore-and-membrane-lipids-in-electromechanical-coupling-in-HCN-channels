#!/usr/bin/env Rscript 

library(RColorBrewer)
library(ggplot2)

#USAGE: R --vanilla --slave --args noHCND < Figure2-S1.R

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("1 argument must be supplied (system), 
       USAGE : R --vanilla --slave --args withHCND < Figure2-S1.R", call.=FALSE)
}

sys=args[1]
if (sys == "noHCND" | sys == "withHCND"){
 	print(paste("argument",args[1],"is ok"))
}else{
	stop("argument must be noHCND or withHCND",call.=FALSE)
}

path=paste("./data/",sys,"/",sep="")

#Reading Z position of the gating charges 
fa=read.table(paste(path,"zF186_A.dat",sep=""),header=F)
r267a=read.table(paste(path,"zF267_A.dat",sep=""),header=F)
r270a=read.table(paste(path,"zF270_A.dat",sep=""),header=F)
r273a=read.table(paste(path,"zF273_A.dat",sep=""),header=F)
r276a=read.table(paste(path,"zF276_A.dat",sep=""),header=F)

fb=read.table(paste(path,"zF186_B.dat",sep=""),header=F)
r267b=read.table(paste(path,"zF267_B.dat",sep=""),header=F)
r270b=read.table(paste(path,"zF270_B.dat",sep=""),header=F)
r273b=read.table(paste(path,"zF273_B.dat",sep=""),header=F)
r276b=read.table(paste(path,"zF276_B.dat",sep=""),header=F)

fc=read.table(paste(path,"zF186_C.dat",sep=""),header=F)
r267c=read.table(paste(path,"zF267_C.dat",sep=""),header=F)
r270c=read.table(paste(path,"zF270_C.dat",sep=""),header=F)
r273c=read.table(paste(path,"zF273_C.dat",sep=""),header=F)
r276c=read.table(paste(path,"zF276_C.dat",sep=""),header=F)

fd=read.table(paste(path,"zF186_D.dat",sep=""),header=F)
r267d=read.table(paste(path,"zF267_D.dat",sep=""),header=F)
r270d=read.table(paste(path,"zF270_D.dat",sep=""),header=F)
r273d=read.table(paste(path,"zF273_D.dat",sep=""),header=F)
r276d=read.table(paste(path,"zF276_D.dat",sep=""),header=F)

#Calculation of the distance from the gating charge center
R267A=r267a-fa
R270A=r270a-fa
R273A=r273a-fa
R276A=r276a-fa
              
R267B=r267b-fb
R270B=r270b-fb
R273B=r273b-fb
R276B=r276b-fb
              
R267C=r267c-fc
R270C=r270c-fc
R273C=r273c-fc
R276C=r276c-fc
              
R267D=r267d-fd
R270D=r270d-fd
R273D=r273d-fd
R276D=r276d-fd

#Reading the bending angle (bendangle) and the rotation angle date
ben=read.table(paste(path,"bendangle",sep=""),header=T)
rot=read.table(paste(path,"rotangle",sep=""),header=T)

#Choice of system 
if(sys=="noHCND"){
	fig="noHCND"
	}else{
	fig="withHCND"
}

# Making the figure
png(paste("Figure2-S1",fig,".png",sep=""),width=800, height=1200, res=110)
par(mfrow=c(3,2))
par(mar=c(4,4.5,2,2))
plot(1:nrow(R267A),R267A[,1],type='l',main="Chain A", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="Z relative to F186",xlab="Time (ns)",ylim=c(-20,20),xaxt='n',lwd=2)
lines(1:nrow(R270A),R270A[,1],col="steelblue2",xaxt='n',lwd=2)
lines(1:nrow(R273A),R273A[,1], col="steelblue3",xaxt='n',lwd=2)
lines (1:nrow(R276A),R276A[,1], col="steelblue4",xaxt='n',lwd=2)
abline (h=0,col="gray")
axis(1,seq(1,nrow(R267A),500),labels=seq(0,(nrow(R267A)-1)/2,250),cex.axis=2)
legend("topright",legend=c("R267","R270","R273","R276"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)

plot(1:nrow(R267B),R267B[,1],type='l',main="Chain B", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="Z relative to F186",xlab="Time (ns)",ylim=c(-20,20),xaxt='n',lwd=2)
lines(1:nrow(R270B),R270B[,1],col="steelblue2",xaxt='n',lwd=2)
lines(1:nrow(R273B),R273B[,1], col="steelblue3",xaxt='n',lwd=2)
lines (1:nrow(R276B),R276B[,1], col="steelblue4",xaxt='n',lwd=2)
abline (h=0,col="gray")
axis(1,seq(1,nrow(R267A),500),labels=seq(0,(nrow(R267A)-1)/2,250),cex.axis=2)
legend("topright",legend=c("R267","R270","R273","R276"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)


plot(1:nrow(R267C),R267C[,1],type='l',main="Chain C", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="Z relative to F186",xlab="Time (ns)",ylim=c(-20,20),xaxt='n',lwd=2)
lines(1:nrow(R270C),R270C[,1],col="steelblue2",xaxt='n',lwd=2)
lines(1:nrow(R273C),R273C[,1], col="steelblue3",xaxt='n',lwd=2)
lines (1:nrow(R276C),R276C[,1], col="steelblue4",xaxt='n',lwd=2)
abline (h=0,col="gray")
axis(1,seq(1,nrow(R267A),500),labels=seq(0,(nrow(R267A)-1)/2,250),cex.axis=2)
legend("topright",legend=c("R267","R270","R273","R276"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)

plot(1:nrow(R267D),R267D[,1],type='l',main="Chain D", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="Z relative to F186",xlab="Time (ns)",ylim=c(-20,20),xaxt='n',lwd=2)
lines(1:nrow(R270D),R270D[,1],col="steelblue2",xaxt='n',lwd=2)
lines(1:nrow(R273D),R273D[,1], col="steelblue3",xaxt='n',lwd=2)
lines (1:nrow(R276D),R276D[,1], col="steelblue4",xaxt='n',lwd=2)
abline (h=0,col="gray")
axis(1,seq(1,nrow(R267A),500),labels=seq(0,(nrow(R267A)-1)/2,250),cex.axis=2)
legend("topright",legend=c("R267","R270","R273","R276"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)



#bending
plot(ben[,1],ben[,2]*180/3.14,type='l',main="S4 bending", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="Bending angle",xlab="Time (ns)",ylim=c(0,80),xaxt='n',lwd=2)
lines(ben[,1],ben[,3]*180/3.14,col="steelblue2",xaxt='n',lwd=2)
lines(ben[,1],ben[,4]*180/3.14,col="steelblue3",xaxt='n',lwd=2)
lines(ben[,1],ben[,5]*180/3.14,col="steelblue4",xaxt='n',lwd=2)
axis(1,seq(0,nrow(ben),500),labels=seq(0,(nrow(ben)/2),250),cex.axis=2)
legend("topleft",legend=c("chain A","chain B","chain C","chain D"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)



#rotation
plot(rot[,1],rot[,2]*180/3.14,type='l',main="W281 rotation", cex=2, col="steelblue1", cex.main=2, cex.lab=2, cex.axis=2, ylab="rotation angle",xlab="Time (ns)",ylim=c(40,180),xaxt='n',lwd=2, yaxt="n")
lines(rot[,1],rot[,3]*180/3.14,col="steelblue2",xaxt='n',lwd=2)
lines(rot[,1],rot[,4]*180/3.14,col="steelblue3",xaxt='n',lwd=2)
lines(rot[,1],rot[,5]*180/3.14,col="steelblue4",xaxt='n',lwd=2)
axis(1,seq(0,nrow(rot),500),labels=seq(0,(nrow(rot)/2),250),cex.axis=2)
axis(2,seq(40,180,35),las=2,cex.axis=2)
legend("bottomleft",legend=c("chain A","chain B","chain C","chain D"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=2)


dev.off()
