#!/usr/bin/env Rscript 

library(RColorBrewer)
library(ggplot2)

#USAGE: R --vanilla --slave --args noHCND < Figure2-BCEF.R

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("1 argument must be supplied (system), 
       USAGE : R --vanilla --slave --args withHCND < Figure2-BCEF.R", call.=FALSE)
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

#Choice of chain 
if(sys=="noHCND"){
	fig="EF"
	text="S4 Activation in absence of HCND"
	R267=R267A
	R270=R270A
	R273=R273A
	R276=R276A
	#bending vs rotation
        x=ben[,2]*180/3.14
        y=rot[,2]*180/3.14
	}else{
	fig="BC"
	text="S4 Activation in presence of HCND"
	R267=R267D
        R270=R270D
        R273=R273D
        R276=R276D
	#bending vs rotation
	x=ben[,5]*180/3.14
	y=rot[,5]*180/3.14
}


# Making the figure
png(paste("Figure2-",fig,".png",sep=""),width=1550, height=850, res=125)
par(mfrow=c(1,2))
par(mar=c(4,5,2,2))

plot(1:nrow(R267),R267[,1],type='l',main=text, 
cex=2, col="steelblue1", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
ylab=expression(paste("Z relative to F186 (",ring(A),")",sep="")),
xlab="Time (ns)",ylim=c(-20,20),xaxt='n',lwd=2)
lines(1:nrow(R270),R270[,1],col="steelblue2",xaxt='n',lwd=2)
lines(1:nrow(R273),R273[,1], col="steelblue3",xaxt='n',lwd=2)
lines (1:nrow(R276),R276[,1], col="steelblue4",xaxt='n',lwd=2)
abline (h=0,col="gray")
axis(1,seq(1,nrow(R267A),500),labels=seq(0,(nrow(R267A)-1)/2,250),cex.axis=1.5)
legend("topright",legend=c("R267","R270","R273","R276"),
col=c("steelblue1","steelblue2","steelblue3","steelblue4"),horiz=F,bty="n",pch=9,cex=1.5)

#bending vs rotation
dat <- data.frame(x,y)

rbPal <- colorRampPalette(c('violetred3','white'))

dat$Col <- rbPal(10)[as.numeric(cut(dat$y,breaks = 10))]

plot(dat$x,dat$y,type='p',pch=16,main="S4 conformational changes", 
cex=2, col=dat$Col, cex.main=1.5, cex.lab=1.5, cex.axis=1.5, 
xlab="S4 Bending angle",ylab="W281 rotation angle",xlim=c(0,80),ylim=c(180,20),lwd=2)
text(70,175,paste("R=",round(cor(x, y, method = c("pearson")),2),sep=""),cex=1.5)

dev.off()
