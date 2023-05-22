#!/usr/bin/env Rscript 

#USAGE: R --vanilla --slave --args noHCND < Figure2-S2.R

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("1 argument must be supplied (system), 
       USAGE : R --vanilla --slave --args withHCND < Figure2-S2.R", call.=FALSE)
}

sys=args[1]
if (sys == "noHCND" | sys == "withHCND"){
 	print(paste("argument",args[1],"is ok"))
}else{
	stop("argument must be noHCND or withHCND",call.=FALSE)
}

path=paste("./data/",sys,"/",sep="")

#Reading electrostatic and hydrophobic interfaces data 
s4_s5_elec=read.table(paste(path,"s4_s5.elec",sep=""),header=F)
s4_s5_hph=read.table(paste(path,"s4_s5.hph",sep=""),header=F)
s4_s1_elec=read.table(paste(path,"s4_s1.elec",sep=""),header=F)
s4_s1_hph=read.table(paste(path,"s4_s1.hph",sep=""),header=F)


chain=c("A","B","C","D")

#Choice of system
if(sys=="noHCND"){
	fig="noHCND"
	}else{
	fig="withHCND"
}

#Making the figure
png(paste("Figure2-S2-",fig,".png",sep=""),width=1800, height=750, res=110)
par(mfrow = c(2,4))
par(mar=c(5.5,6,4,0.3))
for (i in 1:4) {
	plot(s4_s5_hph[which(s4_s5_hph[,2] == chain[i]),1]*5,
	s4_s5_hph[which(s4_s5_hph[,2] == chain[i]),3], type='l', col="snow4",pch=16,
	main=paste("S4-S5 interface",sep=""),ylab=expression(paste("CA (",ring(A)^2,")",sep="")),
	xlab="Time (ns)",ylim=c(0,170),cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
	lines(s4_s5_elec[which(s4_s5_elec[,2] == chain[i]),1]*5,pch=16,
        s4_s5_elec[which(s4_s5_elec[,2] == chain[i]),3], type='l', col="springgreen3",lwd=3, cex=2)
}
for (i in 1:4) {
	plot(s4_s1_hph[which(s4_s1_hph[,2] == chain[i]),1]*5,
        s4_s1_hph[which(s4_s1_hph[,2] == chain[i]),3], type='l', col="snow4",pch=16,
        main="S4-S1 interface",ylab=expression(paste("CA (",ring(A)^2,")",sep="")),
        xlab="Time (ns)",ylim=c(0,170),cex.main=2,cex.lab=2,cex=2,cex.axis=2,lwd=3)
        lines(s4_s1_elec[which(s4_s1_elec[,2] == chain[i]),1]*5,pch=16,
        s4_s1_elec[which(s4_s1_elec[,2] == chain[i]),3], type='l', col="springgreen3",lwd=3, cex=2)
	}	
dev.off()


