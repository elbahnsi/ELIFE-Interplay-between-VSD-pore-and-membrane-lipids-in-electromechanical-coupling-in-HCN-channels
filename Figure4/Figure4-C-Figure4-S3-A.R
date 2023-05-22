#!/usr/bin/env Rscript 

#USAGE: R --vanilla --slave --args 297 < Figure4-C-Figure4-S3-A.R

library(dplyr)

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("1 argument must be supplied (res number)", call.=FALSE)
}

num=args[1]

rest="./data/Resting/"
act="./data/Activated/"
abmd1="./data/A1/"
abmd2="./data/A2/"

f_open = function(sys,ext,N) {
	a=read.table(paste(sys,"r_",num,ext,sep=""), header =F)
	res=paste(a[,5],a[,6],sep='')
	
	all=table(res)/N
	
	return(all)
}

rest_E=f_open(rest,".elec",804)
act_E=f_open(act,".elec",804)
abmd1_E=f_open(abmd1,".elec",844)
abmd2_E=f_open(abmd2,".elec",832)

lipcomb = function(mat) {
        mat2=as.data.frame(c(mat[which(substr(names(mat),1,3) != "POP" & mat > 0.05)],
        sum(mat[which(substr(names(mat),1,3) == "POP")])))
        rownames(mat2)[nrow(mat2)]="POP000"
        mat3=cbind(rownames(mat2),mat2)
        colnames(mat3)=c("res","CA")
        return (mat3)
}


rest_E2=lipcomb(rest_E)
act_E2=lipcomb(act_E)
abmd1_E2=lipcomb(abmd1_E)
abmd2_E2=lipcomb(abmd2_E)


fct_combine=function(type,sim){
	if (type == "elec"){
		mat=Reduce(function(...) merge(..., all=TRUE,by="res"), 
		list(rest_E2,act_E2,abmd1_E2,abmd2_E2))
	}
	else if (type == "hph"){
		mat=Reduce(function(...) merge(..., all=TRUE,by="res"),
                list(rest_H2,act_H2,abmd1_H2,abmd2_H2))
	}
	mat=mat[order(substr(mat[,1],4,10)),]
	mat2=mat[,2:ncol(mat)]
	rownames(mat2)=mat[,1]
	colnames(mat2)= c("REST","ACT","TA","CA")
	return(mat2)
}

elec=fct_combine("elec")


couleurs=c("blue2","red","purple","orchid")

val=ncol(t(as.matrix(elec)))*80
#print(val)

#Making the Figure
png(paste('res',num,'.png',sep=''), width=val, height=600,res=100)
par(mfrow = c(1,1))
par(mar=c(7.5,5.5,2,1))

mp=barplot(t(as.matrix(elec)),beside=T, col=couleurs,border=NA,
xaxt='n',yaxt='n', ylim=c(0,1.2),main=paste("Polar contacts"), cex.main=2)
axis(1, at=seq(mp[2,1]+0.5,mp[2,ncol(mp)]+0.5, mp[2,2]-mp[2,1]),las = 2,labels=rownames(elec),cex.axis=2)
axis(2, at=seq(0,1,by=0.2), las = 2,cex.axis=2)
title(ylab=expression("Occurence"), cex.lab= 2, mgp=c(3.5,10.2,9))

dev.off()



