#!/usr/bin/env Rscript 

library(dplyr)

#path
rest="./data/Resting/"
act="./data/Activated/"
R1="./data/A1/"
R2="./data/A2/"

#reading the data
f_open = function(sys,ext,N) {
	a=read.table(paste(sys,ext,sep=""), header =F)
	res=paste(a[,3],a[,4],sep='')
	
	all=table(res)/N
	
	return(all)
}

rest_E=f_open(rest,"lip.elec",804)
act_E=f_open(act,"lip.elec",804)
r1=f_open(R1,"lip.elec",844)
r2=f_open(R2,"lip.elec",832)

rest_E2=f_open(rest,"lip2.elec",804)
act_E2=f_open(act,"lip2.elec",804)
r1_E2=f_open(R1,"lip2.elec",844)
r2_E2=f_open(R2,"lip2.elec",832)

fct_combine=function(type,sim){
	if (type == "elec"){
		mat=Reduce(function(...) merge(..., all=TRUE,by="res"), 
		list(rest_E,act_E,r1,r2))
	}
	else if (type == "hph"){
		mat=Reduce(function(...) merge(..., all=TRUE,by="res"),
                list(rest_E2,act_E2,r1_E2,r2_E2))
	}
	mat=mat[order(substr(mat[,1],4,10)),]
	mat2=mat[,2:ncol(mat)]
	rownames(mat2)=mat[,1]
	colnames(mat2)= c("REST","ACT","TA","CA")
	return(mat2)
}

elec=fct_combine("elec")
elec2=fct_combine("hph")

couleurs=c("blue2","red","purple","orchid")


#Making the Figure
png(paste('Figure5-A.png',sep=''), width=1800, height=1600,res=170)
layout(matrix(c(1, 1, 1, 1, 1, 1,
                2, 2, 2, 2, 2, 3), nrow=2, byrow=TRUE))
layout.show(n=3)
par(mar=c(9,6.9,3,1))

mp=barplot(t(as.matrix(elec2)),beside=T, col=couleurs,border=NA,cex.main=2.5,
xaxt='n',yaxt='n', ylim=c(0,1),main=paste("POPC headgroup contacts"))
axis(1, at=seq(mp[2,1]-0.5,mp[2,ncol(mp)]-0.5, mp[2,2]-mp[2,1]),las = 2,labels=rownames(elec2),cex.axis=2.5)
axis(2, at=seq(0,1,by=0.2), las = 2,cex.axis=2.5)
title(ylab=expression("Occurence"), cex.lab= 2.5, mgp=c(4.5,10.2,9.5))

mp=barplot(t(as.matrix(elec)),beside=T, col=couleurs,border=NA,cex.main=2.5,
xaxt='n',yaxt='n', ylim=c(0,1),main="")
axis(1, at=seq(mp[2,1]-0.5,mp[2,ncol(mp)]-0.5, mp[2,2]-mp[2,1]),las = 2,labels=rownames(elec),cex.axis=2.5)
axis(2, at=seq(0,1,by=0.2), las = 2,cex.axis=2.5)
title(ylab=expression("Occurence"), cex.lab= 2.5, mgp=c(4.5,10.2,9.5))

dev.off()



