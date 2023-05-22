#!/usr/bin/env Rscript 

#SCRIPT FOR making Figures 2I and Supplementary figure 3

library(dplyr)

#path for data
rest="./data/Resting/lipids/"
act="./data/Activated/lipids/"

#functions for reading data and calculating average and standard deviation tables
averag = function(sys,ext,N) {
	a=read.table(paste(sys,ext,sep=""), header =F)
	res=paste(a[,3],a[,4],sep='')
	mat=cbind(res,as.numeric(as.character(a[,5])))
	colnames(mat)=c("RES","CA")

	all=xtabs(as.numeric(as.character(CA))~RES,mat)/N

	return(all)
}

sd2 =  function(sys,ext,N) {
        a=read.table(paste(sys,ext,sep=""), header =F)
        res=paste(a[,3],a[,4],sep='')
        mat=as.data.frame(cbind(res,as.numeric(as.character(a[,5]))))
        colnames(mat)=c("RES","CA")

	meanx=xtabs(as.numeric(as.character(CA))~RES,mat)/N
	meanx2=as.vector(meanx)
	names(meanx2)=names(meanx)
	all=NULL
	for(i in names(meanx)){
		x=as.numeric(mat[mat[,1]==i,2])
		xm=meanx2[i]
		deviations = x - xm
		s <- deviations^2
		m <- sum(s)/n
		sd <- sqrt(m)
		all=c(all,sd)
	}
	all=as.data.frame(cbind(names(meanx),as.numeric(all)))
	colnames(all)=c("RES","SD")
        return(all)
}


n=804
rest_mean=averag(rest,"lip.hph",n)
act_mean=averag(act,"lip.hph",n)

act_sd=sd2(act,"lip.hph",n)
rest_sd=sd2(rest,"lip.hph",n)

#merging systems (resting and activated)
#average table
mean_comb=Reduce(function(...) merge(..., all=TRUE,by="RES"),
                list(rest_mean,act_mean))
mean_comb=mean_comb[order(substr(mean_comb[,1],4,10)),]
MEAN1=mean_comb[,2:3]
rownames(MEAN1)=mean_comb[,1]
colnames(MEAN1)=c("REST","ACT")
class(MEAN1[,1])="numeric"
class(MEAN1[,2])="numeric"

#standard deviation table
sd_comb=Reduce(function(...) merge(..., all=TRUE,by="RES"),
                list(rest_sd,act_sd))
sd_comb=sd_comb[order(substr(sd_comb[,1],4,10)),]
SD1=sd_comb[,2:3]
rownames(SD1)=sd_comb[,1]
colnames(SD1)=c("REST","ACT")
class(SD1[,1])="numeric"
class(SD1[,2])="numeric"

#subset of residues extracted from the whole table for Figure2-I.png
res=c("TYR277","ILE278","TRP281","LEU291",
"ALA292","VAL295","VAL296","ARG297","MET304")
#average table
MEAN2=mean_comb[which(mean_comb[,1] %in% res),2:3]
row.names(MEAN2)=res
colnames(MEAN2)=c("REST","ACT")
class(MEAN2[,1])="numeric"
class(MEAN2[,2])="numeric"
#standard deviation table
SD2=sd_comb[which(sd_comb[,1]%in%res),2:3]
row.names(SD2)=res
colnames(SD2)=c("REST","ACT")
class(SD2[,1])="numeric"
class(SD2[,2])="numeric"


#colors for systems
couleurs=c("blue2","red")

#function for plotting standard deviations
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=0, length=length, col=couleurs, ...)
}

#Making the figures
png(paste('Figure2-S2-Figure2-I.png',sep=''), width=1800, height=1300,res=170)
layout(matrix(c(1, 1, 1, 1, 1, 1,
                2, 2, 3, 4, 5, 6), nrow=2, byrow=TRUE))
layout.show(n=6)
par(mar=c(9,6.9,3,1))
mp=barplot(t(as.matrix(MEAN1)),beside=T, col=couleurs,border=NA,cex.main=2.5,
xaxt='n',yaxt='n', ylim=c(0,100),main=paste("POPC tail contacts"))
axis(1, at=seq(mp[2,1]-0.5,mp[2,ncol(mp)]-0.5, mp[2,2]-mp[2,1]),las = 2,labels=rownames(MEAN1),cex.axis=2.5)
axis(2, at=seq(0,100,by=50), las = 2,cex.axis=2.5)
title(ylab=expression(paste("CAav (",ring(A)^2,")",sep="")), cex.lab= 2.5, mgp=c(3.5,10.2,9))
error.bar(mp,t(as.matrix(MEAN1)),t(as.matrix(SD1)) )

mp=barplot(t(as.matrix(MEAN2)),beside=T, col=couleurs,border=NA, cex.main=2.5,
xaxt='n',yaxt='n', ylim=c(0,80),main=paste("POPC tail contacts"))
axis(1, at=seq(mp[2,1]-0.5,mp[2,ncol(mp)]-0.5, mp[2,2]-mp[2,1]),las = 2,labels=rownames(MEAN2),cex.axis=2.5)
axis(2, at=seq(0,80,by=40), las = 2,cex.axis=2.5)
title(ylab=expression(paste("CAav (",ring(A)^2,")",sep="")), cex.lab= 2.5, mgp=c(3.5,10.2,9))
error.bar(mp,t(as.matrix(MEAN2)),t(as.matrix(SD2)) )
dev.off()



