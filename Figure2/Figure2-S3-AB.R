#!/usr/bin/env Rscript 

library(RColorBrewer)
library(ggplot2)

#USAGE: R --vanilla --slave --args Activated S5 < Figure2-S3.R

args = commandArgs(trailingOnly=TRUE)

if (length(args)<2) {
  stop("2 argument must be supplied (system and helix), 
       USAGE : R --vanilla --slave --args Resting S4 < Figure2-S3.R", call.=FALSE)
}

sys=args[1]
if (sys == "Resting" | sys == "Activated"){
 	print(paste("argument",args[1],"is ok"))
}else{
	stop("argument must be Resting or Activated",call.=FALSE)
}

helix=args[2]
if (helix == "S4" | helix == "S5"){
        print(paste("argument",args[2],"is ok"))
}else{
        stop("argument 2 must be S4 or S5",call.=FALSE)
}

path=paste("./data/",sys,"/",helix,"/",sep="")


#Reading the data
aahph=read.table(paste(path,"aa.hph",sep=""),header=F)
aaelec=read.table(paste(path,"aa.elec",sep=""),header=F)
watelec=read.table(paste(path,"wat.elec",sep=""),header=F)
liphph=read.table(paste(path,"lip.hph",sep=""),header=F)
lipelec=read.table(paste(path,"lip.elec",sep=""),header=F)

#functions
moy= function(vec){
	vec2=sum(vec)/(nrow(aahph)/52)
#	print(length(vec))
	return (vec2)
}

moditab = function(mat){
	mat2=data.frame(cbind(paste(paste(mat[,2],mat[,3],sep="-"),
	mat[,4],sep=""),mat[,5]))
	
	mat3=aggregate(as.numeric(as.character(mat2[,2])),
	by=list(mat2[,1]),FUN=moy)
	
	colnames(mat3)=c("ID","MeanCA")
	return (mat3)
}

sdtab = function(mat){
        mat2=data.frame(cbind(paste(paste(mat[,2],mat[,3],sep="-"),
        mat[,4],sep=""),mat[,5]))

        mat3=aggregate(as.numeric(as.character(mat2[,2])),
        by=list(mat2[,1]),FUN=sd)

        colnames(mat3)=c("ID","SdCA")
        return (mat3)
}

#calculating the averaged CA using moditab
ahp=moditab(aahph)
ael=moditab(aaelec)
lip=moditab(liphph)
wat=moditab(watelec)
lec=moditab(lipelec)

#calculating the standard deviation using sdtab #NOT USED
sahp=sdtab(aahph)
sael=sdtab(aaelec)
slip=sdtab(liphph)
swat=sdtab(watelec)
slec=sdtab(lipelec)


#MERGING DATA
merge.all = function(x, y) {
    merge(x, y,all=T, by="ID")

}
output = Reduce(merge.all, list(ahp,ael,lip,lec,wat))
colnames(output)=c("ID","AAH","AAE","LIP","LEC","WAT")

soutput = Reduce(merge.all, list(sahp,sael,slip,slec,swat))
colnames(soutput)=c("ID","AAH","AAE","LIP","LEC","WAT")

rownames(output)=output[,1]
output=output[order(substr(output[,1],6,10)),2:6]

output[is.na(output)] <- 0

rownames(soutput)=soutput[,1]
soutput=soutput[order(substr(soutput[,1],6,10)),2:6]
soutput[is.na(soutput)] <- 0
###############################################################

Bar=as.matrix(t(output))
Err=as.matrix(t(soutput))

#Making the plot
png(paste("Figure2-S3-",sys,helix,".png",sep=""),width=1700,height=900,res=170)
par(mar=c(9,7.9,2,1))
	mp=barplot(Bar,border=F,ylim=c(0,150),xaxt="n",main="S4",cex.main=2.5,
	ylab="",yaxt="n",#beside=T, 
	col=c("snow2","springgreen3","snow4","yellowgreen","turquoise3"),cex.lab=2.5,cex.axis=2.5)
	axis(2,seq(0,150,50),cex.axis=2.5,las=2)
        axis(1,mp[seq(2,length(mp),by=4)]+0.6,labels=substr(rownames(output)[seq(1,nrow(output),by=4)],3,10),las=2,cex.axis=2.5) 
	title(ylab=expression(paste("CAav (",ring(A)^2,")",sep="")), cex.lab= 2.5, mgp=c(4.5,10.2,9))
	dev.off()






