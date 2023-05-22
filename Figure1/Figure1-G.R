#!/usr/bin/env Rscript 

#USAGE: R --vanilla --slave --args 281 < Figure1-G.R

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("1 argument must be supplied (res number), 
       USAGE : R --vanilla --slave --args 281 < Figure1-G.R", call.=FALSE)
}

num=args[1]


#Paths for data
rest="./data/resting/"
act="./data/activated/"
y289d="./data/intermediate/"

#funtion to open and treat the data for each system
f_open = function(sys,ext,N) {
	a=read.table(paste(sys,"r_",num,ext,sep=""), header =F)
	res=paste(a[,5],a[,6],sep='') 
	all=table(res)/N
	return(all)
}

#function to tranform the table to data.frame 
#and to sum all the lipid contacts with the residue of interest
lipcomb = function(mat) {
        mat2=as.data.frame(c(mat[which(substr(names(mat),1,3) != "POP" & mat > 0.05)],
        sum(mat[which(substr(names(mat),1,3) == "POP")])))
        rownames(mat2)[nrow(mat2)]="POP000"
        mat3=cbind(rownames(mat2),mat2)
        colnames(mat3)=c("res","CA")
        return (mat3)
}


#function to combine resting, activated and intermediate data in a unique table
fct_combine=function(type,sim){
        if (type == "elec"){
                mat=Reduce(function(...) merge(..., all=TRUE,by="res"),
                list(rest_E2,y289d_E2,act_E2))
        }
        else if (type == "hph"){
                mat=Reduce(function(...) merge(..., all=TRUE,by="res"),
                list(rest_H2,y289d_H2,act_H2))
        }
        mat=mat[order(substr(mat[,1],4,10)),]
        mat2=mat[,2:ncol(mat)]
        rownames(mat2)=mat[,1]
        colnames(mat2)= c("REST","Y289D","ACT")
        return(mat2)
}



#Reading data for electrostatic contacts with f_open, 
#for act and rest states 201 frames are used multiplied by 4 subunits
rest_E=f_open(rest,".elec",804)
act_E=f_open(act,".elec",804)
y289d_E=f_open(y289d,".elec",784)
#Reading data for hydrophobic contacts with f_open, 
#for act and rest states 201 frames are used multiplied by 4 subunits
rest_H=f_open(rest,".hph",804)
act_H=f_open(act,".hph",804)
y289d_H=f_open(y289d,".hph",784)



#Treatment of the data with lipcomb for hydrophobic contacts
rest_H2=lipcomb(rest_H)
act_H2=lipcomb(act_H)
y289d_H2=lipcomb(y289d_H)
#Treatment of the data with lipcomb for electrostatic contacts
rest_E2=lipcomb(rest_E)
act_E2=lipcomb(act_E)
y289d_E2=lipcomb(y289d_E)



#Treatment of the data with fct_combine 
#to get the final data.frame combining rest, act and intermediate states
#2 fnale data.frames are obtained for electrostatic and hydrophobic contacts
elec=fct_combine("elec")
hph=fct_combine("hph")

#Defining colors for the resting, activated and intermediate states in the final figure.
couleurs=c("blue2","yellow2","red")


# Making the final figure
png(paste('res',num,'.png',sep=''), width=1800, height=550,res=170)
par(mar=c(8.8,6.5,2,1))

layout(
  matrix(
    c(1,1,1,2), 
    nc=4, byrow = TRUE
  )
)

mp=barplot(t(as.matrix(hph)),beside=T, col=couleurs,border=NA,
xaxt='n',yaxt='n',  ylim=c(0,1), main="hydrophobic", cex.main=2.5)
axis(1, at=seq(mp[2,1],mp[2,ncol(mp)], mp[2,2]-mp[2,1]),las = 2,labels=rownames(hph),cex.axis=2.5)
axis(2, at=seq(0,1,by=0.2), las = 2,cex.axis=2.5)
title(ylab=expression("Occurence"), cex.lab=2.5, mgp=c(4.5,10.2,9))

mp=barplot(t(as.matrix(elec)),beside=T, col=couleurs,border=NA,
xaxt='n',yaxt='n',  ylim=c(0,1), main="electrostatic",cex.main=2.5)
axis(1, at=seq(mp[2,1],mp[2,ncol(mp)], mp[2,2]-mp[2,1]),las = 2,labels=rownames(elec),cex.axis=2.5)
axis(2, at=seq(0,1,by=0.2), las = 2,cex.axis=2.5)
title(ylab=expression("Occurence"), cex.lab=2.5, mgp=c(4.5,10.2,9))
dev.off()

