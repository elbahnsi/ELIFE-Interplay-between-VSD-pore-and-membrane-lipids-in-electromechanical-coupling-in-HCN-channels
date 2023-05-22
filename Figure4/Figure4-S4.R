#!/usr/bin/env Rscript 
library(dplyr)

#path
rest="./data/Resting/"
act="./data/Activated/"
abmd2="./data/A2/"
abmd1="./data/A1/"

sys=c(act,rest,abmd1,abmd2)
names(sys)=c("A","R","A1","A2")
for (i in 1:4){
	assign(paste(names(sys)[i],"s6_s6_elec",sep=""),read.table(paste(sys[i],"s6_s6.elec",sep=""),header=F))
	assign(paste(names(sys)[i],"s6_s6_hph",sep=""),read.table(paste(sys[i],"s6_s6.hph",sep=""),header=F))
	assign(paste(names(sys)[i],"s6_s5_elec",sep=""),read.table(paste(sys[i],"s6_s5.elec",sep=""),header=F))
	assign(paste(names(sys)[i],"s6_s5_hph",sep=""),read.table(paste(sys[i],"s6_s5.hph",sep=""),header=F))
}

chainS6=c(c("A","B"),c("A","D"),c("B","C"),c("C","D"))
chainS5=c(c("A","A"),c("B","B"),c("C","C"),c("D","D"))


legtext <- c("REST","ACT","A1","A2")
xcoords <- c(0, 10, 30)
secondvector <- (1:length(legtext))-1
textwidths <- c(37,40,45)
colo=c("blue2","red","purple","orchid")

S5mean=NULL
S5sd=NULL
S6mean=NULL
S6sd=NULL
for (i in seq(1,8,2)) {
	S5tab1=Rs6_s5_hph[which(Rs6_s5_hph[,2] == chainS5[i] & Rs6_s5_hph[,3] == chainS5[i+1] ),4]
	S5tab2=As6_s5_hph[which(As6_s5_hph[,2] == chainS5[i] & As6_s5_hph[,3] == chainS5[i+1] ),4]
	S5tab3=A1s6_s5_hph[which(A1s6_s5_hph[,2] == chainS5[i] & A1s6_s5_hph[,3] == chainS5[i+1] ),4]
	S5tab4=A2s6_s5_hph[which(A2s6_s5_hph[,2] == chainS5[i] & A2s6_s5_hph[,3] == chainS5[i+1] ),4]
	S5mean=rbind(S5mean,c(mean(S5tab1),mean(S5tab2),mean(S5tab3),mean(S5tab4)))
	S5sd=rbind(S5sd,c(sd(S5tab1),sd(S5tab2),sd(S5tab3),sd(S5tab4)))

	S6tab1=Rs6_s6_hph[which(Rs6_s6_hph[,2] == chainS6[i] & Rs6_s6_hph[,3] == chainS6[i+1]),4]
	S6tab2=As6_s6_hph[which(As6_s6_hph[,2] == chainS6[i] & As6_s6_hph[,3] == chainS6[i+1]),4]
	S6tab3=A1s6_s6_hph[which(A1s6_s6_hph[,2] == chainS6[i] & A1s6_s6_hph[,3] == chainS6[i+1]),4]
	S6tab4=A2s6_s6_hph[which(A2s6_s6_hph[,2] == chainS6[i] & A2s6_s6_hph[,3] == chainS6[i+1]),4]
	S6mean=rbind(S6mean,c(mean(S6tab1),mean(S6tab2),mean(S6tab3),mean(S6tab4)))
        S6sd=rbind(S6sd,c(sd(S6tab1),sd(S6tab2),sd(S6tab3),sd(S6tab4)))
}

#Making the figure
png('Figure4-S4.png', width=1600, height=350,res=100)
par(mfrow = c(1,2))
par(mar=c(6,7.5,2,1))
mp=barplot(t(S5mean),beside=T, col=colo,border=NA,
xaxt='n',yaxt='n', ylim=c(0,250),main=paste("S5-S6 interface"),cex.main=2)
segments(mp, t(S5mean) - t(S5sd), mp, t(S5mean) + t(S5sd), lwd = 2)
arrows(mp, t(S5mean) - t(S5sd), mp,
       t(S5mean) + t(S5sd), lwd = 2, angle = 90, code = 0, length = 0.05,col=colo)
axis(1, at=as.vector(mp[,1:4]),las = 2,labels=rep(legtext,4),cex.axis=2)
axis(2, at=seq(0,250,by=50), las = 2,cex.axis=2)
title(ylab=expression(paste("CAav (",ring(A)^2,")",sep="")), cex.lab=2, mgp=c(4.5,9.2,9))

mp=barplot(t(S6mean),beside=T, col=colo,border=NA,
xaxt='n',yaxt='n', ylim=c(0,200),main=paste("S6-S6 interface"),cex.main=2)
segments(mp, t(S6mean) - t(S6sd), mp, t(S6mean) + t(S6sd), lwd = 2)
arrows(mp, t(S6mean) - t(S6sd), mp,
       t(S6mean) + t(S6sd), lwd = 2, angle = 90, code = 0, length = 0.05,col=colo)
axis(1, at=as.vector(mp[,1:4]),las = 2,labels=rep(legtext,4),cex.axis=2)
axis(2, at=seq(0,200,by=50), las = 2,cex.axis=2)
title(ylab=expression(paste("CAav (",ring(A)^2,")",sep="")), cex.lab=2, mgp=c(4.5,9.2,9))

dev.off()


