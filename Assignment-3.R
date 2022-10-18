#Q1
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
#Q2
bio <- read.csv(file='inchBio.csv')
#Q3
print(head(bio))
print(tail(bio))
print(str(bio))
#Q4
counts<-count(bio,vars="species")
#Q5
print(counts$species)
#Q6
tmp<-counts
print(tmp)
#Q7
tmp2<-bio$species
print(head(tmp2,5))
#Q8
w<-table(tmp2)
print(class(w))
#Q9
t<-as.data.frame(w)
#Q10
print(t$Freq)
#Q11
cSpec<-table(bio$species)
print(cSpec)
#Q12
cSpecPct<-prop.table(cSpec)
#Q13
u<-as.data.frame(cSpecPct)
print(is.data.frame(u))
print(class(u))
#Q14
barplot(cSpec,main="Fish Count",horiz=TRUE,ylab="COUNTS",col="chartreuse",cex.axis=0.6,las=1)
#Q15
barplot(cSpecPct,ylim=c(0,4),main="Fish Relative Frequency",col.axis="lightblue")
#Q16
d<-u[order(-u$Freq),]
#Q17
names(d)[names(d) == "Var1"] <- "Species"
names(d)[names(d) == "Freq"] <- "RelFreq"
#Q18
d<-cbind(d,cumfreq=cumsum(d$RelFreq))
d<-cbind(d,counts=d$RelFreq*length(bio$species))
d<-cbind(d,cumcounts=cumsum(d$counts))
#Q19
def_par<-names(d)
#Q20
pc=barplot(d$counts,main="Species Pareto", width=1,
           space=0.15,border=NA,ylim=c(0,3.05*max(d$counts)), na.rm= TRUE, axes= FALSE,
           ylab="Cummulative counts", names.arg = d$Species, cex.names=0.7,las=2)
#Q21
points(d$cumcounts,pch=19,type="b",col="cyan4")
box(which="plot",lty="solid",col="grey")
axis(side = 2, at=d$cumcounts,las=2, col="grey62",cex.axis=0.8)
axis(side=4, at=c(d$cumcounts),col="cyan4",
     col.axis="cyan4",cex.axis=0.8,label=formatC(d$RealFreq*100))
text(4,500,labels="Mandumula")
#Q22
#Q23
#Q24
#Q25
