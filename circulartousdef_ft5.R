rm(list=ls())
#' ---
#' title : Statistiques circulaires 
#' author : DY Ou?draogo / B. Angoboy Ilondea
#' documentclass: article
#' classoption: a4paper
#' ---
# packages pour generer le rapport
library(knitr)
library(xtable)
library(tidyverse)
opts_chunk$set(echo=FALSE, warning = F)
options(knitr.kable.NA = '-')
fullcompilation=T
########################################################################################################################################################
library(data.table)
library(circular)
#adapter sur le fichier 2 données phéno
setwd("C:/PROJECTS_R/ATLAS_LUKI")

phenoluki<- data.frame(read.csv2(file.choose(), h=TRUE, sep=";"))

phenoluki$def<-as.numeric(as.character(phenoluki$def))
phenoluki$flo<-as.numeric(as.character(phenoluki$flo))
phenoluki$fru<-as.numeric(as.character(phenoluki$fru))
phenoluki$diss<-as.numeric(as.character(phenoluki$diss))
################################## ANALYSES ##################################
s <- "Prioria balsamifera"
#s<-"Croton sylvaticus"

################################## Proportion d'individu dans la ph?nophase par mois
tmp <- phenoluki[phenoluki$sps == s,]
#tmp<-setDT(tmp)
tmp$yrmth <- paste(tmp$yr, tmp$mth, sep="-")

tmp <- tmp[!is.na(tmp$def),]

## individus mature$
## Nb d'evenements de defraison dans le mois
summary(tmp$def)
tmp$def<-as.numeric(as.character(tmp$def))

idmat <- unique(tmp[tmp$def == 1,]$id)
list_months_years<-unique(tmp[tmp$def == 1,]$yrmth)

tmp_filter<- tmp[tmp$id %in% idmat,]
tmpp<-aggregate(tmp_filter$def, by=list(yrmth=tmp_filter$yrmth, id=tmp_filter$id), FUN=sum, na.rm=TRUE)
colnames(tmpp)[3]<-"totdef"
tmpp[tmpp$totdef >0, ]$totdef <- 1

tmppp_1<-vector(length=length(list_months_years))
tmppp_2<-vector(length=length(list_months_years))
tmppp_3<-vector(length=length(list_months_years))
tmppp_4<-vector(length=length(list_months_years))
tmppp_5<-vector(length=length(list_months_years))
tmppp_6<-vector(length=length(list_months_years))
i<-1
invisible(sapply(
  list_months_years,
  function(x)
  {
    #print(x)
    tmpp_3<-tmpp[tmpp$yrmth==x,]
    tmppp_1[i]<<-as.character(x)
    totdef<-sum(tmpp_3$totdef)
    totN<-length(tmpp_3$totdef)
    tmppp_2[i]<<-as.numeric(totdef)
    tmppp_3[i]<<-as.numeric(totN)
    tmppp_4[i]<<-as.numeric(totdef/totN)
    #print(str_split(x, "-"))
    date_arr<-str_split(x, "-")
    tmppp_5[i]<<-as.character(date_arr[[1]][1])
    tmppp_6[i]<<-as.character(date_arr[[1]][2])
    
    i<<-i+1
  }
  
))
tmppp<-data.frame(tmppp_1,tmppp_2,tmppp_3, tmppp_4,tmppp_5, tmppp_6, stringsAsFactors =FALSE) 
colnames(tmppp)<-c("yrmth","totdef", "totN","propdef","yr","mth" )

## Graphique toute la periode
plot(tmppp$propdef,type="l")
par(mfrow=c(1,1), mar=c(6,4,2,0.5))
plot(tmppp$propdef,type="l", xaxt="n", xlab="")
axis(1, at=1:length(tmppp$yrmth), labels=tmppp$yrmth, las=2, cex.axis=0.8)

## Graphique mensuel une ligne par annee
windows()
levyr <- unique(tmppp$yr)
plot(c(1,12), c(0,0.6), cex=0, xlab="Month",xlim=c(1,12),ylab="Proportion of trees defoliating")
for(y in 1:12){
  points(tmppp[tmppp$yr == levyr[y], ]$propdef, type="l", col=y, lty=1:12)
}
axis(1, at=seq(1, 12, 1))
legend(1,0.5,levyr, lty=1, col=1:12,horiz=F)

## Graphique mensuel MOYENNE
par(mfrow=c(1,2), mar=c(4,3,2,0.5))
df_mean<-aggregate(tmppp$propdef, list(tmppp$mth), mean)

colnames(df_mean)<-c("mth", "propdef")
sapply(1:12,
       function(x)
       {
         print(x)
         if(!(x %in% df_mean$mth) )
         {
           print("add")
           df_mean<<-rbind(df_mean,c(x,0))
         }
       }
)
df_mean$mth<-as.numeric(as.character(df_mean$mth))
plot(df_mean, cex=1, 
     type="h", lwd=15,
     xlab="Month", ylab="Proportion of trees defoliating", main=paste(s,", N=", length(idmat), " 1947-57", sep=""))

################################## Stat circulaires

tmp <- phenoluki[phenoluki$sps == s,]

### statistiques pour chaque individu
def<-as.numeric
tmp$def<-as.numeric(tmp$def)


tmp <- tmp[!is.na(tmp$def)&!is.na(tmp$id)&tmp$def==1,]
#graphe moins significatif mais p_value
#tmp <- tmp[!is.na(tmp$def)&!is.na(tmp$id),]

idmat <- unique(tmp[tmp$def == 1,]$id)
#View(idmat)
## individus mature$
## Nb d'evenements de defraison dans le mois
summary(tmp$def)
tmp$def<-as.numeric(as.character(tmp$def))

rho_id_1<-aggregate(tmp$def, by=list(tmp$id, tmp$degree), FUN=sum)
colnames(rho_id_1)<-c("id","degree","totdef")
rho_id_2<-aggregate(tmp$degree, by=list(tmp$id), FUN=function(x) round(rho.circular(circular(x,type="angles", units="degrees", template = "none")),2))
colnames(rho_id_2)<-c("id","rdef")
rho_id_3<-aggregate(tmp$degree, by=list(tmp$id), FUN=function(x) round(rayleigh.test(circular(x,type="angles", units="degrees", template = "none"))$p,4))
colnames(rho_id_3)<-c("id","pdef")
rho_id_4<-aggregate(tmp$degree, by=list(tmp$id), FUN=function(x) format(as.Date(round(mean.circular(circular(tmp$degree,type="angles", units="degrees", template = "none"))[[1]]*(360/365)), format="%j", origin="01-01"), "%d-%b"))
colnames(rho_id_4)<-c("id","datedef")
merged_id<-merge(rho_id_1, rho_id_2)
merged_id<-merge(merged_id, rho_id_3)
merged_id<-merge(merged_id, rho_id_4)
### statistiques pour chaque ann?e

rho_yr_1<-aggregate(tmp$def, by=list(tmp$yr, tmp$degree), FUN=sum)
colnames(rho_yr_1)<-c("id","degree","totdef")
rho_yr_2<-aggregate(tmp$degree, by=list(tmp$yr), FUN=function(x) round(rho.circular(circular(x,type="angles", units="degrees", template = "none")),2))
colnames(rho_yr_2)<-c("id","rdef")
rho_yr_3<-aggregate(tmp$degree, by=list(tmp$yr), FUN=function(x) round(rayleigh.test(circular(x,type="angles", units="degrees", template = "none"))$p,4))
colnames(rho_yr_3)<-c("id","pdef")
rho_yr_4<-aggregate(tmp$degree, by=list(tmp$yr), FUN=function(x) format(as.Date(round(mean.circular(circular(tmp$degree,type="angles", units="degrees", template = "none"))[[1]]*(360/365)), format="%j", origin="01-01"), "%d-%b"))
colnames(rho_yr_4)<-c("id","datedef")
merged_yr<-merge(rho_yr_1, rho_yr_2)
merged_yr<-merge(merged_yr, rho_yr_3)
merged_yr<-merge(merged_yr, rho_yr_4)


###FILTRE SUR TMPPP A VOIR AVEC BHELY
#circdat <- circular(tmp[tmppp$yr == 1,]$degree, 
#  
#type="angles", units="degrees", template = "none")
circdat <- circular(merged_yr$degree, 
                    type="angles", units="degrees", template = "none")
summary(circdat)
# Rho (mean resultant length)
rho.circular(circdat)
# Rayleigh test of uniformity (significance of the mean resultant length)
rayleigh.test(circdat)

### date du pic
# en degree
mean.circular(circdat)
# converti en date
dir <- as.Date(round(mean.circular(circdat)[[1]]*(360/365)), format="%j", origin="01-01")



### Graphique (pas forc?ment n?cessaire...)
mth <- paste("1/",1:12,sep="")
mth <- as.Date(mth, format = "%d/%m")
mth <- as.numeric(format(mth, "%j"))
windows()

plot(circdat, axes=F, col=1, pch=16, cex=1.5, tol=0,
     main=paste("Def. ",format(dir, "%d-%b"),
                ", r=",round(rho.circular(circdat),2), 
                ", pval=",round(rayleigh.test(circdat)$p,4), sep=""))
arrows.circular(circular(mean.circular(circdat), type="angles", units="degrees", template = "none"), 
                shrink = rho.circular(circdat),
                length=0.2, lwd=3)
arrows.circular(circular(mth*(360/365), type="angles", units="degrees", template = "none"), length=0, col=8, lwd=0.8)
rose.diag(circdat, axes="F",bins=12,rscale=2,radii.scale = "linear",col="red",xlim = c(-1, 1), ylim = c(-1, 1),prop=3.7,units="degrees",cex=1.5,add=TRUE,template = "clock12")

########################################################################################################################################################

