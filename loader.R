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
opts_chunk$set(echo=FALSE, warning = F)
options(knitr.kable.NA = '-')
fullcompilation=T
########################################################################################################################################################
library(data.table)
library(circular)
#adapter sur le fichier 2 données phéno
setwd("C:/PROJECTS_R/ATLAS_LUKI")
######################################
# Luki 
######################################


luki <- data.table(read.csv2(file.choose(), h=TRUE, sep="\t"))


### Renommer les colonnes
names(luki)
colnames(luki)[1] <- "TreeID"
colnames(luki)[2] <- "vernacular"
colnames(luki)[3] <- "sps"
colnames(luki)[c(5,6,7)] <- c("startyear","pheno","1947Jan1")

census_name<-paste("y",rep(1947:1958, each=36), "m",
                   rep(paste(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"), each=3),c(5,15,25),sep="d"), 12),
                   sep="")
colnames(luki)[7:438] <- census_name



unique(luki$sps)


#### type de phenophase
unique(luki$pheno)
# [1] "FL"   "FR"   "DISS" "HIV"  "DEF" 
luki[luki$pheno == "HIV", ]$pheno <- "DEF"

luki <- data.frame(luki)
#View(luki)
#View(luki$sps)
## transformation du tableau de donnees
phenoluki_save <- NULL
rg_tmp<-seq(7,length(luki))
rg_tmp2<-seq(1,length(census_name))
print(rg_tmp)
print(Sys.time())
#invisible pour supprimer l’affichage dans console

size_list<-length(unique(luki$TreeID))*length(rg_tmp2)
v_sps<-vector(length=size_list)
v_ids<-vector(length=size_list)
v_def<-vector(length=size_list)
v_flo<-vector(length=size_list)
v_fru<-vector(length=size_list)
v_diss<-vector(length=size_list)
v_census<-vector(length=size_list)
i<-1
invisible(lapply(unique(luki$TreeID),
                 function(x)
                 {
                   #print(x)
                   filter_id<-which(luki$TreeID==x)
                   filter_luki<-luki[filter_id,]
                   current_species<-as.character(filter_luki$sps[1])
                   filter_def<-as.numeric(which(filter_luki$pheno=="DEF"))
                   filter_fl<-as.numeric(which(filter_luki$pheno=="FL"))
                   filter_fr<-as.numeric(which(filter_luki$pheno=="FR"))
                   filter_diss<-as.numeric(which(filter_luki$pheno=="DISS"))
                   #print(current_species)
                   #View(filter_luki)
                   lapply(rg_tmp2,
                          function(y)
                          {
                            
                            #print(y)
                            census_val=census_name[y]
                            #print(census_val)
                            v_ids[i]<<-as.character(x)
                            #print(current_species)
                            v_sps[i]<<-as.character(current_species)
                            v_def[i]<<-as.character(filter_luki[filter_def[1],y+6])
                            v_flo[i]<<-as.character(filter_luki[filter_fl[1],y+6])
                            v_fru[i]<<-as.character(filter_luki[filter_fr[1],y+6])
                            v_diss[i]<<-as.character(filter_luki[filter_diss[1],y+6])
                            v_census[i]<<-census_val
                            i<<-i+1
                            
                          }
                   )
                 }
))
phenoluki_save<-data.frame(v_sps,v_ids,v_def,v_flo,v_fru,v_diss,v_census,stringsAsFactors =FALSE) 
colnames(phenoluki_save)<-c("sps", "id","def", "flo", "fru", "diss", "census")
print(Sys.time())
View(phenoluki_save)

i<-1


phenoluki_save$date <- as.Date(phenoluki_save$census, format="y%Ym%md%e") # d=jour01-31 ou e=jour 1-31
phenoluki_save$jourjul <- as.numeric(format(phenoluki_save$date, "%j"))
phenoluki_save$degree <- phenoluki_save$jourjul*(360/365)

phenoluki_save$mth <- format(phenoluki_save$date, "%m")
phenoluki_save$yr <- format(phenoluki_save$date, "%Y")
write.csv2(phenoluki_save,file=file.choose())

