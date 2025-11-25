################################################################################
# Title:        Isotope baselines _ Computing trophic metrics at the species level 
# Description:  This scripts computes trophic metrics (Layman and no-layman's) at the species and community level from original and simulated baselines
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: dplyr, ggplot2, tidyr, SIBER
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages

library('dplyr')
library(mgcv)
library(gratia)
library(ggplot2)
library(grid)
library(ggpubr)
library(rprojroot)
library(SIBER)

## Set the root directory for the project
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'scripts')
figures.dir = paste0(root.dir,'/figures')
models.dir = paste0(root.dir,'/models')
setwd(root.dir)


## load datasets
load("data/processed/full_datasets.RData")

################################################################################
# 2. COMPUTE METRICS AT THE SPECIES LEVEL ----------
################################################################################

df<-full_datasets
## Correcting for the baseline effect -----------

### for 15N, substracting the mean of the baseline d15N values 
df$mean_d15N_baseline<-(df$d15N_benthic+df$d15N_pelagic)/2
df$d15N_cor<-df$d15N-df$mean_d15N_baseline

### for 13C, Olson et al's correction (2009) 
df$range_d13C_baseline<-abs(df$d13C_benthic-df$d13C_pelagic)
df$mean_d13C_baseline<-(df$d13C_benthic+df$d13C_pelagic)/2
df$d13C_cor<-(df$d13C-df$mean_d13C_baseline)/df$range_d13C_baseline

## Computing non_layman's metrics ----------
### Dealing with the  trophic fractionation factor-----
###set fixed and them randomly picked within a SD from Post
df$TF_13C_fixed<-rep(0.5,nrow(df))
df$TF_15N_fixed<-rep(3.4,nrow(df))
df$TF_13C_var<-rnorm(nrow(df), mean=0.5, sd=1.3)   
df$TF_15N_var<-rnorm(nrow(df), mean=3.4, sd=1.0)   

### alpha_benthic (benthic contribution) ------
df$alpha_benthic_fixed<-(df$d13C-df$TF_13C_fixed-df$d13C_benthic)/(df$d13C_pelagic-df$d13C_benthic)
df$alpha_benthic_var<-(df$d13C-df$TF_13C_var-df$d13C_benthic)/(df$d13C_pelagic-df$d13C_benthic)

### unweighted trophic position ------
df$unw_TP_fixed<-2+df$d15N_cor/df$TF_15N_fixed
df$unw_TP_var<-2+df$d15N_cor/df$TF_15N_var

### weighted trophic position --------
df$w_TP_fixed<-2+(df$d15N-df$d15N_pelagic-df$alpha_benthic_fixed*(df$d15N_benthic-df$d15N_pelagic))/df$TF_15N_fixed
df$w_TP_var<-2+(df$d15N-df$d15N_pelagic-df$alpha_benthic_var*(df$d15N_benthic-df$d15N_pelagic))/df$TF_15N_var

### Gathering in a final table

df <- df %>%
  mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))
df <- df %>%
  mutate(across(everything(), ~ ifelse(. == Inf, NA, .)))


species_dataset_with_metrics<-df
save(species_dataset_with_metrics, file="data/processed/species_dataset_with_metrics.RData")
dim(species_dataset_with_metrics)

################################################################################
# 3. COMPUTE METRICS AT THE COMMUNITY LEVEL ----------
################################################################################
 ## Load the dataset and clean up NAs
load(file="data/processed/species_dataset_with_metrics.RData")

df<-species_dataset_with_metrics
Nas<-which(is.na(df$dataset))
df <- df %>%
  mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))
df <- df %>%
  mutate(across(everything(), ~ ifelse(. == Inf, NA, .)))

## Compute the no-Layman's metrics at the community level ------
 ### FCL = Food Chain Length
dFCL<-aggregate(df$unw_TP_fixed, by=list(df$dataset,df$FW_ID,df$Ecosystem_Type), max,na.rm=TRUE)
colnames(dFCL)<-c("dataset","FW_ID","Ecosystem_Type","FCL")

 ### TPmean = Mean trophic position
dTPmean<-aggregate(df$unw_TP_fixed, by=list(df$dataset,df$FW_ID,df$Ecosystem_Type), mean,na.rm=TRUE)
colnames(dTPmean)<-c("dataset","FW_ID","Ecosystem_Type","TPmean")

### Corrected d13C max
dd13Ccormax<-aggregate(df$d13C_cor, by=list(df$dataset,df$FW_ID,df$Ecosystem_Type), max,na.rm=TRUE)
colnames(dd13Ccormax)<-c("dataset","FW_ID","Ecosystem_Type","d13C_cor_max")

### Final data_frame
dfNL<-merge(dFCL,dTPmean,by=c("dataset","FW_ID","Ecosystem_Type"))
dfNL<-merge(dfNL,dd13Ccormax,by=c("dataset","FW_ID","Ecosystem_Type"))
dfNL$ID<-paste(dfNL$dataset,dfNL$FW_ID,sep="_")



## Compute the layman's metrics ----
df<-species_dataset_with_metrics
df$ID<-paste(df$dataset,df$FW_ID,sep="_")
l<-levels(as.factor(df$ID))
n<-length(l)

dfL<-data.frame(ID=l,dY_range=rep(NA,n),dX_range=rep(NA,n),CD=rep(NA,n),NND=rep(NA,n),SDNND=rep(NA,n),TA=rep(NA,n),SEA=rep(NA,n),SEAc=rep(NA,n))


for (i in 1:n)
{
  group<-df$ID[df$ID==l[i]]
  community=rep(1,length(group))
  dfi<-df[df$ID==l[i],]
  x<-dfi$d13C_cor
  y<-dfi$d15N_cor
  mydata<-data.frame(iso1=x,iso2=y,group=group,community=community)
  U<-try(laymanMetrics(x,y),silent=TRUE)
  siber.met <- try(createSiberObject(mydata),silent=TRUE)
  X<-try(groupMetricsML(siber.met),silent=TRUE)
  dfL[i,2]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[1]))
  dfL[i,3]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[2]))
  dfL[i,4]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[4]))
  dfL[i,5]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[5]))
  dfL[i,6]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[6]))
  dfL[i,7]<-ifelse(class(U)=="try-error",NA,as.vector(U$metrics[3]))
  dfL[i,8]<-ifelse(class(U)=="try-error",NA,as.vector(X[2]))
  dfL[i,9]<-ifelse(class(U)=="try-error",NA,as.vector(X[3]))
  
  
}


dff<-merge(dfNL,dfL,by="ID")
dim(dff)



## we remove the outliers (The isotope biplot is not within the range of d13C baselines)
###  outliers when max d13Ccor >2
pbl<-which(abs(dff$d13C_cor_max)>=1.5)
length(pbl)
dff<-dff[-pbl,]

### replace -Inf values by NA
dff <- dff %>%
  mutate(across(everything(), ~ ifelse(. == -Inf, NA, .)))
dff <- dff %>%
  mutate(across(everything(), ~ ifelse(. == Inf, NA, .)))


dataset_with_metrics_community<-dff
save(dataset_with_metrics_community,file="data/processed/dataset_with_metrics_community.RData")



