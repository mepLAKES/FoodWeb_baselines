
# This script is used to run baselines prediction models for lotic and lentic sites
##load librairies
#libraries

library(dplyr)
library(mgcv)
library(gratia)
library(ggplot2)
library(grid)
library(ggpubr)
library(plotrix)
library(GGally)
library(gam.hp)

# LOTIC
##working dataset
load(file="DF_lotic_models.RData")
df<-DF_lotic_models
df2<-data.frame(FW_ID=df$FW_ID,
                Hab=df$Resource_habitat,Trophic=df$Resource_trophic_group,
                d15N=df$d15N_baseline,
                d13C=df$d13C_baseline,
                Lat=abs(df$Latitude),
                Lon=abs(df$Longitude),
                Ele=log10(df$ele_mt_cav+3.1),
                dis_m3_pyr=log10(df$dis_m3_pyr),
                ria_ha_csu=df$ria_ha_csu,
                tmp_dc_cyr=df$tmp_dc_cyr,
                pop_ct_csu=log10(df$pop_ct_csu+1),
                pre_mm_cyr=df$pre_mm_cyr,
                soc_th_uav=df$soc_th_uav,
                stra_uo=as.factor(df$ORD_STRA),
                hft=df$hft_ix_c09)
df<-df2


##Optimized GAMs
###GAMs with d15N as response variable
load("model_N_lotic.RData")
GAM_N_lotic<-GAM_sel
summary(GAM_N_lotic)
plot.check(GAM_N_lotic)

##GAMs with d13C as response variable
load("model_C_lotic.RData")
GAM_C_lotic<-GAM_sel
summary(GAM_C_lotic)
plot.check(GAM_C_lotic)


# LENTIC
##working dataset
load(file="DF_lentic_models.RData")
df<-DF_lentic_models
df2<-data.frame(FW_ID=df$FW_ID,
                Hab=df$Resource_habitat,Trophic=df$Resource_trophic_group,
                d15N=df$d15N_baseline,
                d13C=df$d13C_baseline,
                Lat=abs(df$Latitude),
                Lon=abs(df$Longitude),
                Ele=log10(df$Elevation+3.1),
                Lake_area=log10(df$Lake_area_km2),
                pop_ct_vsu=log10(df$pop_ct_vsu+1),
                Res_time=df$Res_time,
                tmp_dc_lyr=df$tmp_dc_lyr,
                pre_mm_lyr=log10(df$pre_mm_lyr+1),
                soc_th_uav=df$soc_th_uav,
                hft=df$hft_ix_v09)
df<-df2


##Optimized GAMs
###GAMs with d15N as response variable
load("model_N_lentic.RData")
GAM_N_lentic<-GAM_sel
summary(GAM_N_lentic)
plot.check(GAM_N_lentic)

##GAMs with d13C as response variable
load("model_C_lentic.RData")
GAM_C_lentic<-GAM_sel
summary(GAM_C_lentic)
plot.check(GAM_C_lentic)