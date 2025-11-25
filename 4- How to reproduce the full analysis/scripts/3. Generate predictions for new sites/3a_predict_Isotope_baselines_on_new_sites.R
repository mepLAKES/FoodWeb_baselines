################################################################################
# Title:        Isotope baselines _ Generate predicted baselines for all sites
# Description:  This scripts uses data-driven models to generate isotope baselines for all sites
#               in the dataset, including those without isotope baseline measurements
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: dplyr, ggplot2, tidyr, mgcv
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages

library('dplyr')
library(mgcv)
library(rprojroot)
library(maps)
library(mapdata)
library(ggrepel)
library(grid)
library(gridExtra)


## Set the root directory for the project
root.dir = find_rstudio_root_file()
script.dir = paste0(root.dir,'/scripts')
models.dir = paste0(root.dir,'/models')
models.dir = paste0(root.dir,'/data')
setwd(root.dir)


## load datasets
load(file="data/processed/DF_lotic_new.RData")
load(file="data/processed/DF_lentic_new.RData")
DF_lotic_models<-DF_lotic_new
DF_lentic_models<-DF_lentic_new

## load models
load(file="models/model_C_lentic.RData")
load(file="models/model_N_lentic.RData")
load(file="models/model_C_lotic.RData")
load(file="models/model_N_lotic.RData")


################################################################################
# 3. GENERATE NEW ENV DATASETS AND PREDICTIONS ----------
################################################################################

## new lotic data
df1<-DF_lotic_models %>%
  filter (!is.na(ele_mt_cav) &
            !is.na(dis_m3_pyr) &
            !is.na(pop_ct_csu) &
            !is.na(ORD_STRA) &
            !is.na(hft_ix_c09) )%>%
  distinct() 

new_env_data_lotic<-rbind(df1,df1) %>%
  rename (hft=hft_ix_c09) %>%
  mutate (  Lat=abs(Latitude),
            Lon=abs(Longitude),
            Ele=log10(ele_mt_cav+3.1),
            dis_m3_pyr=log10(dis_m3_pyr),
            pop_ct_csu=log10(pop_ct_csu+1),
            stra_uo=as.factor(ORD_STRA))%>%
  tidyr::crossing(Hab = c("Pelagic", "Benthic"))

new_env_data_lotic$Trophic<-rep("Primary_consumer",nrow(new_env_data_lotic))
new_env_data_lotic$Ecosystem_Type<-rep("Lotic",nrow(new_env_data_lotic))

## lotic predictions

preds_C<-predict.gam(model_C_lotic, newdata=new_env_data_lotic, type="response", se.fit=TRUE)
preds_N<-predict.gam(model_N_lotic, newdata=new_env_data_lotic, type="response", se.fit=TRUE)

DF_lotic_preds<-new_env_data_lotic %>%
  select(FW_ID,Latitude, Longitude, Habitat=Hab, Trophic, Ecosystem_Type)%>%
  mutate(d13C_baseline=preds_C$fit,
         d13C_baseline_SE=preds_C$se.fit,
         d15N_baseline=preds_N$fit,
         d15N_baseline_SE=preds_N$se.fit)

## new lentic data
df2<-DF_lentic_models %>%
  filter (!is.na(Elevation) &
            !is.na(Lake_area) &
            !is.na(pop_ct_vsu) &
            !is.na(Res_time) &
            !is.na(hft_ix_v09) )%>%
  distinct()

new_env_data_lentic<-rbind(df2,df2) %>%
  rename (Res_time=Res_time,
          hft=hft_ix_v09) %>% 
  mutate(Lat=abs(Latitude),
         Lon=abs(Longitude),
         Ele=log10(Elevation+3.1),
         Lake_area=log10(Lake_area),
         pop_ct_vsu=log10(pop_ct_vsu+1),
         pre_mm_lyr=log10(pre_mm_lyr+1))%>%
  tidyr::crossing(Hab = c("Pelagic", "Benthic"))

new_env_data_lentic$Trophic<-rep("Primary_consumer",nrow(new_env_data_lentic))
new_env_data_lentic$Ecosystem_Type<-rep("Lentic",nrow(new_env_data_lentic))

## lentic predictions
preds_C<-predict.gam(model_C_lentic, newdata=new_env_data_lentic, type="response", se.fit=TRUE)
preds_N<-predict.gam(model_N_lentic, newdata=new_env_data_lentic, type="response", se.fit=TRUE)

DF_lentic_preds<-new_env_data_lentic %>%
  select(FW_ID,Latitude, Longitude, Habitat=Hab, Trophic, Ecosystem_Type)%>%
  mutate(d13C_baseline=preds_C$fit,
         d13C_baseline_SE=preds_C$se.fit,
         d15N_baseline=preds_N$fit,
         d15N_baseline_SE=preds_N$se.fit)

## join datasets
Isotope_baselines_predictions_all_sites<-rbind(DF_lotic_preds, DF_lentic_preds)
save(Isotope_baselines_predictions_all_sites, file="data/processed/Isotope_baselines_predictions_all_sites.RData")


