################################################################################
# Title:        Isotope baselines _ definition and Goodness of fit of GAM for prediction 
# Description:  This scripts provides GAM for both the 13C and 15N baselines for lentic and lotic env
# Date:         2025-05-05
# Version:      6.0
# Notes:        Any additional information or context
# Dependencies: dplyr, ggplot2, tidyr, mgcv,gratia, grid, ggpubr, plotrix, GGally, gam.hp
################################################################################


################################################################################
# 1. SETUP ----------
################################################################################
## Clear environment
rm(list = ls())

## Load required packages
required.packages<-c("dplyr","ggplot2","tidyr","readr","mgcv","gratia","grid","ggpubr","plotrix","GGally","gam.hp","rprojroot","writexl")
install.packages(setdiff(required.packages, installed.packages()[,1]))


library('dplyr')
library(mgcv)
library(gratia)
library(ggplot2)
library(grid)
library(ggpubr)
library(plotrix)
library(GGally)
library(gam.hp)
library(rprojroot)
library("writexl")

# Set the root directory for the project
root.dir = find_rstudio_root_file()
data.dir = paste0(root.dir,'/data')
script.dir = paste0(root.dir,'/scripts')
figures.dir = paste0(root.dir,'/figures')

setwd(root.dir)
# Load required functions
source(paste0(script.dir,"/1. Generate_prediction_GAM/goodness_of_fit_gam_function.R"))

################################################################################
# 2. DATA PREPARATION AND NORMALIZATION -----
################################################################################

## Load data --------
### environmental predictors
load(file=paste0(data.dir,"/raw/DF_lentic_models.RData"))
load(file=paste0(data.dir,"/raw/DF_lotic_models.RData"))

# Normalize predictors
DF_models_lentic_transformed<-DF_lentic_models %>%
  rename (Hab=Resource_habitat,
          Trophic=Resource_trophic_group,
          d15N=d15N_baseline,
          d13C=d13C_baseline,
          Res_time=Res_time,
          hft=hft_ix_v09) %>% 
  mutate(Lat=abs(Latitude),
         Lon=abs(Longitude),
         Ele=log10(Elevation+3.1),
         Lake_area=log10(Lake_area_km2),
         pop_ct_vsu=log10(pop_ct_vsu+1),
         pre_mm_lyr=log10(pre_mm_lyr+1))

DF_models_lotic_transformed<-DF_lotic_models %>%
  rename (Hab=Resource_habitat,
          Trophic=Resource_trophic_group,
          d15N=d15N_baseline,
          d13C=d13C_baseline,
          hft=hft_ix_c09) %>%
  mutate (  Lat=abs(Latitude),
            Lon=abs(Longitude),
            Ele=log10(ele_mt_cav+3.1),
            dis_m3_pyr=log10(dis_m3_pyr),
            pop_ct_csu=log10(pop_ct_csu+1),
            stra_uo=as.factor(ORD_STRA))

################################################################################
# 3. GAM MODELING for isotope baselines in Lentic systems -------
################################################################################
## Separate in training and test datasets (80% training, 20% test) -----

df<-DF_models_lentic_transformed
set.seed(124)

Train<-sample(nrow(df),round(0.8*nrow(df)))
test_df<-df[-Train,]
train_df<-df[Train,]

################################################################################

## Best model for 13C -----
model_C_lentic <- gam(d13C ~ s(Lat,k=20,by=as.factor(Hab)) +
                       s(Lon,k=20)+
                       s(Lake_area,k=20,by=as.factor(Hab))+
                       Hab+Trophic,
                     data=train_df, method="REML", family="gaussian",select = TRUE)

### NOT RUN Check model for concurvity and residuals
#par(mfrow=c(2,2))
#gam.check(model_C_lentic)
#plot(model_C_lentic,pages=1,residuals=TRUE)
#concurvity(model_C_lentic, full = FALSE)



### summary performances and check for overfitting ------
GoF_13C_lentic<-goodness_of_fit_gam(model_C_lentic,response="d13C",test_df=test_df,Hab_col=2,trophic_col=3)
results_lentic_13C<-GoF_13C_lentic$GoF_results

### Save the model and the data ------

df_C_lentic<-df
df_test_train<-GoF_13C_lentic$df_all
save(df_test_train,file="data/processed/df_test_train_C_lentic.RData")
save(df_C_lentic,file="data/processed/df_C_lentic.RData")
save(model_C_lentic,file="models/model_C_lentic.RData")


### Predictions ----------
X<-predict(model_C_lentic,newdata=df,se.fit=TRUE)
preds_Clentic_1<-data.frame(FW_ID=df$FW_ID,
                            Resource_Trophic_group=df$Trophic,
                            Resource_habitat=df$Hab,
                            d13C=round(X$fit, digits=1),
                            SE_d13C=round(X$se.fit,digits=1))
################################################################################

## Best model for 15N -------
model_N_lentic <- gam(d15N ~ s(Lat,k=20) +
                       s(Lon)+
                       s(Ele)+
                       s(Lake_area)+
                       s(hft)+
                       Hab+Trophic,
                     data=train_df, method="REML", family="gaussian",select = TRUE)

### NOT RUN Check model for concurvity and residuals
#par(mfrow=c(2,2))
#gam.check(model_N_lentic_lentic)
#plot(model_N_lentic,pages=1,residuals=TRUE)

#concurvity(model_N_lentic, full = FALSE)


### summary performances and check for overfitting -----
GoF_15N_lentic<-goodness_of_fit_gam(model_N_lentic,response="d15N",test_df=test_df,Hab_col=2,trophic_col=3)
results_lentic_15N<-GoF_15N_lentic$GoF_results

df_N_lentic<-df
df_test_train<-GoF_15N_lentic$df_all


### Save the model ------
save(df_test_train,file="data/processed/df_test_train_N_lentic.RData")
save(df_N_lentic,file="data/processed/df_N_lentic.RData")
save(model_N_lentic,file="models/model_N_lentic.RData")



### predictions ------
X<-predict(model_N_lentic,newdata=df,se.fit=TRUE)
preds_Nlentic_1<-data.frame(FW_ID=df$FW_ID,
                            Resource_Trophic_group=df$Trophic,
                            Resource_habitat=df$Hab,
                            d15N=round(X$fit, digits=1),
                            SE_d15N=round(X$se.fit,digits=1))

preds_all_lentic<-merge(preds_Clentic_1,preds_Nlentic_1,by=c("FW_ID","Resource_Trophic_group","Resource_habitat"),all=TRUE)
n<-nrow(preds_all_lentic)
preds_all_lentic$N_sample<-rep(3,n)
preds_all_lentic$Error_Type<-rep("SE",n)
preds_all_lentic$Ecosystem_Type<-rep("Lentic",n)
preds_all_lentic$Data_Type<-rep("Predicted",n)
save(preds_all_lentic,file="data/processed/preds_all_lentic.RData")

################################################################################
# 4. GAM MODELING for isotope baselines in Lotic systems ------
################################################################################
## Separate in training and test datasets (80% training, 20% test) -----

df<-DF_models_lotic_transformed
set.seed(124)

Train<-sample(nrow(df),round(0.8*nrow(df)))
test_df<-df[-Train,]
train_df<-df[Train,]

################################################################################

## Best model for 13C ------
model_C_lotic <- gam(d13C ~ 
                      s(tmp_dc_cyr,by=stra_uo)+
                      s(pre_mm_cyr,by=stra_uo)+
                      s(soc_th_uav,by=stra_uo)+
                      Hab+Trophic,
                    data=train_df, method="REML", family="gaussian",select = TRUE)

### NOT RUN Check model for concurvity and residuals
#par(mfrow=c(2,2))
#gam.check(model_C_lotic_lotic)
#plot(model_C_lotic,pages=1,residuals=TRUE)
#concurvity(model_C_lotic, full = FALSE)


### summary performances and check for overfitting -----
GoF_13C_lotic<-goodness_of_fit_gam(model_C_lotic,response="d13C",test_df=test_df,Hab_col=2,trophic_col=3)
results_lotic_13C<-GoF_13C_lotic$GoF_results

### Save the model and the data ------

df_C_lotic<-df
df_test_train<-GoF_13C_lotic$df_all
save(df_test_train,file="data/processed/df_test_train_C_lotic.RData")
save(df_C_lotic,file="data/processed/df_C_lotic.RData")
save(model_C_lotic,file="models/model_C_lotic.RData")


### predictions -----
X<-predict(model_C_lotic,newdata=df,se.fit=TRUE)
preds_Clotic_1<-data.frame(FW_ID=df$FW_ID,
           Resource_Trophic_group=df$Trophic,
           Resource_habitat=df$Hab,
           d13C=round(X$fit, digits=1),
           SE_d13C=round(X$se.fit,digits=1))

################################################################################

## Best model for 15N ------
model_N_lotic <- gam(d15N ~ s(dis_m3_pyr, by = as.factor(Trophic)) + 
                      s(tmp_dc_cyr, by = stra_uo) + s(pre_mm_cyr) +
                      s(pop_ct_csu, by = stra_uo) + 
                      Hab+Trophic,
                     data=train_df, method="REML", family="gaussian",select = TRUE)

## NOT RUN Check model for concurvity and residuals
#par(mfrow=c(2,2))
#gam.check(model_N_lotic)
#plot(model_N_lotic,pages=1,residuals=TRUE)

#concurvity(model_N_lotic, full = FALSE)


### summary performances and check for overfitting -----
GoF_15N_lotic<-goodness_of_fit_gam(model_N_lotic,response="d15N",test_df=test_df,Hab_col=2,trophic_col=3)
results_lotic_15N<-GoF_15N_lotic$GoF_results

### Save the model and the data ----

df_N_lotic<-df
df_test_train<-GoF_15N_lotic$df_all
save(df_test_train,file="data/processed/df_test_train_N_lotic.RData")
save(df_N_lotic,file="data/processed/df_N_lotic.RData")
save(model_N_lotic,file="models/model_N_lotic.RData")



### predictions -----
X<-predict(model_N_lotic,newdata=df,se.fit=TRUE)
preds_Nlotic_1<-data.frame(FW_ID=df$FW_ID,
                            Resource_Trophic_group=df$Trophic,
                            Resource_habitat=df$Hab,
                            d15N=round(X$fit, digits=1),
                            SE_d15N=round(X$se.fit,digits=1))



preds_all_lotic<-merge(preds_Clotic_1,preds_Nlotic_1,by=c("FW_ID","Resource_Trophic_group","Resource_habitat"),all=TRUE)
n<-nrow(preds_all_lotic)
preds_all_lotic$N_sample<-rep(3,n)
preds_all_lotic$Error_Type<-rep("SE",n)
preds_all_lotic$Ecosystem_Type<-rep("Lotic",n)
preds_all_lotic$Data_Type<-rep("Predicted",n)
save(preds_all_lotic,file="data/processed/preds_all_lotic.RData")
################################################################################


# 5. Save data and results for all GAMs------

## 5.1 Save goodness of fit results -----
results<-rbind(results_lentic_13C, results_lentic_15N,
               results_lotic_13C,results_lotic_15N)
results$model<-c("model_C_lentic", "model_N_lentic","model_C_lotic","model_N_lotic")
write_xlsx(results,file="models/GAM_isotope_baselines_goodness_of_fit_results.xlsx")

## 5.1 Save dataset of observed and predicted baselines -----

df_pred<-rbind(preds_all_lotic,preds_all_lentic) %>%
  select(FW_ID,Ecosystem_Type,Resource_habitat,Resource_Trophic_group,
         d13C,SE_d13C,d15N,SE_d15N,
         Data_Type)

df_obs<-DF_lentic_models %>%
  select(FW_ID,
         Resource_habitat,
         Resource_Trophic_group=Resource_trophic_group,
         d13C=d13C_baseline,
         d15N=d15N_baseline) %>%
  mutate(Ecosystem_Type="Lentic",
        SE_d13C=NA,
         SE_d15N=NA,
         d13C=round(d13C,digits=1),
         d15N=round(d15N,digits=1),
         Data_Type="Observed") %>%
  rbind(
    DF_lotic_models %>%
      select(FW_ID,
             Resource_habitat,
             Resource_Trophic_group=Resource_trophic_group,
             d13C=d13C_baseline,
             d15N=d15N_baseline) %>%
      mutate(Ecosystem_Type="Lotic",
             SE_d13C=NA,
             SE_d15N=NA,
             d13C=round(d13C,digits=1),
             d15N=round(d15N,digits=1),
             Data_Type="Observed") )

observed_predicted_baselines<-full_join(df_obs,df_pred)
write_xlsx(observed_predicted_baselines,
           "data/processed/observed_predicted_isotope_baselines.xlsx")

##############END_OF_THE_SCRIPT#################

