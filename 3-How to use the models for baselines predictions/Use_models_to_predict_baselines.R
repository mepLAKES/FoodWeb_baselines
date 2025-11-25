# ==============================================================================
# HOW TO USE THE MODELS TO GENERATE C&N ISOTOPE BASELINES FOR LOTIC AND LENTIC SITES
# ==============================================================================
# This script runs baseline GAM (Generalized Additive Models) for simulate and predict
# isotopic signatures (d15N and d13C) in lotic (flowing water) and lentic 
# (standing water) freshwater ecosystems.
# The models are based on environmental and geographical predictors.
#Necessary predictors are 
# LAKES/LENTIC SITES: Latitude, Longitude, Elevation, Lake area, Human footprint
# RIVERS/LOTIC SITES: Annual average air temp, Strahler order,
#annual cumulated precipitation,average soil organic content,annual average discharge,human population count
# Date: 09 Sept 2025
# Authors:@marie-elodie Perga
# ==============================================================================
# LOAD REQUIRED LIBRARIES AND MODELS
# ==============================================================================

# Libraries 
library(dplyr) #data handling
library(mgcv) #GAM

# load models
load("model_C_lentic.RData")
load("model_N_lentic.RData")
load("model_C_lotic.RData")
load("model_N_lotic.RData")

# ==============================================================================
# USE MODELS FOR BASELINES PREDICTIONS
# ==============================================================================

# LENTIC
##example dataset _ necessary predictors for lentic sites

df_example<-data.frame(FW_ID="example-lentic-site",# site identifier
                 Latitude=6.164449,#Latitude of the site, decimal degree
                 Longitude=141.15489,#Latitude of the site, decimal degree
                 Elevation=50,#Elevation above sea level of the site, in meters
                 Lake_area=200,#Lake area in km2
                 hft=200) #Human footprint, dimensionless)


## Generate the adequate predictors dataframe to simulate 
## both benthic and pelagic baselines 
## for both primary consumers and primary producers (resources)

newdata<- df_example %>%
  mutate(Lat=abs(Latitude), #Transform predictors according to the model's specifications
         Lon=abs(Longitude),
         Ele=log10(Elevation+3.1),
         Lake_area=log10(Lake_area))%>%
  slice(rep(1:1,4))%>% #repeat lines to add the baselines habitat and trophic level to simulate
  mutate(Hab=c(rep("Benthic",2),rep("Pelagic",2)),
         Trophic=rep(c("Primary_consumer","Primary_resource"),2))

newdata$d13C<-predict(model_C_lentic,newdata=newdata,type="response",se.fit=TRUE)$fit
newdata$d15N<-predict(model_N_lentic,newdata=newdata,type="response",se.fit=TRUE)$fit


# LOTIC
##example dataset _ necessary predictors for lotic sites

df_example<-data.frame(FW_ID="example-lotic-site",# site identifier
                       tmp_dc_cyr=50,#average annual air temperature in °C,X10
                       ORD_STRA=3,#Strahler order, dimensionless
                       pre_mm_cyr=300,#cumulated precipitation, in mm
                       soc_th_uav=500,#soil organic content, tons/ha
                       dis_m3_pyr=200, #annual average discharge, m3/s
                       pop_ct_csu=100) #human population count, number of inhabitants

## Generate the adequate predictors dataframe to simulate 
## both benthic and pelagic baselines 
## for both primary consumers and primary producers (resources)

newdata<- df_example %>%
  mutate(ORD_STRA = case_when( #Recode Strahler order into categories 
    ORD_STRA == 1~"1",
    ORD_STRA == 2~"2",
    ORD_STRA %in% c(3,4)~"3-4",
    ORD_STRA %in% c(5,6)~"5-6" ,
    ORD_STRA %in% c(7,8,9)~"7-9",
    TRUE ~ "Unknown" # Default case (optional)
  )) %>%
  mutate(pop_ct_csu=log10(pop_ct_csu+1), #Transform predictors according to the model's specifications
         stra_uo=as.factor(ORD_STRA),
         dis_m3_pyr=log10(dis_m3_pyr))%>%
  slice(rep(1:1,4))%>% #repeat lines to add the baselines habitat and trophic level to simulate
  mutate(Hab=c(rep("Benthic",2),rep("Pelagic",2)),
         Trophic=rep(c("Primary_consumer","Primary_resource"),2))

newdata$d13C<-predict(model_C_lotic,newdata=newdata,type="response",se.fit=TRUE)$fit
newdata$d15N<-predict(model_N_lotic,newdata=newdata,type="response",se.fit=TRUE)$fit






