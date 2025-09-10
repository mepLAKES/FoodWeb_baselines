# ==============================================================================
# CHECK THE GAM-MODELS FOR ISOTOPIC BASELINES
# ==============================================================================
# This script runs baseline GAM (Generalized Additive Models) for simulate and predict
# isotopic signatures (d15N and d13C) in lotic (flowing water) and lentic 
# (standing water) freshwater ecosystems.
# Date: 09 SEpt 2025
# Authors:@XX
# ==============================================================================
# LOAD REQUIRED LIBRARIES AND MODELS
# ==============================================================================

# Libraries 
library(dplyr) #data handling
library(mgcv) #GAM
library(GGally)#GAM
library(gam.hp)#GAM


# load models
load("model_C_lentic.RData")
load("model_N_lentic.RData")
load("model_C_lotic.RData")
load("model_N_lotic.RData")

# ==============================================================================
# CHECK MODELS
# ==============================================================================

# LOTIC
## 15N
summary(model_N_lotic)
par(mfrow=c(2,2))
gam.check(model_N_lotic)

## 13C
summary(model_C_lotic)
par(mfrow=c(2,2))
gam.check(model_C_lotic)

# ==============================================================================

# LENTIC
## 15N
summary(model_N_lentic)
par(mfrow=c(2,2))
gam.check(model_N_lentic)

## 13C
summary(model_C_lentic)
par(mfrow=c(2,2))
gam.check(model_C_lentic)