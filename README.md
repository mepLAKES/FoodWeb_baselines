# FoodWeb_baselines

A data-driven model for simulating the C and N isotopic baselines in global freshwater ecosystems.

**Project information**

## 1. **data**
   - `lentic_data/` contains the data used in the model for lentic systems.
   
    FW_ID, site identifier. can be related to the site name and other characteristics from the ISOFRESH dataset (Bouletreau et al, in revision),
    Resource-Habitat, typical habitat of the organisms used as baselines (Benthic or Pelagic),
    Resource_trophic_group, trophic group of the organisms used as baselines (Primary_resource or Primary_consumer),
    d13C_baseline and d15N_baseline, isotopic values of the baselines used in the model (in per mil),
    Ecosystem_Type, type of ecosystem (Lentic or Lotic),
    Latitude, latitude of the site (in decimal degrees),
    Longitude, longitude of the site (in decimal degrees),
    Elevation, elevation above sea level of the site (in meters),
    Lake_area_km2, area of the lake (in km2),
    dis_m3_pyr, annual average discharge of the lake (in m3/s),
    tmp_dc_pyr, annual average air temperature (X10 °C),
    pre_mm_lyr, annual cumulated precipitation (in mm),
    pop_ct_vsu, Human population count (thousands within 3 km of the lake),
    soc_th_uav, Average organic carbon content in soils of the upstream catchment (tons/ha),
    Res_time, water residence time (days),
    hft_ix_v09, Human footprint index (dimensionless).
    
    
   - `lotic_data/` contains the data used in the model for lotic systems.
   
    FW_ID, site identifier. can be related to the site name and other characteristics from the ISOFRESH dataset (Bouletreau et al, in revision),
    Resource-Habitat, typical habitat of the organisms used as baselines (Benthic or Pelagic),
    Resource_trophic_group, trophic group of the organisms used as baselines (Primary_resource or Primary_consumer),
    d13C_baseline and d15N_baseline, isotopic values of the baselines used in the model (in per mil),
    Ecosystem_Type, type of ecosystem (Lentic or Lotic),
    Latitude, latitude of the site (in decimal degrees),
    Longitude, longitude of the site (in decimal degrees),
    Ele_vation_mt_cav, elevation above sea level of the site (in meters),
    ria_ha_csu, River area along the reach segment (ha),
    dis_m3_pyr, annual average discharge of the lake (in m3/s),
    tmp_dc_cyr, annual average air temperature (X10 °C),
    pre_mm_cyr, annual cumulated precipitation (in mm),
    pop_ct_vsu, Human population count (thousands within 3 km of the lake),
    soc_th_uav, Average organic carbon content in soils of the upstream catchment (tons/ha),
    ORD_STRA, Strahler order of the river segment,
    hft_ix_c09, Human footprint index (dimensionless).

## 2. **baseline prediction models**
   - `model_C_lotic.RData` and 'model_N_lotic.RData' contains the models used to predict the d13C and d15N baselines in lotic systems
   -`DF_lotic_models.RData` contains the data used to train the models for lotic systems
   - `model_C_lentic.RData` and 'model_N_lentic.RData' contains the models used to predict the d13C and d15N baselines in lentic systems
   - `DF_lentic_models.RData` contains the data used to train the models for lentic systems
   - `run_selected_models.R` contains the scripts to run the models, and proceed to the checks.
   
## 3. **observed and predicted baselines**
   -`datasets_observed_predicted_baselines.csv` contains for all sites the observed baselines values and the predicted values from the models.
