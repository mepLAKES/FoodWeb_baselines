# FoodWeb_baselines

A data-driven model for simulating the C and N isotopic baselines in global freshwater ecosystems.

**Project information**

## 1. **Datasets used for models training and testing**
This folder contains two .csv files of the datasets used to train and test the baseline prediction models.

   - `lentic_data/` contains the data used in the model for lentic systems.
   
    __FW_ID__  site identifier. can be related to the site name and other characteristics from the ISOFRESH dataset (Bouletreau et al, 2025),
    __Resource-Habitat__ typical habitat of the organisms used as baselines (Benthic or Pelagic),
    __Resource_trophic_group__ trophic group of the organisms used as baselines (Primary_resource or Primary_consumer),
    __d13C_baseline__ and __d15N_baseline__ isotopic values of the baselines used in the model (in per mil),
    __Ecosystem_Type__ type of ecosystem (Lentic or Lotic),
    __Latitude__ latitude of the site (in decimal degrees),
    __Longitude__, longitude of the site (in decimal degrees),
    __Elevation__, elevation above sea level of the site (in meters),
    __Lake_area_km2__, area of the lake (in km2),
    __dis_m3_pyr__, annual average discharge of the lake (in m3/s),
    __tmp_dc_pyr__, annual average air temperature (X10 °C),
    __pre_mm_lyr__, annual cumulated precipitation (in mm),
    __pop_ct_vsu__, Human population count (thousands within 3 km of the lake),
    __soc_th_uav__, Average organic carbon content in soils of the upstream catchment (tons/ha),
    __Res_time__, water residence time (days),
    __hft_ix_v09__, Human footprint index (dimensionless).
    
    
   - `lotic_data/` contains the data used in the model for lotic systems.
   
    __FW_ID__, site identifier. can be related to the site name and other characteristics from the ISOFRESH dataset (Bouletreau et al, 2025),
    __Resource-Habitat__, typical habitat of the organisms used as baselines (Benthic or Pelagic),
    __Resource_trophic_group__, trophic group of the organisms used as baselines (Primary_resource or Primary_consumer),
    __d13C_baseline__ and __d15N_baseline__, isotopic values of the baselines used in the model (in per mil),
    __Ecosystem_Type__, type of ecosystem (Lentic or Lotic),
    __Latitude__, latitude of the site (in decimal degrees),
    __Longitude__, longitude of the site (in decimal degrees),
    __Elevation_mt_cav__, elevation above sea level of the site (in meters),
    __ria_ha_csu__, River area along the reach segment (ha),
    __dis_m3_pyr__, annual average discharge of the lake (in m3/s),
    __tmp_dc_cyr__, annual average air temperature (X10 °C),
    __pre_mm_cyr__, annual cumulated precipitation (in mm),
    __pop_ct_vsu__, Human population count (thousands within 3 km of the lake),
    __soc_th_uav__, Average organic carbon content in soils of the upstream catchment (tons/ha),
    __ORD_STRA__, Strahler order of the river segment,
    __hft_ix_c09__, Human footprint index (dimensionless).

## 2. **Outputs of the optimized models**
   -`observed_predicted_isotope_baselines./` contains for all sites the observed baselines values and the predicted values from the models.

      __FW_ID__, site identifier. can be related to the site name and other characteristics from the ISOFRESH dataset,
    __Resource-Habitat__, typical habitat of the organisms used as baselines (Benthic or Pelagic),
    __Resource_Trophic_group__, trophic group of the organisms used as baselines (Primary_resource or Primary_consumer),
    __d13C__ and __d15N__, isotopic values of the baselines used in the model (in per mil),
    __Ecosystem_Type__, type of ecosystem (Lentic or Lotic),
    __SE_d13C__ and __SE_d15N__, standard error of the isotopic values of the baselines  (in per mil),only for predicted values,
    __Data_Type__, whether isotope values were measured on the field (Observed) or simulated by the models (Predicted). 
   

## 3. **How to use the models for baselines predictions**
__ Scripts __
   - Run `check_optimized_models` to check models'specifications, outputs and performances
   - The script __`Use_models_to_predict_baselines.R`__ provides examples of how to simulate baselines for a given lake or river site.

__ Objects __
   
   - `model_C_lotic.RData` and `model_N_lotic.RData` contains the models used to predict the d13C and d15N baselines in lotic systems.
   - `model_C_lentic.RData` and `model_N_lentic.RData` contain the models used to predict the d13C and d15N baselines in lentic systems

___ Dependencies __

Running the scripts requires the following librairies : tidyr, dplyr, mgcv

The models were built under R version 4.4.2 (2024-10-31), Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.6.1
attached base packages:
stats, graphics, grDevices utils, datasets, methods, base     

other attached packages:
gam.hp_0.0-2  
GGally_2.2.1  
mgcv_1.9-1  
tidyr_1.3.1  
dplyr_1.1.4 


## 4. **How to reproduce the full analysis**
These codes and data underly the paper "A global estimator of C and N isotope baselines for freshwaters" by Perga et al.

It allows to check computations and models therein and to rerun the baselines estimations for new freshwater sites.
Please be aware that those simulations are useful for macro-ecological approaches (i.e., to compare sites at a large geographical scale) but not for single-site studies.
It contains its own READme file for further information.

## 5. **Running with Docker**

If you have Docker installed on your system, you can load the models and run the test scripts directly from the Docker container.
First build the container:

```bash
docker build --tag "foodweb" .
```

and finally run the container:

```bash
docker run --rm -ti -p 8787:8787 foodweb
```

You can then open the page http://localhost:8787/ on your web browser and navigate and source the scripts of interest.
