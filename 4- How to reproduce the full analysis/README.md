**Project Title**

__A global estimator of C and N isotope baselines for freshwaters__


These codes and data underly the paper "A global estimator of C and N isotope baselines for freshwaters" by Perga et al.

It allows to check computations and models therein and to rerun the baselines estimations for new freshwater sites.
Please be aware that those simulations are useful for macro-ecological approaches (i.e., to compare sites at a large geographical scale) but not for single-site studies.

**Project information**
all necessary data are stored in the data folder (raw and processed), the scripts contains all required scripts for the described steps below and generated figures are in the figures folder. the models folder contains the best GAM.

1.  **Generate predictions GAM**

    These codes provides the best Generalized additive models to simulate the C and N isotope compositions of both the benthic and pelagic/open water baselines for streams and rivers across the World.
    Isotope values for baselines and fish species were extracted from the ISOFRESH database (Bouletreau et al, 2025)

     Different variable combinations were used for river vs. lake models. Data were sourced from HydroATLAS v. 1.0.1, specifically RiverATLAS (Linke et al. 2019) and LakeATLAS (Lehner et al. 2022).
    The database included an extensive range of sizes and elevations, with sites within forested or relatively undeveloped catchments up to highly urbanized and agricultural catchments (Table 1). 

 How to run simulations 
Run the script "1a_GAM_check_and_goodness_of_fit.R". 
The isotope data and environmental attributes are already combined in datasets provided in the data folder, 
DF_lentic_models.RData and DF_lotic_models.RData are the datasets for the lentic and lotic sites respectively.
"goodness_of_fit_gam_function.R" provides a function to compute major parameters to assess models'goodness of fit. It is called when running "1a_GAM_check_and_goodness_of_fit.R".

Running "1b_figure goodness of fit.R" generates a figure comparing the simulated baselines to the observed baselines.

Models generated (stored in the "models" folder)

model_C_lentic.RData - Carbon isotopes for lakes
model_N_lentic.RData - Nitrogen isotopes for lakes
model_C_lotic.RData - Carbon isotopes for rivers/streams
model_N_lotic.RData - Nitrogen isotopes for rivers/streams
and models performances are stored in "GAM_Isotope_baselines_goodness_of_fit.xlxs"

2.  **Compare trophic metrics for observed and simulated baselines**

    in the "scripts" folder, the script "2a_compute_metrics_species_and_community_level.R' allows to compute, from observed and simulated baselines a number of trophic metrics at the species and community scale. Metrics computed are weighted and unweighted trophic position (TPu and TPw), benthic reliance (alpha), mean distance to centroid (CD), mean nearest neighbor distance (NND) and standard deviation of the nearest neighbor distance (SDNND); the Total Area of the convex hull or the Standard Ellipse Area (SEA and SEAc; when corrected for sample size).
    2b and 2c provide scripts to generate figures at the species and community scales.
    the script "2d_usefulness of metrics.R" performs statistical comparisons and visualization of metric performance

Necessary data are in the data folder.

3.  **Generate predictions for new sites**
in the "scripts" folder, the script "3a_predict_isotope_baselines_on_new_sites.R'ses data-driven models to generate isotope baselines for all sites in the dataset, including those without isotope baseline measurements.
""b_create_maps_predicted_baselines.R" generates maps of predicted baselines with zoomed panels for Europe, Scandinavia, and North America

5.  **How to run the codes**


**Licence MIT licence **

see Licence.txt
