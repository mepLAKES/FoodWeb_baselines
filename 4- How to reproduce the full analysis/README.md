**Project Title**

__A global estimator of C and N isotope baselines for freshwaters__


These codes and data underly the paper "A global estimator of C and N isotope baselines for freshwaters" by Perga et al.

It allows to check computations and models therein and to rerun the baselines estimations for new freshwater sites.
Please be aware that those simulations are useful for macro-ecological approaches (i.e., to compare sites at a large geographical scale) but not for single-site studies.

**Project information**

1.  **Generate predictions GAM**

    These codes provides the best Generalized additive models to simulate the C and N isotope compositions of both the benthic and pelagic/open water baselines for streams and rivers across the World.
    Isotope values for baselines and fish species were extracted from the ISOFRESH database (Bouletreau et al, 2025)

    Predictors are extracted from various databases. Variables included:
    (1) surface elevation (m a.s.l.);
    (2) lake surface area (km2);
    (3) river surface area (km2) based on estimated channel and bankfull width (Lehner and Grill 2013);
    (4) modeled long-term (1971–2000) average annual ‘naturalized’ discharge (m3/s) according to the global integrated water model WaterGap v2.2 (Döll et al. 2003);
    (5) Strahler stream order; (6) long-term (1950–2000) average annual air temperature (°C) and
    (7) average total annual precipitation (mm) according to WorldClim (Hijmans et al. 2005);
    (8) modeled average organic carbon content in soils of the upstream catchment (Hengl et al. 2014);
    (9) human population count (2010) and (10) human footprint index (2009) representing a composite measure of the direct and indirect human pressures on the land’s surface within a 3 km vicinity of the lake or in the reach catchment (Venter et al. 2016).
     Different variable combinations were used for river vs. lake models. Data were sourced from HydroATLAS v. 1.0.1, specifically RiverATLAS (Linke et al. 2019) and LakeATLAS (Lehner et al. 2022).
    The database included an extensive range of sizes and elevations, with sites within forested or relatively undeveloped catchments up to highly urbanized and agricultural catchments (Table 1). 

 How to run simulations 
Run the script "GAM_check_and_goodness_of_fit.R". 
The isotope data and environmental attributes are already combined in datasets provided in the data folder, 
DF_lentic_models.RData and DF_lotic_models.RData are the datasets for the lentic and lotic sites respectively.

Running script_for_figure_2.R generates a figure comparing the simulated baselines to the observed baselines.


2.  **Compare trophic metrics for observed and simulated baselines**

    in the "scripts" folder, the script "compute_metrics_species_and_community_level.R' allows to compute, from observed and simuéated baselines a number of trophic metrics at the species and community scale.
    it also includes how to produce the graphs to compare values of trophic metrics when extrcated from observed or simuéated baselines.
necessary data are in the data folder.

3.  **How to run the codes**


**Licence MIT licence **

see Licence.txt
