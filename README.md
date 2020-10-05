# MELNHE_NEON_AOP
This code uses data collected by the NEON Airborne Observation Platform to distinguish spectral properties of chronic N and P addition


# Workflow for processing data and analysis

Script 1: Tree tops from Lidar
1. Project plot shapefile in UTM
2. Use plot location to download CHM from neonUtilities
	A. Data product id:
3. Crop each lidar file to the plot area
4. Set variable window filter
	A. young stands = 
	B. Mid-aged and Old stands = 
5. Write shapefile of tree top point pixels

Script 2: Read .h5 files and extract pixels by treetop
1. Process .h5 file to a multi-band raster
2. Calculate NDVI using bands 54 (548.2 nm) and 96 (858.6 nm)
	A. Mask pixels with NDVI >= 0.9
3. Brightness normalization
4. Shade mask using digital surface model:Flightline slope and aspect, angle and azimuth
	A. Mask pixels with shade <= 0.8

Script 3: ... was wrangle .csv but now outdated.. fpar?

Script 4: Lda ordination of nutrient treatment and forest age

Script 5: All wavelength analysis and figures (Pri and average vis)
