
Soil_to_KernAg_fun = function(year){

  timestamp()
  
  ### Read in raw Kern County Agriculture shapefile ###
  ag_sf = read_sf(paste0("../R_input/spatial/kern_AG_shp/kern",year,"/kern",year,".shp")) %>% 
    st_transform(.,CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) %>% 
    # filter(P_STATUS == "A" & !str_detect(COMM,"UNCULTIVATED")) %>%  # Keep active permits
    mutate(AG_TRS = paste0(TOWNSHIP,RANGE,str_pad(SECTION,2,pad="0",side=c("left")))) %>% 
    dplyr::select(PERMIT,PERMITTEE,PMT_SITE,COMM,S_STATUS,P_STATUS,ACRES,AG_TRS) 
  
  ###### Deal with the rows where all data is exactly the same, except geometries are different. 
  ag_sf = ag_sf %>% 
    group_by(PERMIT,PERMITTEE,PMT_SITE,COMM,S_STATUS,P_STATUS,ACRES,AG_TRS) %>% # Group by all column except geometry
    summarise() # Summarise by geometry. The effect is that rows with identical information will get combined geometries AKA >1 polygons to 1.  
  
  ag_sf$COMM <- as.character(ag_sf$COMM)
  
  ag_merge <- left_join(ag_sf,agro_class, 
                    by = c("COMM"))
 
  ag_slct <- ag_merge %>% 
    dplyr::select(-c(COMM_EDIT,COMM_CODEOLD))
  
  
  ######## Soil raster with (few) NAs ##########
  
  ## Extract values from soil raster that correspond to each polygon in the ag shapefile
  r.vals.na = raster::extract(soil_ras, # Storie Index Raster Values
                           ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                           fun = mean, # Find the mean value of raster cells that overlap with each polygon
                           small = TRUE, # Include mean values for small polygons
                           weights = TRUE, # Add weights for weighted means
                           normalizeWeights = TRUE,
                           na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                           df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals.na$layer[r.vals.na$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil_NA = r.vals.na$layer  
  
  ######## Soil raster with no NAs ##########
  
  ## Extract values from soil raster that correspond to each polygon in the ag shapefile
  r.vals.NOna = raster::extract(soil_ras_covered, # Storie Index Raster Values
                           ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                           fun = mean, # Find the mean value of raster cells that overlap with each polygon
                           small = TRUE, # Include mean values for small polygons
                           weights = TRUE, # Add weights for weighted means
                           normalizeWeights = TRUE,
                           na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                           df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals.NOna$layer[r.vals.NOna$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil_noNA = r.vals.NOna$layer  
  
  ######## Factor X - Chemical Properties ##########
  
  ## Extract values from factor x - chemistry soil raster that correspond to each polygon in the ag shapefile
  r.vals.chem = raster::extract(chem_ras, # Storie Index Raster Values
                                ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                                fun = mean, # Find the mean value of raster cells that overlap with each polygon
                                small = TRUE, # Include mean values for small polygons
                                weights = TRUE, # Add weights for weighted means
                                normalizeWeights = TRUE,
                                na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                                df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals.chem$layer[r.vals.chem$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil_chem = r.vals.chem$layer  
  
  ######## Factor X - Temperature Properties ##########
  
  ## Extract values from factor x - temperature regime soil raster that correspond to each polygon in the ag shapefile
  r.vals.temp = raster::extract(temp_ras, # Storie Index Raster Values
                                ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                                fun = mean, # Find the mean value of raster cells that overlap with each polygon
                                small = TRUE, # Include mean values for small polygons
                                weights = TRUE, # Add weights for weighted means
                                normalizeWeights = TRUE,
                                na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                                df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals.temp$layer[r.vals.temp$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil_temp = r.vals.temp$layer
  
  ######## Factor X - Hydrologic Properties ##########
  
  ## Extract values from factor x - hydrologic features soil raster that correspond to each polygon in the ag shapefile
  r.vals.hydro = raster::extract(hydro_ras, # Storie Index Raster Values
                                ag_slct, # Extract based on Kern Ag polygon outlines from the Kern Ag shapefile
                                fun = mean, # Find the mean value of raster cells that overlap with each polygon
                                small = TRUE, # Include mean values for small polygons
                                weights = TRUE, # Add weights for weighted means
                                normalizeWeights = TRUE,
                                na.rm = TRUE, # Ignore NAs when calculating mean values -- BUT then need to change 0s back to NAs
                                df = TRUE) # Return results as a data.frame
  
  # Change '0' values to NA
  r.vals.hydro$layer[r.vals.hydro$layer == 0] = NA
  
  ## Add the mean values to the Kern Agriculture Shapefile
  ag_slct$soil_hydro = r.vals.hydro$layer
  
  ######
  ## Convert back into Shapefile
  ag_shp_withSoil = as(ag_slct, "Spatial")
  
  #Create year subfolder
  dir.create(paste0("../R_output/spatial/KernAg_withSoil/", year))
  
  ## Write output shapefile
  writeOGR(obj = ag_shp_withSoil,
           dsn = paste0("../R_output/spatial/KernAg_withSoil/",year),
           layer = paste0("KernAg_withSoil_",year),
           driver = "ESRI Shapefile",
           overwrite_layer = TRUE)
  
}

