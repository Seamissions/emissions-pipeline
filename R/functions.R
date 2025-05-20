#R/functions.R

# Prep FAO data
prep_fao <- function(fao_files) {
  
  # Read in emissions data files
  for (i in seq_along(fao_files)) {
    # Detect capture quantity
    if(str_detect(fao_files[i], "Capture_Quantity")) {
      table_name <- paste("fao_capture_quantity")
      assign(table_name, read_csv(fao_files[i], show_col_types = FALSE) %>%
               clean_names())
      # Detect country groups
    } else if (str_detect(fao_files[i], "CL_FI_COUNTRY_GROUPS")) {
      table_name <- paste("fao_country_groups")
      assign(table_name, read_csv(fao_files[i], show_col_types = FALSE) %>%
               clean_names())
      # Detect species groups
    } else if (str_detect(fao_files[i], "CL_FI_SPECIES_GROUPS")) {
      table_name <- paste("fao_species_groups")
      assign(table_name, read_csv(fao_files[i], show_col_types = FALSE) %>%
               clean_names())
    } 
  }
  
  # --- Clean Data ----
  fao_country_groups_clean <- fao_country_groups %>%
    select(un_code, iso3_code, name_en, continent_group_en, official_name_en)
  
  fao_species_groups_clean <- fao_species_groups %>%
    select(x3a_code, taxonomic_code, identifier, scientific_name, contains("_en"), major_group) %>%
    # Filter out mammals, plants, and reptiles (gear types not targeting these categories)
    filter(major_group %in% c("PISCES", "CRUSTACEA", "MOLLUSCA", "INVERTEBRATA AQUATICA")) %>%
    # Filter out freshwater species
    filter(!str_detect(isscaap_group_en, '(?i)freshwater')) %>%
    # Filter out corals
    filter(!isscaap_group_en == "Corals") %>%
    # Filter out river species
    filter(!isscaap_group_en == "River eels")
  
  # Left join country groups
  fao_capture_join <- left_join(fao_capture_quantity, fao_country_groups_clean, by = join_by(country_un_code == un_code)) 
  
  # Left join species groups
  fao_capture_join <- left_join(fao_capture_join, fao_species_groups_clean, by = join_by(species_alpha_3_code == x3a_code))
  
  # Define regions
  regions <- c(18, 21, 27, 31, 34, 37, 41, 47, 48, 51, 57, 58, 61, 67, 71, 77, 81, 87, 88)
  
  # Clean resulting dataset
  fao_catch <- fao_capture_join %>%
    # Select desired columns
    select(area_code,
           period,
           iso3_code,
           # This will be the species identifier to join species info in final dataset
           identifier,
           major_group,
           value) %>%
    # Rename columns for consistency
    rename(zone = area_code,
           year = period,
           flag = iso3_code,
           species_identifier = identifier,
           catch = value) %>%
    # Filter for >2015 to match emissions data
    filter(year > 2015) %>%
    filter(zone %in% regions) %>%
    mutate(zone = as.integer(zone),
           year = as.integer(year))
  
  # Combine by zone, year, flag, species
  fao_cleaned <- fao_catch %>%
    group_by(zone, year, flag, species_identifier) %>%
    summarise(fao_catch_tons = sum(catch, na.rm = TRUE), .groups = "drop") %>%
    filter(fao_catch_tons != 0) %>%
    # create identifier for NA (species unk)
    mutate(species_identifier = ifelse(is.na(species_identifier), 1000000, species_identifier))
  
  return(fao_cleaned)
}

# Merge emissions
merge_emissions <- function(emissions_files) {
  
  # Turn off scientific notation
  options(scipen=999)
  
  # Read in emissions data files
  for (i in seq_along(emissions_files)) {
    # Detect "non" to name non-broadcasting
    if(str_detect(emissions_files[i], "non")) {
      table_name <- paste("non_broadcasting")
      assign(table_name, read_csv(emissions_files[i], show_col_types = FALSE) %>%
               clean_names())
      # Detect "ais" to name broadcasting
    } else if (str_detect(emissions_files[i], "ais")) {
      table_name <- paste("broadcasting")
      assign(table_name, read_csv(emissions_files[i], show_col_types = FALSE) %>%
               clean_names())
      # Stop if extra files are detected
    } else {
      stop("Extra file(s) detected.")
    }
  }
  
  # Clean non_broadcasting
  non_broadcasting <- non_broadcasting %>%
    mutate(date = lubridate::ymd(month)) %>%
    mutate(year_month = format(date, '%Y-%m')) %>%
    mutate(flag = "DARK")

  # Rename pollutant columns in non-broadcasting data to match broadcasting data
  colnames(non_broadcasting)[str_detect(colnames(non_broadcasting), "non_broadcasting")] <- str_remove(colnames(non_broadcasting)[str_detect(colnames(non_broadcasting), "non_broadcasting")], "_non_broadcasting") 
  
  # Clean broadcasting
  broadcasting <- broadcasting %>%
    mutate(date = lubridate::ymd(month)) %>%
    mutate(year_month = format(date, '%Y-%m')) %>%
    # Replace NA values with "UNK"
    mutate(flag = ifelse(is.na(flag), "UNK", flag)) %>%
    # Only select fishing vessels (dataset already only selects for fishing activity)
    filter(vessel_class %in% c("fishing","squid_jigger","drifting_longlines",
                               "pole_and_line","other_fishing","trollers","fixed_gear",
                               "pots_and_traps","set_longlines","set_gillnets","trawlers",
                               "dredge_fishing","seiners","purse_seines","tuna_purse_seines",
                               "other_purse_seines","other_seines","driftnets"))
  
  # Warning if pollutant column names don't match (for join)
  if(unique(colnames(non_broadcasting)[str_detect(colnames(non_broadcasting), "emissions")] == colnames(broadcasting)[str_detect(colnames(broadcasting), "emissions")]) != TRUE) {
    stop("Column names don't match.")
  }
  
  #return(broadcasting)
  
  # Combine broadcasting and non-broadcasting data
  emissions <- bind_rows(broadcasting, non_broadcasting) %>%
    select(year_month, everything(), -c(month, date))
  
  # Stop if rows don't add up
  if(nrow(broadcasting) + nrow(non_broadcasting) != nrow(emissions)) {
    stop("Number of rows don't add up, emissions data lost.")
  }
  
  return(emissions)
}

# Assign zones
intersection <- function(emissions) {
  
  # -------- EMISSIONS --------
  # Filter for desired variables
  emissions_filtered <- emissions %>%
    # Create year column
    mutate(year = as.integer(substr(year_month, 1, 4))) %>%
    # Group by pixel, year, and flag
    group_by(lat_bin, lon_bin, year, flag) %>% # vessel_class (?)
    # Sum emissions
    summarise(emissions_co2_mt = sum(emissions_co2_mt, na.rm = TRUE),
              emissions_ch4_mt = sum(emissions_ch4_mt, na.rm = TRUE),
              emissions_n2o_mt = sum(emissions_n2o_mt, na.rm = TRUE),
              emissions_nox_mt = sum(emissions_nox_mt, na.rm = TRUE),
              emissions_sox_mt = sum(emissions_sox_mt, na.rm = TRUE),
              emissions_co_mt = sum(emissions_co_mt, na.rm = TRUE),
              emissions_vocs_mt = sum(emissions_vocs_mt, na.rm = TRUE),
              emissions_pm2_5_mt = sum(emissions_pm2_5_mt, na.rm = TRUE),
              emissions_pm10_mt = sum(emissions_pm10_mt, na.rm = TRUE)) %>%
    # Filter years to match non-broadcasting emissions
    filter(year > 2015)
  
  # Convert lat/long to point geometry (degrees)
  emissions_sf <- emissions_filtered %>%
    # add lat and lon bin to preserve for merge 
    dplyr::mutate(lat = lat_bin, lon = lon_bin) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) # import as WGS (unit = degrees)
  
  # Create grid throughout extent of emissions_sf from point geometry
  emissions_grid <- emissions_sf %>%
    st_make_grid(cellsize = c(1,1),
                 what = "polygons") %>%
    st_sf() %>%
    # Fix invalid geometries
    st_make_valid() %>%
    # Add grid number to keep track
    mutate(grid_id = row_number())
  
  # Spatially join emissions data to the grid
  emissions_grid_sf <- st_join(emissions_grid, emissions_sf, left = FALSE) %>%
    # Transform to Equal Earth projection
    st_transform(6933)
  
  # -------- FAO REGIONS --------
  # Import regions data (set same crs) from workbench-2
  fao_regions <- st_read(file.path("/capstone/seamissions/data/fao_region_shapefile")) %>%
    # Transform to same crs as grid
    st_transform(st_crs(emissions_grid_sf)) %>%
    # Fix geometries
    st_make_valid()
  
  # Confirm crs match
  if (st_crs(emissions_grid_sf) != st_crs(fao_regions)){
    stop("CRS don't match.")
  }
  
  # Find which grid cells intersect with the FAO regions
  emissions_zones <- st_intersection(emissions_grid_sf, fao_regions) 
  
  # Create area column (as numeric)
  emissions_zones$area <- as.numeric(st_area(emissions_zones))
  
  return(emissions_zones)
}

# Partition emissions
partition_emissions <- function(emissions_zones) {
  
  # Inspect geometry types in emissions_zones
  geom_types <- unique(st_geometry_type(emissions_zones))
  
  # Specify desired geometry types: POLYGON or MULTIPOLYGON 
  valid_types <- c("POLYGON", "MULTIPOLYGON")
  
  # Identify unexpected types
  invalid_types <- setdiff(geom_types, valid_types) # contains POINT (?) result of points of overlap
  
  # Warning if geometry types besides POLYGON or MULTIPOLYGON found
  if(length(invalid_types > 0)) {
    warning("Additional geometry types detected. Proceed with filter.")
  } 
  
  # Remove other geometry types (filter out POINT)
  emissions_zones_filtered <- emissions_zones %>%
    filter(st_geometry_type(.) %in% valid_types) # in valid_types, c("POLYGON", "MULTIPOLYGON")
  
  # # Save for dashboard after filtering out points
  # dashboard <- emissions_zones_filtered %>%
  #   group_by(grid_id, lat_bin, lon_bin, flag, year, zone, geometry) %>% # geometry?
  #   summarize(emissions_co2_mt = sum(emissions_co2_mt, na.rm = TRUE))

  # Break MULTIPOLYGONS down into POLYGONS
  emissions_zones_exploded <- st_cast(emissions_zones_filtered, "MULTIPOLYGON") %>% 
    st_cast("POLYGON")
  
  # Save for dashboard after filtering out points
  dashboard <- emissions_zones_exploded %>%
    group_by(grid_id, lat_bin, lon_bin, flag, year, zone, geometry) %>% # geometry?
    summarize(emissions_co2_mt = sum(emissions_co2_mt, na.rm = TRUE))
  
  saveRDS(dashboard, file = "/capstone/seamissions/checkpoint/dashboard.rds")
  
  # Generate areas by grid_id for sub polygons
  emissions_zones_summary_1 <- emissions_zones_exploded %>%
    group_by(grid_id, geometry) %>%
    summarise(
      number_areas = n_distinct(area),
      .groups = "drop") %>% # keep?
    mutate(area_summary = as.numeric(st_area(geometry))) %>%
    ungroup()
  
  # Add up sub polygon areas for overall grid_id area
  emissions_zones_summary_2 <- emissions_zones_summary_1 %>%
    group_by(grid_id) %>%
    summarise(
      unique_areas = paste(unique(area_summary), collapse = ", "),
      number_areas = sum(number_areas),
      total_unique_area = sum(area_summary, na.rm = TRUE))
  
  # Made a key of grid_id and overall grid_id area
  grid_id_key <- emissions_zones_summary_2 %>%
    select(grid_id, total_unique_area) %>%
    st_drop_geometry()
  
  # Join key with emissions_zones
  emissions_zones_joined <- left_join(emissions_zones, grid_id_key)
  
  # build in warning for matching rows? ids?
  if(nrow(emissions_zones_joined) != nrow(emissions_zones)) {
    stop("Incorrect join: too many rows.")
  }
  
  # Partition out emissions by area proportion
  emissions_partitioned <- emissions_zones_joined %>%
    # Calculate proportion
    mutate(prop = area/total_unique_area) %>%
    # Partition emissions
    mutate(prop_co2_mt = emissions_co2_mt * prop,
           prop_ch4_mt = emissions_ch4_mt * prop,
           prop_n2o_mt = emissions_n2o_mt * prop,
           prop_nox_mt = emissions_nox_mt * prop,
           prop_sox_mt = emissions_sox_mt * prop,
           prop_co_mt = emissions_co_mt * prop,
           prop_vocs_mt = emissions_vocs_mt * prop,
           prop_pm2_5_mt = emissions_pm2_5_mt * prop,
           prop_pm10_mt = emissions_pm10_mt * prop)
  
  # # -------- Check for discrepancies --------
  # # List pollutants
  # pollutants <- c("co2", 
  #                 "ch4",
  #                 "n2o",
  #                 "nox",
  #                 "sox",
  #                 "co",
  #                 "vocs",
  #                 "pm2_5",
  #                 "pm10")
  # 
  # for (i in seq_along(pollutants)) {
  #   
  #   # Assign column names
  #   sf_col <- paste0("emissions_", pollutants[i], "_mt")
  #   prop_col <- paste0("prop_", pollutants[i], "_mt")
  #   
  #   # Total emissions before factoring
  #   em_before <- sum(emissions_grid_sf[[sf_col]], na.rm = TRUE)
  #   
  #   # Total emissions after factoring
  #   em_after <- sum(emissions_partitioned[[prop_col]], na.rm = TRUE)
  #   
  #   # Difference in emissions
  #   diff <- em_after - em_before
  #   
  #   # Percent error
  #   error <- ((diff/em_before)*100)
  #   
  #   #print(error)
  #   
  #   # Error warning for lost emissions over 0.001% (DECISION POINT)
  #   if (error > 0.001) {
  #     stop(paste0("Error over 0.001% returned for ", {pollutants[i]}, " emissions partitioning. Some emissions may be lost."))
  #   }
  # }
  
  # Drop geometry
  emissions_partitioned_no_geo <- emissions_partitioned %>%
    st_drop_geometry()
  
  # Group by zone, flag, and year
  emissions_partitioned_grouped <- emissions_partitioned_no_geo %>%
    group_by(zone, flag, year) %>%
    summarise(emissions_co2_mt = sum(prop_co2_mt, na.rm = TRUE),
              emissions_ch4_mt = sum(prop_ch4_mt, na.rm = TRUE),
              emissions_n2o_mt = sum(prop_n2o_mt, na.rm = TRUE),
              emissions_nox_mt = sum(prop_nox_mt, na.rm = TRUE),
              emissions_sox_mt = sum(prop_sox_mt, na.rm = TRUE),
              emissions_co_mt = sum(prop_co_mt, na.rm = TRUE),
              emissions_vocs_mt = sum(prop_vocs_mt, na.rm = TRUE),
              emissions_pm2_5_mt = sum(prop_pm2_5_mt, na.rm = TRUE),
              emissions_pm10_mt = sum(prop_pm10_mt, na.rm = TRUE)) %>%
    ungroup()
  
  return(emissions_partitioned_grouped)
 
}

# Merge emissions and FAO catch
merge_catch_fao <- function(emissions_partitioned_grouped, fao_catch) {

  # Define regions
  regions <- c(18, 21, 27, 31, 34, 37, 41, 47, 48, 51, 57, 58, 61, 67, 71, 77, 81, 87, 88)
  
  # Create empty vector to append Region #
  option_1 <- vector("list", length = length(regions))
  
  # Partition emissions
  for (i in seq_along(regions)){
    
    # -------- TABLE 1 --------
    # 1.1 All flags reporting FAO catch in Region #
    # total_region_catch <- fao_catch %>%
    #   filter(zone == regions[i]) %>%
    #   group_by(zone, year, flag, species_identifier) %>%
    #   summarise(country_total_tons_by_species = sum(fao_catch_tons, na.rm = TRUE), .groups = "drop") %>%
    #   ungroup() %>%
    #   group_by(zone, year) %>%
    #   mutate(region_total_tons = sum(country_total_tons_by_species, na.rm = TRUE)) %>%
    #   mutate(prop_fao_catch = country_total_tons_by_species/region_total_tons) %>%
    #   select(-region_total_tons) %>%
    #   filter(country_total_tons_by_species > 0)
    
    total_region_catch <- fao_catch %>%
      filter(zone == regions[i]) %>%
      group_by(zone, year, flag, species_identifier) %>%
      summarise(total_tons_by_species = sum(fao_catch_tons, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      group_by(zone, year) %>%
      mutate(region_total_tons = sum(total_tons_by_species, na.rm = TRUE)) %>%
      mutate(prop_fao_catch = total_tons_by_species/region_total_tons) %>%
      select(-region_total_tons) %>%
      filter(total_tons_by_species > 0)
    
    # 1.2 Summarise non-broadcasting emissions by Region #
    fao_summary_non_broadcasting <- emissions_partitioned_grouped %>%
      # Filter for Region # (to account for emissions later)
      filter(zone == regions[i]) %>%
      # Select non-broadcasting emissions
      filter(flag == "DARK") %>%
      # Rename columns for non-broadcasting emissions
      mutate(non_broad_co2_mt = emissions_co2_mt,
             non_broad_ch4_mt = emissions_ch4_mt,
             non_broad_n2o_mt = emissions_n2o_mt,
             non_broad_nox_mt = emissions_nox_mt,
             non_broad_sox_mt = emissions_sox_mt,
             non_broad_co_mt = emissions_co_mt,
             non_broad_vocs_mt = emissions_vocs_mt,
             non_broad_pm2_5_mt = emissions_pm2_5_mt,
             non_broad_pm10_mt = emissions_pm10_mt) %>%
      # Keep only desired columns
      select(zone, 
             year, 
             contains("non_broad_"))
    
    # TABLE 1: Allocate non-broadcasting emissions to FAO reporting countries by Region #
    total_region_non_broad_allocation <- full_join(total_region_catch, fao_summary_non_broadcasting, by = c("zone", "year")) %>%
      mutate(dist_non_broad_co2_mt = ifelse(is.na(prop_fao_catch), non_broad_co2_mt, prop_fao_catch * non_broad_co2_mt),
             dist_non_broad_ch4_mt = ifelse(is.na(prop_fao_catch), non_broad_ch4_mt, prop_fao_catch * non_broad_ch4_mt),
             dist_non_broad_n2o_mt = ifelse(is.na(prop_fao_catch), non_broad_ch4_mt, prop_fao_catch * non_broad_n2o_mt),
             dist_non_broad_nox_mt = ifelse(is.na(prop_fao_catch), non_broad_nox_mt, prop_fao_catch * non_broad_nox_mt),
             dist_non_broad_sox_mt = ifelse(is.na(prop_fao_catch), non_broad_sox_mt, prop_fao_catch * non_broad_sox_mt),
             dist_non_broad_co_mt = ifelse(is.na(prop_fao_catch), non_broad_co_mt, prop_fao_catch * non_broad_co_mt),
             dist_non_broad_vocs_mt = ifelse(is.na(prop_fao_catch), non_broad_vocs_mt, prop_fao_catch * non_broad_vocs_mt),
             dist_non_broad_pm2_5_mt = ifelse(is.na(prop_fao_catch), non_broad_pm2_5_mt, prop_fao_catch * non_broad_pm2_5_mt),
             dist_non_broad_pm10_mt = ifelse(is.na(prop_fao_catch), non_broad_pm10_mt, prop_fao_catch * non_broad_pm10_mt)
      ) %>%
      select(zone,
             year,
             flag,
             species_identifier,
             total_tons_by_species,
             contains("dist_non_broad_")) 
    
    # Account for emissions transfer in TABLE 1
    before <- sum(fao_summary_non_broadcasting$non_broad_co2_mt, na.rm = TRUE) 
    after <- sum(total_region_non_broad_allocation$dist_non_broad_co2_mt, na.rm = TRUE) 
    
    percent_diff <- ((after-before)/before)*100
    
    # Warning if emissions loss is greater than 0.001%
    if(percent_diff > 0.001) {
      stop(paste0("Greater than 0.001% of non-broadcasting emissions lost during allocation to FAO reporting countries in Region ", regions[i], ". Check Table 1."))
    }
    
    # -------- TABLE 2 --------
    # 2.1: All flags with broadcasting emissions by Region #
    emissions_partitioned <- emissions_partitioned_grouped %>%
      # Filter for Region #
      filter(zone == regions[i]) %>%
      # Remove non-broadcasting emissions
      filter(!flag == "DARK") %>%
      mutate(broad_co2_mt = emissions_co2_mt,
             broad_ch4_mt = emissions_ch4_mt,
             broad_n2o_mt = emissions_n2o_mt,
             broad_nox_mt = emissions_nox_mt,
             broad_sox_mt = emissions_sox_mt,
             broad_co_mt = emissions_co_mt,
             broad_vocs_mt = emissions_vocs_mt,
             broad_pm2_5_mt = emissions_pm2_5_mt,
             broad_pm10_mt = emissions_pm10_mt) %>%
      select(-contains("emissions_"))
               
    # 2.2 Distribute broadcasting for countries with catch ASSUMPTION: 1 ton = same emissions for all species
    total_flag_catch <- fao_catch %>%
      filter(zone == regions[i]) %>%
      group_by(zone, year, flag, species_identifier) %>%
      summarise(country_total_tons_by_species = sum(fao_catch_tons, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      group_by(zone, year, flag) %>%
      mutate(country_total_tons = sum(country_total_tons_by_species, na.rm = TRUE)) %>%
      mutate(prop_species_catch = country_total_tons_by_species/country_total_tons) %>%
      filter(country_total_tons_by_species > 0) %>%
      select(-c(country_total_tons)) %>% #country_total_tons_by_species
      filter(!is.na(prop_species_catch))
    
    # TABLE 2
    total_region_broad_allocation <- full_join(total_flag_catch, emissions_partitioned, by = c("zone", "year", "flag")) %>%
      mutate(dist_broad_co2_mt = ifelse(is.na(prop_species_catch), broad_co2_mt, prop_species_catch * broad_co2_mt),
             dist_broad_ch4_mt = ifelse(is.na(prop_species_catch), broad_ch4_mt, prop_species_catch * broad_ch4_mt),
             dist_broad_n2o_mt = ifelse(is.na(prop_species_catch), broad_n2o_mt, prop_species_catch * broad_n2o_mt),
             dist_broad_nox_mt = ifelse(is.na(prop_species_catch), broad_nox_mt, prop_species_catch * broad_nox_mt),
             dist_broad_sox_mt = ifelse(is.na(prop_species_catch), broad_sox_mt, prop_species_catch * broad_sox_mt),
             dist_broad_co_mt = ifelse(is.na(prop_species_catch), broad_co_mt, prop_species_catch * broad_co_mt),
             dist_broad_vocs_mt = ifelse(is.na(prop_species_catch), broad_vocs_mt, prop_species_catch * broad_vocs_mt),
             dist_broad_pm2_5_mt = ifelse(is.na(prop_species_catch), broad_pm2_5_mt, prop_species_catch * broad_pm2_5_mt),
             dist_broad_pm10_mt = ifelse(is.na(prop_species_catch), broad_pm10_mt, prop_species_catch * broad_pm10_mt)) %>%
      select(-country_total_tons_by_species)
    
    # -------- TABLE 3 --------
    # TABLE 3: Full join Tables 1 and 2 (to not lose flags with non-broad emissions but no broad emissions)
    total_region_emissions <- full_join(total_region_broad_allocation, total_region_non_broad_allocation, by = c("zone", "year", "flag", "species_identifier")) %>%
      #filter(zone == 18) %>%
      # Sum broadcasting and non-broadcasting emissions
      mutate(total_co2_mt = rowSums(across(c(dist_broad_co2_mt, dist_non_broad_co2_mt)), na.rm = TRUE),
             total_ch4_mt = rowSums(across(c(dist_broad_ch4_mt, dist_non_broad_ch4_mt)), na.rm = TRUE),
             total_n2o_mt = rowSums(across(c(dist_broad_n2o_mt, dist_non_broad_n2o_mt)), na.rm = TRUE),
             total_nox_mt = rowSums(across(c(dist_broad_nox_mt, dist_non_broad_nox_mt)), na.rm = TRUE),
             total_sox_mt = rowSums(across(c(dist_broad_sox_mt, dist_non_broad_sox_mt)), na.rm = TRUE),
             total_co_mt = rowSums(across(c(dist_broad_co_mt, dist_non_broad_co_mt)), na.rm = TRUE),
             total_vocs_mt = rowSums(across(c(dist_broad_vocs_mt, dist_non_broad_vocs_mt)), na.rm = TRUE),
             total_pm2_5_mt = rowSums(across(c(dist_broad_pm2_5_mt, dist_non_broad_pm2_5_mt)), na.rm = TRUE),
             total_pm10_mt = rowSums(across(c(dist_broad_pm10_mt, dist_non_broad_pm10_mt)), na.rm = TRUE)) %>%
      mutate(species_identifier = ifelse(is.na(species_identifier), 99999999, species_identifier))
    
    # Keep desired columns
    total_region_emissions <- total_region_emissions %>%
      select(zone,
             year,
             flag,
             species_identifier,
             #total_tons_by_species,
             contains("total_"))
    
    # Account for emissions after full join
    before <- sum(emissions_partitioned$broad_co2_mt, na.rm = TRUE) + sum(fao_summary_non_broadcasting$non_broad_co2_mt, na.rm = TRUE)
    after <- sum(total_region_emissions$total_co2_mt, na.rm = TRUE) 
    
    percent_diff <- ((after-before)/before)*100
    
    # Warning if emissions loss is greater than 0.001%
    if(percent_diff > 0.001) {
      stop(paste0("Greater than 0.001% of emissions lost during full join of broadcasting and non-broadcasting in Region ", regions[i], ". Check Table 3."))
    }
    
    check <- emissions_partitioned_grouped %>%
      # Filter for Region #
      filter(zone == regions[i])
    
    before <- sum(check$emissions_co2_mt, na.rm = TRUE)
    after <- sum(total_region_emissions$total_co2_mt, na.rm = TRUE)
    
    percent_diff <- ((after-before)/before)*100
    
    # Warning if emissions loss is greater than 0.001%
    if(percent_diff > 0.001) {
      stop(paste0("Greater than 0.001% of emissions lost during full partitioning in Region ", regions[i], "."))
    }
    
    # Append to vector
    option_1[[i]] <- total_region_emissions
  }
  
  # Full emissions dataset
  full_emissions_fao <- bind_rows(option_1)
  
  # Import species key
  full_species_key <- read_csv(here("data-keys", "full_species_key.csv"), show_col_types = FALSE)
  
  # Join to FAO data
  full_emissions_fao_species <- left_join(full_emissions_fao, full_species_key, by = c("species_identifier" = "identifier")) %>%
    relocate(c(name_en, scientific_name, major_group, isscaap_group, isscaap), .after = flag)
  
  write_csv(full_emissions_fao_species, file.path("/capstone/seamissions/checkpoint/full_emissions_fao_species.csv"))

  return(full_emissions_fao_species)
  
}

prep_sau <- function(sau_files) {
  
  # Read in flag key
  flag_key <- read_csv(here("data-keys", "flag_key.csv"), show_col_types = FALSE)
  
  sau_regions <- vector("list", length = length(sau_files))
  
  # Read in SAU files by region
  for (i in seq_along(sau_files)) {
    table_name <- paste0("sau_region_", str_sub(sau_files[i], 41, 42))
    sau_regions[[i]] <- assign(table_name, read.csv(sau_files[i], stringsAsFactors = FALSE, check.names = TRUE) %>%
                                 clean_names() %>%
                                 mutate(zone = as.numeric(str_sub(sau_files[i], 41, 42)),
                                        year = as.numeric(year)) %>%
                                 filter(year >= 2015) %>%
                                 # WORK ON THIS
                                 left_join(flag_key, by = c("fishing_entity" = "sau_name")) %>%
                                 rename(flag = iso3_code))
  }
  
  # Bind all regions
  sau_catch_data <- bind_rows(sau_regions)
  
  # Summarise SAU catch
  sau_catch <- sau_catch_data %>%
    group_by(zone, year, flag, scientific_name, common_name, commercial_group) %>%
    summarise(sau_catch_tons = sum(tonnes, na.rm = TRUE)) %>%
    rename(species = scientific_name) %>% 
    filter(year > 2015)
  
  return(sau_catch)
}

merge_catch_sau <- function(emissions_partitioned_grouped, sau_catch){
  
  # Define regions
  regions <- c(18, 21, 27, 31, 34, 37, 41, 47, 48, 51, 57, 58, 61, 67, 71, 77, 81, 87, 88)
  
  # Create empty vector to append Region #
  option_1 <- vector("list", length = length(regions))
  
  # Partition emissions
  for (i in seq_along(regions)){
    
    # -------- TABLE 1 --------
    # 1.1 All flags reporting FAO catch in Region #
    # total_region_catch <- sau_catch %>%
    #   filter(zone == regions[i]) %>%
    #   group_by(zone, year, flag, species) %>%
    #   summarise(country_total_tons_by_species = sum(sau_catch_tons, na.rm = TRUE), .groups = "drop") %>%
    #   ungroup() %>%
    #   group_by(zone, year) %>%
    #   mutate(region_total_tons = sum(country_total_tons_by_species, na.rm = TRUE)) %>%
    #   mutate(prop_sau_catch = country_total_tons_by_species/region_total_tons) %>%
    #   select(-region_total_tons) %>%
    #   filter(country_total_tons_by_species > 0)
    
    total_region_catch <- sau_catch %>%
      filter(zone == regions[i]) %>%
      group_by(zone, year, flag, species) %>%
      summarise(total_tons_by_species = sum(sau_catch_tons, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      group_by(zone, year) %>%
      mutate(region_total_tons = sum(total_tons_by_species, na.rm = TRUE)) %>%
      mutate(prop_sau_catch = total_tons_by_species/region_total_tons) %>%
      select(-region_total_tons) %>%
      filter(total_tons_by_species > 0)
    
    # 1.2 Summarise non-broadcasting emissions by Region #
    sau_summary_non_broadcasting <- emissions_partitioned_grouped %>%
      # Filter for Region # (to account for emissions later)
      filter(zone == regions[i]) %>%
      # Select non-broadcasting emissions
      filter(flag == "DARK") %>%
      # Rename columns for non-broadcasting emissions
      mutate(non_broad_co2_mt = emissions_co2_mt,
             non_broad_ch4_mt = emissions_ch4_mt,
             non_broad_n2o_mt = emissions_n2o_mt,
             non_broad_nox_mt = emissions_nox_mt,
             non_broad_sox_mt = emissions_sox_mt,
             non_broad_co_mt = emissions_co_mt,
             non_broad_vocs_mt = emissions_vocs_mt,
             non_broad_pm2_5_mt = emissions_pm2_5_mt,
             non_broad_pm10_mt = emissions_pm10_mt) %>%
      # Keep only desired columns
      select(zone, 
             year,
             contains("non_broad_"))
    
    # TABLE 1: Allocate non-broadcasting emissions to FAO reporting countries by Region #
    total_region_non_broad_allocation <- full_join(total_region_catch, sau_summary_non_broadcasting, by = c("zone", "year")) %>%
      mutate(dist_non_broad_co2_mt = ifelse(is.na(prop_sau_catch), non_broad_co2_mt, prop_sau_catch * non_broad_co2_mt),
             dist_non_broad_ch4_mt = ifelse(is.na(prop_sau_catch), non_broad_ch4_mt, prop_sau_catch * non_broad_ch4_mt),
             dist_non_broad_n2o_mt = ifelse(is.na(prop_sau_catch), non_broad_ch4_mt, prop_sau_catch * non_broad_n2o_mt),
             dist_non_broad_nox_mt = ifelse(is.na(prop_sau_catch), non_broad_nox_mt, prop_sau_catch * non_broad_nox_mt),
             dist_non_broad_sox_mt = ifelse(is.na(prop_sau_catch), non_broad_sox_mt, prop_sau_catch * non_broad_sox_mt),
             dist_non_broad_co_mt = ifelse(is.na(prop_sau_catch), non_broad_co_mt, prop_sau_catch * non_broad_co_mt),
             dist_non_broad_vocs_mt = ifelse(is.na(prop_sau_catch), non_broad_vocs_mt, prop_sau_catch * non_broad_vocs_mt),
             dist_non_broad_pm2_5_mt = ifelse(is.na(prop_sau_catch), non_broad_pm2_5_mt, prop_sau_catch * non_broad_pm2_5_mt),
             dist_non_broad_pm10_mt = ifelse(is.na(prop_sau_catch), non_broad_pm10_mt, prop_sau_catch * non_broad_pm10_mt)
      ) %>%
      select(zone,
             year,
             flag,
             species,
             total_tons_by_species,
             contains("dist_non_broad"))
    
    # Account for emissions transfer in TABLE 1
    before <- sum(sau_summary_non_broadcasting$non_broad_co2_mt, na.rm = TRUE) 
    after <- sum(total_region_non_broad_allocation$dist_non_broad_co2_mt, na.rm = TRUE) 
    
    percent_diff <- ((after-before)/before)*100
    
    # Warning if emissions loss is greater than 0.001%
    if(percent_diff > 0.001) {
      warning(paste0("Greater than 0.001% of non-broadcasting emissions lost during allocation to SAU reporting countries in Region ", regions[i], ". Check Table 1."))
    }
    
    # -------- TABLE 2 --------
    # 2.1: All flags with broadcasting emissions by Region #
    emissions_partitioned <- emissions_partitioned_grouped %>%
      # Filter for Region #
      filter(zone == regions[i]) %>%
      # Remove non-broadcasting emissions
      filter(!flag == "DARK") %>%
      mutate(broad_co2_mt = emissions_co2_mt,
             broad_ch4_mt = emissions_ch4_mt,
             broad_n2o_mt = emissions_n2o_mt,
             broad_nox_mt = emissions_nox_mt,
             broad_sox_mt = emissions_sox_mt,
             broad_co_mt = emissions_co_mt,
             broad_vocs_mt = emissions_vocs_mt,
             broad_pm2_5_mt = emissions_pm2_5_mt,
             broad_pm10_mt = emissions_pm10_mt) %>%
      select(-c(contains("emissions_")))
       # emissions_co2_mt, emissions_ch4_mt, emissions_n2o_mt, emissions_nox_mt, emissions_sox_mt, emissions_co_mt, emissions_vocs_mt, emissions_pm2_5_mt, emissions_pm10_mt))
    
    # 2.2 Distribute broadcasting for countries with catch ASSUMPTION: 1 ton = same emissions for all species
    total_flag_catch <- sau_catch %>%
      filter(zone == regions[i]) %>%
      group_by(zone, year, flag, species) %>%
      summarise(country_total_tons_by_species = sum(sau_catch_tons, na.rm = TRUE), .groups = "drop") %>%
      ungroup() %>%
      group_by(zone, year, flag) %>%
      mutate(country_total_tons = sum(country_total_tons_by_species, na.rm = TRUE)) %>%
      mutate(prop_species_catch = country_total_tons_by_species/country_total_tons) %>%
      filter(country_total_tons_by_species > 0) %>%
      select(-c(country_total_tons)) %>% #country_total_tons_by_species
      filter(!is.na(prop_species_catch))
    
    # TABLE 2
    total_region_broad_allocation <- full_join(total_flag_catch, emissions_partitioned, by = c("zone", "year", "flag")) %>%
      mutate(dist_broad_co2_mt = ifelse(is.na(prop_species_catch), broad_co2_mt, prop_species_catch * broad_co2_mt),
             dist_broad_ch4_mt = ifelse(is.na(prop_species_catch), broad_ch4_mt, prop_species_catch * broad_ch4_mt),
             dist_broad_n2o_mt = ifelse(is.na(prop_species_catch), broad_n2o_mt, prop_species_catch * broad_n2o_mt),
             dist_broad_nox_mt = ifelse(is.na(prop_species_catch), broad_nox_mt, prop_species_catch * broad_nox_mt),
             dist_broad_sox_mt = ifelse(is.na(prop_species_catch), broad_sox_mt, prop_species_catch * broad_sox_mt),
             dist_broad_co_mt = ifelse(is.na(prop_species_catch), broad_co_mt, prop_species_catch * broad_co_mt),
             dist_broad_vocs_mt = ifelse(is.na(prop_species_catch), broad_vocs_mt, prop_species_catch * broad_vocs_mt),
             dist_broad_pm2_5_mt = ifelse(is.na(prop_species_catch), broad_pm2_5_mt, prop_species_catch * broad_pm2_5_mt),
             dist_broad_pm10_mt = ifelse(is.na(prop_species_catch), broad_pm10_mt, prop_species_catch * broad_pm10_mt)) %>%
      select(-country_total_tons_by_species)
    
    
    # -------- TABLE 3 --------
    # TABLE 3: Full join Tables 1 and 2 (to not lose flags with non-broad emissions but no broad emissions)
    total_region_emissions <- full_join(total_region_broad_allocation, total_region_non_broad_allocation, by = c("zone", "year", "flag", "species")) %>%
      #filter(zone == 18) %>%
      # Sum broadcasting and non-broadcasting emissions
      mutate(total_co2_mt = rowSums(across(c(dist_broad_co2_mt, dist_non_broad_co2_mt)), na.rm = TRUE),
             total_ch4_mt = rowSums(across(c(dist_broad_ch4_mt, dist_non_broad_ch4_mt)), na.rm = TRUE),
             total_n2o_mt = rowSums(across(c(dist_broad_n2o_mt, dist_non_broad_n2o_mt)), na.rm = TRUE),
             total_nox_mt = rowSums(across(c(dist_broad_nox_mt, dist_non_broad_nox_mt)), na.rm = TRUE),
             total_sox_mt = rowSums(across(c(dist_broad_sox_mt, dist_non_broad_sox_mt)), na.rm = TRUE),
             total_co_mt = rowSums(across(c(dist_broad_co_mt, dist_non_broad_co_mt)), na.rm = TRUE),
             total_vocs_mt = rowSums(across(c(dist_broad_vocs_mt, dist_non_broad_vocs_mt)), na.rm = TRUE),
             total_pm2_5_mt = rowSums(across(c(dist_broad_pm2_5_mt, dist_non_broad_pm2_5_mt)), na.rm = TRUE),
             total_pm10_mt = rowSums(across(c(dist_broad_pm10_mt, dist_non_broad_pm10_mt)), na.rm = TRUE))
    
    # Keep desired columns
    total_region_emissions <- total_region_emissions %>%
      select(zone,
             year,
             flag,
             species,
             #total_tons_by_species,
             contains("total_"))
             # total_co2_mt,
             # total_ch4_mt,
             # total_n2o_mt,
             # total_nox_mt,
             # total_sox_mt,
             # total_co_mt,
             # total_vocs_mt,
             # total_pm2_5_mt,
             # total_pm10_mt
    
    # Account for emissions after full join
    before <- sum(emissions_partitioned$broad_co2_mt, na.rm = TRUE) + sum(sau_summary_non_broadcasting$non_broad_co2_mt, na.rm = TRUE)
    after <- sum(total_region_emissions$total_co2_mt, na.rm = TRUE) 
    
    percent_diff <- ((after-before)/before)*100
    
    # Warning if emissions loss is greater than 0.001%
    if(percent_diff > 0.001) {
      warning(paste0("Greater than 0.001% of emissions lost during full join of broadcasting and non-broadcasting in Region ", regions[i], ". Check Table 3."))
    }
    
    # Append to vector
    option_1[[i]] <- total_region_emissions
  }
  
  # Full emissions dataset
  full_emissions_sau <- bind_rows(option_1)
  
  # Save SAU emissions dataset to workbench-2
  write_csv(full_emissions_sau, file.path("/capstone/seamissions/checkpoint/full_emissions_sau.csv"))
  
  return(full_emissions_sau)
  
}