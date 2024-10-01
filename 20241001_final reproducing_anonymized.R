# Clear the work space-----------------------------------------------------------
rm(list=ls())

# Record the start time for benchmarking----------------------------------------
start_time <- Sys.time()

# Function: create sub folder ---------------------------------------------------
make_folder <- function(name) {
  if (!dir.exists(here::here(name))) {
    dir.create(here::here(name))
  }
}
# meantion this in all following save option 


# Call the function to create the output folder
make_folder("20240101_ann_output_sso")


# Function: quick-and-easy ggplot saver ----------------------------------------
#   SVG is a vector format (can be enlarged without losing resolution)
ggsave_svg <- function(ggp, output = "output", ...) {
  ggplot2::ggsave(filename = paste(substitute(ggp),".svg", sep=""), 
                  device = "svg", plot = ggp, path = output, 
                  limitsize = TRUE, ...)
}

# Function to save as png-------------------------------------------------------
ggsave_png <- function(ggp, output = "output", width = 8, height = 6, dpi = 1200) {
  ggplot2::ggsave(
    filename = paste0(deparse(substitute(ggp)), ".png", sep = ""),
    device = "png",
    plot = ggp,
    path = output,
    width = width,
    height = height,
    dpi = dpi,
    limitsize = TRUE
    
  )
}

# Read and Preprocess SSO Data--------------------------------------------------
# Note: The original dataset was processed to anonymize observer names and make certain corrections for consistency and completeness.
# Only this anonymized and cleaned dataset is shared for review & reproducibility and analysis to ensure the privacy of observers is maintained.

# Load the anonymized data for subsequent analyses. 
df_sso_data <- read.csv(here::here("./data_anonymized/20240919_sso_data_anon.csv"))

df_sso_data <- df_sso_data |>
  (\(.) dplyr::filter(., rowSums(is.na(.)) / ncol(.) < 0.9))()

# Correct typos/errors in the street_segment values
df_sso_data$street_segment <- dplyr::recode(df_sso_data$street_segment, 
                                         "153-145282" = "145281", 
                                         "153-145505" = "145505",
                                         "153-610650" = "610650", 
                                         "153-559431" = "559431",
                                         "153-559431" = "559431",
                                         "359614" = "358614",
                                         "146770" = "140770",
                                         "3555744" = "358744",
                                         "1420209" = "142029",
                                         "14087" = "140487",
                                         "151591" = "141591",
                                         "610682" = "610681",
                                         "140882" = "140822",
                                         "140444" = "140777",
                                         "559 475" = "559475",
                                         "141137" = "141131",
                                         "130612" = "140612",
                                         "207116" = "267116",
                                         "145009" = "145609",
                                         "14199" = "141499",
                                         "627914"=  "145383",
                                         "141360" = "141366",
                                         "141775" = "145755",
                                         "141154" = "145755",
                                         "140445" = "140498",
                                         "44-158"=  "145984",
                                         "44-185"=  "145984",
                                         "92-107"=  "145796",
                                         "2-24A" =  "358849",
                                         "3(A)-18"=  "145632",
                                         "30-72E" =  "145852",
                                         "146945" = "140975",
                                         "627914" =  "145383",
                                         "3935" = "3936",
                                         "369113" = "359113")

# Converting variables as numeric and replacing NAs with zeros 
df_sso_data <- df_sso_data |>
  
  # Convert the specified columns to numeric type first
  dplyr::mutate(
    dplyr::across(
      c(no_of_graffiti, bars, liquor_stores, night_shops, night_clubs, 
        restaurants, shops, hotels_or_hostels, offices, public_parking_lots, 
        bus_or_tram_stops, schools, residences, surveillance_cameras, police_station, 
        trees_along_street, public_seating, flower_beds, public_restrooms),
      ~ base::as.numeric(.x)  # Convert the specified columns to numeric
    )
  ) |>
  
  # Replacing NAs with "0" across the specified columns
  dplyr::mutate(
    dplyr::across(
      c(no_of_graffiti, bars, liquor_stores, night_shops, night_clubs, 
        restaurants, shops, hotels_or_hostels, offices, public_parking_lots, 
        bus_or_tram_stops, schools, residences, surveillance_cameras, police_station, 
        trees_along_street, public_seating, flower_beds, public_restrooms), 
      ~ tidyr::replace_na(.x, 0)  # Replace NA values with 0
    )
  )



# Inter-rater reliability analysis for multi-coded street segments--------------
SSO_multiple_coded_long <- df_sso_data |>
  
  # Group by street segment
  dplyr::group_by(street_segment) |>
  
  # Filter for street segments that have been coded by multiple observers
  dplyr::filter(dplyr::n() > 1) |>
  
  # Group by street segment and observer ID to ensure the same observer didn't code a segment multiple times
  dplyr::group_by(street_segment, observer_id) |>
  dplyr::filter(dplyr::n() == 1) |>
  
  # Re-group by only street segment and filter again for segments coded by multiple observers
  # This step is necessary because the previous filtering may have left segments that are now only coded once
  dplyr::group_by(street_segment) |>
  dplyr::filter(dplyr::n() > 1) |>
  
  # Arrange data by street segment and observer for readability
  dplyr::arrange(street_segment, observer_id) |>
  dplyr::ungroup()


# Viewing a subset of the anonymized, multi-coded street segment data
SSO_multiple_coded_long |>
  
  # Select specific columns of interest
  dplyr::select(street_segment, observer_id, no_of_graffiti) |>
  
  # Display the first 10 rows for a quick overview
  head(10)

# Analyzing the number of street segments coded by each observer
segments_per_observer <- SSO_multiple_coded_long |>
  # Group by observer ID
  dplyr::group_by(observer_id) |>
  
  # Count the number of segments each observer coded
  dplyr::summarize(segments_coded = dplyr::n(), .groups = "drop") |>
  
  # Group by the number of segments coded
  dplyr::group_by(segments_coded) |>
  
  # Count the number of observers that coded that many segments
  dplyr::summarize(observer_id = dplyr::n())

# Display the results
print(segments_per_observer, n = Inf)

# Analyzing the frequency with which each street segment was coded by observers
observers_per_segment <- SSO_multiple_coded_long |>
  
  # Group by street segment
  dplyr::group_by(street_segment) |>
  
  # Count the number of times each segment was coded
  dplyr::summarize(times_coded = dplyr::n(), .groups = "drop") |>
  
  # Group by the number of times coded
  dplyr::group_by(times_coded) |>
  
  # Count the number of segments that were coded that many times
  dplyr::summarize(segments = dplyr::n())

# Display the results
print(observers_per_segment, n = Inf)

observers_code_multi_segment <-length(unique(SSO_multiple_coded_long$observer_id))
segment_code_multi_segment <-length(unique(SSO_multiple_coded_long$street_segment))

multi_observers_segment <- tibble::tibble(
  num_multiple_coded_segments = segment_code_multi_segment,
  num_observers_more_than_one_segment = observers_code_multi_segment)

# Define a function 'i_alpha' to calculate Krippendorff's alpha reliability coefficient
i_alpha <- function(x) {
  
  # Use the dataset 'SSO_multiple_coded_long' and prepare it for alpha computation
  data_for_alpha <-
    SSO_multiple_coded_long |>
    
    # Select relevant columns: street_segment, observer_id and the variable specified by 'x'
    dplyr::select(street_segment, observer_id, {
      {
        x
      }
    }) |>
    
    # Pivot the data to a wider format to have one column per observer
    tidyr::pivot_wider(
      names_from = "observer_id",
      values_from = {
        {
          x
        }
      },
      names_prefix = "obs_"
    ) |>
    
    # Keep only the observer columns
    dplyr::select(dplyr::starts_with("obs_")) |>
    
    # Convert the dataframe to a matrix and transpose it
    base::as.matrix() |>
    base::t()
  
  # Using the 'icr::krippalpha' function, compute Krippendorff's alpha for the data matrix
  alpha_output <-
    icr::krippalpha(
      data_for_alpha,
      metric = "interval",   # Define the level of measurement as interval
      bootstrap = TRUE,      # Enable bootstrap sampling
      bootnp = FALSE         # Do not show bootstrap progress bar
    )
  
  # Calculate the lower and upper bounds of the 95% confidence interval from the bootstrap results
  ci_lower <- stats::quantile(alpha_output$bootstraps, 0.025, na.rm = TRUE)
  ci_upper <- stats::quantile(alpha_output$bootstraps, 0.975, na.rm = TRUE)
  
  # Return the calculated alpha and the confidence interval in a formatted tibble
  tibble::tibble(
    varname  = rlang::as_name(quote({
      {
        x
      }
    })),  # Extract the name of the variable 'x' and store it
    alpha    = alpha_output$alpha,     # Alpha coefficient
    ci_lower = ci_lower,               # Lower bound of the 95% CI
    ci_upper = ci_upper                # Upper bound of the 95% CI
  )
}

# Variables to compute Krippendorff's alpha for
variables_to_check <- c(
  "no_of_graffiti", "bars", "liquor_stores", "night_shops", "night_clubs", 
  "restaurants", "shops", "hotels_or_hostels", "offices", "bus_or_tram_stops", 
  "schools", "residences", "surveillance_cameras", "police_station", 
  "public_parking_lots", "trees_along_street", "public_seating", 
  "flower_beds", "public_restrooms"
)

# Compute alpha for each variable and collate results
inter_rater_reliability <- 
  purrr::map_dfr(variables_to_check, i_alpha) |> 
  dplyr::arrange(desc(alpha)) |> 
  dplyr::mutate(dplyr::across(where(is.numeric), round, digits = 3))|>
  print(n = Inf)

actual_names <- c(
  "no_of_graffiti" = "Number of graffiti",
  "bars" = "Bars",
  "liquor_stores" = "Shops selling liquor",
  "night_shops" = "Night shops",
  "night_clubs" = "Night clubs",
  "restaurants" = "Restaurants",
  "shops" = "Shops",
  "hotels_or_hostels" = "Hotels or hostels",
  "offices" = "Offices",
  "public_parking_lots" = "Public parking lots",
  "bus_or_tram_stops" = "Bus or tram stops",
  "schools" = "Schools",
  "residences" = "Residential Units",
  "surveillance_cameras" = "Surveillance cameras",
  "police_station" = "Police station",
  "trees_along_street" = "Trees",
  "public_seating" = "Public Seating",
  "flower_beds" = "Plantings or rlower beds",
  "public_restrooms" = "Public restrooms"
)

# Apply the renaming
inter_rater_reliability$varname <- actual_names[inter_rater_reliability$varname]

# Print the updated inter_rater_reliability
inter_rater_reliability

# Save the results as a CSV file
write.csv(inter_rater_reliability, "20240101_ann_output_sso/20240919_inter_rater_reliability.csv")


# Remove duplicates-------------------------------------------------------------
# set seed for reproducibility
set.seed(123456789)

# Using slice_sample from dplyr to get a random row for each street segment
df_sso_data_new <- dplyr::slice_sample(df_sso_data, n = 1, by = "street_segment")

# Read sf file for city center street segment data 
sf_ghent_street_segments <- sf::st_read(dsn = here::here("data_anonymized", "sf_ghent_street_segments.shp"))

# find the street segment that is not matching with the street segment data 
# this might be possible typos and remove it
# Extract the relevant columns
UIDN_Ghent <- sf_ghent_street_segments$UIDN  # UIDN from sf_ghent_street_segments
UIDN_SSO <- df_sso_data_new$street_segment      # street_segment from df_sso_data_new

# Filter df_sso_data_new to keep only rows where the street_segment is in the UIDN from sf_ghent_street_segments
df_sso_data_new <- df_sso_data_new |>
  dplyr::filter(street_segment %in% UIDN_Ghent)


# Define a custom theme to be used in ggplot2 visualizations
custom_theme <- ggplot2::theme_minimal() +
  
  # Use a minimal base theme and add some custom modifications
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "white", color = NA), # Set the plot background to white
    axis.text = ggplot2::element_blank(), # Remove axis text
    axis.ticks = ggplot2::element_blank() # Remove axis ticks
  )


# Adjust the order of labels in the LBLTYPE column for better plotting
sf_ghent_street_segments$LBLTYPE <-
  factor(
    sf_ghent_street_segments$LBLTYPE,
    levels = c("street segment", "intersection", "bridge") # Define the order for segment types
  )

# Plots-------------------------------------------------------------------------
# Create a ggplot2 map visualization of the Ghent street segments
ghent_map_grey <- ggplot2::ggplot() +
  
  # Use the spatial dataframe to plot each segment with its corresponding type color
  ggplot2::geom_sf(data = sf_ghent_street_segments, ggplot2::aes(fill = LBLTYPE)) +
  ggplot2::scale_fill_manual(
    
    # Manually specify the colors for each segment type
    values = c(
      "street segment" = "grey60",
      "intersection" = "grey40",
      "bridge" = "grey20"
    ),
    labels = c("Street segment", "Intersection", "Bridges"), # Rename legend labels
    name = "Segment type" # Legend title
  ) +
  custom_theme + # Apply the custom theme defined earlier
  ggplot2::theme(
    # Further refine the appearance of the legend
    legend.text = ggplot2::element_text(size = 8),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.1, 0.1),
    legend.justification = c(0, 0),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)  # Add a scale bar at the bottom left

# Save the created map to a .svg & png file in the "20240101_ann_output_sso" directory
ghent_map_grey |> ggsave_svg(output = "20240101_ann_output_sso")
ghent_map_grey |> ggsave_png(output = "20240101_ann_output_sso")


# Mark street segments as 'Observed' or 'Not Observed' in the shape file data
sf_ghent_street_segments$observed <-
  sf_ghent_street_segments$UIDN %in% df_sso_data_new$street_segment
sf_ghent_street_segments$observed <-
  base::factor(
    sf_ghent_street_segments$observed,
    levels = c(TRUE, FALSE),
    labels = c("Observed", "Not Observed")
  )

# Calculate the counts for observed and not observed segments
count_observed <-
  base::sum(sf_ghent_street_segments$observed == "Observed")
count_not_observed <-
  base::sum(sf_ghent_street_segments$observed == "Not Observed")

# Create a plot of observed vs. not observed street segments
Obs_not_obs <- ggplot2::ggplot(sf_ghent_street_segments) +
  ggplot2::geom_sf(ggplot2::aes(fill = observed)) +
  ggplot2::scale_fill_manual(
    values = c(
      "Observed" = "grey20",
      "Not Observed" = "grey60"
    ),
    name = "Street segment",
    labels = c(
      base::paste0("Observed ", "(", count_observed, ")"),
      base::paste0("Not Observed ", "(", count_not_observed, ")")
    )
  ) +
  ggplot2::labs(title = "Street Segments: Observed and not Observed") +
  custom_theme + # Assuming custom_theme has been predefined
  ggplot2::theme(
    legend.text = ggplot2::element_text(size = 8),
    legend.key.size = grid::unit(0.5, "lines"),
    legend.position = c(0.2, 0.2),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    legend.box.background = ggplot2::element_rect(fill = "transparent")
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5)

# Save the generated plot as SVG and PNG formats
Obs_not_obs |> ggsave_svg(output = "20240101_ann_output_sso")
Obs_not_obs |> ggsave_png(output = "20240101_ann_output_sso")
Obs_not_obs


# Load the large shape file------------------------------------------------------
# Copy the CRS from WBN/bridges
copied_crs <- sf::st_crs(sf_ghent_street_segments)

# Read, transform, and bind large files together
sf_large_file <- sf::st_read(dsn = here::here("data_anonymized", "sf_large_file_new_combined.shp")) |>
  sf::st_transform(copied_crs) |>
  dplyr::mutate(
    LBLTYPE = dplyr::case_when(
      LBLTYPE == "kruispuntzone" ~ "intersection",
      LBLTYPE == "wegsegment" ~ "street segment",
      LBLTYPE == "overbrugging" ~ "bridge",
      TRUE ~ LBLTYPE  # Keep original label for other cases
    )
  )
  

# Extend bounding box to address edge effects  bbox <- sf::st_bbox(sf_ghent_street_segments)
bbox <- sf::st_bbox(sf_ghent_street_segments)
bbox_expanded <- bbox + c(-1000, -1000, 1000, 1000)
sf_large_file_cropped <- sf::st_crop(sf_large_file, bbox_expanded)

# Inspect the result
#plot(sf_large_file_cropped["geometry"], reset = FALSE)

# Network analysis--------------------------------------------------------------
adj_file <- here::here("./data/extened_adjacent_within5m.rds") 

# Check if the file already exists
if (file.exists(adj_file)) {
  
  # If the file exists, load the saved adjacency list
  extened_adjacent_within5m <- readRDS(file = adj_file)
  message("Loaded extened_adjacent_within5m from saved file.")
  
} else {
  
  # If the file does not exist, calculate the adjacency list
  extened_adjacent_within5m <- sf::st_is_within_distance(
    x = sf_large_file_cropped,
    
    # The y argument can be omitted but including it
    #   better shows what is happening
    y = sf_large_file_cropped,
    
    # Distance is measured in unit of the CRS (meter)
    dist = 5,
    
    # A segment must not be connected to itself
    remove_self = TRUE
  )
  
  # Save the calculated adjacency list to the file
  saveRDS(extened_adjacent_within5m, file = adj_file)
  message("Calculated and saved extened_adjacent_within5m to file.")
}

# adjacent_within5m is a list of length nrow(ghent_street_segments_sf) in
#   which each element is a vector if indices to the street segments the focal
#   segment is connected to (is within 5m)

# Create a street segment network from adjacency matrix
cropped_ghent_street_network <-
  igraph::graph_from_adj_list(adjlist = extened_adjacent_within5m,
                              mode = "total",
                              duplicate = TRUE)

# Compute centrality measures for street segments
sf_croped_ghent_street_network <-
  sf_large_file_cropped |>
  dplyr::bind_cols(
    tibble::tibble(
      betweenness_norm = igraph::betweenness(
        cropped_ghent_street_network,
        directed = FALSE,
        normalized = TRUE
      ),
      betweenness_pure = igraph::betweenness(
        cropped_ghent_street_network,
        directed = FALSE,
        normalized = FALSE
      ),
      closeness_norm = igraph::closeness(cropped_ghent_street_network, normalized = TRUE),
      closeness_pure = igraph::closeness(cropped_ghent_street_network, normalized = FALSE),
      degree_norm = igraph::degree(cropped_ghent_street_network, normalized = TRUE),
      degree_pure = igraph::degree(cropped_ghent_street_network, normalized = FALSE),
      eigenvector = igraph::evcent(cropped_ghent_street_network)$vector
    )
  )


# Calculate and display correlations between centrality measures
correlation_matrix <-
  sf_croped_ghent_street_network |>
  sf::st_drop_geometry() |>
  dplyr::select(degree_pure, closeness_pure, betweenness_pure, eigenvector) |>
  cor()

print(correlation_matrix)

# Process and merge the data sets----------------------------------------------

# Calculate the area of each street segment in the merged sf object
sf_croped_ghent_street_network$area <-
  sf_croped_ghent_street_network |> sf::st_area() |> base::as.numeric()

# Convert area from square meters to a unit of 10 square meters for easier interpretation
sf_croped_ghent_street_network$area_10m2 <-
  sf_croped_ghent_street_network$area / 10 ^ 2

# Convert sf objects to data frames for further analysis
df_croped_ghent_street_network <-base::as.data.frame(sf_croped_ghent_street_network)

# Remove rows with duplicate UIDN values, keeping only the first occurrence
df_croped_ghent_street_network <- df_croped_ghent_street_network[!duplicated(df_croped_ghent_street_network$UIDN), ]

df_ghent_street_segments <- base::as.data.frame(sf_ghent_street_segments)

# Merge the two datasets based on the "UIDN" identifier
df_ghent_street_network_joined <-
  base::merge(
    df_ghent_street_segments,
    df_croped_ghent_street_network,
    by = "UIDN",
    all.x = TRUE
  )

# Merge the df_sso_data_new dataframe with the sf_ghent_street_network_joined spatial dataframe
# using the street_segment and UIDN columns as keys
df_merged_data_normal <-
  base::merge(
    df_sso_data_new,
    df_ghent_street_network_joined,
    by.x = "street_segment",
    by.y = "UIDN",
    all.x = TRUE
  )

# Convert LBLTYPE.y column to a factor datatype for better categorization
df_merged_data_normal$LBLTYPE.y <-
  base::as.factor(df_merged_data_normal$LBLTYPE.y)

df_merged_data_normal$no_of_graffiti <-as.numeric(df_merged_data_normal$no_of_graffiti)

# Normalize betweenness_norm, filter out rows with NA values in betweenness_norm,
# and rescale the residences column by dividing it by 10
df_merged_data_normal <-
  df_merged_data_normal |>
  
  # remove cases with missing values on betweenness (islands?)
  dplyr::filter(!is.na(betweenness_norm)) |>
  
  # re-normalize 'betweenness_norm' and rescale 'residences' columns
  dplyr::ungroup() |>
  dplyr::mutate(
    betweenness_renormalized =
      (betweenness_norm - mean(betweenness_norm)) /
      sd(betweenness_norm),
    residences_10 = residences / 10
  )


# Convert the processed dataframe into a spatial dataframe
sf_merged_data_normal <- sf::st_as_sf(df_merged_data_normal)


# Adjusting the levels of the LBLTYPE.y column in the df_merged_data_normal dataframe
df_merged_data_normal$LBLTYPE.y <-
  relevel(df_merged_data_normal$LBLTYPE.y, ref = "bridge")

# Create binary variables indicating segment types: bridge, street segment, and intersection
df_merged_data_normal <-
  dplyr::mutate(
    df_merged_data_normal,
    bridge = dplyr::if_else(LBLTYPE.y == "bridge", 1, 0),
    street_segment_01 = dplyr::if_else(LBLTYPE.y == "street segment", 1, 0),
    intersection = dplyr::if_else(LBLTYPE.y == "intersection", 1, 0)
  )


# Observer, street segment and graffiti count-----------------------------------
# Calculate the statistics
unique_observers <- length(unique(df_sso_data$observer_id))
unique_street_segments <- length(unique(df_merged_data_normal$street_segment))
total_graffiti <- sum(df_merged_data_normal$no_of_graffiti, na.rm = TRUE)

# Create a data frame with the results
df_basic_summary <- data.frame(
  unique_observers = unique_observers,
  unique_street_segments = unique_street_segments,
  total_graffiti = total_graffiti
)

print(df_basic_summary)
# Save the data frame to a CSV file
write.csv(df_basic_summary, "./20240919_output_sso/basic_summary.csv", row.names = FALSE)
# Summary statistics------------------------------------------------------------
# Define a function to compute summary statistics
summary_stats <- function(x) {
  stats <- c(
    min = round(min(x, na.rm = TRUE), 2),
    mean = round(mean(x, na.rm = TRUE), 2),
    median = round(median(x, na.rm = TRUE), 2),
    max = round(max(x, na.rm = TRUE), 2),
    sd = round(sd(x, na.rm = TRUE), 2),
    percentage_of_zeros = round(sum(x == 0, na.rm = TRUE) / length(x) * 100, 2)
  )
  return(stats)
}


# Define a list of variables for which we want to calculate statistics
variables <- c(
  "no_of_graffiti",
  "night_clubs",    
  "night_shops",
  "bars",         
  "schools",
  "restaurants",
  "shops",        
  "trees_along_street",   
  "residences",     
  "area_10m2",
  "bridge",
  "intersection",
  "street_segment_01"
)

# Calculate summary statistics for each variable
variable_stats <- sapply(variables, function(v) summary_stats(df_merged_data_normal[[v]]))

# Transpose for clarity and convert to data frame
variable_stats <- t(variable_stats)  
variable_stats <- as.data.frame(variable_stats)

# Rename, rearrange, and round values for better clarity and presentation
variable_stats <- 
  variable_stats |>
  tibble::rownames_to_column(var = "variable") |>
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "no_of_graffiti" ~ "Number of graffiti",
      variable == "night_clubs" ~ "Night clubs",
      variable == "night_shops" ~ "Night shops",
      variable == "bars" ~ "Bars", 
      variable == "schools" ~ "Schools",
      variable == "restaurants" ~ "Restaurants",
      variable == "shops" ~ "Shops",
      variable == "trees_along_street" ~ "Trees",
      variable == "residences" ~ "Residential Units",
      variable == "area_10m2" ~ "Area (10m2)",
      variable == "bridge" ~ "Bridge",
      variable == "intersection" ~ "Intersection",
      variable == "street_segment_01" ~ "Street Segments",
      TRUE ~ variable
    )
  ) |>
  dplyr::arrange(dplyr::desc(percentage_of_zeros)) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), base::round, 3))

# View the final result
print(variable_stats)

# Save the results as a CSV file
write.csv(variable_stats, "20240101_ann_output_sso/20240919_variable_stats.csv")

# Table 1---------------------------------------------------------------------
table_1 <- variable_stats |>
  # Merge with inter_rater_reliability
  dplyr::left_join(inter_rater_reliability, by = c("variable" = "varname")) |>
  # rename columns 
  dplyr::select(
    Variables = variable,
    Min = min,
    Mean = mean,
    Median = median,
    Max = max,
    SD = sd,
    `0%` = percentage_of_zeros,
    `K_alpha` = alpha,              
    `K_alpha 95% CI Low` = ci_lower,
    `K_alpha 95% CI Up` = ci_upper  
  ) |>
  # Manually reorder based on the percentage of zero order of variables
  dplyr::mutate(Variables = factor(Variables, levels = c(
    "Number of graffiti", "Night clubs", "Night shops", "Bars", "Schools",
    "Restaurants", "Shops", "Residential Units", "Trees",
    "Area (10m2)", "Bridge", "Intersection", "Street Segments"
  ))) |>
  dplyr::arrange(Variables)

# Display the final table
print(table_1)

# Write the table to CSV
write.csv(table_1, "20240919_output_sso/table_1.csv", row.names = FALSE)

# Calculate total graffiti count for reference
total_graffiti <- sum(df_merged_data_normal$no_of_graffiti, na.rm = TRUE)
total_graffiti

# Poisson model-----------------------------------------------------------------
# MODEL 1: Poisson regression considering the effect of segment area, type of segment, and betweenness on graffiti numbers
poisson_model_1 <-
  glm(
    no_of_graffiti ~ area_10m2 + LBLTYPE.y + betweenness_renormalized,
    family = "poisson",
    data = df_merged_data_normal
  )
summary(poisson_model_1)
exp(coef(poisson_model_1))

# Extracting model details using broom::tidy() and calculating Odds Ratios
results_model_1 <- broom::tidy(poisson_model_1)
results_model_1$Odds_Ratio <- exp(results_model_1$estimate)

# Calculating 95% Confidence Interval for Odds Ratios
lower_log_odds1 <-
  results_model_1$estimate - (1.96 * results_model_1$std.error)
upper_log_odds1 <-
  results_model_1$estimate + (1.96 * results_model_1$std.error)
results_model_1$lower_OR <- exp(lower_log_odds1)
results_model_1$upper_OR <- exp(upper_log_odds1)
results_model_1 <- results_model_1 |>  dplyr::rename(variable = term)
results_model_1

# MODEL 2: Poisson regression with all  predictors
poisson_model_2 <-
  glm(
    no_of_graffiti ~ residences_10 +  
      bars + restaurants + night_shops + night_clubs + shops + schools + 
      LBLTYPE.y +  area_10m2 + trees_along_street + 
      betweenness_renormalized, family = "poisson", data = df_merged_data_normal
  )


# Summarizing the model and viewing exponentiated coefficients
summary(poisson_model_2)
exp(coef(poisson_model_2))

# Extracting detailed model information using broom::tidy() and calculating Odds Ratios
results_model_2 <- broom::tidy(poisson_model_2)
results_model_2$Odds_Ratio <- exp(results_model_2$estimate)

# Calculating the 95% Confidence Interval for Odds Ratios
lower_log_odds2 <-
  results_model_2$estimate - (1.96 * results_model_2$std.error)
upper_log_odds2 <-
  results_model_2$estimate + (1.96 * results_model_2$std.error)
results_model_2$lower_OR <- exp(lower_log_odds2)
results_model_2$upper_OR <- exp(upper_log_odds2)
results_model_2 <- results_model_2 |>  dplyr::rename(variable = term)
results_model_2

# Check the VIF
vif_values_1 <- car::vif(poisson_model_1)
print(vif_values_1)

vif_values_2 <- car::vif(poisson_model_2)
print(vif_values_2)

# Formatting the results ---------------------------------------------------------
# Formatting function to represent numbers with 3 decimal points
# Function to format numeric values to three decimal places
format_to_3dp <- function(x) {
  sapply(x, function(y) {
    if (is.character(y)) y <- as.numeric(y)  # Convert character to numeric if necessary
    ifelse(is.na(y), NA, sprintf("%.3f", y))  # Format to three decimals or keep NA
  })
}

# Format results from each model to 3 decimals
results_model_1[c("Odds_Ratio", "lower_OR", "upper_OR", "p.value")] <- 
  apply(results_model_1[c("Odds_Ratio", "lower_OR", "upper_OR", "p.value")], 2, format_to_3dp)

results_model_2[c("Odds_Ratio", "lower_OR", "upper_OR", "p.value")] <- 
  apply(results_model_2[c("Odds_Ratio", "lower_OR", "upper_OR", "p.value")], 2, format_to_3dp)

# Extract Akaike Information Criterion (AIC) for model comparison and ensure consistency as character
aic_values <- tibble::tibble(
  variable = "AIC",
  Odds_Ratio.model1 = sprintf("%.3f", poisson_model_1$aic),
  lower_OR.model1 = as.character(NA),
  upper_OR.model1 = as.character(NA),
  p.value.model1 = as.character(NA),
  Odds_Ratio.model2 = sprintf("%.3f", poisson_model_2$aic),
  lower_OR.model2 = as.character(NA),
  upper_OR.model2 = as.character(NA),
  p.value.model2 = as.character(NA)
)

# Rename and select only the necessary columns from both models
results_model_1 <- results_model_1 |>
  dplyr::rename(
    Odds_Ratio.model1 = Odds_Ratio,
    lower_OR.model1 = lower_OR,
    upper_OR.model1 = upper_OR,
    p.value.model1 = p.value
  ) |>
  dplyr::select(variable, Odds_Ratio.model1, lower_OR.model1, upper_OR.model1, p.value.model1)

results_model_2 <- results_model_2 |>
  dplyr::rename(
    Odds_Ratio.model2 = Odds_Ratio,
    lower_OR.model2 = lower_OR,
    upper_OR.model2 = upper_OR,
    p.value.model2 = p.value
  ) |>
  dplyr::select(variable, Odds_Ratio.model2, lower_OR.model2, upper_OR.model2, p.value.model2)

# Full join based on 'variable' column
joined_results <- dplyr::full_join(results_model_1, results_model_2, by = "variable")

# Ensure all columns in joined_results are character before binding rows
joined_results[] <- lapply(joined_results, as.character)

# Append the AIC row at the bottom by binding rows
joined_results_with_aic <- dplyr::bind_rows(joined_results, aic_values)

# Display the final joined results
print(joined_results_with_aic)

# Saving the results with AIC as a CSV
write.csv(joined_results_with_aic, "20240101_ann_output_sso/20240919_joined_results_with_aic.csv")

# Table 2-----------------------------------------------------------------------
# Create a mapping for renaming variables
variable_mapping <- c(
  "area_10m2" = "Area (10m2)",
  "LBLTYPE.yintersection" = "  Intersection",
  "LBLTYPE.ystreet segment" = "  Street segment",
  "betweenness_renormalized" = "Betweenness",
  "bars" = "Bars",
  "restaurants" = "Restaurants",
  "night_shops" = "Night shops",
  "night_clubs" = "Night clubs",
  "shops" = "Shops",
  "schools" = "Schools",
  "residences_10" = "Residences (10 units)",
  "trees_along_street" = "Trees",
  "AIC" = "AIC"
)

# Desired order of variables
desired_order <- c(
  "Area (10m2)", "Segment type", "  Bridge (reference)", "  Intersection", "  Street segment", 
  "Betweenness", "Bars", "Night shops", "Night clubs", "Restaurants", 
  "Shops", "Schools", "Residences (10 units)", "Trees", "AIC"
)

# Function to format odds ratios and add significance stars
add_significance_stars <- function(odds_ratio, p_value) {
  odds_ratio <- as.numeric(odds_ratio)
  if (is.na(odds_ratio)) return("")  # Return empty string if NA
  stars <- ifelse(is.na(p_value), "", ifelse(p_value < 0.01, "***", ifelse(p_value < 0.05, "**", ifelse(p_value < 0.10, "*", ""))))
  return(paste0(sprintf("%.3f", odds_ratio), stars))  # Format to 3 decimals and append stars
}

# Function to format CIs with three decimals
format_ci <- function(ci_value) ifelse(is.na(ci_value), "", sprintf("%.3f", as.numeric(ci_value)))

# Create rows for "Segment type" and "Bridge (reference)"
segment_type_row <- tibble::tibble(variable = "Segment type", Odds_Ratio.model1 = NA, lower_OR.model1 = NA, upper_OR.model1 = NA,
                                   Odds_Ratio.model2 = NA, lower_OR.model2 = NA, upper_OR.model2 = NA)

bridge_reference_row <- tibble::tibble(variable = "  Bridge (reference)", Odds_Ratio.model1 = "1", lower_OR.model1 = "1", upper_OR.model1 = "1",
                                       Odds_Ratio.model2 = "1", lower_OR.model2 = "1", upper_OR.model2 = "1")

# Process the data
table_2 <- joined_results_with_aic |>
  dplyr::filter(variable != "(Intercept)") |>
  dplyr::mutate(
    variable = dplyr::recode(variable, !!!variable_mapping),
    Odds_Ratio.model1 = mapply(add_significance_stars, Odds_Ratio.model1, p.value.model1),
    Odds_Ratio.model2 = mapply(add_significance_stars, Odds_Ratio.model2, p.value.model2),
    lower_OR.model1 = sapply(lower_OR.model1, format_ci),
    upper_OR.model1 = sapply(upper_OR.model1, format_ci),
    lower_OR.model2 = sapply(lower_OR.model2, format_ci),
    upper_OR.model2 = sapply(upper_OR.model2, format_ci)
  ) |>
  dplyr::select(-p.value.model1, -p.value.model2) |>
  dplyr::bind_rows(segment_type_row, bridge_reference_row) |>
  dplyr::mutate(variable = factor(variable, levels = desired_order)) |>
  dplyr::arrange(variable) |>
  dplyr::bind_rows(tibble::tibble(
    variable = "*** p < 0.01, ** p < 0.05, * p < 0.10", Odds_Ratio.model1 = NA, lower_OR.model1 = NA, upper_OR.model1 = NA,
    Odds_Ratio.model2 = NA, lower_OR.model2 = NA, upper_OR.model2 = NA
  )) |>
  dplyr::mutate_all(~ tidyr::replace_na(., ""))

# Display the final table
print(table_2)

# write as csv
write.csv(table_2, "20240101_ann_output_sso/table_2.csv")

# Write all output in one excel----------------------------------------------- 
# Create a list of dataframes of all the results to save as separate sheets
list_of_dfs <- list(
  "Basic Summary" = df_basic_summary,
  "multi_observers_segment" = multi_observers_segment,
  "Inter Rater Reliability" = inter_rater_reliability,
  "Variable Stats" = variable_stats,
  "Results- Model 1 & 2" = joined_results_with_aic,
  "Table 1" = table_1,
  "Table 2" = table_2
)

# Write the list of dataframes to an Excel file with multiple sheets
writexl::write_xlsx(list_of_dfs, "./20240101_ann_output_sso/all_results.xlsx")
# time taken-------------
end_time <- Sys.time()
time_taken <- end_time - start_time
time_taken

sessionInfo()
