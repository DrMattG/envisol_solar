# Load necessary libraries
library(sf)
library(tidyverse)
library(tmap)
library(terra)
library(DBI)
library(RPostgres)
library(units)

Sys.setlocale(locale = 'no_NB.utf8')
options(scipen = 999)

#install.packages("tmap")

# Load spatial data
envisol_points <- st_read("P:/412331_envisol/envisol_spatial_analysis/data/envisol_projects.gpkg")
solar_polygons <- st_read("P:/412331_envisol/envisol_spatial_analysis/data/solar_polygons.gpkg")
solar_points <- st_read("P:/412331_envisol/envisol_spatial_analysis/data/solar_points.gpkg")
my_map <- st_read("P:/412331_envisol/envisol_spatial_analysis/data/norge.gpkg")

# Connect to database and load custom geometry
con <- NinaR::postgreSQLConnect()
myQG <- "SELECT \"FYLKESNUMMER\", navn, geom FROM \"AdministrativeUnits\".\"Norway_County_Fylke_polygons\""
customGeom <- st_read(con, query = myQG)
southern_norway <- customGeom |> filter(!(FYLKESNUMMER %in% c(15, 16, 17, 18, 19, 20)))

# Load and stack rasters
continous_rasters <- rast("P:/412331_envisol/envisol_spatial_analysis/data/continous_rasters.tif")
rasters <- c(continous_rasters)

# Define legend labels and colors
legend_labels <- c("Envisol-prosjekt", "Annet bakkemontert solkraftprosjekt (NVE)")
legend_col <- c("red", "forestgreen")

crs(envisol_points)
crs(solar_polygons)

# Create buffered region around southern Norway
southern_norway_buffer <- st_buffer(southern_norway, 10000)

#Map layout
map1 <- tm_shape(southern_norway_buffer) +
  tm_borders(col = "white") +
  # tm_shape(rasters$bioclim10) +
  # tm_raster(palette = "Greys", midpoint = NA, style = "cont", alpha = 0.2, legend.show = FALSE) +
  tm_shape(my_map) +
  tm_borders() +
  tm_shape(customGeom) +
  tm_borders() +
  tm_graticules(lines = FALSE, ticks = TRUE) +
  tm_layout(
    main.title = "Bakkemontert solkraft, 2024",
    main.title.size = 1.5,
    main.title.position = c("left", "bottom"),
    legend.position = c("left", "top"),
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.text.size = 1.1,
    frame = TRUE
  ) +
  tm_shape(solar_points) +
  tm_symbols(col = "forestgreen", size = 0.7, border.col = "black", border.lwd = 0.5) +
  tm_shape(envisol_points) +
  tm_symbols(col = "red", size = 0.7, border.col = "black", border.lwd = 0.5) +
  tm_add_legend(type = "symbol", labels = legend_labels, col = legend_col) +
  tm_compass(position = c("right", "bottom"), size = 1) +
  tm_scale_bar(position = c("right", "bottom"), width = 0.2, text.size = 0.7, breaks = c(0, 50, 100))

# Plot map
tmap::tmap_mode("plot")
map1

# Save the map
output_path <- "P:/412331_envisol/envisol_spatial_analysis/outputs/oversikt_envisol.png"
tmap_save(map1, filename = output_path, width = 38, height = 18, units = "cm", dpi = 600, bg = "white")


# AR5 for each plot -------------------------------------------------------

# My interest point
envisol_points
my_interest_point <- envisol_points |> filter(kortnamn == "Buer")
my_interest_point
my_interest_point <- my_interest_point |> st_transform(25833)
my_interest_point

# Assuming your interest point is already loaded as 'my_interest_point'
my_buffer <- st_buffer(my_interest_point, dist = 1000)
# Map window for plotting
map_window_buffer <- st_buffer(my_interest_point, dist = 500)

map_window_buffer

# Convert the point geometry to WKT (Well-Known Text) to use in SQL
wkt_geom <- st_as_text(my_buffer$geom)
wkt_geom

# Connect to database
con <- NinaR::postgreSQLConnect()

# List schemas
dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata")

# List tables in 'Topography' schema
dbGetQuery(con, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'Topography'")

dbGetQuery(con, "SELECT table_schema, table_name FROM information_schema.tables WHERE table_name LIKE '%AR5%'")

ar5_query <- paste0("
SELECT ar5.*
FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
")

ar5_data <- sf::st_read(con, query = ar5_query)


# Summarize the area of each land type within the buffer
ar5_summary <- ar5_data %>%
  mutate(artype = as.factor(arealtype)) %>%
  group_by(artype) %>%
  summarize(artype_area = sum(st_area(geom)))

ar5_summary

# Convert arealtype to numeric in ar5_data
ar5_data$arealtype <- as.numeric(ar5_data$arealtype)

# Create a lookup table for areal types
arealtype_lookup <- data.frame(
  arealtype = c(21, 22, 23, 30, 50, 60, 81, 82, 11, 12, 99),
  artype = c("Fulldyrka jord", "Overflatedyrka jord", "Innmarksbeite", "Skog", 
             "Åpen fastmark", "Myr", "Ferskvann", "Hav", "Bebygd", "Samferdsel", 
             "Ikke kartlagt")
)

# Join the lookup table to ar5_data
ar5_data <- ar5_data %>%
  left_join(arealtype_lookup, by = "arealtype")


# Step 1: Subset AR5 data to only include polygons intersecting with the buffer
ar5_in_buffer <- st_intersection(ar5_data, my_buffer)

# Step 2: Set up the tmap plot
tmap_mode("plot") # Use plot mode for a static map

# Create the plot with the specified colors and adjustments
tm_shape(map_window_buffer) +
  tm_borders(col = "grey20", lwd = 1)+
  tm_shape(ar5_in_buffer) +
  tm_borders(col = "grey30", lwd = 0.3)+
  tm_polygons("artype", 
              palette = c(
                "Fulldyrka jord" = "#FFD16E", 
                "Overflatedyrka jord" = "#FFFF4C", 
                "Innmarksbeite" = "#FFFFAD", 
                "Skog" = "#9ECC73", 
                "Myr" = "#CBCBFB", 
                "Åpen fastmark" = "#D9D9D9", 
                "Ferskvann" = "#91E7FF", 
                "Hav" = "#CCFEFE", 
                "Bebygd" = "#FCD9D6", 
                "Samferdsel" = "#B3784C"
              ),
              title = "Arealtype AR5") +
  tm_shape(solar_polygons) +
  tm_borders(col = "red", lwd = 4) + # Add red borders for solar polygons
  tm_graticules(n.x = 2, n.y = 2, ticks = TRUE, lines = FALSE) + # Show only ticks at margins
  tm_shape(my_buffer) +
  tm_borders(col = "black", lwd = 2) + # Add the buffer outline in blue
  tm_layout(
    title = ar5_in_buffer$kortnamn, # Use 'kortnamn' as the title
    title.position = c("left", "top"), # Position the title to the left above the plot
    legend.outside = TRUE # Move the legend outside the plot
  ) +
  tm_compass(position = c("left", "bottom"), size = 1.5) + # Add a small north arrow
  tm_scale_bar(width = 0.25, breaks = c(0.05, 0.1), text.size = 0.7) # Add a scale bar with 250m width


tmap_mode("plot") # Use plot mode for a static map


# Panel plot --------------------------------------------------------------

# Connect to database
con <- NinaR::postgreSQLConnect()

# Create a lookup table for areal types
arealtype_lookup <- data.frame(
  arealtype = c(21, 22, 23, 30, 50, 60, 81, 82, 11, 12, 99),
  artype = c("Fulldyrka jord", "Overflatedyrka jord", "Innmarksbeite", "Skog", 
             "Åpen fastmark", "Myr", "Ferskvann", "Hav", "Bebygd", "Samferdsel", "Ikke kartlagt")
)

# Function to create a map for each interest point
create_ar5_map <- function(point) {
  point <- st_transform(point, 25833)
  buffer <- st_buffer(point, dist = 500)
  wkt_geom <- st_as_text(buffer$geom)
  
  # Query AR5 data
  ar5_query <- paste0("
    SELECT ar5.*
    FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
    WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
  ")
  ar5_data <- sf::st_read(con, query = ar5_query)
  
  ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
  ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
  ar5_in_buffer <- st_intersection(ar5_data, buffer)
  
  # Create the tmap plot
  tm_shape(ar5_in_buffer) +
    tm_borders(col = "grey20", lwd = 1) +
    tm_polygons("artype", 
                palette = c(
                  "Fulldyrka jord" = "#FFD16E", 
                  "Overflatedyrka jord" = "#FFFF4C", 
                  "Innmarksbeite" = "#FFFFAD", 
                  "Skog" = "#9ECC73", 
                  "Myr" = "#CBCBFB", 
                  "Åpen fastmark" = "#D9D9D9", 
                  "Ferskvann" = "#91E7FF", 
                  "Hav" = "#CCFEFE", 
                  "Bebygd" = "#FCD9D6", 
                  "Samferdsel" = "#B3784C"
                ),
                title = "Arealtype AR5") +
    tm_shape(solar_polygons) +
    tm_borders(col = "red", lwd = 2) + # Add red borders for solar polygons
    tm_graticules(n.x = 2, n.y = 2, ticks = TRUE, lines = FALSE) +
    tm_shape(buffer) +
    tm_borders(col = "black", lwd = 2) +
    tm_layout(
      title = point$kortnamn,
      title.position = c("left", "top"),
      legend.outside = TRUE,
      frame = FALSE # Remove the frame
    )
}

# Generate a list of maps
maps_list <- lapply(split(envisol_points, seq(nrow(envisol_points))), create_ar5_map)

# # Create the extra plot with the legend, scale bar, and compass
# legend_plot <- tm_shape(ar5_in_buffer) +
#   tm_polygons("artype", 
#               palette = c(
#                 "Fulldyrka jord" = "#FFD16E", 
#                 "Overflatedyrka jord" = "#FFFF4C", 
#                 "Innmarksbeite" = "#FFFFAD", 
#                 "Skog" = "#9ECC73", 
#                 "Myr" = "#CBCBFB", 
#                 "Åpen fastmark" = "#D9D9D9", 
#                 "Ferskvann" = "#91E7FF", 
#                 "Hav" = "#CCFEFE", 
#                 "Bebygd" = "#FCD9D6", 
#                 "Samferdsel" = "#B3784C"
#               ),
#               title = "Arealtype AR5") +
#   tm_layout(legend.position = c("left", "top")) +
#   tm_compass(position = c("right", "bottom"), size = 1.5) + # Include north arrow
#   tm_scale_bar(width = 0.25, breaks = c(0, 0.05, 0.1), text.size = 0.7) # Include scale bar

# Combine maps into a single panel with an extra legend plot
panel_map <- tmap_arrange(c(maps_list, list(legend_plot)), ncol = 4, nrow = 4)

# Display the panel map
panel_map

# Load necessary libraries
library(sf)
library(dplyr)
library(tmap)
library(DBI)
library(RPostgres)

Sys.setlocale(locale = 'no_NB.utf8')
options(scipen = 999)

# Connect to database
con <- NinaR::postgreSQLConnect()

# Create a lookup table for areal types
arealtype_lookup <- data.frame(
  arealtype = c(21, 22, 23, 30, 50, 60, 81, 82, 11, 12, 99),
  artype = c("Fulldyrka jord", "Overflatedyrka jord", "Innmarksbeite", "Skog", 
             "Åpen fastmark", "Myr", "Ferskvann", "Hav", "Bebygd", "Samferdsel", "Ikke kartlagt")
)

# Function to create a map for each interest point
create_ar5_map <- function(point) {
  point <- st_transform(point, 25833)
  buffer <- st_buffer(point, dist = 500)
  wkt_geom <- st_as_text(buffer$geom)
  
  # Query AR5 data
  ar5_query <- paste0("
    SELECT ar5.*
    FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
    WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
  ")
  ar5_data <- sf::st_read(con, query = ar5_query)
  
  ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
  ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
  ar5_in_buffer <- st_intersection(ar5_data, buffer)
  
  # Create the tmap plot
  tm_shape(ar5_in_buffer) +
    tm_borders(col = "grey20", lwd = 1) +
    tm_polygons("artype", 
                palette = c(
                  "Fulldyrka jord" = "#FFD16E", 
                  "Overflatedyrka jord" = "#FFFF4C", 
                  "Innmarksbeite" = "#FFFFAD", 
                  "Skog" = "#9ECC73", 
                  "Myr" = "#CBCBFB", 
                  "Åpen fastmark" = "#D9D9D9", 
                  "Ferskvann" = "#91E7FF", 
                  "Hav" = "#CCFEFE", 
                  "Bebygd" = "#FCD9D6", 
                  "Samferdsel" = "#B3784C"
                ),
                title = "Arealtype AR5") +
    tm_shape(solar_polygons) +
    tm_borders(col = "red", lwd = 2) + # Add red borders for solar polygons
    tm_graticules(n.x = 2, n.y = 2, ticks = TRUE, lines = FALSE) +
    tm_shape(buffer) +
    tm_borders(col = "black", lwd = 2) +
    tm_layout(
      title = point$kortnamn,
      title.position = c("left", "top"),
      legend.outside = TRUE,
      frame = FALSE # Remove the frame
    )
}

# Generate a list of maps
maps_list <- lapply(split(envisol_points, seq(nrow(envisol_points))), create_ar5_map)

# Create the extra plot with only north arrow and scale bar
blank_sf <- st_sfc(st_point(c(0, 0)), crs = 25833) # Create a blank SF object
legend_plot <- tm_shape(blank_sf) +
  tm_compass(position = c("right", "bottom"), size = 1.5) + # Include north arrow
  tm_scale_bar(width = 0.25, breaks = c(0, 0.05, 0.1), text.size = 0.7) + # Include scale bar
  tm_layout(frame = FALSE) # Remove frame

# Combine maps into a single panel with an extra legend plot
panel_map <- tmap_arrange(c(maps_list, list(legend_plot)), ncol = 4, nrow = 4)

# Display the panel map
panel_map


# Rectangular plots -------------------------------------------------------


# # Function to create a rectangular map for each interest point
# create_ar5_map_rect <- function(point) {
#   point <- st_transform(point, 25833)
#   buffer <- st_buffer(point, dist = 500)
#   bbox <- st_as_sfc(st_bbox(buffer))  # Create a rectangular border from the bounding box
# 
#   wkt_geom <- st_as_text(buffer$geom)
# 
#   # Query AR5 data
#   ar5_query <- paste0("
#     SELECT ar5.*
#     FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
#     WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
#   ")
#   ar5_data <- sf::st_read(con, query = ar5_query)
# 
#   ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
#   ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
#   ar5_in_buffer <- st_intersection(ar5_data, buffer)
# 
#   # Create the tmap plot with rectangular border
#   tm_shape(ar5_in_buffer) +
#     tm_borders(col = "grey20", lwd = 1) +
#     tm_polygons("artype",
#                 palette = c(
#                   "Fulldyrka jord" = "#FFD16E",
#                   "Overflatedyrka jord" = "#FFFF4C",
#                   "Innmarksbeite" = "#FFFFAD",
#                   "Skog" = "#9ECC73",
#                   "Myr" = "#CBCBFB",
#                   "Åpen fastmark" = "#D9D9D9",
#                   "Ferskvann" = "#91E7FF",
#                   "Hav" = "#CCFEFE",
#                   "Bebygd" = "#FCD9D6",
#                   "Samferdsel" = "#B3784C"
#                 ),
#                 title = "") +  # Remove legend title
#     tm_shape(solar_polygons) +
#     tm_borders(col = "red", lwd = 2) + # Add red borders for solar polygons
#     tm_graticules(n.x = 2, n.y = 2, ticks = TRUE, lines = FALSE) +
#     tm_shape(bbox) +  # Use the rectangular bounding box instead of buffer
#     tm_borders(col = "black", lwd = 2) +
#     tm_layout(
#       title = point$kortnamn,
#       title.position = c("left", "top"),
#       legend.outside = TRUE,
#       frame = FALSE # Remove the frame
#     )
# }
# 
# # Generate a list of maps with rectangular borders
# maps_list_rect <- lapply(split(envisol_points, seq(nrow(envisol_points))), create_ar5_map_rect)
# 
# # Create the extra plot with only north arrow and scale bar
# blank_sf <- st_sfc(st_point(c(0, 0)), crs = 25833) # Create a blank SF object
# legend_plot <- tm_shape(blank_sf) +
#   tm_compass(position = c("right", "bottom"), size = 1.5) + # Include north arrow
#   tm_scale_bar(width = 0.25, breaks = c(0, 0.05, 0.1), text.size = 0.7) + # Include scale bar
#   tm_layout(frame = FALSE) # Remove frame
# 
# # Combine maps into a single panel with an extra legend plot
# panel_map_rect <- tmap_arrange(c(maps_list_rect, list(legend_plot)), ncol = 4, nrow = 4)
# 
# # Display the panel map
# panel_map_rect



# Rectangle plots improved ------------------------------------------------

solar_points_50m_circles <- st_buffer(envisol_points, 100)

# Function to create a map with a rectangular bounding box for plotting
create_ar5_map_with_bbox <- function(point) {
  # Transform point and create buffers
  point <- st_transform(point, 25833)
  extraction_buffer <- st_buffer(point, dist = 1000) # Buffer for data extraction
  plotting_buffer <- st_buffer(point, dist = 500)   # Smaller buffer for bounding box
  plotting_bbox <- st_as_sfc(st_bbox(plotting_buffer)) # Create bounding box
  
  # Convert extraction buffer geometry to WKT for SQL query
  wkt_geom <- st_as_text(extraction_buffer$geom)
  
  # Query AR5 data based on extraction buffer
  ar5_query <- paste0("
    SELECT ar5.*
    FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
    WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
  ")
  ar5_data <- sf::st_read(con, query = ar5_query)
  
  # Join AR5 data with lookup table and convert types
  ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
  ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
  ar5_in_buffer <- st_intersection(ar5_data, extraction_buffer)
  
  # Create the tmap plot using the bounding box for extent
  tm_shape(plotting_bbox) + # Set extent using bounding box
    tm_borders(col = "black", lwd = 2) + # Black border for bounding box
    tm_shape(ar5_in_buffer) +
    tm_borders(col = "grey20", lwd = 1) +
    tm_polygons("artype", 
                palette = c(
                  "Fulldyrka jord" = "#FFD16E", 
                  "Overflatedyrka jord" = "#FFFF4C", 
                  "Innmarksbeite" = "#FFFFAD", 
                  "Skog" = "#9ECC73", 
                  "Myr" = "#CBCBFB", 
                  "Åpen fastmark" = "#D9D9D9", 
                  "Ferskvann" = "#91E7FF", 
                  "Hav" = "#CCFEFE", 
                  "Bebygd" = "#FCD9D6", 
                  "Samferdsel" = "#B3784C"
                ),
                title = "") +  # No legend title
    tm_shape(solar_polygons) +
    tm_borders(col = "red", lwd = 2) + # Add red borders for solar polygons
    tm_shape(solar_points_100m_circles) +
    tm_borders(col = "red", lwd = 2) + # Add red borders for solar polygons
    tm_graticules(n.x = 2, n.y = 2, ticks = TRUE, lines = FALSE) +
    tm_layout(
      title = point$kortnamn,
      title.position = c("left", "top"),
      legend.outside = TRUE,
      frame = TRUE # Remove the frame
    )
}

# Generate a list of maps with improved bounding box approach
maps_list_bbox <- lapply(split(envisol_points, seq(nrow(envisol_points))), create_ar5_map_with_bbox)

# Create the extra plot with only north arrow and scale bar
blank_sf <- st_sfc(st_point(c(0, 0)), crs = 25833) # Create a blank SF object
legend_plot <- tm_shape(blank_sf) +
  tm_compass(position = c("right", "bottom"), size = 1.5) + # Include north arrow
  tm_scale_bar(width = 0.25, breaks = c(0, 0.05, 0.1), text.size = 0.7) + # Include scale bar
  tm_layout(frame = FALSE) # Remove frame

# Combine maps into a single panel with an extra legend plot
panel_map_bbox <- tmap_arrange(c(maps_list_bbox, list(legend_plot)), ncol = 4, nrow = 4)

# Display the panel map
panel_map_bbox

# Define the output path
output_path <- "P:/412331_envisol/envisol_spatial_analysis/outputs/panel_map_a4_landscape.png"

# Save the combined panel map as A4 landscape with 300 DPI
tmap_save(panel_map_bbox, filename = output_path, 
          width = 29.7, height = 21, units = "cm", dpi = 300, bg = "white")




# # Land cover statistics ---------------------------------------------------
# 
# solar_points
# 
# # Load necessary libraries
# library(sf)
# library(dplyr)
# library(tmap)
# library(ggplot2)
# library(units)
# 
# # Connect to database
# con <- NinaR::postgreSQLConnect()
# 
# # Create a lookup table for areal types
# arealtype_lookup <- data.frame(
#   arealtype = c(21, 22, 23, 30, 50, 60, 81, 82, 11, 12, 99),
#   artype = c("Fulldyrka jord", "Overflatedyrka jord", "Innmarksbeite", "Skog", 
#              "Åpen fastmark", "Myr", "Ferskvann", "Hav", "Bebygd", "Samferdsel", "Ikke kartlagt")
# )
# 
# # Define the color palette
# land_cover_colors <- c(
#   "Fulldyrka jord" = "#FFD16E", 
#   "Overflatedyrka jord" = "#FFFF4C", 
#   "Innmarksbeite" = "#FFFFAD", 
#   "Skog" = "#9ECC73", 
#   "Myr" = "#CBCBFB", 
#   "Åpen fastmark" = "#D9D9D9", 
#   "Ferskvann" = "#91E7FF", 
#   "Hav" = "#CCFEFE", 
#   "Bebygd" = "#FCD9D6", 
#   "Samferdsel" = "#B3784C"
# )
# 
# # Function to extract AR5 data and aggregate by land cover type
# extract_land_cover_data <- function(point) {
#   # Transform the point to the correct CRS
#   point <- st_transform(point, 25833)
#   
#   # Create a buffer of 500m around each point
#   buffer <- st_buffer(point, dist = 500)
#   wkt_geom <- st_as_text(buffer$geom)
#   
#   # Query AR5 data
#   ar5_query <- paste0("
#     SELECT ar5.*
#     FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
#     WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
#   ")
#   
#   # Fetch data
#   ar5_data <- sf::st_read(con, query = ar5_query)
#   ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
#   ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
#   
#   # Calculate total area per land cover type
#   ar5_summary <- ar5_data %>%
#     mutate(artype = as.factor(artype)) %>%
#     group_by(artype) %>%
#     summarize(total_area = sum(st_area(geom))) %>%
#     na.omit()
#   
#   return(ar5_summary)
# }
# 
# # Apply the extraction function to all solar points
# land_cover_data <- lapply(split(solar_points, seq(nrow(solar_points))), extract_land_cover_data)
# 
# # Combine results into a single data frame
# all_land_cover_data <- bind_rows(land_cover_data)
# 
# # Summarize total area for each land cover type across all points
# total_land_cover <- all_land_cover_data %>%
#   group_by(artype) %>%
#   summarize(total_area = sum(total_area, na.rm = TRUE))
# 
# # Print summary table
# print(total_land_cover)
# 
# # Remove units from the area values to avoid issues with ggplot2
# total_land_cover_df <- total_land_cover %>%
#   mutate(total_area = as.numeric(set_units(total_area, "m^2")))
# 
# # Create a barplot with ggplot2
# ggplot(total_land_cover_df, aes(x = reorder(artype, -total_area), y = total_area, fill = artype)) +
#   geom_bar(stat = "identity", color = "black") +
#   scale_fill_manual(values = land_cover_colors) +
#   labs(x = "Arealtype", y = "Total Area (m²)", title = "Total Land Cover Area for Planned PV Parks") +
#   theme_minimal() +
#   theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Save the barplot as a high-resolution image
# ggsave("P:/412331_envisol/envisol_spatial_analysis/outputs/land_cover_barplot.png", 
#        width = 29.7, height = 21, units = "cm", dpi = 300)
# 
# 
# # Save the barplot
# ggsave("P:/412331_envisol/envisol_spatial_analysis/outputs/land_cover_barplot.png", 
#        width = 29.7, height = 21, units = "cm", dpi = 300)



# Improved land cover statistics ------------------------------------------


# Load necessary libraries
library(sf)
library(dplyr)
library(tmap)
library(ggplot2)
library(units)

# Connect to database
con <- NinaR::postgreSQLConnect()

# Create a lookup table for areal types
arealtype_lookup <- data.frame(
  arealtype = c(21, 22, 23, 30, 50, 60, 81, 82, 11, 12, 99),
  artype = c("Fulldyrka jord", "Overflatedyrka jord", "Innmarksbeite", "Skog", 
             "Åpen fastmark", "Myr", "Ferskvann", "Hav", "Bebygd", "Samferdsel", "Ikke kartlagt")
)

# Define the color palette
land_cover_colors <- c(
  "Fulldyrka jord" = "#FFD16E", 
  "Overflatedyrka jord" = "#FFFF4C", 
  "Innmarksbeite" = "#FFFFAD", 
  "Skog" = "#9ECC73", 
  "Myr" = "#CBCBFB", 
  "Åpen fastmark" = "#D9D9D9", 
  "Ferskvann" = "#91E7FF", 
  "Hav" = "#CCFEFE", 
  "Bebygd" = "#FCD9D6", 
  "Samferdsel" = "#B3784C"
)

# Function to extract AR5 data and aggregate by land cover type
extract_land_cover_data <- function(point) {
  # Transform the point to the correct CRS
  point <- st_transform(point, 25833)
  
  # Create a buffer of 500m around each point
  buffer <- st_buffer(point, dist = 500)
  wkt_geom <- st_as_text(buffer$geom)
  
  # Query AR5 data
  ar5_query <- paste0("
    SELECT ar5.*
    FROM \"Topography\".\"Norway_FKB_AR5_omrade_2023\" as ar5
    WHERE ST_Intersects(ST_GeomFromText('", wkt_geom, "', 25833), ar5.geom);
  ")
  
  # Fetch data
  ar5_data <- sf::st_read(con, query = ar5_query)
  ar5_data$arealtype <- as.numeric(ar5_data$arealtype)
  ar5_data <- ar5_data %>% left_join(arealtype_lookup, by = "arealtype")
  
  # Calculate total area per land cover type
  ar5_summary <- ar5_data %>%
    mutate(artype = as.factor(artype)) %>%
    group_by(artype) %>%
    summarize(total_area = sum(st_area(geom))) %>%
    na.omit()
  
  return(ar5_summary)
}

# Apply the extraction function to all solar points
land_cover_data <- lapply(split(solar_points, seq(nrow(solar_points))), extract_land_cover_data)

# Combine results into a single data frame
all_land_cover_data <- bind_rows(land_cover_data)

# Summarize total area for each land cover type across all points
total_land_cover <- all_land_cover_data %>%
  group_by(artype) %>%
  summarize(
    total_area_m2 = sum(total_area, na.rm = TRUE),
    total_area_km2 = set_units(total_area_m2, "km^2"),
    total_area_ha = set_units(total_area_m2, "hectare"),
    total_area_daa = total_area_m2 / 1000  # Convert to dekar (daa)
  )

# Print summary table
print(total_land_cover)

# Prepare the data for plotting (removing units for compatibility with ggplot2)
total_land_cover_df <- total_land_cover %>%
  mutate(
    total_area_m2 = as.numeric(total_area_m2),
    total_area_km2 = as.numeric(total_area_km2),
    total_area_ha = as.numeric(total_area_ha),
    total_area_daa = as.numeric(total_area_daa)
  )

# Create a barplot with ggplot2, ensuring no frame and a white background
ggplot(total_land_cover_df, aes(x = reorder(artype, -total_area_km2), y = total_area_km2, fill = artype)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = land_cover_colors) +
  labs(x = "Arealtype", y = "Total areal i kvadratkilometer", title = "Arealfordeling rundt planlagte solkraftverk (500m radius)") +
  theme_minimal() +
  theme(
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white and remove border
    plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white and remove border
    panel.grid = element_blank()  # Remove grid lines if you want a cleaner look
  )

# Save the barplot as a PNG file with white background
ggsave("P:/412331_envisol/envisol_spatial_analysis/outputs/land_cover_barplot.png", 
       width = 20, height = 15, units = "cm", dpi = 300, bg = "white")


