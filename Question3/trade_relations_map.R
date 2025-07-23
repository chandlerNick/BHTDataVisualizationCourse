# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(maps)
library(ggrepel)
library(scales)
library(ggarrow)  # For better arrow visualization

# Read the data
data <- read_csv("/home/luisa/Documents/University/BHT/Data Visualization/BHTDataVisualizationCourse/data/clean_data.csv", show_col_types = FALSE)

# Country alternatives for name matching
country_alternatives <- list(
  "United Kingdom" = c("Britain", "UK", "England", "British"),
  "United States" = c("USA", "US"),
  "China" = c("PRC", "Chinese", "Chin"),
  "Turkey (Turkiye)" = c("Turkiye", "Turkey", "Turkish"),
  "Congo, Democratic Republic of the" = c("DRC", "DR Congo", "Congo-Kinshasa", "Congo republics"),
  "Congo, Republic of the" = c("Congo-Brazzaville", "Congo republics"),
  "Czechia" = c("Czech Republic", "Czech"),
  "Eswatini" = c("Swaziland", "Swazee"),
  "Burma" = c("Myanmar", "Burmese"),
  "Holy See (Vatican City)" = c("Vatican", "Vatican City"),
  "Micronesia, Federated States of" = c("FSM", "Micronesia"),
  "Gambia, The" = c("The Gambia", "Gambia"),
  "Bahamas, The" = c("The Bahamas", "Bahamas"),
  "Korea, North" = c("North Korea", "DPRK", "N. Korea"),
  "Korea, South" = c("South Korea", "ROK", "S. Korea"),
  "Timor-Leste" = c("East Timor"),
  "Taiwan" = c("Republic of China", "ROC"),
  "Vietnam" = c("Viet Nam"),
  "Germany" = c("German"),
  "France" = c("French"),
  "Italy" = c("Italian"),
  "Spain" = c("Spanish"),
  "Portugal" = c("Portuguese"),
  "Greece" = c("Greek"),
  "Netherlands" = c("Dutch"),
  "Belgium" = c("Belgian"),
  "Sweden" = c("Swedish"),
  "Norway" = c("Norwegian"),
  "Denmark" = c("Danish"),
  "Finland" = c("Finnish"),
  "Switzerland" = c("Swiss"),
  "Poland" = c("Polish"),
  "Hungary" = c("Hungarian"),
  "United Arab Emirates" = c("UAE", "Emirates"),
  "New Zealand" = c("NZ"),
  "Trinidad and Tobago" = c("Trinidad & Tobago")
)

# Function to parse coordinates from text format "33 00 N, 65 00 E"
parse_coordinates <- function(coord_text) {
  if (is.na(coord_text) || coord_text == "") {
    return(list(lat = NA, lon = NA))
  }

  
  # Split by comma
  parts <- str_split(coord_text, ",")[[1]]
  if (length(parts) != 2) {
    return(list(lat = NA, lon = NA))
  }
  
  # Parse latitude
  lat_part <- str_trim(parts[1])
  lat_match <- str_match(lat_part, "([0-9]+)\\s+([0-9]+)\\s+([NS])")
  if (is.na(lat_match[1])) {
    return(list(lat = NA, lon = NA))
  }
  lat_deg <- as.numeric(lat_match[2])
  lat_min <- as.numeric(lat_match[3])
  lat_dir <- lat_match[4]
  lat <- lat_deg + lat_min/60
  if (lat_dir == "S") lat <- -lat
  
  # Parse longitude
  lon_part <- str_trim(parts[2])
  lon_match <- str_match(lon_part, "([0-9]+)\\s+([0-9]+)\\s+([EW])")
  if (is.na(lon_match[1])) {
    return(list(lat = NA, lon = NA))
  }
  lon_deg <- as.numeric(lon_match[2])
  lon_min <- as.numeric(lon_match[3])
  lon_dir <- lon_match[4]
  lon <- lon_deg + lon_min/60
  if (lon_dir == "W") lon <- -lon
  
  return(list(lat = lat, lon = lon))
}

# Function to extract numeric value from import data (first value in billions)
extract_import_value <- function(import_text) {
  if (is.na(import_text) || import_text == "") {
    return(0)
  }
  
  # Look for first dollar amount and extract number
  match <- str_match(import_text, "\\$([0-9.]+)\\s*(trillion|billion|million)?")
  if (is.na(match[1])) {
    return(0)
  }
  
  value <- as.numeric(match[2])
  if (!is.na(match[3])) {
    if (match[3] == "million") {
      value <- value / 1000  # Convert to billions
    } else if (match[3] == "trillion") {
      value <- value * 1000  # Convert to billions
    }
    # billion stays as is
  }
  
  return(value)
}

# Function to standardize country names using alternatives
standardize_country_name <- function(country_name) {
  country_name <- str_trim(country_name)
  
  # Check if it's already a standard name
  if (country_name %in% names(country_alternatives)) {
    return(country_name)
  }
  
  # Check alternatives
  for (standard_name in names(country_alternatives)) {
    if (country_name %in% country_alternatives[[standard_name]]) {
      return(standard_name)
    }
  }
  
  return(country_name)
}

# Function to parse trade partners from text
parse_trade_partners <- function(partners_text) {
  if (is.na(partners_text) || partners_text == "") {
    return(data.frame())
  }
  
  # Remove notes
  partners_text <- str_split(partners_text, "note:")[[1]][1]
  partners_text <- str_trim(partners_text)

  # Replace "Gambia, The" with "Gambia"
  partners_text <- str_replace(partners_text, "Gambia, The", "Gambia")
  partners_text <- str_replace(partners_text, "Bahamas, The", "Bahamas")
  
  # Split by commas and extract country-percentage pairs
  pairs <- str_split(partners_text, ",")[[1]]
  
  result <- data.frame()
  
  for (pair in pairs) {
    # Extract percentage
    percent_match <- str_match(pair, "([0-9.]+)%")
    if (is.na(percent_match[1])) next
    
    percentage <- as.numeric(percent_match[2])
    
    # Extract country name (everything before the percentage)
    country_part <- str_replace(pair, "\\s*[0-9.]+%.*", "")
    country_name <- str_trim(country_part)
    
    if (country_name != "" && !is.na(percentage)) {
      result <- rbind(result, data.frame(
        partner = standardize_country_name(country_name),
        percentage = percentage
      ))
    }
  }
  
  return(result)
}

# Process the data
print("Processing coordinates...")
coords_list <- lapply(data$`Geography..Geographic.coordinates`, parse_coordinates)
data$lat <- sapply(coords_list, function(x) x$lat)
data$lon <- sapply(coords_list, function(x) x$lon)

print("Processing import values...")
data$import_value_billions <- sapply(data$`Economy..Imports`, extract_import_value)

print("Processing trade relationships...")
# Create trade relationships dataframe
trade_relationships <- data.frame()

for (i in 1:nrow(data)) {
  country <- data$Country[i]
  import_value <- data$import_value_billions[i]
  country_lat <- data$lat[i]
  country_lon <- data$lon[i]
  
  if (is.na(country_lat) || is.na(country_lon) || import_value == 0) {
    next
  }
  
  partners <- parse_trade_partners(data$`Economy..Imports...partners`[i])
  
  if (nrow(partners) > 0) {
    for (j in 1:nrow(partners)) {
      partner_name <- partners$partner[j]
      percentage <- partners$percentage[j]
      
      # Find partner coordinates
      partner_row <- which(data$Country == partner_name)
      if (length(partner_row) == 0) {
        # Try to find using alternatives
        found <- FALSE
        for (alt_name in names(country_alternatives)) {
          if (partner_name %in% country_alternatives[[alt_name]]) {
            partner_row <- which(data$Country == alt_name)
            if (length(partner_row) > 0) {
              found <- TRUE
              break
            }
          }
        }
        if (!found) {
          cat("Country not found:", partner_name, "\n")
          next
        }
      }
      
      partner_lat <- data$lat[partner_row[1]]
      partner_lon <- data$lon[partner_row[1]]
      
      if (!is.na(partner_lat) && !is.na(partner_lon)) {
        # Calculate line thickness (percentage * import value)
        line_thickness <- (percentage / 100) * import_value
        
        # Calculate dynamic curvature based on distance AND line thickness (closer + bigger = more curved)
        distance <- sqrt((partner_lon - country_lon)^2 + (partner_lat - country_lat)^2)
        
        # Create enhanced curvature categories considering both distance and trade volume
        if (distance < 10 && line_thickness > 20) {
          curvature_category <- "extreme"
          curvature_value <- 1.2
        } else if (distance < 10 || line_thickness > 10) {
          curvature_category <- "very_high"
          curvature_value <- 0.9
        } else if (distance < 10) {
          curvature_category <- "high"
          curvature_value <- 0.6
        } else if (distance < 30 || line_thickness > 5) {
          curvature_category <- "medium_high"
          curvature_value <- 0.9
        } else if (distance < 30) {
          curvature_category <- "medium"
          curvature_value <- 0.3
        } else {
          curvature_category <- "low" 
          curvature_value <- 0.1
        }
        
        trade_relationships <- rbind(trade_relationships, data.frame(
          from_country = country,
          to_country = partner_name,
          from_lat = country_lat,
          from_lon = country_lon,
          to_lat = partner_lat,
          to_lon = partner_lon,
          percentage = percentage,
          import_value = import_value,
          line_thickness = line_thickness,
          curvature_category = curvature_category,
          curvature = curvature_value,
          arrow_size = 0.2 + (0.1 * log1p(line_thickness)),  # Base size 0.12cm + small logarithmic increase
          distance = distance
        ))
      }
    }
  }
}

print(paste("Created", nrow(trade_relationships), "trade relationships"))

# Identify top 50 trade relationships by line_thickness
print("Identifying top 50 trade relationships...")
trade_relationships <- trade_relationships[order(-trade_relationships$line_thickness), ]
trade_relationships$is_top_50 <- 1:nrow(trade_relationships) <= 50

print(paste("Top 50 relationships range from", 
            round(min(trade_relationships$line_thickness[trade_relationships$is_top_50]), 2),
            "to", 
            round(max(trade_relationships$line_thickness[trade_relationships$is_top_50]), 2),
            "billion USD"))

# Get world map data
world_map <- map_data("world")

# Function to get bend factor based on level and distance
get_bend_factor <- function(level, distance) {
  base_levels <- list(
    "low" = 3.5,         
    "medium" = 3.5,      
    "medium_high" = 4, 
    "high" = 4.5,        
    "very_high" = 5,   
    "extreme" = 5.5      
  )
  # Scale factor based on distance - longer edges get more curve
  # Using log scale to prevent extreme values for very long distances
  distance_factor <- 1 + log1p(distance/20)  # normalize by typical distance
  return(base_levels[[level]] * distance_factor)
}

# Function to determine bend direction based on start and end points
get_bend_direction <- function(start_lon, start_lat, end_lon, end_lat) {
  # For nearly vertical lines, determine direction based on whether going up or down
  if (abs(start_lon - end_lon) < 5) {  # Increased threshold for "vertical"
    # If going up, bend east (positive), if going down, bend west (negative)
    if (end_lat > start_lat) return(1) else return(-1)
  }
  # For other lines, bend based on direction
  if (end_lon > start_lon) return(1) else return(-1)
}

# Create arrow path data
print("Creating arrow paths...")
arrow_data <- data.frame()
t <- seq(0, 1, length.out = 50)  # Parameter from 0 to 1

for (i in 1:nrow(trade_relationships)) {
  rel <- trade_relationships[i,]
  
  # Invert direction: arrows now point from partners to importers
  from_lon <- rel$to_lon
  to_lon <- rel$from_lon
  from_lat <- rel$to_lat
  to_lat <- rel$from_lat
  
  x_range <- to_lon - from_lon
  y_range <- to_lat - from_lat
  
  # Calculate edge length using great circle distance
  distance <- sqrt(x_range^2 + y_range^2)
  
  bend_dir <- get_bend_direction(from_lon, from_lat, to_lon, to_lat)
  bend_factor <- get_bend_factor(rel$curvature_category, distance)
  
  # For nearly vertical lines, add extra x-offset
  is_vertical <- abs(x_range) < 5
  if (is_vertical) {
    # Add more pronounced x-deviation for vertical paths
    x <- from_lon + x_range * t + bend_dir * bend_factor * sin(pi * t) * 1.2
    y <- from_lat + y_range * t
  } else {
    # Regular curved path
    x <- from_lon + x_range * t
    y <- from_lat + y_range * t + bend_dir * bend_factor * sin(pi * t)
  }
  
  # Compress the width scale to reduce extreme differences
  width_scaled <- sqrt(rel$line_thickness) * 2  # Using sqrt to compress the range
  
  # Set alpha based on whether it's in top 50
  alpha_value <- ifelse(rel$is_top_50, 0.8, 0.3)
  
  # Add to arrow data
  arrow_data <- rbind(arrow_data, data.frame(
    x = x,
    y = y,
    group = i,
    width = width_scaled,
    is_top_50 = rel$is_top_50,
    alpha_value = alpha_value
  ))
}

# Create the plot
print("Creating the map...")
p <- ggplot() +
  # Add world map
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), 
               fill = "lightgray", 
               color = "white", 
               size = 0.2) +
  
  # Add trade relationship arrows
  geom_arrow(data = arrow_data,
             aes(x = x, y = y, 
                 group = group,
                 linewidth = width * 1.5,
                 alpha = alpha_value * width/50),  
             color = "cyan4") +
  
  # Add country points
  geom_point(data = data[!is.na(data$lat) & !is.na(data$lon), ],
             aes(x = lon, y = lat),
             color = "red", size = 1, alpha = 0.8) +
  
  # Customize scales
  scale_linewidth_continuous(name = "Trade Volume\n(Billions USD)",
                         range = c(0.2, 3),  # Reduced maximum width
                         guide = guide_legend(override.aes = list(alpha = 1)),
                         trans = "identity") +
  
  scale_alpha_identity() +  # Use alpha values directly from data
  
  # Map styling
  theme_void() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 20)),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.background = element_rect(fill = "lightblue", color = NA)
  ) +
  
  labs(
    title = "Global Trade Relationships - Top 50 Highlighted",
    subtitle = "Directed import flows: arrows point from trade partners to importing countries\nTop 50 relationships by trade volume highlighted, others shown with low opacity",
    caption = "Arrow thickness = Import Volume (billions) × Trade Share (%)\nArrows point from trade partner to importing country"
  ) +
  
  # Set coordinate limits
  coord_fixed(ratio = 1.3, xlim = c(-180, 180), ylim = c(-60, 80))

# Save the plot
ggsave("trade_relations_map.pdf", plot = p, width = 16, height = 10, dpi = 300)
ggsave("trade_relations_map.png", plot = p, width = 16, height = 10, dpi = 300)

print("Map saved as trade_relations_map.pdf and trade_relations_map.png")

# Print summary statistics
cat("\n=== Summary Statistics ===\n")
cat("Total countries with coordinates:", sum(!is.na(data$lat) & !is.na(data$lon)), "\n")
cat("Total trade relationships plotted:", nrow(trade_relationships), "\n")
cat("Average line thickness:", mean(trade_relationships$line_thickness, na.rm = TRUE), "\n")
cat("Max line thickness:", max(trade_relationships$line_thickness, na.rm = TRUE), "\n")
cat("Countries with highest import volumes:\n")
top_importers <- data[order(-data$import_value_billions), ][1:5, c("Country", "import_value_billions")]
print(top_importers)
