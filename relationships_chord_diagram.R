# Load required packages
library(readr)  # For reading CSV files
library(circlize)  # For creating chord diagrams
library(RColorBrewer)  # For color palettes

# Load the CSV file
countries_data <- read_csv("clean_data.csv", show_col_types = FALSE)

# Convert the CSV data to a more usable format
countries <- list()
for (i in 1:nrow(countries_data)) {
  country_name <- countries_data$Country[i]
  countries[[country_name]] <- as.list(countries_data[i, -1])  # Exclude the country column
}

# Remove "World" from the list of countries if it exists
if ("World" %in% names(countries)) {
  countries <- countries[names(countries) != "World"]
}

# Create a data structure to store country relationships
country_relationships <- list()

# Define alternative names for certain countries
country_alternatives <- list(
  "United Kingdom" = c("Britain", "UK", "England"),
  "United States" = c("USA", "US"),
  "China" = c("PRC"),
  "Turkey (Turkiye)" = c("Turkiye", "Turkey"),
  "Congo, Democratic Republic of the" = c("DRC", "DR Congo", "Congo-Kinshasa"),
  "Congo, Republic of the" = c("Congo-Brazzaville"),
  "Czechia" = c("Czech Republic"),
  "Eswatini" = c("Swaziland"),
  "Burma" = c("Myanmar"),
  "Holy See (Vatican City)" = c("Vatican"),
  "Micronesia, Federated States of" = c("FSM", "Micronesia"),
  "Gambia, The" = c("The Gambia", "Gambia"),
  "Bahamas, The" = c("The Bahamas", "Bahamas"),
  "Korea, North" = c("North Korea", "DPRK"),
  "Korea, South" = c("South Korea", "ROK"),
  "Timor-Leste" = c("East Timor"),
  "Taiwan" = c("Republic of China", "ROC"),
  "Vietnam" = c("Viet Nam")
)

# For each country, find which other countries are mentioned in its background
for (country_name in names(countries)) {
  country_background <- countries[[country_name]]$`Introduction..Background`
  related_countries <- c()
  
  if (!is.null(country_background) && !is.na(country_background)) {
    # Check for mentions of each other country
    for (other_country in names(countries)) {
      # Skip self-references
      if (other_country != country_name) {
        # Check if the other country name appears in the background
        if (grepl(other_country, country_background, ignore.case = TRUE)) {
          related_countries <- c(related_countries, other_country)
        } else {
          # Check alternative names if they exist for this country
          if (other_country %in% names(country_alternatives)) {
            for (alt_name in country_alternatives[[other_country]]) {
              if (grepl(alt_name, country_background, ignore.case = TRUE)) {
                related_countries <- c(related_countries, other_country)
                break  # Found a match, no need to check other alternatives
              }
            }
          }
        }
      }
    }
  }
  
  # Store the relationships for this country
  country_relationships[[country_name]] <- related_countries
}

# Print the relationships for each country
for (country_name in names(country_relationships)) {
  related <- country_relationships[[country_name]]
  if (length(related) > 0) {
    cat(country_name, "is related to:", paste(related, collapse = ", "), "\n")
  } else {
    cat(country_name, "has no detected relationships\n")
  }
}

# Convert the relationships to a matrix format for the chord diagram
countries_list <- unique(names(country_relationships))
n_countries <- length(countries_list)

# Get continent information for each country
country_continents <- sapply(countries_list, function(country) {
  continent <- countries[[country]]$`Geography..Map.references`
  if(is.null(continent) || is.na(continent)) {
    return("Unknown")
  } else {
    return(continent)
  }
})

# Create an empty matrix
relationship_matrix <- matrix(0, nrow = n_countries, ncol = n_countries)
rownames(relationship_matrix) <- countries_list
colnames(relationship_matrix) <- countries_list

# Fill the matrix with relationship data
for (i in 1:n_countries) {
  country_name <- countries_list[i]
  related_countries <- country_relationships[[country_name]]
  
  for (related in related_countries) {
    if (related %in% countries_list) {
      j <- which(countries_list == related)
      relationship_matrix[i, j] <- relationship_matrix[i, j] + 1
    }
  }
}

# Filter to include only countries with at least one relationship
has_relationships <- rowSums(relationship_matrix) > 0 | colSums(relationship_matrix) > 0
filtered_matrix <- relationship_matrix[has_relationships, has_relationships]
filtered_continents <- country_continents[has_relationships]

# Create a more manageable matrix by keeping only countries with multiple relationships
# Adjust the threshold as needed
relationship_threshold <- 2
significant_relationships <- rowSums(filtered_matrix) >= relationship_threshold | 
                            colSums(filtered_matrix) >= relationship_threshold
final_matrix <- filtered_matrix[significant_relationships, significant_relationships]
final_continents <- filtered_continents[significant_relationships]

# Sort countries by continent
continent_order <- order(final_continents)
final_matrix <- final_matrix[continent_order, continent_order]
final_continents <- final_continents[continent_order]

# Group countries by continent
unique_continents <- unique(final_continents)
continent_countries <- lapply(unique_continents, function(cont) {
  rownames(final_matrix)[final_continents == cont]
})
names(continent_countries) <- unique_continents

# Create a continent-level matrix
continent_matrix <- matrix(0, nrow = length(unique_continents), ncol = length(unique_continents))
rownames(continent_matrix) <- unique_continents
colnames(continent_matrix) <- unique_continents

# Fill the continent matrix with aggregated relationship data
for (i in 1:nrow(final_matrix)) {
  for (j in 1:ncol(final_matrix)) {
    if (final_matrix[i, j] > 0) {
      source_continent <- final_continents[i]
      target_continent <- final_continents[j]
      continent_matrix[source_continent, target_continent] <- 
        continent_matrix[source_continent, target_continent] + final_matrix[i, j]
    }
  }
}

# Normalize by the number of countries in each continent
# for (i in 1:nrow(continent_matrix)) {
#   for (j in 1:ncol(continent_matrix)) {
#     source_continent <- rownames(continent_matrix)[i]
#     target_continent <- colnames(continent_matrix)[j]
#     # Normalize by the number of countries in the source continent
#     num_countries_in_source <- length(continent_countries[[source_continent]])
#     if (num_countries_in_source > 0) {
#       continent_matrix[i, j] <- continent_matrix[i, j] / num_countries_in_source
#     }
#   }
# }

# Create a PDF for the continent-level chord diagram
pdf("continent_relationships_chord.pdf", width = 12, height = 12)

# Create a more pleasant color palette for continents
# Using a custom palette with visually distinct but harmonious colors
continent_colors <- c(
  "Africa" = "#1E88E5",       # Blue
  "Antarctica" = "#00ACC1",   # Cyan
  "Arctic Ocean" = "#26C6DA", # Light Cyan
  "Asia" = "#F44336",         # Red
  "Atlantic Ocean" = "#039BE5", # Light Blue
  "Australia" = "#8BC34A",    # Light Green
  "Central America" = "#FF9800", # Orange
  "Europe" = "#7CB342",       # Green
  "Indian Ocean" = "#29B6F6", # Sky Blue
  "Middle East" = "#FF7043",  # Deep Orange
  "North America" = "#9575CD", # Purple
  "Oceania" = "#4DB6AC",      # Teal
  "Pacific Ocean" = "#4FC3F7", # Light Blue
  "South America" = "#FFC107", # Amber
  "Southeast Asia" = "#EC407A" # Pink
)

# For any continents not in our predefined list, use a fallback palette
missing_continents <- setdiff(unique_continents, names(continent_colors))
if (length(missing_continents) > 0) {
  additional_colors <- colorRampPalette(brewer.pal(8, "Pastel1"))(length(missing_continents))
  names(additional_colors) <- missing_continents
  continent_colors <- c(continent_colors, additional_colors)
}

# Rename "Central America and the Caribbean" to "Central America" in the data
final_continents[final_continents == "Central America and the Caribbean"] <- "Central America"
unique_continents <- unique(final_continents)

# Update continent_countries list with the new name
if ("Central America and the Caribbean" %in% names(continent_countries)) {
  continent_countries[["Central America"]] <- continent_countries[["Central America and the Caribbean"]]
  continent_countries[["Central America and the Caribbean"]] <- NULL
}

# Update continent_matrix with the new name
if ("Central America and the Caribbean" %in% rownames(continent_matrix)) {
  rownames(continent_matrix)[rownames(continent_matrix) == "Central America and the Caribbean"] <- "Central America"
  colnames(continent_matrix)[colnames(continent_matrix) == "Central America and the Caribbean"] <- "Central America"
}

# Create the chord diagram for continents
chordDiagram(
  continent_matrix,
  grid.col = continent_colors,
  transparency = 0.2,  
  directional = TRUE,
  direction.type = "arrows",
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  annotationTrack = "grid",
  preAllocateTracks = list(track.height = 0.1),
  annotationTrackHeight = c(0.05, 0.1)
)

# Add labels for continents
circos.track(track.index = 1, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  
  circos.text(
    mean(xlim), ylim[1], 
    sector.index, 
    facing = "clockwise",
    niceFacing = TRUE, 
    adj = c(0, 0.5),
    cex = 0.8
  )
}, bg.border = NA)

# Add title
title("World Region Relationships")
dev.off()

# # Also create an interactive HTML version could be cool for the final version
# if (requireNamespace("htmlwidgets", quietly = TRUE) && 
#     requireNamespace("networkD3", quietly = TRUE)) {
#   library(networkD3)
  
#   # Convert matrix to a format suitable for networkD3
#   network_data <- as.data.frame(which(final_matrix > 0, arr.ind = TRUE))
#   network_data$value <- final_matrix[cbind(network_data$row, network_data$col)]
#   network_data$source <- rownames(final_matrix)[network_data$row]
#   network_data$target <- colnames(final_matrix)[network_data$col]
  
#   # Add continent information
#   network_data$source_continent <- final_continents[match(network_data$source, rownames(final_matrix))]
#   network_data$target_continent <- final_continents[match(network_data$target, rownames(final_matrix))]
  
#   # Create the chord diagram
#   chord <- chordNetwork(
#     Data = network_data[, c("source", "target", "value")],
#     width = 800,
#     height = 800,
#     fontSize = 12,
#     labels = rownames(final_matrix),
#     padding = 0.01,
#     opacity = 0.8,
#     colors = JS(paste0("d3.scaleOrdinal().domain([", 
#                       paste(shQuote(unique_continents), collapse = ", "), 
#                       "]).range(", 
#                       paste0("[", paste(shQuote(continent_colors[unique_continents]), collapse = ", "), "]"), 
#                       ")"))
#   )
  
#   # Save the chord diagram as an HTML file
#   htmlwidgets::saveWidget(chord, "country_relationships_chord.html")
# }

# # Print a summary of the relationships
cat("\nSummary of country relationships:\n")
cat("Total countries analyzed:", length(countries_list), "\n")
cat("Countries with at least one relationship:", sum(has_relationships), "\n")
cat("Countries in the final visualization:", nrow(final_matrix), "\n")




