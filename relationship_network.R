# Load required packages
library(readr)  # For reading CSV files
library(RColorBrewer)  # For color palettes
library(igraph)


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
    # Standardize continent names to match chord diagram
    if(continent == "Central America and the Caribbean") {
      return("Central America")
    } else {
      return(continent)
    }
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

# Get the filtered list of countries and their continents
filtered_countries <- countries_list[has_relationships]
filtered_continents <- country_continents[has_relationships]

# Create a network plot

# Create an igraph object from the filtered matrix
g <- graph_from_adjacency_matrix(filtered_matrix, mode = "directed", weighted = TRUE)

# Set vertex attributes for continents
V(g)$continent <- filtered_continents

# Replace the continent colors section with updated colors as requested
continent_colors <- c(
  # Western-aligned regions in blue spectrum
  "North America" = "#0D47A1",    # Dark blue
  "Europe" = "#1976D2",           # Medium blue
  
  # Asian regions in red spectrum
  "Asia" = "#B71C1C",             # Dark red
  "Southeast Asia" = "#E57373",   # Lighter red
  
  # Updated colors as requested
  "Middle East" = "#E65100",      # Orange
  "Africa" = "#2E7D32",           # Green
  "Central America" = "#FBC02D",  # Yellow
  "South America" = "#F9A825",    # Darker yellow
  
  # Other regions
  "AsiaEurope" = "#6A1B9A",       # Purple
  "Oceania" = "#00838F",          # Teal
  "Arctic Region" = "#004D40"     # Dark teal
)

# Map continents to colors (modified to use the new color palette)
V(g)$color <- sapply(V(g)$continent, function(cont) {
  if(cont %in% names(continent_colors)) {
    return(continent_colors[cont])
  } else {
    return("#808080")  # Default gray for any unmapped continents
  }
})

# Set all edge weights to a low value to reduce attraction
E(g)$weight <- 0.1  # Reduce attraction between connected nodes

# Create a better layout with reduced attraction to spread countries more
# Set a fixed seed for reproducible layout
set.seed(1)  # 44 is nice
layout <- layout_with_fr(g, 
                        niter = 2000,  # Increase iterations for better convergence
                        dim = 2,
                        grid = "grid",
                        weights = E(g)$weight,  # Use our reduced weights
                        start.temp = 20  # Higher temperature for more initial movement
)

# Manually scale the layout to spread nodes more
layout <- layout * 6  # Increased to 6 for much more spread

# Determine which countries have at least 20 connections
node_degrees <- degree(g, mode = "all")
show_labels <- node_degrees >= 13

# Calculate node sizes based on connectivity
# Scale from 3 (minimum) to 14 (maximum) based on degree - increased from 2-12
min_size <- 3  # Increased from 2
max_size <- 14 # Increased from 12
min_degree <- min(node_degrees)
max_degree <- max(node_degrees)
vertex_sizes <- min_size + (node_degrees - min_degree) * (max_size - min_size) / (max_degree - min_degree)

# Modify country labels for display
country_display_names <- V(g)$name
country_display_names[country_display_names == "United States"] <- "USA"
country_display_names[country_display_names == "Korea, South"] <- "South Korea"

# Determine which countries to show labels for
show_labels <- node_degrees >= 15
# Force specific countries to show/hide
show_labels[V(g)$name == "Ukraine"] <- TRUE  # Always show Ukraine
show_labels[V(g)$name == "Micronesia, Federated States of"] <- FALSE  # Hide Micronesia
show_labels[V(g)$name == "Brazil"] <- TRUE  # Always show Brazil
show_labels[V(g)$name == "Mexico"] <- TRUE  # Always show Mexico

# Open PDF device
pdf("country_relationships_network.pdf", width = 12, height = 10)

# Plot the network with improved aesthetics
plot(g, 
     layout = layout, 
     vertex.size = vertex_sizes,     # Size based on connectivity (now bigger)
     vertex.color = V(g)$color,      # Color by continent
     vertex.label = ifelse(show_labels, country_display_names, NA),
     vertex.label.cex = 0.75,         # Increased from 0.7 for bigger text
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.label.font = 2,
     vertex.frame.color = "gray90",
     edge.width = 0.5,               # Reduced from 0.8 for lighter edges
     edge.color = "gray85",          # Lightened from gray70 to gray85
     edge.arrow.size = 0,
     main = "Country Relationships Network")

# Add a legend for continents
legend("bottomright", 
       legend = names(continent_colors),
       fill = continent_colors, 
       title = "Continents",
       cex = 0.7,
       bty = "n")

# Close the PDF device
dev.off()

# Also display the plot in the R interface
plot(g, 
     layout = layout, 
     vertex.size = vertex_sizes,     # Size based on connectivity (now bigger)
     vertex.color = V(g)$color,
     vertex.label = ifelse(show_labels, country_display_names, NA),
     vertex.label.cex = 1.4,         # Increased from 1.2 for bigger text
     vertex.label.dist = 0.8,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.label.font = 2,
     vertex.frame.color = "gray90",
     edge.width = 0.5,               # Reduced from 0.8 for lighter edges
     edge.color = "gray85",          # Lightened from gray70 to gray85
     edge.arrow.size = 0,
     main = "Country Relationships Network")

# Add a legend for continents
legend("bottomright", 
       legend = names(continent_colors),
       fill = continent_colors, 
       title = "Continents",
       cex = 0.7,
       bty = "n")


  