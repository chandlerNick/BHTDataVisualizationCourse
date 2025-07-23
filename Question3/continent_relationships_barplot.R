# Load required packages
library(readr)  # For reading CSV files
library(ggplot2)  # For creating plots
library(dplyr)  # For data manipulation
library(tidyr)  # For data reshaping

# Load the CSV file
countries_data <- read_csv("/home/luisa/Documents/University/BHT/Data Visualization/BHTDataVisualizationCourse/data/clean_data.csv", show_col_types = FALSE)

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

# Define alternative names for certain countries (same as in network script)
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
  "Korea, North" = c("North Korea", "DPRK"),
  "Korea, South" = c("South Korea", "ROK"),
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
  "United Arab Emirates" = c("UAE", "Emirates")
)

# Create a data structure to store country relationships
country_relationships <- list()

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

# Calculate relationships OUT for each country (countries mentioned in their own text)
relationships_out <- sapply(names(country_relationships), function(country) {
  length(country_relationships[[country]])
})

# Calculate relationships IN for each country (how many other countries mention this country)
relationships_in <- sapply(names(countries), function(target_country) {
  count <- 0
  for (country_name in names(country_relationships)) {
    if (target_country %in% country_relationships[[country_name]]) {
      count <- count + 1
    }
  }
  return(count)
})

# Ensure both vectors have the same names
names(relationships_in) <- names(countries)

# Create a data frame for plotting
relationship_data <- data.frame(
  Country = names(countries),
  Relationships_In = relationships_in,
  Relationships_Out = relationships_out[names(countries)],
  stringsAsFactors = FALSE
)

# Replace NA values with 0
relationship_data$Relationships_Out[is.na(relationship_data$Relationships_Out)] <- 0

# Filter to include only countries with at least one relationship (in or out)
relationship_data <- relationship_data[
  relationship_data$Relationships_In > 0 | relationship_data$Relationships_Out > 0, 
]

# Sort by total relationships (in + out) in descending order
relationship_data$Total_Relationships <- relationship_data$Relationships_In + relationship_data$Relationships_Out
relationship_data <- relationship_data[order(-relationship_data$Total_Relationships), ]

# Take top 30 countries for better visibility
top_countries <- head(relationship_data, 30)

# Reshape data for side-by-side bar plot
plot_data <- top_countries %>%
  select(Country, Relationships_In, Relationships_Out) %>%
  pivot_longer(cols = c(Relationships_In, Relationships_Out), 
               names_to = "Direction", 
               values_to = "Count")

# Clean up direction labels
plot_data$Direction <- ifelse(plot_data$Direction == "Relationships_In", 
                             "Mentioned by Others", 
                             "Mentions Others")

# Shorten country names for better display
plot_data$Country_Short <- plot_data$Country
plot_data$Country_Short[plot_data$Country_Short == "United States"] <- "USA"
plot_data$Country_Short[plot_data$Country_Short == "United Kingdom"] <- "UK"
plot_data$Country_Short[plot_data$Country_Short == "Korea, South"] <- "South Korea"
plot_data$Country_Short[plot_data$Country_Short == "Korea, North"] <- "North Korea"
plot_data$Country_Short[plot_data$Country_Short == "Turkey (Turkiye)"] <- "Turkey"

# Create the bar plot
p <- ggplot(plot_data, aes(x = reorder(Country_Short, -Count), y = Count, fill = Direction)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.9) +
  scale_fill_manual(values = c("Mentioned by Others" = "#2E7D32", "Mentions Others" = "#1976D2")) +
  labs(
    title = "Country Relationships: Incoming vs Outgoing Mentions",
    subtitle = "Top 30 countries by total relationship count",
    x = "Country",
    y = "Number of Relationships",
    fill = "Relationship Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# Display the plot
print(p)

# Save the plot as PDF with landscape dimensions
ggsave("continent_relationships_barplot.pdf", plot = p, width = 20, height = 8, dpi = 300)

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total countries with relationships:", nrow(relationship_data), "\n")
cat("Average relationships in:", round(mean(relationship_data$Relationships_In), 2), "\n")
cat("Average relationships out:", round(mean(relationship_data$Relationships_Out), 2), "\n")

# Show top 10 countries by each metric
cat("\nTop 10 countries by incoming relationships (mentioned by others):\n")
top_in <- relationship_data[order(-relationship_data$Relationships_In), ][1:10, ]
print(top_in[, c("Country", "Relationships_In")])

cat("\nTop 10 countries by outgoing relationships (mentions others):\n")
top_out <- relationship_data[order(-relationship_data$Relationships_Out), ][1:10, ]
print(top_out[, c("Country", "Relationships_Out")])
