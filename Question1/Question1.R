# Nick Chandler
# 29.04.2025
# Question 1: How does the amount of arable land (or geographic information) relate to different quality of life indicators?


# Read in data

# Extract and clean all data
# Extract geographic information
#   Environment: Climate -- first semicolon delimited section
#   Geography: Map references
#   Geography: Land use - agricultural land
#   Geography: Land use - forest
#   Geography: Land use - other
#   Geography: Irrigated land
#   Geography: Elevation - mean elevation
#   Geography: Elevation - lowest point
#   Geography: Elevation - highest point
#   Geography: Area - land
#   Geography: Area - total

# Extract population and quality of life indicators

# Population indicators
#   People and Society: Population - total
#   People and Society: Population - male
#   People and Society: Population - female
#   People and Society: Population growth rate
#   People and Society: Birth rate
#   People and Society: Death rate
#   People and Society: Net migration rate

# Quality of life indicators
#   People and Society: Maternal mortality ratio
#   People and Society: Infant mortality rate - total
#   People and Society: Infant mortality rate - male
#   People and Society: Infant mortality rate - female
#   People and Society: Life expectancy at birth - total population
#   People and Society: Life expectancy at birth - male
#   People and Society: Life expectancy at birth - female
#   People and Society: Current health expenditure
#   People and Society: Physician density
#   People and Society: Obesity - adult prevalence rate
#   People and Society: Education expenditures
#   Communications: Internet users - percent of population
#   Communications: Telephones - mobile cellular - subscriptions per 100 inhabitants

# Look at a sub dataframe
#   map references, climate, Area total, agricultural land, forest, population total, maternal mortality ratio, infant morality rate (total), life expectancy at birth (total), physician density, current health expenditure, education expenditure, cellphones per 100 inhabitants
# Do pca - expect the health vars to make up one pc
# Plot maps of all countries with their values for a PC after "meaning" of the PC has been deduced.
#   Get biplot
#   Look at triplot too
# Look at pairs plots
# Cluster using the elbow method (on loadings and the original data)
#   Look at the elbow plots and with/without cluster plots for 2 PCs
# Use the map references to incorporate categorical variables: 
#   boxplots of all numerical vars separated by the categorical variable
# Do a categorical to categorical comparison



