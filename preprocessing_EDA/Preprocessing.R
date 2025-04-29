# Preprocessing 
library(dplyr)
library(stringr)
library(tidyr)

options(width = 300)

df <- read.csv("countries.csv")

print(cat(colnames(df), sep = "\n"))

bad_values <- c('Akrotiri', 'American Samoa', 'Antarctica', 'Antigua and Barbuda',
                'Ashmore and Cartier Islands', 'Baker Island', 'Bouvet Island',
                'British Indian Ocean Territory', 'British Virgin Islands',
                'Cayman Islands', 'Christmas Island', 'Clipperton Island', 'Cocos (Keeling) Islands',
                'Coral Sea Islands', 'Dhekelia', 'Falkland Islands (Islas Malvinas)', 'Faroe Islands',
                'French Polynesia', 'French Southern and Antarctic Lands', 'Gaza Strip', 'Gibraltar',
                'Guernsey', 'Heard Island and McDonald Islands', 'Howland Island', 'Isle of Man',
                'Jan Mayen', 'Jarvis Island', 'Jersey', 'Johnston Atoll', 'Kingman Reef', 'Macau', 'Midway Islands',
                'Montserrat', 'Navassa Island', 'New Caledonia', 'Niue', 'Norfolk Island', 'Northern Mariana Islands',
                'Palmyra Atoll', 'Paracel Islands', 'Pitcairn Islands', 'Saint Barthelemy', 'Saint Helena, Ascension, and Tristan da Cunha',
                'Saint Martin', 'Saint Pierre and Miquelon', 'Sint Maarten', 'Spratly Islands', 'Svalbard', 'Tokelau',
                'Turks and Caicos Islands', 'Virgin Islands', 'Wake Island', 'Wallis and Futuna')

df_clean <- df %>%
  filter(if_all(everything(), ~ !. %in% bad_values))

length(bad_values)
nrow(df)
nrow(df_clean)

write.csv(df_clean, "clean_data.csv", row.names = FALSE)

