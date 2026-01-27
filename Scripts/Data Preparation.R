library(readr)
library(dplyr)

### Data Preprocessing

accidents <- read.csv("Data/KTZH_Accident.csv")

# Remove language specific columns to reduce file size
accidents_short <- accidents %>%
  select(-ends_with(c("_de", "_fr", "_it")))

write.csv(accidents_short, "Data/KTZH_Accident_short.csv")

### Data Processing

