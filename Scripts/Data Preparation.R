library(readr)
library(dplyr)

### Data Preprocessing

accidents <- read.csv("Data/KTZH_Accident.csv")

# Remove language specific columns to reduce file size
accidents_short <- accidents %>% 
  select(-ends_with(c("_de", "_fr", "_it")))

write.csv(accidents_short, "Data/KTZH_Accident_short.csv")

### Data Processing

# Load data
ebd <- read.csv("Data/KTZH_EBD.csv")
income <- read.csv("Data/KTZH_Income_updt.csv")

# Remove unnecessary records
ebd <- ebd %>% 
  filter(ebd$Tatbestand == 'Einbrüche insgesamt') %>% 
  select(-c(Gesetz_Nummer, Gesetz_Abk))

# Remove non-municipality data and unused columns
income <- income %>% 
  filter(income$BFS_NR != 0) %>% 
  select(BFS_NR, GEBIET_NAME, INDIKATOR_JAHR, INDIKATOR_VALUE) %>% 
  rename(INCOME_VALUE = INDIKATOR_VALUE)

# Aggregate total trafic accident per municipality and year
accidents_agg <- accidents_short %>% 
  group_by(MunicipalityCode_Aktuell, AccidentYear) %>% 
  summarise(AccidentCount = n(), .groups = "drop")

# Merge data sets
data <- ebd %>% 
  left_join(income, by = c("Gemeinde_BFS_Nr" = "BFS_NR",
                           "Ausgangsjahr" = "INDIKATOR_JAHR")) %>% 
  left_join(accidents_agg, by = c("Gemeinde_BFS_Nr" = "MunicipalityCode_Aktuell",
                                  "Ausgangsjahr" = "AccidentYear"))