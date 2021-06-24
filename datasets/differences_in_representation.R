
library(tidyverse)

all_covid_data <- read.csv("Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography_NY.csv")

name_shift <- data.frame(old_race = c("Asian", "Black", "American Indian/Alaska Native", 
                                      "Native Hawaiian/Other Pacific Islander", "White", "Multiple/Other"),
                         new_race = c("AA", "BA", "IA", "NA", "WA", "TOM"))

ny_covid_data <- all_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown"))) %>%
  count(county_fips_code, sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("FIPS"="county_fips_code","race"="new_race") %>%
  mutate(sex_race = paste(sex, race, sep = "_")) %>%
  group_by(FIPS) %>%
  mutate(percent = n/sum(n)) %>%
  select(-n, -sex, -race) %>%
  spread(sex_race, percent, fill = 0) 

full_pct_diff <- combined_pop_df %>%
  select("FIPS", "CENSUS_F_WA"="PCT_WA_FEMALE", "CENSUS_F_BA"="PCT_BA_FEMALE", "CENSUS_F_IA"="PCT_IA_FEMALE", "CENSUS_F_AA"="PCT_AA_FEMALE", "CENSUS_F_NA"="PCT_NA_FEMALE", "CENSUS_F_TOM"="PCT_TOM_FEMALE",
         "CENSUS_M_WA"="PCT_WA_MALE", "CENSUS_M_BA"="PCT_BA_MALE", "CENSUS_M_IA"="PCT_IA_MALE", "CENSUS_M_AA"="PCT_AA_MALE", "CENSUS_M_NA"="PCT_NA_MALE", "CENSUS_M_TOM"="PCT_TOM_MALE") %>%
  left_join(ny_covid_data, by="FIPS") %>%
  mutate(DIFF_F_WA = Female_WA - CENSUS_F_WA,
         DIFF_F_BA = Female_BA - CENSUS_F_BA,
         DIFF_F_IA = Female_IA - CENSUS_F_IA,
         DIFF_F_AA = Female_AA - CENSUS_F_AA,
         DIFF_F_NA = Female_NA - CENSUS_F_NA,
         DIFF_F_TOM = Female_TOM - CENSUS_F_TOM,
         DIFF_M_WA = Male_WA - CENSUS_M_WA,
         DIFF_M_BA = Male_BA - CENSUS_M_BA,
         DIFF_M_IA = Male_IA - CENSUS_M_IA,
         DIFF_M_AA = Male_AA - CENSUS_M_AA,
         DIFF_M_NA = Male_NA - CENSUS_M_NA,
         DIFF_M_TOM = Male_TOM - CENSUS_M_TOM)

write.csv(full_pct_diff, "Documents/Hackathons/NIH/ny_race_diff_pct.csv")

