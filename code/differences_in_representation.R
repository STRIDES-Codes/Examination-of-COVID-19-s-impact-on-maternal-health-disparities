
library(tidyverse)

all_covid_data <- read.csv("Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography_NY.csv")

# Mapping for similar names
name_shift <- data.frame(old_race = c("Asian", "Black", "American Indian/Alaska Native", 
                                      "Native Hawaiian/Other Pacific Islander", "White", "Multiple/Other"),
                         new_race = c("AA", "BA", "IA", "NA", "WA", "TOM"))

# Percentages of NY covid data by county, race/sex, filtering for adults! 
ny_covid_data <- all_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown")),
         age_group != "0 - 17 years") %>%
  count(county_fips_code, sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("FIPS"="county_fips_code","race"="new_race") %>%
  mutate(sex_race = paste(sex, race, sep = "_")) %>%
  group_by(FIPS) %>%
  mutate(percent = n/sum(n)) %>%
  select(-n, -sex, -race) %>%
  spread(sex_race, percent, fill = 0) 

# Total counts of covid data by county, race/sex
ny_count_total_cases <- all_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown")),
         age_group != "0 - 17 years") %>%
  count(county_fips_code, sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("FIPS"="county_fips_code","race"="new_race") %>%
  mutate(sex_race = paste(sex, race, sep = "_")) %>%
  # group_by(FIPS) %>%
  # mutate(percent = n/sum(n)) %>%
  select("FIPS", "sex_race", "Observed_Counts" = "n") %>%
  spread(sex_race, Observed_Counts, fill = 0) %>%
  gather(sex_race, Observed_Counts, 2:13)

# Read population data
combined_pop_df <- read_csv("Documents/Hackathons/NIH/population_ny_cleaned.csv")

# adding population proportions
# (NOte: May want to just double check the columns match up!!)
props <- combined_pop_df %>%
  select("FIPS", "Female_WA"="PCT_WA_FEMALE", "Female_BA"="PCT_BA_FEMALE", "Female_IA"="PCT_IA_FEMALE", 
         "Female_AA"="PCT_AA_FEMALE", "Female_NA"="PCT_NA_FEMALE", "Female_TOM"="PCT_TOM_FEMALE",
         "Male_WA"="PCT_WA_MALE", "Male_BA"="PCT_BA_MALE", "Male_IA"="PCT_IA_MALE", 
         "Male_AA"="PCT_AA_MALE", "Male_NA"="PCT_NA_MALE", "Male_TOM"="PCT_TOM_MALE") %>%
  gather(sex_race, Census_Props, 2:13) %>%
  left_join(ny_count_total_cases, by=c("FIPS", "sex_race"))

# FIll 0 for NA values for counts
props[is.na(props)] <- 0

counties <- unique(props$FIPS)

for (county in counties){
  if (!(county %in% c(36041, 36097, 36099))){ #these counties didn't have observations
    print("###########")
    print(paste0("County: ", county))
    c_data <- filter(props, FIPS == county)
    test <- chisq.test(c_data$Observed_Counts, c_data$Census_Props)
    print(test$p.value)
  }
}

see_diff <- props %>%
  group_by(FIPS) %>%
  mutate(Observed_Pct = Observed_Counts/sum(Observed_Counts)) %>%
  select(-Observed_Counts) %>%
  group_by(FIPS, sex_race) %>%
  gather(DataSet, n, 3:4)

ggplot(see_diff, aes(x=sex_race, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  facet_wrap(~FIPS)

# Looking at Percent differences!
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

just_diff <- full_pct_diff %>%
  select("FIPS", starts_with("DIFF_")) %>%
  gather(Group, Diff, 2:13)

# Cool graph that needs to be spruced up! 
ggplot(just_diff, aes(x=Group, y=Diff, label=round(Diff, 2))) + 
  geom_point(stat='identity', width=.5)  + 
  coord_flip()
