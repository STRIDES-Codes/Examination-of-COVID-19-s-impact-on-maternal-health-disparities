# Code for drilling down to county-level
# Code is very similar to state_picker.R (see there for more comments, right now)

library(tidyverse)
library(maps)
library(ggthemes)

entire_covid_data <- read.csv("Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography_full.csv")

name_shift <- data.frame(old_race = c("Asian", "Black", "American Indian/Alaska Native", 
                                      "Native Hawaiian/Other Pacific Islander", "White", "Multiple/Other"),
                         new_race = c("AA", "BA", "IA", "NA", "WA", "TOM"))

name_shift_graph <- data.frame(graph_name = c("Asian (M)", "Black (M)", "American Indian/Alaska Native (M)", 
                                              "Native Hawaiian/Other Pacific Islander (M)", "White (M)", "Multiple/Other (M)",
                                              "Asian (F)", "Black (F)", "American Indian/Alaska Native (F)", 
                                              "Native Hawaiian/Other Pacific Islander (F)", "White (F)", "Multiple/Other (F)"),
                               old_name = c("AA_MALE", "BA_MALE", "IA_MALE", "NA_MALE", "WA_MALE", "TOM_MALE",
                                            "AA_FEMALE", "BA_FEMALE", "IA_FEMALE", "NA_FEMALE", "WA_FEMALE", "TOM_FEMALE"))

# Fill in desired state here
# Note: DC & Wyoming should not be selected because county-level data does not exist
state_abbrev <- "PA"
state_name <- "Pennsylvania"

observed_cases_pa <- entire_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown")),
         age_group != "0 - 17 years",
         res_state == state_abbrev) %>%
  count(county_fips_code, sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("FIPS"="county_fips_code","race"="new_race") %>%
  mutate(sex_race = paste(race, toupper(sex), sep = "_")) %>%
  select("FIPS", "sex_race", "Observed_Counts" = "n") %>%
  spread(sex_race, Observed_Counts, fill = 0) %>%
  mutate(NA_FEMALE = 0,
         NA_MALE = 0) %>%
  gather(sex_race, Observed_Counts, 2:13)

# pop data 2019 census
pop_data <- read_csv("Downloads/cc-est2019-alldata.csv")

cleaned_pop_data_pa <- pop_data %>%
  filter(YEAR == 12, AGEGRP >= 4, STNAME == state_name) %>%
  select(c(2,3,4,5, 7:22)) %>%
  mutate(FIPS = ifelse(as.numeric(STATE) < 10, (as.numeric(STATE)*10000)+as.numeric(COUNTY), 
                   (as.numeric(STATE)*1000)+as.numeric(COUNTY))) %>%
  select(FIPS, everything(), -COUNTY, -STATE, -STNAME) %>%
  group_by(FIPS, CTYNAME) %>%
  summarise_at(vars(-AGEGRP), sum)

cleaned_pop_data_states_pa <- cleaned_pop_data_pa %>%
  group_by(FIPS, CTYNAME, TOT_POP) %>%
  summarize_all(funs(./TOT_POP)) %>%
  select(-TOT_POP) %>%
  rename("MALE" = "TOT_MALE", "FEMALE" = "TOT_FEMALE")

props_states_pa <- cleaned_pop_data_states_pa %>%
  select(c(1:2, 5:16)) %>%
  gather(sex_race, Census_Props, 3:14) %>%
  left_join(observed_cases_pa, by=c("FIPS", "sex_race"))

see_diff_state_pa <- props_states_pa %>%
  group_by(FIPS) %>%
  mutate(Observed_Pct = Observed_Counts/sum(Observed_Counts)) %>%
  ungroup() %>%
  select(-Observed_Counts) %>%
  group_by(FIPS, CTYNAME, sex_race) %>%
  gather(DataSet, n, 4:5) %>%
  left_join(name_shift_graph, by=c("sex_race"="old_name")) %>%
  mutate(DataSet = ifelse(DataSet == "Observed_Pct", "COVID-19 Tests Proportion", "Census Proportion"))

# shows all counties for the state
ggplot(see_diff_state_pa, aes(x=sex_race, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  facet_wrap(~FIPS)

# FILL in county here!
county_picked <- "Chester County"

ggplot(filter(see_diff_state_pa, CTYNAME==county_picked), aes(x=graph_name, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=paste(round(n,2)*100, "%")), position = position_dodge(0.9), hjust=1.15) +
  ggtitle(county_picked) +
  ylab("Percent") +
  xlab("Sex/Race Group") +
  labs(fill = "Populations") +
  theme_bw() +
  ggeasy::easy_center_title()

