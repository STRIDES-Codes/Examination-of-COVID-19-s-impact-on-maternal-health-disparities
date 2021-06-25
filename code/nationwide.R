# Looking at entire covid dataset bnation-wide
# This is looking at representatio in COVID cases in general
# similar code to state_picker.R but looking at US as a whole - see 
#   state_picker.R for now for mode commented code

library(tidyverse)
library(maps)
library(ggthemes)
library(ggeasy)

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

observed_cases_usa <- entire_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown")),
         age_group != "0 - 17 years") %>%
  count(sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("race"="new_race") %>%
  mutate(sex_race = paste(race, toupper(sex), sep = "_")) %>%
  # group_by(FIPS) %>%
  # mutate(percent = n/sum(n)) %>%
  select("sex_race", "Observed_Counts" = "n") %>%
  spread(sex_race, Observed_Counts, fill = 0) %>%
  gather(sex_race, Observed_Counts, 1:12)

pop_data <- read_csv("Downloads/cc-est2019-alldata.csv")

cleaned_pop_data_usa <- pop_data %>%
  filter(YEAR == 12, AGEGRP >= 4) %>%
  select(c(7:22)) %>%
  summarise_at(vars(-AGEGRP), sum)

tot_pop <- cleaned_pop_data_usa$TOT_POP
cleaned_pop_data_states_usa <- cleaned_pop_data_usa %>%
  mutate_all(funs(./tot_pop)) #%>%
  # select(-TOT_POP) %>%
  # rename("MALE" = "TOT_MALE", "FEMALE" = "TOT_FEMALE") 

props_states_usa <- cleaned_pop_data_states_usa %>%
  select(c(4:15)) %>%
  gather(sex_race, Census_Props, 1:12) %>%
  left_join(observed_cases_usa, by=c("sex_race"))

see_diff_state_usa <- props_states_usa %>%
  mutate(Observed_Pct = Observed_Counts/sum(Observed_Counts)) %>%
  select(-Observed_Counts) %>%
  group_by(sex_race) %>%
  gather(DataSet, n, 2:3) %>%
  left_join(name_shift_graph, by=c("sex_race"="old_name")) %>%
  mutate(DataSet = ifelse(DataSet == "Observed_Pct", "COVID-19 Tests Proportion", "Census Proportion"))

ggplot(filter(see_diff_state_usa), aes(x=graph_name, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  ggtitle("United States") +
  ylab("Percent") +
  xlab("Sex/Race Group") +
  labs(fill = "Populations") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggeasy::easy_center_title()
