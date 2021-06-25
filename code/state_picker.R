# Looking at entire covid dataset by state

# This is looking at representatio in COVID cases in general

# DC (11) and Wyoming (56) did not have any observations, because their counties aren't listed
#   DC does not have any counties, but Wyoming does... (weird)
# TO DO: adjust for this county issue when getting overall state props ^

library(tidyverse)
library(maps)
library(ggthemes)
library(ggeasy)

entire_covid_data <- read.csv("Downloads/COVID-19_Case_Surveillance_Public_Use_Data_with_Geography_full.csv")

# name maping for demographic groups
name_shift <- data.frame(old_race = c("Asian", "Black", "American Indian/Alaska Native", 
                                      "Native Hawaiian/Other Pacific Islander", "White", "Multiple/Other"),
                         new_race = c("AA", "BA", "IA", "NA", "WA", "TOM"))

name_shift_graph <- data.frame(graph_name = c("Asian (M)", "Black (M)", "American Indian/Alaska Native (M)", 
                                      "Native Hawaiian/Other Pacific Islander (M)", "White (M)", "Multiple/Other (M)",
                                      "Asian (F)", "Black (F)", "American Indian/Alaska Native (F)", 
                                      "Native Hawaiian/Other Pacific Islander (F)", "White (F)", "Multiple/Other (F)"),
                         old_name = c("AA_MALE", "BA_MALE", "IA_MALE", "NA_MALE", "WA_MALE", "TOM_MALE",
                                      "AA_FEMALE", "BA_FEMALE", "IA_FEMALE", "NA_FEMALE", "WA_FEMALE", "TOM_FEMALE"))

#subset to entities with sex/race listed
observed_cases <- entire_covid_data %>%
  filter(!is.na(sex),
         !(sex %in% c("Missing", "Unknown")),
         !is.na(race),
         !(race %in% c("Missing", "Unknown")),
         age_group != "0 - 17 years") %>% #adults only
  count(county_fips_code, sex, race) %>%
  left_join(name_shift, by = c("race"="old_race")) %>%
  select(-race) %>%
  rename("FIPS"="county_fips_code","race"="new_race") %>%
  mutate(sex_race = paste(race, toupper(sex), sep = "_")) %>%
  select("FIPS", "sex_race", "Observed_Counts" = "n") %>%
  spread(sex_race, Observed_Counts, fill = 0) %>%
  gather(sex_race, Observed_Counts, 2:13)

#gets observed counts by sex/race
observed_cases_states <- observed_cases %>%
  mutate(STATE = round(FIPS/1000)) %>%
  select(-FIPS) %>%
  group_by(STATE, sex_race) %>%
  summarise_all(sum)

# 2019 census population data
pop_data <- read_csv("Downloads/cc-est2019-alldata.csv")

# sums population by state,county
cleaned_pop_data <- pop_data %>%
  filter(YEAR == 12, AGEGRP >= 4) %>%
  select(c(2,3,4, 7:22)) %>%
  mutate(FIPS = ifelse(as.numeric(STATE) < 10, (as.numeric(STATE)*10000)+as.numeric(COUNTY), 
                       (as.numeric(STATE)*1000)+as.numeric(COUNTY))) %>%
  select(FIPS, everything(), -COUNTY) %>%
  group_by(STATE, STNAME, FIPS) %>%
  summarise_at(vars(-AGEGRP), sum)

# summarizes by state
cleaned_pop_data_states <- cleaned_pop_data %>%
  select(-FIPS) %>%
  group_by(STATE, STNAME) %>%
  summarise_all(sum) %>%
  ungroup() %>%
  group_by(STATE, STNAME, TOT_POP) %>%
  summarize_all(funs(./TOT_POP)) %>%
  select(-TOT_POP) %>%
  rename("MALE" = "TOT_MALE", "FEMALE" = "TOT_FEMALE") %>%
  mutate(STATE = as.numeric(STATE))

# gets prop of state population by demographic group, and joins counts of covid entities
props_states <- cleaned_pop_data_states %>%
  mutate(STATE = as.numeric(STATE)) %>%
  select(c(1:2, 5:16)) %>%
  gather(sex_race, Census_Props, 3:14) %>%
  left_join(observed_cases_states, by=c("STATE", "sex_race"))

# gets prop of covid entities (with the census props)
see_diff_state <- props_states %>%
  group_by(STATE) %>%
  mutate(Observed_Pct = Observed_Counts/sum(Observed_Counts)) %>%
  ungroup() %>%
  select(-Observed_Counts, -STATE) %>%
  group_by(STNAME, sex_race) %>%
  gather(DataSet, n, 3:4) %>%
  left_join(name_shift_graph, by=c("sex_race"="old_name")) %>%
  mutate(DataSet = ifelse(DataSet == "Observed_Pct", "COVID-19 Tests Proportion", "Census Proportion"))

# states picked for ggplot graphic
states_to_sub <- c("Nevada", "Pennsylvania","North Dakota")

# Plot of props for states picked
ggplot(filter(see_diff_state, STNAME %in% states_to_sub), aes(x=graph_name, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  facet_grid(~STNAME) +
  ggtitle("United States") +
  ylab("Percent") +
  xlab("Sex/Race Group") +
  labs(fill = "Populations") +
  theme_bw() +
  theme(legend.position="bottom") +
  ggeasy::easy_center_title()

# plot of specific state picked 
state_picked <- "Pennsylvania"
ggplot(filter(see_diff_state, STNAME==state_picked), aes(x=sex_race, y=n, fill=DataSet)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=paste(round(n,2)*100, "%")), position = position_dodge(0.9)) +
  labs(title = state_picked)

states <- unique(props_states$STATE)
