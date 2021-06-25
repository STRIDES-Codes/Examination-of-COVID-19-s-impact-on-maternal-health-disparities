
pop_dataset = read_csv("Documents/Hackathons/NIH/cc-est2019-NY-pop-totals.csv")

pop_ny_cleaned <- pop_dataset %>%
  filter(YEAR == 12, AGEGRP %in% c(4:18)) %>%
  select(-SUMLEV, -STATE, -STNAME, -YEAR) %>%
  group_by(COUNTY, CTYNAME) %>%
  summarize_all(sum) %>%
  rename("FIPS"="COUNTY") %>%
  select(-AGEGRP)

write.csv(pop_ny_cleaned, "Documents/Hackathons/NIH/population_ny_cleaned.csv")
