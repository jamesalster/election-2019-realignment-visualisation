
### Create dataset for analysis 
### Using YouGov MRP  and House of Commons library data, downloaded early 2020.
### Original files not provided on github, only the transformed dataset

library(tidyverse)

#### Import Data ####

## 2017 Election

election_17 <- read_csv("source_data/YouGov_MRP_2017/HoC-GE2017-constituency-results.csv") %>%
  #get percentages
  mutate(across(c(majority:other), ~ .x / valid_votes * 100)) %>% 
  #remove NI. left_join() below will remove it from all other datasets as well.
  filter(! region_name %in% c("Northern Ireland", "Wales", "Scotland")) %>%
  #id column identifies constituencies and will be used to join the tables
  select(id = ons_id,
         constituency = constituency_name,
         first_party17 = first_party,
         majority:pc, other) %>%
  rename_with(~str_c(., "17"), majority:other) %>%
  mutate(first_party17 = tolower(first_party17))

## 2019 Election

election_19 <- read_csv("source_data/yougov_MRP_2019/constituency_results_dec.csv") %>%
  select(-constituency) %>%
  rename_all(tolower) %>%
  rename_with(~str_c(., "19"), -code) %>%
  rename(id = code) 

## Brexit referendum

leave <- read_csv("source_data/constituency_data/leave_vote.csv") %>%
  select(id = `ONS ID`,
         leave_vote = `TO USE`) %>%
  mutate(leave_vote = as.numeric(str_replace_all(leave_vote, ",", ".")),
         leave_vote = round(leave_vote * 100, 1))

## Age groups

pop_age <- read_csv("source_data/constituency_data/population-by-age.csv") %>%
  filter(Date == "2018") %>% #age in 2018
  select(age_group = `Age group`,
         id = ONSConstID,
         date = Date,
         percent = `Const%`) %>%
  #remove percent signs
  mutate(percent = as.numeric(str_replace_all(percent, "%", ""))) %>%
  #get it into wide form
  pivot_wider(names_from = age_group, values_from = percent, names_repair = "minimal")

## Misc social mobility data

social <- read_csv("source_data/constituency_data/social_mobility_constituencies_values.csv") %>%
  #just get a few selected columns
  select(id = CON_code,
         two_a_levels = 14,
         median_weekly_salary = 16,
         house_prices_vs_salary = 17,
         manag_prof = 18,
         less_than_living_wage = 19,
         home_owners = 20) %>%
  #remove percent signs
  mutate(across(c(two_a_levels, manag_prof, less_than_living_wage, home_owners),
            ~ str_replace_all(., "%", "") %>% as.numeric))

## Business data
business <- read_csv("source_data/constituency_data/Business-numbers.csv") %>%
  filter(DateOfDataset == "15/03/2019") %>% #2019 figures only
  select(id = ONSConstID,
         number_of_businesses = BusConstNumber,
         net_change_businesses = BusConstNetChange) 
  
## Wages data
wages <- read_csv("source_data/constituency_data/Wages.csv") %>%
  select(id = ONSConstID,
         median_wage = WageMedianConst)

## Join all the data
data <- left_join(election_17, election_19, by = "id") %>%
  left_join(leave, by = "id") %>%
  left_join(pop_age, by = "id") %>%
  left_join(social, by = "id" ) %>% 
  left_join(business, by = "id") %>%
  left_join(wages, by = "id") %>%
  #create column for change in labour vote
  mutate(lab_change = lab19 - lab17)


#### Write to File ####

write_rds(data, "constituency_data.rds.gz", compress = "gz")
