library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)

rc_disaster_data = read_csv("/users/nickbecker/downloads/2009-2014_RedCross_DisasterCases.csv")
str(rc_disaster_data)
glimpse(rc_disaster_data)

#load("/Users/nickbecker/Documents/Github/Superseded/arc_smoke_alarm_old_2/data_from_david/2015-09-16.Rdata") 
#all.equal(rc_disaster_data$case_num, RC_response$case_num)


rc_disaster_data %>% group_by(case_num) %>%
  summarise(number = n()) %>% arrange(desc(number))

filter(rc_disaster_data, case_num == "1-78233518")


# Count number of cases per "region"
region_cases = rc_disaster_data %>% 
  group_by(region_name) %>%
  summarise(number_cases = length(unique(case_num))) %>%
  as.data.frame() %>%
  arrange(desc(number_cases))

head(region_cases, 10)
str(region_cases)

# reorder factors for plotting
region_cases$region_name = factor(region_cases$region_name,
                               levels = region_cases[order(region_cases$number_cases, decreasing = FALSE),1])

ggplot(region_cases, aes(x = region_name, y = number_cases)) + geom_point() +
  coord_flip()


# Count number of cases per state-county
county_cases = rc_disaster_data %>% 
  group_by(primary_affected_state, primary_affected_county) %>%
  summarise(number_cases = length(unique(case_num))) %>%
  as.data.frame() %>%
  arrange(desc(number_cases))

head(county_cases, 10)


# Count number of cases per zipcode
zipcode_cases = rc_disaster_data %>% 
  group_by(esri_zip) %>%
  summarise(number_cases = length(unique(case_num))) %>%
  as.data.frame() %>%
  arrange(desc(number_cases))

head(zipcode_cases, 10)


# Count number of cases per state
state_cases = rc_disaster_data %>% 
  group_by(esri_state) %>%
  summarise(number_cases = length(unique(case_num))) %>%
  as.data.frame() %>%
  arrange(desc(number_cases))

head(state_cases, 10) # 61425 + 41 missing a state

state_cases_nonmissing = filter(state_cases, !is.na(esri_state),
                                esri_state != "") %>%
  mutate(esri_state = factor(esri_state))

# reorder factors for plotting
state_cases_nonmissing$esri_state = factor(state_cases_nonmissing$esri_state,
                                  levels = state_cases_nonmissing[order(state_cases_nonmissing$number_cases, decreasing = FALSE),1])
str(state_cases_nonmissing)
ggplot(state_cases_nonmissing, aes(x = esri_state, y = number_cases)) + geom_point() +
  coord_flip()


