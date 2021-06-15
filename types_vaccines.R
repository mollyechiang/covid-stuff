# libraries 
library(tidyverse)
library(reshape2)
library(janitor)

# percent of administered doses as of 6/8/21 that are pfz/mod/jj
# by state

####---------DATA LOAD----------

vaccines <- read.csv("https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD") %>%
  clean_names() %>% 
  mutate(date = as.Date(date, '%m/%d/%Y'))

####---------ANALYSIS----------
x <- vaccines %>% 
  select(date, location, administered_janssen, administered_moderna, 
         administered_pfizer) %>%
  # filter(date == max(date)) %>% 
  rename('Johnson and Johsnon' = administered_janssen, 'Moderna' = administered_moderna, 
         'Pfizer' = administered_pfizer) %>%
  melt(id.vars=c("date", "location"), 
       variable.name="vaccine_type", value.name="count") %>%
  arrange(location) %>% 
  group_by(location) %>%  
  filter(location != "US") %>% 
  filter(location != "BP2") %>% 
  filter(location != "DD2") %>% 
  filter(location != "DS2") %>% 
  filter(location != "FM") %>% 
  filter(location != "GU") %>% 
  filter(location != "IH2") %>% 
  filter(location != "LTC") %>% 
  filter(location != "MH") %>%
  filter(location != "VA2") %>% 
  filter(location != "RP") %>%
  filter(location != "PR") %>% 
  filter(location != "MP") %>%
  filter(location != "VI") %>% 
  filter(location != "AS") %>%
  group_by(location, vaccine_type) %>%
  mutate(total = sum(count)) %>% 
  select(-date, -count) %>%
  distinct(location, vaccine_type, total) %>% 
  ungroup() %>%
  group_by(location) %>%
  mutate(percent = total/sum(total)*100)
  
  
ggplot(x, aes(x = location, y = count, fill = vaccine_type)) +
  geom_col(position = "fill") +
  coord_flip() + 
  labs(x = "",
       y = "Breakdown",
       title = "States",
       fill = "Vaccine") +
  theme_minimal()



