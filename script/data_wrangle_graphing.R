# Final project - making a graph and tables
# Jonathan Huang 2023-04-10

#load libraries
library(tidyverse)
library(here)

fish <- read_csv(here("data","songs_clean.csv")) 

songs_count <- fish %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.numeric(year)) %>%    
  summarise(count = sum(count), #sum the count through the years
            total_area_sampled = sum(total_area_sampled)) %>%    
  uncount(count) %>%                   #reorder so each row = 1 count
  select(year, species_code,reef_code,total_length,total_area_sampled) #reorder df order

#get summary of counts by site by year by species

songs_count_sum <- fish %>% 
  group_by( year,total_length, species_code, reef_code) %>%  #group analysis 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as_factor(year)) %>%    
  summarise(count = sum(count) #sum the count through the years
) 

print(songs_count_sum)
