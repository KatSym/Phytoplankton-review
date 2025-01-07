## Data cleaning of the extraction table
## The table was downloaded from Google sheets and saved as a CSV file

# install.packages(c("CoordinateCleaner", "janitor"))
library(tidyverse)
library(CoordinateCleaner)

full_table <- read.csv("Data/Data_Extraction_Table.csv", header = T, check.names = F) %>% 
  janitor::clean_names() %>% 
  
  # drop unnecessary columns
  select(-c(title, abstract, authors, doi)) %>%
  
  # rename columns to shorter names
  # lat,long in decimal degrees; dates in yyyy-mm-dd; volumes in L; temperatures in deg C; time in days
  rename(lat = 6,
         long = 7,
         start_date = 10,
         end_date = 11,
         incubation_vol = 12,
         n_mesocoscm_repl = 15,
         trophic_state = 16, 
         mutrient_details = 18,
         n_pulses = 29,
         sample_before_T_change = 30,
         sample_during_T_incr = 31,
         sample_pateau_press = 32,
         sample_cooldown = 33,
         sample_recovery = 34,
         control_T = 35,
         max_T = 38,
         time_before_T = 36,
         time_increase_T = 37,
         time_max_T = 39,
         time_cooldown = 40,
         time_recovery = 41,
         base_T_start = 42,
         base_T_end = 43,
         T_fluctuations = 44) %>% 
  
  # drop rejected studies - empty in the columns below
  filter(country != "",
         lat != "",
         long != "") %>% 
  
  # fix comments and weird 'x' column
  mutate(Comments = case_when(comments == "" ~ x,
                              .default = comments),
            comments = NULL,
            x = NULL) %>% 
  mutate_at(c("n_pulses", "time_before_T", "time_increase_T", 
              "time_max_T", "time_cooldown", "time_recovery"),
            ~replace_na(., "NAp"))

