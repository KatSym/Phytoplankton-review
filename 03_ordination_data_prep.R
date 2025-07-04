## The table was downloaded from Google sheets as a CSV file

# install.packages("janitor")
library(tidyverse)

# Function to parse and compute median for delta temp - generated from ChatGPT
get_row_median <- function(entry) {
  # Split by semicolon
  parts <- unlist(strsplit(entry, ";"))
  
  nums <- unlist(lapply(parts, function(x) {
    # Match ranges using regex: supports negative/decimal ranges
    if (grepl("^-?[0-9.]+--?[0-9.]+$", x)) {
      bounds <- as.numeric(unlist(strsplit(x, "--?")))
      return(seq(bounds[1], bounds[2], by = 1))
    } else if (grepl("^-?[0-9.]+-[-]?[0-9.]+$", x)) {
      bounds <- as.numeric(unlist(strsplit(x, "-")))
      return(seq(bounds[1], bounds[2], by = 1))
    } else {
      return(as.numeric(x))
    }
  }))
  
  return(median(nums, na.rm = TRUE))
}



# read data
ordination_data <- read.csv("Data/Data_extraction_table-changes_20250630.csv", header = T, check.names = F) %>% 
  janitor::clean_names() %>% 
  
  # drop unnecessary columns
  select(rayyan_id, system, multi_season, multi_year, incubation_vol_l, mesocosm_type, stratified, press,
         pulse, d_temp_control_treatment_c_value, additional_treatment) %>% 
  
  # filter out excluded studies
  filter(d_temp_control_treatment_c_value != "") %>% 
  
  # some corrections
  mutate_all(~na_if(., "NP")) %>% 
  mutate_all(~na_if(., "NE")) %>% # this is probably doable in one line but I don't know how
  mutate(
    system = case_when(system == "shallow lake " ~ "shallow lake",
                       system == "System" ~ "shallow lake",
                       system == "shallow lake/pond" ~ "pond",
                       system == "sea" ~ "off-shore/fully marine",
                       .default = system),
    mesocosm_type = case_when(mesocosm_type == "No" ~ "Not specified",
                              mesocosm_type == "" ~ "Not specified",
                              .default = mesocosm_type),
    incubation_vol_l = as.numeric(case_when(incubation_vol_l == "~1763" ~ "1763",
                                            incubation_vol_l == "2,7" ~ "2.7",
                                            .default = incubation_vol_l)),
    d_temp_control_treatment_c_value = case_when(
      d_temp_control_treatment_c_value == "4; fluctuating 4" ~ "4; 0.27-8.7",
      d_temp_control_treatment_c_value == "3; variable" ~ "3; 2.8",
      .default = d_temp_control_treatment_c_value),
    
    # calculate median for delta T - the original column is there for checking
    d.T = sapply(d_temp_control_treatment_c_value, get_row_median),
    addit_treat = ifelse(is.na(additional_treatment), 0, str_count(additional_treatment, ";") + 1),
    .keep = "unused"
  ) %>% 
  # recode values
  mutate(
    multi_season = case_when(multi_season == "No" ~ 0,
                             multi_season == "Yes" ~ 1,
                             multi_season == "Not specified" ~ 2),
    multi_year = case_when(multi_year == "No" ~ 0,
                           multi_year == "Yes" ~ 1,
                           multi_year == "Not specified" ~ 2),
    mesocosm_type = case_when(mesocosm_type == "Outdoor" ~ 1,
                              mesocosm_type == "Indoor" ~ 2,
                              mesocosm_type == "In-Situ" ~ 3,
                              mesocosm_type == "Not specified" ~ 4),
    stratified = case_when(stratified == "No" ~ 0,
                           stratified == "Yes" ~ 1,
                           stratified == "Not specified" ~ 2),
    disturbance = case_when(press == "No" & pulse == "Yes" ~ 1, # pulse
                            press == "Yes" & pulse == "No" ~ 2, # press
                            press == "Yes" & pulse == "Yes" ~ 3, # both
                            press == "Yes" & pulse == "Not specified" ~ 2), 
    system = case_when(system == "shallow lake" ~ 1.1,
                     system == "pond" ~ 1.2,
                     system == "lake (not specified)" ~ 1.3,
                     system == "deep lake" ~ 1.4,
                     system == "Alpine lake" ~ 1.5,
                     system == "Wetland" ~ 1.6,
                     system == "brackish" ~ 2.1,
                     system == "Estuary" ~ 2.2,
                     system == "lagoon" ~ 3.1,
                     system == "coastal (fjord, bay)" ~ 3.2,
                     system == "off-shore/fully marine" ~ 3.3),
    .keep = "unused"
  )

ordination_data$d.T <- as.numeric(ordination_data$d.T)

write.csv(ordination_data, "ordination_data.csv", quote = F, row.names = F)
