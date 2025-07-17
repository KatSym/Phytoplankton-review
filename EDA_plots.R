data <- read.csv("Data/Data_extraction_table-changes_20250630.csv", header = T, check.names = F) %>% 
  janitor::clean_names() %>% 
  
  # drop unnecessary columns
  select(rayyan_id, system, multi_season, multi_year, incubation_vol_l, mesocosm_type, stratified, press,
         pulse, d_temp_control_treatment_c_value, additional_treatment, phyto, zoo, fish, sediment, macrophytes, 
         macroinvertebrates) %>% 
  
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
                             multi_season == "Not specified" ~ NA),
    multi_year = case_when(multi_year == "No" ~ 0,
                           multi_year == "Yes" ~ 1,
                           multi_year == "Not specified" ~ NA),
    # mesocosm_type = case_when(mesocosm_type == "Outdoor" ~ 1,
    #                           mesocosm_type == "Indoor" ~ 2,
    #                           mesocosm_type == "In-Situ" ~ 3,
    #                           mesocosm_type == "Not specified" ~ NA),
    stratified = case_when(stratified == "No" ~ 0,
                           stratified == "Yes" ~ 1,
                           stratified == "Not specified" ~ NA),
    
    disturbance = case_when(press == "No" & pulse == "Yes" ~ 1, # pulse
                            press == "Yes" & pulse == "No" ~ 2, # press
                            press == "Yes" & pulse == "Yes" ~ 3, # both
                            press == "Yes" & pulse == "Not specified" ~ 2), 
    system_gr = case_when(system == "shallow lake" ~ "freshwater",
                       system == "pond" ~ "freshwater",
                       system == "lake (not specified)" ~ "freshwater",
                       system == "deep lake" ~ "freshwater",
                       system == "Alpine lake" ~ "freshwater",
                       system == "Wetland" ~ "freshwater",
                       system == "brackish" ~ "brackish",
                       system == "Estuary" ~ "brackish",
                       system == "lagoon" ~ "marine",
                       system == "coastal (fjord, bay)" ~ "marine",
                       system == "off-shore/fully marine" ~ "marine"),
    phyto = case_when(phyto == "Yes"~1,
                      phyto == "No" ~0,
                      phyto == "Not specified" ~ NA),
    zoo = case_when(zoo == "Yes"~1,
                    zoo == "No" ~0,
                    zoo == "Not specified" ~ NA),
    fish = case_when(fish == "Yes"~1,
                     fish == "No" ~0,
                     fish == "Not specified" ~ NA),
    neritic = rowSums(pick(phyto, zoo, fish), na.rm = T),
    sediment = case_when(sediment == "Yes"~1,
                         sediment == "No" ~0,
                         sediment == "Not specified" ~ NA),
    macrophytes = case_when(macrophytes == "Yes"~1,
                            macrophytes == "No" ~0,
                            macrophytes == "Not specified" ~ NA),
    macroinvertebrates = case_when(macroinvertebrates == "Yes"~1,
                                   macroinvertebrates == "No" ~0,
                                   macroinvertebrates == "Not specified" ~ NA),
    benthic = rowSums(pick(sediment, macrophytes, macroinvertebrates), na.rm = T),
    all_trophic = neritic + benthic,
    .keep = "unused") |> 
  select(-c(phyto, zoo, fish, sediment, macrophytes, macroinvertebrates, d_temp_control_treatment_c_value))


plot.dat <- data |> 
  group_by(rayyan_id) |> 
  summarize(nhits = n(),
            across(where(is.character), ~ toString(unique(.))),
            across(where(~ !is.character(.)), ~ median(.))
  ) |> 
  mutate(disturbance = ifelse(disturbance == 1.5, 3, disturbance)) |> 
  ungroup()

plot.dat |> 
  group_by(mesocosm_type) |> 
  summarise(n.studies = n()) |> 
  ggplot(aes(x = n.studies, 
             y = fct_rev(factor(mesocosm_type, 
                                levels = c("In-Situ", "Outdoor", "Indoor",
                                           "Indoor, Outdoor", "Not specified"))))) +
  geom_bar(stat = "identity")+
  ylab("mesocosm type")+
  xlab("number of studies")+
  theme_minimal()
# ggsave("output/plots/mesoc_type.png", bg ="white")


plot.dat |> 
  group_by(system_gr) |> 
  summarise(n.studies = n()) |> 
  ggplot(aes(x = n.studies, 
             y = fct_rev(factor(system_gr, 
                                levels = c("marine", "freshwater", "brackish",
                                           "marine, freshwater"))))) +
  geom_bar(stat = "identity")+
  ylab("system")+
  xlab("number of studies")+
  theme_minimal()
# ggsave("output/plots/system.png", bg ="white")

mseas <- plot.dat |> 
  group_by(multi_season) |> 
  summarise(n.seas = n()) |> 
  ungroup()
myear <- plot.dat |> 
  group_by(multi_year) |> 
  summarise(n.years = n()) |> 
  ungroup()
strat <- plot.dat |> 
  group_by(stratified) |> 
  summarise(n.st = n()) |> 
  ungroup()

full_join(mseas, myear, by = join_by(multi_season == multi_year)) |> 
  left_join(strat, by = join_by(multi_season == stratified)) |> 
  rename(yes_no = multi_season,
         multi_season = n.seas,
         multi_year = n.years,
         stratified = n.st) |> 
  pivot_longer(cols = c(multi_season, multi_year, stratified), 
               names_to = "type", values_to = "n.studies") |> 
ggplot(aes(x = n.studies, 
           y = factor(yes_no),
           fill = type)) +
  geom_bar(stat = "identity", position="dodge")+
  scale_fill_viridis_d() +
  ylab("")+
  xlab("number of studies")+
  theme_minimal()
# ggsave("output/plots/type.png", bg ="white")


plot.dat |> 
  group_by(addit_treat) |> 
  summarise(n.studies = n()) |> 
  ggplot(aes(x = n.studies, 
             y = fct_rev(factor(addit_treat)))) +
  geom_bar(stat = "identity")+
  ylab("numbers of additional treatments")+
  xlab("number of studies")+
  theme_minimal()
# ggsave("output/plots/additional_treat.png", bg ="white")


plot.dat |> 
  select(neritic, benthic, all_trophic) |> 
  pivot_longer(cols = c(neritic, benthic, all_trophic), names_to = "trophic_sys", values_to = "levels") |> 
  group_by(trophic_sys, levels) |> 
  summarise(n.studies = n()) |> 
  ungroup() |> 

  ggplot(aes(x = n.studies, 
             y = factor(levels),
             fill = trophic_sys)) +
  geom_bar(stat = "identity", 
           position = position_dodge(preserve = "single"))+
  scale_fill_viridis_d() +
  ylab("number of trophic levels")+
  xlab("number of studies")+
  theme_minimal() +
  theme(legend.title = element_blank())
# ggsave("output/plots/trophic_levels.png", bg ="white")


table(plot.dat$neritic)


plot.dat |> 
  group_by(incubation_vol_l) |> 
  summarise(n.studies = n()) |> 
  arrange(incubation_vol_l, n.studies) |> 
  ggplot(aes(x = incubation_vol_l,
             y = n.studies)) +
    geom_point()
    geom_bar(stat = "identity")+
scale_discrete_manual(aesthetics = "fill", 
                      breaks = c("2", "10", "50", "100", "250", "1000"))+
  ylab("system")+
  xlab("number of studies")+
  theme_minimal()

hist(plot.dat$incubation_vol_l, nclass = 67)
hist(plot.dat$d.T, nclass = 38)


plot.dat |> 
  ggplot(aes(x = d.T)) +
  geom_histogram(bins=38)+
  xlab("delta temp")+
  theme_minimal()
# ggsave("output/plots/d_temp.png", bg ="white")


plot.dat |> 
  mutate(disturbance = case_when(disturbance == 1 ~ "pulse",
                                 disturbance == 2 ~ "press",
                                 disturbance == 3 ~ "both")) |> 
  group_by(disturbance) |> 
  summarise(n.studies = n()) |> 
    ggplot(aes(x = n.studies, 
               y = disturbance)) +
    geom_bar(stat = "identity", 
             position = position_dodge(preserve = "single"))+
  ylab("disturbance")+
  xlab("number of studies")+
  theme_minimal()
# ggsave("output/plots/disturbance.png", bg ="white")
