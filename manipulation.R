# Ambulance -----
# clear environment
rm(list=ls())

# load packages
library(tidyverse)
library(lubridate)
library(ggmap)

# import tas ambo dataset (Original file was in Excel format, I just opened it in Excel and saved the 2nd sheet (dataset) as a csv file)
tas_ambo <- read_csv(file = "160623 Sharon Campbell VACIS data 0801-1605.csv")

# lowercase all column names, much easier to work with lowercase!
tas_ambo <- 
  tas_ambo %>%
  rename_all(tolower) 

# have a look at the structure of the data (patient age, date and time are characters!,we need to make a proper timestamp from them later on)
str(tas_ambo)

# make a timestamp and call it 'date_time' (463 rows will fail to parse because the time_call_received == NULL, we will remove them as well)
# make date only and hour only variable (it can be useful for merging the exposure data)
tas_ambo <- 
  tas_ambo %>%
  mutate(date_time_AEDT = parse_date_time(paste(case_date, time_call_received), "%d/%m/%y %H:%M", tz = "Australia/Hobart")) %>% # make a datastamp from date and time
  filter(!is.na(date_time_AEDT)) %>% # remove the ones which couldn't be parsed due to 'time_call_received == NULL'
  mutate(date = as_date(date_time_AEDT), hour = hour(date_time_AEDT)) %>%
  mutate(patient_age = as.numeric(patient_age)) # lets take care of patient_age format as well


# remove duplicates
# there are quite a few duplicates!!,
# cases with the same date_time,case_no,destination,case_given_as,case_given_as_name,scene_post_code,scene_suburb_name,final_pri_assessment,team_name,age,and gender were assumed to be duplicates
# Observation with the realiest time will be selected from duplicates as duplicates may refer to the same event and the earliest time is the most relevant
tas_ambo <- 
  tas_ambo %>% 
  arrange(date_time_AEDT) %>% # arrange by date_time as distinct function keep the first row of duplicates and we want to have the earliest call time for duplicates
  distinct(
    date_time,case_no,destination,case_given_as,case_given_as_name,scene_post_code,scene_suburb_name,final_pri_assessment,team_name,age,gender,
    .keep_all = TRUE)

# we have postcode and suburb name, lets see how many unique postcodes and suburb names we have
nrow(tas_ambo %>% distinct(scene_suburb_name)) # we have 770 unique suburb names
nrow(tas_ambo %>% distinct(scene_post_code)) # we have 162 unique suburb names
# it seems that we have more suburb names, better to use this as it will give us better spatial resolution

# lowercase the scene_suburb_name
tas_ambo <- 
  tas_ambo %>%
  mutate(scene_suburb_name = tolower(scene_suburb_name))

# lets have a look at cad code (case_given_as) and its interpretation (case_given_as_name) 
tas_ambo %>% distinct(case_given_as)
tas_ambo %>% distinct(case_given_as_name)
tas_ambo %>% distinct(case_given_as, case_given_as_name)
tas_ambo %>% count(case_given_as, case_given_as_name) %>% arrange(desc(n))

# lowercase the case_given_as_name 
tas_ambo <- 
  tas_ambo %>%
  mutate(case_given_as_name = tolower(case_given_as_name))

# lets have a look at final_pri_assessment
nrow(tas_ambo %>% distinct(final_pri_assessment))
tas_ambo %>% count(final_pri_assessment) %>% arrange(desc(n)) %>% View()

# make a mapping data frame
fpa_df <- tas_ambo %>% distinct(final_pri_assessment)

fpa_df <- fpa_df %>% # any other recoding can be done at the bottom of this code following the same style if needed later on
  mutate (fpa_cat = final_pri_assessment %>%
      ifelse(str_detect(final_pri_assessment, "chest infection"), "Chest Infection",.) %>%
      ifelse(str_detect(final_pri_assessment, "pneumonia"), "Chest Infection",.) %>%
      ifelse(str_detect(final_pri_assessment, "bronchitis"), "Chest Infection",.) %>%
      ifelse(str_detect(final_pri_assessment, "bronchiolitis"), "Chest Infection",.) %>%
      ifelse(str_detect(final_pri_assessment, "anxiety"), "Anxiety",.) %>%
      ifelse(str_detect(final_pri_assessment, "short of breath"), "Short of Breath",.) %>%
      ifelse(str_detect(final_pri_assessment, "acute coronary syndrome"), "Acute Coronary Syndrome",.) %>%
      ifelse(str_detect(final_pri_assessment, "angina"), "Angina",.) %>%
      ifelse(str_detect(final_pri_assessment, "arrhythmia"), "Arrhythmia",.) %>%
      ifelse(str_detect(final_pri_assessment, "faint"), "Faint",.) %>%
      ifelse(str_detect(final_pri_assessment, "hypoglycaemia"), "Hypoglycaemia",.) %>%
      ifelse(str_detect(final_pri_assessment, "asthma"), "Asthma",.) %>%
      ifelse(str_detect(final_pri_assessment, "transient ischaemic attack"), "Transient Ischaemic Attack",.) %>%
      ifelse(str_detect(final_pri_assessment, "acute pulmonary oedema"), "Acute Pulmonary Oedema",.) %>%
      ifelse(str_detect(final_pri_assessment, "stroke"), "Stroke",.) %>%
      ifelse(str_detect(final_pri_assessment, "chronic obstructive pulmonary disease"), "Chronic Obstructive Pulmonary Disease",.) %>%
      ifelse(str_detect(final_pri_assessment, "hyperglycaemia"), "Hyperglycaemia",.) %>%
      ifelse(str_detect(final_pri_assessment, "acute myocardial infarction"), "Acute Myocardial Infarction",.) %>%
      ifelse(str_detect(final_pri_assessment, "croup"), "Croup",.) %>%
      ifelse(str_detect(final_pri_assessment, "cardiac failure"), "Cardiac Failure",.) %>%
      ifelse(str_detect(final_pri_assessment, "cardiac arrest"), "Cardiac Arrest",.) %>%
      ifelse(str_detect(final_pri_assessment, "panic attack"), "Panic Attack",.))

tas_ambo <- 
  tas_ambo %>%
  left_join(fpa_df)

tas_ambo %>% count(fpa_cat) %>% arrange(desc(n))

# bring the important variables to the front
tas_ambo <- 
  tas_ambo %>%
  select(date_time_AEDT, date, hour, scene_suburb_name, gender, patient_age, case_given_as_name, fpa_cat, everything())

# find all unique suburb names
suburb_name <- tas_ambo %>% distinct(scene_suburb_name)
test <- 
  suburb_name[1:10, ] %>% 
  mutate(address = paste(scene_suburb_name, "Tasmania, Australia"))
geocode(test$scene_suburb_name)

str(test)
test %>% mutate_geocode(address)

#write_csv(suburb_name, "suburb_name.csv")









# BLANKET----
# clear environment
rm(list=ls())

# load packages
library(tidyverse)
library(lubridate)
library(ggmap)


# import datasets
blanket1 <- read_csv("comb_met_pm.csv")
#blanket2 <- read_csv("comb_airrater.csv")
geocode <- read_csv("blanket_station_geocodes.csv")

# structure of each BLANKET dataset
str(blanket1)
str(blanket2)

# lowercase all the columns
blanket1 <- 
  blanket1 %>%
  rename_all(tolower)
#blanket2 <- 
  #blanket2 %>%
  #rename_all(tolower)

# rename 'code' in blanket2 to 'station'
#blanket2 <- 
  #blanket2 %>%
  #rename(station = code)

# add lon, and lat to blanket1
blanket1 <- 
  blanket1 %>%
  left_join(geocode) %>%
  select(-alt, -station_name)

# change hour from character to numeric
blanket1 <- blanket1 %>% mutate(hour = as.numeric(hour))
#blanket2 <- blanket2 %>% mutate(hour = as.numeric(hour))

# make timestamp
blanket1 <- 
  blanket1 %>%
  mutate(date_time_AEST = parse_date_time(paste(date, hour), "%y/%m/%d %H", tz = "Australia/Queensland")) %>%
  mutate(date_time_AEDT = with_tz(date_time_AEST, tz = "Australia/Tasmania"))


