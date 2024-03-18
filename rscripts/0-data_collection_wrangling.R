## loading packages 
library(tidyverse)

# Importing CSV file ----
seoulbike_data <- read_csv("data/SeoulBikeData.csv",  
                           locale = locale(encoding = "latin1")) %>%
  janitor::clean_names()
seoulbike_data

# Assessing the Data ----
# There are 8760 observations. With 15 columns in total, with 3 being 
# categorical, two date time related variables, and 10 numeric variables. 

## Checking for Missingness ----
seoulbike_data %>% skimr::skim_without_charts()
seoulbike_data %>% naniar::miss_var_summary()

# There is no missingness in my data

# Data Wrangling ----
# turning the date variable into a date variable


seoulbike_data <- seoulbike_data %>% 
  mutate(
    date = as_date(date, format = "%d/%m/%Y"),
    seasons = as_factor(seasons),
    holiday = as_factor(holiday),
    functioning_day = as_factor(functioning_day)
  )

seoulbike_data

# combining the two columns 
seoulbike_data <- seoulbike_data %>% 
  mutate(
    datetime = make_datetime(year(date), month(date), day(date), hour = hour))
# showcasing this 
seoulbike_data %>% relocate(datetime)

seoulbike_data %>% distinct(seasons)

write_rds(seoulbike_data, file = "data/seoulbike_data.rds")
