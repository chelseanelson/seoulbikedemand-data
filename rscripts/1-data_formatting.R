### Data Made to use in Shiny

### loading packages and data ----
library(shiny)
library(tidyverse)
library(ggcorrplot)
library(shinythemes)
library(patchwork)
library(car)
library(broom)
seoulbike_data <- read_rds("data/seoulbike_data.rds")

### Data Formatting ----

# making the correlation matrix
correlation_matrix <- seoulbike_data %>% 
  select(rented_bike_count, temperature_c, humidity_percent, 
         wind_speed_m_s,visibility_10m, dew_point_temperature_c, 
         solar_radiation_mj_m2,rainfall_mm, snowfall_cm) %>% 
  cor()

# changing variable names 
custom_labels <- c(
  "rented_bike_count" = "Rented Bike Count",
  "temperature_c" = "Temperature (C)",
  "humidity_percent" = "Humidity (%)", 
  "wind_speed_m_s" = "Wind Speed (m/s)",
  "visibility_10m" = "Visibility (10m)",
  "dew_point_temperature_c" = "Dew Point Temperature (C)",
  "solar_radiation_mj_m2" = "Solar Radiation (MJ/m2)",
  "rainfall_mm" = "Rainfall (mm)", 
  "snowfall_cm" = "Snowfall (cm)"  
)

colnames(correlation_matrix) <- rownames(correlation_matrix) <- custom_labels
correlation_matrix <- round(correlation_matrix, 1)

# making the actual correlation matrix plot
correlation_matrix_plot <- ggcorrplot(correlation_matrix, 
                                      lab = TRUE, outline.color = "white",
                                      type = "lower") + 
  labs(title = "Correlations Between Weather Conditions and Rent Bike Count",
       x = NULL,
       y = NULL,
       subtitle = "Plot 1") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 13, hjust = 0.5))


# correlation based on bike rent count
seoulbike_data_numerical <- seoulbike_data %>%  
  select(rented_bike_count, temperature_c, humidity_percent, wind_speed_m_s,
         visibility_10m, dew_point_temperature_c, solar_radiation_mj_m2,
         rainfall_mm, snowfall_cm) 

# still need to work on making the names right
colnames(seoulbike_data_numerical)[2:9] <- c(
  "Temperature (C)",
  "Humidity (%)", 
  "Wind Speed (m/s)",
  "Visibility (10m)",
  "Dew Point Temperature (C)",
  "Solar Radiation (MJ/m2)",
  "Rainfall (mm)", 
  "Snowfall (cm)"  
)

# making second correlation matrix 
corr_plot <- seoulbike_data_numerical %>%
  lares::corr_var(rented_bike_count) + 
  labs(title = "Correlations of Bike Rent Count",
       subtitle = "Plot 2") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
        plot.margin = margin(t = 10, r = 60, b = 30, l = 60, unit = "pt"),
        plot.subtitle = element_text(size = 13, hjust = 0.5))


# combining the plots
correlation_plots <- correlation_matrix_plot + corr_plot + plot_layout(ncol = 2)

# average bike rent count per month
average_per_month <- seoulbike_data %>%
  group_by(month = month(date, label = TRUE)) %>%
  summarise(average_rented_bike_count = mean(rented_bike_count))

# average bike rent count per season
average_per_season <- seoulbike_data %>%
  group_by(seasons) %>%
  summarise(average_rented_bike_count = mean(rented_bike_count))