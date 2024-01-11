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

### Start of Code For Shiny ----
# remember to annotate every plot that I do. 
ui <- fluidPage(
    theme = shinytheme("paper"),
    # Application Title
    titlePanel("Seoul Bike Sharing Demands Based on Weather"),
    
    tags$head(
        tags$style(HTML("
    .tab-content {
      padding-top: 20px;
    }
    "))
    ),
    
    tags$div(
        tags$div(
            class = "app_title", 
            tags$h4(
                "Chelsea Nelson"
            )
        )),
    
    # Creating the Different Tabs
    tabsetPanel(
        tabPanel("Quick Overview of Dashboard",
                 sidebarLayout(
                     sidebarPanel(width = 6,
                        h4("Hello! And Welcome to the Seoul Bike 
                               Sharing Demands Based on Weather Dashboard."),
                        p("This dashboard was created to help see the 
                              relationship between bike sharing demands in 
                              Seoul, Korea for a full year from December 1st, 
                              2017 to November 30th, 2018 to that of 
                              different forms of meterological data, such as 
                              temperature, humidity, wind speed, visibility, 
                              dew point, solar radiation, snowfall, and 
                              rainfall."),
                        p(a("The data used was sourced from the
                              Seoul Bike Sharing Demand Predicition dataset on 
                              Kaggle.com.", 
                          href = 
    "https://www.kaggle.com/datasets/joebeachcapital/seoul-bike-sharing/data"), 
    "Specifically, this dataset provides insights 
                              into the number of bikes
                              rented per hour during our associated time 
                              period, while also
                              showcasing the related weather information for 
                              that time and date. Thus from this information, 
                              we can draw and create insights from the 
                              visualizations that I have included."),
                          p("I choose to personally look at this data, 
                          because I really 
                          love to see the usage of more public transportation 
                          by people around the globe. While additionally, 
                          I think it is benefical and interesting to portray 
                          and showcase which meterological factors play 
                          larger roles in the collective decision to use or 
                          not use sources of public transportation, such as 
                          bike sharing."),
    p("If you would like to see the code behind this Shiny App, I have provided the", a("Github Repo link", href = "https://8cfzgy-chelsea-nelson.shinyapps.io/seoulbikedemand_app/"), "here."),
                        br(),
                        h4("Explaining the Visualizations Shown"),
                        p(strong("Plot 1"),"is a correlation matrix plot that 
                          visualizes the relationships between the various 
                          weather conditions (temperature, humidity, wind speed, 
                          visibility, dew point temperature, solar radiation, 
                          rainfall, snowfalll) and the number of rented bikes 
                          in Seoul. The plot uses a color-coded matrix to 
                          represent the strength and direction of correlations, 
                          providing insights into how different weather factors 
                          may influence bike rental counts and each other."),
                        p(strong("Plot 2"),"complements", strong("Plot 1"), 
                          "by providing a more focused view 
                          specifically exploring the 
                          correlations of the weather conditions with the 
                          rented bike count."),
                      
                     align = "center"),
                     mainPanel(width = 6,
                         plotOutput("CorrelationPlot_one_one"),
                         plotOutput("CorrelationPlot_one_two"),
                     )
                 )),
                 # do a sidepanel and mainpanel for this one ,
        tabPanel("Looking at Individual Weather Conditions", 
                 fluidRow(
                     column(6,
                            selectInput("one_var", 
                                        "Which variable would you like to 
                                        look at", 
                                        choices = c(
                                            "Temperature (C)" = "temperature_c",
                                            "Humidity (%)" = "humidity_percent", 
                                            "Wind Speed (m/s)" = 
                                                "wind_speed_m_s",
                                            "Visibility (10m)" = 
                                                "visibility_10m", 
                                            "Dew Point Temperature (C)" = 
                                                "dew_point_temperature_c",
                                            "Solar Radiation (MJ/m2)" = 
                                                "solar_radiation_mj_m2",
                                            "Rainfall (mm)" = "rainfall_mm", 
                                            "Snowfall (cm)" = "snowfall_cm"
                                           )),
                            plotOutput("CorrelationPlots_two"), wellPanel(
                            h4("Explaining the Visualizations Shown"),
                            p(strong("Plot 3"), "depending on which weather 
                              variable is chosen to looked
                              at, a scatter plot with a 
                              linear regression line is created to visualize the relationship 
                              between the number of rented bikes and the selected 
                              variable. Thus providing insights on the 
                              singular level of how different weather 
                              conditions may influence bike rental patterns 
                              in Seoul.")),
                            align = "center"),
                     column(6, mainPanel(plotOutput("bivariate"), 
                                         align = "right"
        )))),
        tabPanel("Looking at Mutliple Weather Conditions",
                 fluidRow(
                     column(6, 
                            selectInput("multi_var", 
                                        "Which variable(s) would you like to 
                                        look at", 
                                        choices = c(
                                            "Temperature (C)" = "temperature_c",
                                            "Humidity (%)" = "humidity_percent", 
                                            "Wind Speed (m/s)" = 
                                                "wind_speed_m_s",
                                            "Visibility (10m)" = 
                                                "visibility_10m", 
                                            "Dew Point Temperature (C)" = 
                                                "dew_point_temperature_c",
                                            "Solar Radiation (MJ/m2)" = 
                                                "solar_radiation_mj_m2",
                                            "Rainfall (mm)" = "rainfall_mm", 
                                            "Snowfall (cm)" = "snowfall_cm"
                                        ), 
                                        multiple = TRUE,
                                        selected = c("temperature_c", 
                                                     "rainfall_mm")),
                            plotOutput("CorrelationPlots_three"), wellPanel(
                                h4("Explaining the Visualizations Shown"),
                                p("The", strong("Linear Regression Equation"), 
                                  "describes the relationship between the 
                                  dependent variable, bike rent count, and 
                                  the multiple selected independent weather 
                                  variables. The equation is dynamically 
                                  genereated based on the chosen weather 
                                  variables, thus provding new associated 
                                  relationships for each combination. Additionally, 
                                  the equation provides mathematically understanding to 
                                  the direction and magnitude of these influences."),
                                p(strong("Plot 4"), "visualizes the impact of 
                                  each selected independent weather variable 
                                  on the bike rent count, while also accounting 
                                  for the influence of other variables in the 
                                  same regression model. Thus providing 
                                  insights into the individual contributions 
                                  of the selected weather variables to bike rent count, offering a nuanced understanding of their impact.")),
                            align = "center"),
                     column(6, mainPanel(verbatimTextOutput("lmequation"),
                         plotOutput("mutlivariate"),
                                         align = "center"
                 )))),
        tabPanel("How Months and Seasons Compare",
                 fluidRow(
                     column(6,
                            selectInput("seasons_month_var", 
                                        "Would you like to compare the Seasons
                                        or Months", 
                                        choices = c(
                                            "Seasons" = "seasons",
                                            "Months" = "month"
                                        )),
                            selectInput("one_var_ms", 
                                        "Which variable would you like to 
                                        look at", 
                                        choices = c(
                                            "Temperature (C)" = "temperature_c",
                                            "Humidity (%)" = "humidity_percent", 
                                            "Wind Speed (m/s)" = 
                                                "wind_speed_m_s",
                                            "Visibility (10m)" = 
                                                "visibility_10m", 
                                            "Dew Point Temperature (C)" = 
                                                "dew_point_temperature_c",
                                            "Solar Radiation (MJ/m2)" = 
                                                "solar_radiation_mj_m2",
                                            "Rainfall (mm)" = "rainfall_mm", 
                                            "Snowfall (cm)" = "snowfall_cm"
                                        )),
                            plotOutput("CorrelationPlots_four"), wellPanel(
                                h4("Explaining the Visualizations Shown"),
                                p("Within this tab, there is the option to look
                                  either at the seasons or the months. I will
                                  be providing an overview of the plots showcased
                                  for each type seperately."),
                                h6(strong("Seasons")),
                                p(strong("Plot 5"),
                                  "showcases the average rented bike count 
                                  for each season, providing insights into the
                                  variations in bike renting numbers between 
                                  the different seasons."),
                                p(strong("Plot 6"), "explores the 
                                  bivariate relationships between 
                                  the chosen weather condition variable 
                                  and the rented bike count, such as in", 
                                  strong("Plot 1"), "however this time 
                                  the relationships are further divided 
                                  based on the different seasons. Thus 
                                  providing insights into how bike renting 
                                  might be influeneced by a specific weather 
                                  condition, but also seeing if this influence 
                                  changes based on the season."),
                                h6(strong("Months")),
                                p(strong("Plot 5"),
                                  "similarly as when looking at the seasons, 
                                  showcases the average rented bike count,
                                  however now for each month. 
                                  Thus providing insights into the
                                  variations in bike renting numbers between 
                                  the different months."),
                                p("This time", strong("Plot 6"), "looks again 
                                  at the relationship between rented bike count 
                                  and a singularly chosen weather variable, 
                                  such as in", strong("Plot 1"), "however 
                                  again looking additionally and dividing 
                                  it further based on the different months. 
                                  Thus providing insights into how bike 
                                  renting might both be influenced by 
                                  specific weather conditions, but also 
                                  by seeing if that influence changes based 
                                  on what month it is.")),
                            align = "center"),
                     column(6, mainPanel(plotOutput("season_weather"),
                                         plotOutput("season_weather2")),
                                         align = "center")
    ))),

)

server <- function(input, output) {

## Overview of Dashboard 
    output$CorrelationPlot_one_one <- renderPlot({
        plot(correlation_matrix_plot)
    })
    
    output$CorrelationPlot_one_two <- renderPlot({
        plot(corr_plot)
    })

## On The Three Data Tabs 
    output$CorrelationPlots_two  <- output$CorrelationPlots_three <- output$CorrelationPlots_four <- renderPlot({
        plot(correlation_plots)
    })

    
## Looking at Individual Weather Conditions
    output$bivariate <- renderPlot({
        
        x_var <- seoulbike_data[[input$one_var]]
        
        x_value <- switch(
            input$one_var,
            "temperature_c" = "Temperature (C)",
            "humidity_percent" = "Humidity (%)", 
            "wind_speed_m_s" = "Wind Speed (m/s)",
            "visibility_10m" = "Visibility (10m)",
            "dew_point_temperature_c" = "Dew Point Temperature (C)",
            "solar_radiation_mj_m2" = "Solar Radiation (MJ/m2)",
            "rainfall_mm" = "Rainfall (mm)", 
            "snowfall_cm" = "Snowfall (cm)"  
        )
        
         seoulbike_data %>% ggplot(aes(x_var, rented_bike_count)) + 
            geom_jitter() + geom_smooth(method = "lm", se = FALSE) + 
             labs(
                x = x_value, y = "Rented Bike Count", 
                title = sprintf("Rented Bike Count by %s", x_value),
                subtitle = "Plot 3") + 
                    theme_minimal() + 
             theme(plot.title = element_text(size = 15, hjust = 0.5, face = 
                                                 "bold"),
                   plot.subtitle = element_text(size = 13, hjust = 0.5)) + 
             scale_y_continuous(limits = c(0,NA), 
                                labels = scales::label_number_si())
    })

    
## Looking at Multiple Weather Conditions
    selected_multivars <- reactive({
        input$multi_var
    })
    
    model <- reactive({
        
        # Create the formula for the regression model
        formula <- as.formula(paste("rented_bike_count~", 
                                    paste(selected_multivars(), 
                                          collapse = "+")))
        
        # Build the model
        lm_model <- lm(formula, data = seoulbike_data)
        
        # Returns the model
        lm_model
    })
    
    tidy_coefficients <- reactive({
        tidy(model())
    })
    
    output$lmequation <- renderPrint({
        coefficients <- tidy_coefficients()
        intercept <- coefficients$estimate[1]
        slopes <- coefficients[-1, ]  # Exclude the intercept
        
        equation_text <- paste("Linear Regression Equation: bike rent count = ", 
                               round(intercept, 3))
        
        for (i in seq_len(nrow(slopes))) {
            variable <- slopes$term[i]
            coefficient <- round(slopes$estimate[i], 3)
            equation_text <- paste(equation_text, 
                                   ifelse(coefficient >= 0, " + ", " - "), 
                                   abs(coefficient), "(", variable, ")")
        }
        
        cat(equation_text, "\n")
    })
    
    # Create an avPlot based on the regression model
    output$mutlivariate <- renderPlot({
        
        # Create an avPlot
        avPlots(model = model(), data = seoulbike_data, 
                main = "Bike Rent Count Regressions - Plot 4",
                ylab = "bike rent count | others",
                )
    })

    
## How Months and Seasons Compare
    output$season_weather <- renderPlot({
        
        if (input$seasons_month_var == "month") {
        
        average_per_month %>% ggplot(aes(month, average_rented_bike_count)) + 
            geom_col(color = "white", fill = "black") + 
            labs(title = "Average Bike Rent Count by Month",
                 x = "Month",
                 y = "Average Rented Bike Count",
                 subtitle = "Plot 5") +
            theme_minimal() +
                theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                                face = "bold"),
                      plot.subtitle = element_text(size = 13, hjust = 0.5)
                      )
        } else if (input$seasons_month_var == "seasons") {
            average_per_season %>% 
                ggplot(aes(seasons, average_rented_bike_count)) + 
                geom_col(color = "white", fill = "black") +
                labs(title = "Average Bike Rent Count by Season",
                     x = "Season",
                     y = "Average Rented Bike Count",
                     subtitle = "Plot 5") + 
                theme_minimal() + 
                theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                                face = "bold"),
                      plot.subtitle = element_text(size = 13, hjust = 0.5)
                      )
        }
    })
    
    output$season_weather2 <- renderPlot({
        
        x_var_ms <- seoulbike_data[[input$one_var_ms]]
        
        x_value_ms <- switch(
            input$one_var_ms,
            "temperature_c" = "Temperature (C)",
            "humidity_percent" = "Humidity (%)", 
            "wind_speed_m_s" = "Wind Speed (m/s)",
            "visibility_10m" = "Visibility (10m)",
            "dew_point_temperature_c" = "Dew Point Temperature (C)",
            "solar_radiation_mj_m2" = "Solar Radiation (MJ/m2)",
            "rainfall_mm" = "Rainfall (mm)", 
            "snowfall_cm" = "Snowfall (cm)"  
        )
        
        if (input$seasons_month_var == "month") {
            seoulbike_data %>% ggplot(aes(x_var_ms, rented_bike_count)) + 
                geom_col() + 
                geom_smooth(method = "lm", se = FALSE) + 
                labs(
                    x = x_value_ms, y = "Rented Bike Count", 
                    title = sprintf("Each Months' Rented Bike Count by %s", 
                                    x_value_ms),
                    subtitle = "Plot 6") +
                theme_minimal() + 
                facet_wrap(~month(seoulbike_data$date, label = TRUE)) + 
                theme(plot.title = element_text(size = 15, hjust = 0.5, 
                                                face = "bold"),
                      plot.subtitle = element_text(size = 13, hjust = 0.5)) + 
                scale_y_continuous(limits = c(0,NA), 
                                   labels = scales::label_number_si())

        } else if (input$seasons_month_var == "seasons") {
            seoulbike_data %>% ggplot(aes(x_var_ms, rented_bike_count)) + 
                geom_col() + 
                geom_smooth(method = "lm", se = FALSE) + 
                labs(
                    x = x_value_ms, y = "Rented Bike Count", 
                    title = sprintf("Each Seasons' Rented Bike Count by %s", 
                                    x_value_ms),
                    subtitle = "Plot 6") +
                theme_minimal() + 
                facet_wrap(~seasons) + 
                theme(plot.title = element_text(size = 15, hjust = 0.5,
                                                 face = "bold"),
                      plot.subtitle = element_text(size = 13, hjust = 0.5)) + 
                scale_y_continuous(limits = c(0,NA), 
                                   labels = scales::label_number_si())
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
