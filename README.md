SustainabilityTypes
================
Özgür Tapan

### Objective

To create interactive plots inside onclick modal dialog, using `ggplot2`
and `ggiraph` libraries.  
Final graph can be checked at
<https://ozgurtapan.shinyapps.io/SustainabilityTypes>

![](https://github.com/OzgurTapan/SustainabilityTypes/blob/main/docs/map.png)
![](https://github.com/OzgurTapan/SustainabilityTypes/blob/main/docs/plot.png)

### Load the libraries

Outlines of countries taken from the `maps` library. Data cleaning,
organizing and transforming operations done using `tidyverse`. Dynamic
plots created from ggplot2 with ggiraph extension wrapped inside a shiny
app.

``` r
#Dependencies
library(shiny)
library(maps)
library(tidyverse)
library(ggiraph)
```

### Tidying data

Purrr’s vectorized functions used on `mutate` for faster operations.  
After data wrangling operations done on both the data and the map
outlines, they merged together.

``` r
# Data
sus25_raw <- read.csv("SDR_2025_TIMESERIES_(Backdated_Data).csv") %>% 
  select(Country,
         year,
         sdgi_s,
         contains("goal"),
         ObjectId)

# Line chart data
sus25 <- sus25_raw %>%
  rowwise() %>%
  mutate(economic_resources =
           mean(c(goal1, goal2, goal3, goal8, goal9), na.rm=T),
         enviromental_resources =
           mean(c(goal6,goal7, goal12, goal13, goal14, goal15), na.rm=T),
         social_resources =
           mean(c(goal4, goal5, goal10, goal11, goal16, goal17), na.rm=T),
         total_resources =
           mean(c(economic_resources, enviromental_resources, social_resources), na.rm=T)
  ) %>% 
  ungroup() %>%
  nest(.by = Country,
       .key = "data")

# Fixes for special cases
# Türkiye to Turkey for better encoding support
sus25[177,1] <- "Turkey"
# Area's name correction
sus25[178,1] <- "French Polynesia"
# Attribute values cannot contain single quote "'". Cote d'Ivoire
sus25[44,1] <- "Ivory Coast"


sus25 <- sus25 %>%
  mutate(data = map(data,
                    ~mutate(., enviromental_resources_dif =
                              enviromental_resources - lag(enviromental_resources)))) %>%
  mutate(data = map(data,
                    ~mutate(., total_resources_dif =
                              total_resources - lag(total_resources)))) %>%
  unnest(data)

# Map coordinates
map_outlines <- map_data("world") %>%
  select(long,
         lat,
         region,
         group)



map_outlines <- mutate(map_outlines,
                       region = case_match(region,
                                           "Antigua" ~ "Antigua and Barbuda",
                                           "Barbuda" ~ "Antigua and Barbuda",
                                           "Bahamas" ~ "Bahamas, The",
                                           "Brunei" ~ "Brunei Darussalam",
                                           "Republic of Congo" ~ "Congo, Rep.",
                                           "Cape Verde" ~ "Cabo Verde",
                                           #"Ivory Coast" ~ "Cote d'Ivoire",
                                           "Czech Republic" ~ "Czechia",
                                           "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
                                           "UK" ~ "United Kingdom",
                                           "Micronesia" ~ "Micronesia, Fed. Sts.",
                                           "Egypt" ~ "Egypt, Arab Rep.",
                                           "Iran" ~ "Iran, Islamic Rep.",
                                           "Gambia" ~ "Gambia, The",
                                           "Laos" ~ "Lao PDR",
                                           "Kyrgyzstan" ~ "Kyrgyz Republic",
                                           "Saint Kitts" ~ "St. Kitts and Nevis",
                                           "South Korea" ~ "Korea, Rep.",
                                           "Saint Lucia" ~ "St. Lucia",
                                           "North Korea" ~ "Korea, Dem. Rep.",
                                           "Russia" ~ "Russian Federation",
                                           "Slovakia" ~ "Slovak Republic",
                                           "Trinidad" ~ "Trinidad and Tobago",
                                           "Tobago" ~ "Trinidad and Tobago",
                                           "Swaziland" ~ "Eswatini",
                                           "Syria" ~ "Syrian Arab Republic",
                                           #"Turkey" ~ "Türkiye",
                                           #"French Polynesia" ~ "Tuvalu",
                                           "Saint Vincent" ~ "St. Vincent and the Grenadines",
                                           "Grenadines" ~ "St. Vincent and the Grenadines",
                                           "USA" ~ "United States",
                                           "Yemen" ~ "Yemen, Rep.",
                                           "Venezuela" ~ "Venezuela, RB",
                                           .default = region
                       )
)


# Joined data-coordinate
sus25 <- sus25 %>% inner_join(x = sus25,
                              y = unique(select(map_outlines, region)),
                              by = c("Country" = "region")
                              )
```

### Shiny ui

Main ggiraph map indicated here as shiny’S `fluidrow` column.

``` r
# ui
ui <- fluidPage(
  fluidRow(column(selectInput("year",
                              label = "Select year",
                              choices = unique(sus25$year),
                              selected = "2024"
                              ),
                  girafeOutput("my_map"),
                  width = 12
                  )
           )
)
```

### Plot inside onclick modal dialog

GGiraph’s `data_id` used to capture clicked value inside the main plot
than included as an input inside `observeEvent`.  
Onclick plot included inside a `modalDialog`.

``` r
# server

# Drawing main map
server <- function(input, output){
  output$my_map <- renderGirafe({
    girafe_map <- sus25 %>%
      filter(year == input$year) %>%
      ggplot() +
      geom_map_interactive(map = map_outlines, 
                           aes(map_id = Country,
                               color = "black",
                               fill = total_resources,
                               data_id = Country,
                               tooltip = paste0(Country,
                                                "<br>",
                                                case_when(total_resources_dif < 0 ~
                                                            "Recessed Sustainability",
                                                          total_resources_dif >= 0 & enviromental_resources_dif < 0 ~
                                                            "Weak Sustainability",
                                                          total_resources_dif >= 0 & enviromental_resources_dif >= 0 ~
                                                            "Strong Sustainability"
                                                          ),
                                                " <br>Score: ", round(total_resources, 1)
                                                )
                               ),
                           linewidth = 0.1
                           ) +
      scale_color_identity() +
      scale_fill_distiller(palette ="RdYlGn",
                           direction = 1
                           ) +
      
      expand_limits(x=map_outlines$long,
                    y=map_outlines$lat
                    ) +
      
      coord_fixed(1.3) +
      
      labs(title = "Sustainability Score Map",
           subtitle = "Click on a country for analysing its sustainability type",
           fill = "Score"
           ) +
      
      theme(
            axis.text = element_blank(),
            #axis.line = element_blank(),
            axis.ticks = element_blank(),
            #panel.border = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            #panel.background = element_rect(fill = "white"),
            #plot.title = element_text()
            )
    
    girafe(code = print(girafe_map),
           options = list(opts_hover(css = "fill:blue;cursor:pointer;"),
                          opts_selection(css = "fill-opacity:1;",
                                         type = "single")
                          )
           )
  })
  
  
# Drawing plot inside modal dialog
  observeEvent(input$my_map_selected,{
    showModal(modalDialog(
      renderPlot({
        sus25 %>%
          filter(Country == input$my_map_selected) %>%
          ggplot(aes(year)) +
          
          geom_line(aes(y=economic_resources),
                    color = "blue",
                    linewidth=1.5
          ) +
          
          geom_line(aes(y=enviromental_resources),
                    color = "cyan2",
                    linewidth=1.5
          ) +
          
          geom_line(aes(y=social_resources),
                    color = "purple",
                    linewidth=1.5
          ) +
          
          geom_rect(
            aes(xmin = lag(year),
                xmax = year,
                fill = case_when(
                  total_resources_dif < 0 ~ "red",
                  total_resources_dif >= 0 & enviromental_resources_dif < 0 ~ "yellow",
                  total_resources_dif >= 0 & enviromental_resources_dif >= 0 ~ "green"
                )
            ),
            ymin = -Inf, ymax = Inf, alpha = 0.2
          ) +
          
          scale_fill_identity() +
          
          geom_text(aes(x = year-0.5,
                        y = -Inf,
                        label = case_when(
                          total_resources_dif < 0 ~ "Recession",
                          total_resources_dif >= 0 & enviromental_resources_dif < 0 ~ "Weak",
                          total_resources_dif >= 0 & enviromental_resources_dif >= 0 ~ "Strong")
          ),
          angle = 90, hjust = 0
          ) +
          
          geom_text(aes(x=last(year),
                        y=max(economic_resources),
                        label = "Economic"
          ),
          color="blue",
          vjust=-1
          ) +
          
          geom_text(aes(x=last(year),
                        y=max(enviromental_resources),
                        label = "Enviromental"
          ),
          color="cyan2",
          vjust=-1
          ) +
          
          geom_text(aes(x=last(year),
                        y=max(social_resources),
                        label = "Social"
          ),
          color="purple",
          vjust=-1
          ) +
          
          scale_x_continuous(breaks = 2000:2024) +
          scale_y_continuous(expand = expansion(mult = c(.25, 0.05))) +
          
          labs(title = paste0(input$my_map_selected, "  \nWeak & Strong Sustainability"),
               subtitle = "Sustainability types are relative to previous years",
               x = "Year",
               y ="Sustainability Score",
               size = 5
          ) +
          
          theme(axis.text.x = element_text(angle = 45,
                                           hjust = 1),
                plot.margin = margin(0, 1, 0, 0, "cm")
          ) +
          coord_cartesian(clip = 'off')
        
        
      })
    ))
  })
      

}
  

# run app
shinyApp(ui = ui,server = server, options = list(height = 850))
```
