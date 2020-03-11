# ----------------------------
# start of shiny app -Anna
# ----------------------------

# Attach packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tmap)
library(sf)
library(janitor)
library(USAboundariesData)
library(USAboundaries)
library(devtools)
library(leaflet)
library(kableExtra)
library(lubridate)
library(gt)

# ----------------------------
# read in data
# ----------------------------
counties_mt <- us_counties(states = "Montana")
plot(st_geometry(counties_mt))

# adding data set
mt <- read_csv("montana_fire.csv") %>% 
  clean_names() %>% 
  select(-fire_code, -fire_size_class)

#making an sf data frame
mt_sf <- st_as_sf(mt, coords = c("longitude", "latitude"), crs = 4326)

#changing the data from julian to normal
mt_date <- mt %>% 
  mutate(new_dis_date = as.Date(discovery_date, origin = structure(-2440588, class = "Date"))) %>% 
  mutate(new_cont_date = as.Date(cont_date, origin = structure(-2440588, class = "Date")))

#combining data and time for containment to find the difference between the discovery time and containment time
mt_date_combined <- mt_date %>% 
  mutate(final_dis_date = lubridate::ymd_hm(paste(mt_date$new_dis_date, mt_date$discovery_time))) %>% 
  mutate(final_cont_date = lubridate::ymd_hm(paste(mt_date$new_cont_date, mt_date$cont_time))) %>% 
  mutate(interval = difftime(final_cont_date,final_dis_date,units = "min")) %>% 
  mutate(interval = round(interval/60, 2)) %>% 
  drop_na(final_cont_date, final_dis_date)

pal <- colorFactor(palette = c("red", "blue","red", "blue","red", "blue","red", "blue","red", "blue","red", "blue","red"), 
                   levels = c(unique(mt_sf$stat_cause_descr)))


# ---------------------------
# create `ui` 
# ---------------------------

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Montana Wildfires"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fires Causes", tabName = "causes", icon = icon("fire")),
      menuItem("Search by Fire", tabName = "search", icon = icon("search")),
      menuItem("Containment Time", tabName = "time", icon = icon("clock"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "causes",
        fluidRow(
          box(title = "Causes Map",
              background = "black",
              checkboxGroupInput("mt_cause",
                                 "Choose a fire cause:",
                                 choices = c(unique(mt_sf$stat_cause_descr)))),
          box(background = "black",
              leafletOutput(outputId = "mymap", width = 1150, height = 750)))
      ),
      tabItem(
        tabName = "search",
        fluidRow(
          box(title = "Montana Wildfires by Name",
              width = 4,
              solidHeader = TRUE,
              background = "black",
              textInput("mt_fire_name",
                          "Search a specific wildfire by name:")),
          box(width = 7,
              background = "black",
              plotOutput(outputId = "name_plot", width = 400, height = 250)),
          box(width = 10,
              background = "black",
              gt_output(outputId = "mt_fire_summary"))
        )
      ),
      tabItem(
        tabName = "time", 
        fluidRow(
          box(background = "black",
              title = "Containment Time by Reporting Source Over the Years",
              selectInput("source_time", 
                          "Choose a reporting source:",
                          choices = c(unique(mt$source_reporting_unit_name)))),
          box(background = "black",
              plotOutput(outputId = "time_plot")) 
        )
      )
    )
  )
)


# ------------------------------
# building the server
# -------------------------------
server <- function(input, output){
  
  filtered_df <- reactive({
    mt_sf %>%
      filter(stat_cause_descr == input$mt_cause)
  })
  
  output$mymap <- renderLeaflet({
    leaflet(filtered_df()) %>% 
      addTiles() %>%
      addCircleMarkers(
        color = ~pal(stat_cause_descr),
        stroke = FALSE, fillOpacity = 0.5) 
    
  })
  
  mt_df3 <- reactive({
    mt %>% 
      filter(fire_name == input$mt_fire_name)
  }) 
  output$name_plot <- renderPlot({
    ggplot()+
      geom_sf(data = counties_mt)+
      geom_point(data = mt_df3(), aes (x = longitude, y = latitude),
                 color = "red",
                 size = 5) +
      theme_minimal()
  })
  
  mt_gt <- reactive({
    mt_date_combined %>%  
      select(fire_name, fire_year, stat_cause_descr, fire_size, source_reporting_unit_name, final_dis_date, final_cont_date) %>% 
      filter(fire_name == input$mt_fire_name) %>% 
      gt() %>% 
      tab_header(
        title = "Fire Summary"
      ) %>% 
      cols_label(
        fire_name = "Name of Fire",
        fire_year = "Year",
        stat_cause_descr = "Cause of Fire",
        fire_size = "Fire Area",
        source_reporting_unit_name = "Name of Reporting Source",
        final_dis_date = "Discovery Time",
        final_cont_date = "Contained Time") %>% 
      tab_options(
        table.width = pct(100))
  })
  
  output$mt_fire_summary <- render_gt({
    expr = mt_gt()
  })
  
  
  mt_df4 <- reactive({
    mt_date_combined %>% 
      filter(source_reporting_unit_name == input$source_time) %>% 
      group_by(fire_year) %>% 
      summarize(
        mean_interval = round(mean(interval),2)
      )
  })
  
  output$time_plot <- renderPlot({
    ggplot(data = mt_df4(), aes(x = fire_year, y = mean_interval)) +
      geom_line(color = "red",
                size = 1) + 
      geom_point(size = 2) + 
      geom_ribbon(aes(ymin = 0, 
                      ymax = mean_interval), 
                      fill = "red", 
                      alpha = 0.1) +
      scale_x_continuous(breaks = seq(1992, 2015, by = 2),
                         expand = c(0, 0)) + 
      labs(title = "Montana Fire Containment Times",
           subtitle = "By Land Management Reporting Agency (1992-2015)",
           caption = "Time series of how Montana wildfire containment times (in hours) have fluctuated annually between 1992-2015 within different land manangement units.",
           x = "Year",
           y = "Containment Time (hours)") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 12),
             axis.title.y = element_text(size = 12),
             plot.title = element_text(size = 16),
             plot.subtitle = element_text(size = 14))
      
  })

  }



# --------------------------------
shinyApp(ui = ui, server = server)









