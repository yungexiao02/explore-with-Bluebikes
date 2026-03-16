# StationFinder 

library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(sf)
# library(htmltools)

# setwd("/Users/yungexiao/Desktop/bluebike analysis/scripts/app/StationFinder")

station_year <- readRDS("stations_byyear.rds")
MA_sf <- st_read("region_grid_map.shp")  # optional base layer
colnames(station_year) # don't need TOWN I think 

ui <- navbarPage("Bluebikes Dashboard",
                 
                 tabPanel("StationFinder",
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              h4("Filter Stations"),
                              
                              selectInput("municipality",
                                          "City/Town:",
                                          choices = sort(unique(station_year$municipality)),
                                          multiple = TRUE),
                              
                              selectInput("seasonal_status",
                                          "Seasonal Status:",
                                          choices = c("All",
                                                      sort(unique(station_year$szn_status))),
                                          selected = "All"),
                              
                              selectInput("year",
                                          "Year:",
                                          choices = sort(unique(station_year$year)),
                                          selected = max(station_year$year)),
                              
                              textInput(
                                inputId = "station_search",
                                label = "Search Station (optional)",
                                placeholder = "Type station name..."
                              ),
                              
                              hr(),
                              
                              selectInput("metric",
                                          "Heat Metric:",
                                          choices = c(
                                            "Total Docks" = "total_docks",
                                            # "Average Departures" = "avg_removed",
                                            # "Average Arrivals" = "avg_returned",
                                            "Daily Average Total Activity" = "avg_tot_activity",
                                            # "Activity per Dock" = "avg_tot_activity_per_dock",
                                            # "Departures per Dock" = "avg_removed_per_dock",
                                            # "Returned per Dock" = "avg_returned_per_dock",
                                            "% Days Departures > Arrivals" = "pct_removed_gt_returned"
                                          ),
                                          selected = "avg_tot_activity_per_dock"),
                              
                              hr(),
                              
                              div(
                                style = "font-size: 12px; color: #555; margin-top: 15px;",
                                HTML(paste0(
                                  "<b>Data Note:</b><br>",
                                  "# stations missing", " stations (",
                                  "round(pct_rides_excluded, 1)", "% of rides) were excluded ",
                                  "because station IDs in ride data could not be identified."
                                ))
                              )
                            ),
                            
                            mainPanel(
                              leafletOutput("station_map", height = 600),
                              br(),
                              DTOutput("top_stations")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  # -----------------------------
  # Metric label lookup
  # -----------------------------
  metric_labels <- c(
    total_docks = "Total Docks",
    avg_tot_activity = "Average Total Activity",
    pct_removed_gt_returned = "% Days Departures > Arrivals"
  )
  
  
  # -----------------------------
  # Reactive Filtered Dataset
  # -----------------------------
  filtered_stations <- reactive({
    
    data <- station_year %>%
      filter(year == input$year)
    
    # Municipality filter
    if (!is.null(input$municipality) && length(input$municipality) > 0) {
      data <- data %>%
        filter(municipality %in% input$municipality)
    }
    
    # Seasonal status filter
    if (input$seasonal_status != "All") {
      data <- data %>%
        filter(szn_status == input$seasonal_status)
    }
    
    # Free-text station search (partial, case-insensitive)
    if (!is.null(input$station_search) && input$station_search != "") {
      pattern <- paste0("(?i)", input$station_search)  # (?i) → ignore case
      data <- data %>%
        filter(grepl(pattern, station_name))
    }
    
    data
  })
  
  
  # -----------------------------
  # Leaflet Map
  # -----------------------------
  output$station_map <- renderLeaflet({
    
    data <- filtered_stations()
    req(nrow(data) > 0)
    
    metric_var <- input$metric
    
    # -----------------------------
    # Scale percent metric if needed
    # -----------------------------
    fill_values <- if (metric_var == "pct_removed_gt_returned") {
      data[[metric_var]] * 100   # 0–1 → 0–100 for percent
    } else {
      data[[metric_var]]
    }
    
    # -----------------------------
    # Color palette
    # -----------------------------
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = fill_values,
      na.color = "#cccccc"
    )
    
    # -----------------------------
    # Build Leaflet map
    # -----------------------------
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 5,
        fillColor = ~pal(fill_values),
        fillOpacity = 0.9,
        stroke = FALSE,
        popup = ~paste0(
          "<b>", station_name, "</b><br>",
          "Municipality: ", municipality, "<br>",
          "Seasonal Status: ", szn_status, "<br>",
          "Total Docks: ", total_docks, "<br><br>",
          "Avg Removed: ", round(avg_removed, 1), "<br>",
          "Avg Returned: ", round(avg_returned, 1), "<br>",
          "Avg Total Activity: ", round(avg_tot_activity, 1), "<br>",
          "Activity per Dock: ", round(avg_tot_activity_per_dock, 2), "<br>",
          "% Days Removed > Returned: ",
          round(pct_removed_gt_returned * 100, 1), "%"
        )
      ) %>%
      
      # -----------------------------
    # Legend
    # -----------------------------
    addLegend(
      pal = pal,
      values = fill_values,
      title = metric_labels[[metric_var]],
      position = "bottomright",
      labFormat = if (metric_var == "pct_removed_gt_returned") {
        labelFormat(digits = 1, suffix = "%")
      } else {
        labelFormat(digits = 2)
      }
    ) %>%
      
      # -----------------------------
    # Auto-fit map bounds
    # -----------------------------
    fitBounds(
      lng1 = min(data$lng, na.rm = TRUE),
      lat1 = min(data$lat, na.rm = TRUE),
      lng2 = max(data$lng, na.rm = TRUE),
      lat2 = max(data$lat, na.rm = TRUE)
    )
  })
  
  # -----------------------------
  # Top 10 Table
  # -----------------------------
  output$top_stations <- renderDT({
    
    filtered_stations() %>%
      arrange(desc(avg_tot_activity)) %>%
      head(10) %>%
      transmute(
        station_name = station_name,
        `City/Town` = municipality,
        `Seasonal Status` = szn_status,
        `Dock Count` = total_docks,
        `Avg Departures` = round(avg_removed, 1),
        `Avg Arrivals` = round(avg_returned, 1),
        `Avg Total Activity` = round(avg_tot_activity, 1),
        `Activity per Dock` = round(avg_tot_activity_per_dock, 2),
        `% Days Departures > Arrivals` = round(pct_removed_gt_returned * 100, 1)
      ) %>%
      rename(`Station Name` = station_name)
    
  }, options = list(pageLength = 10))
  
}



shinyApp(ui, server)

