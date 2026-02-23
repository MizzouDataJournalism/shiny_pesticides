#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readr)
library(leaflet)
library(dplyr)
library(sf)
library(tigris)
library(RColorBrewer)
library(htmltools)


# Set tigris options to avoid caching issues
options(tigris_use_cache = FALSE)


county_data <- read_csv("ultimate_cancer_pesticide_for_sure.csv")


# Complete mapping of all 50 states + DC to FIPS codes
state_fips_mapping <- data.frame(
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                 "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                 "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                 "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                 "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                 "New Hampshire", "New Jersey", "New Mexico", "New York", 
                 "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                 "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                 "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                 "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"),
  fips_code = c("01", "02", "04", "05", "06", "08", "09", "10", "12", "13", 
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", 
                "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", 
                "46", "47", "48", "49", "50", "51", "53", "54", "55", "56", "11")
)

# Function to get county geometries for a specific state
get_county_shapes <- function(state_name) {
  # Look up FIPS code from mapping table
  state_fips <- state_fips_mapping$fips_code[state_fips_mapping$state_name == state_name]
  
  # Handle case where state is not found
  if(length(state_fips) == 0) {
    stop(paste("State not found:", state_name))
  }
  
  # Get county boundaries
  counties <- counties(state = state_fips, cb = TRUE)
  return(counties)
}


# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { 
        font-family: 'Arial', sans-serif; 
        margin: 0; 
        padding: 20px; 
        background-color: #f5f5f5; 
      }
      .main-header { 
        text-align: center; 
        color: #2c3e50; 
        margin-bottom: 10px; 
        font-size: 28px; 
        font-weight: bold; 
      }
      .sub-header { 
        text-align: center; 
        color: #7f8c8d; 
        margin-bottom: 30px; 
        font-size: 16px; 
      }
      .control-panel { 
        background: white; 
        padding: 20px; 
        border-radius: 8px; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
        margin-bottom: 20px; 
      }
      .map-container { 
        background: white; 
        border-radius: 8px; 
        box-shadow: 0 2px 4px rgba(0,0,0,0.1); 
        overflow: hidden; 
      }
      .leaflet { 
        height: 600px !important; 
      }
    "))
  ),
  
  # Header
  div(class = "main-header", "Cancer rates and pesticide use per square mile"),
  
  # Sub-header
  div(class = "sub-header", "Explore how cancer rates and pesticide use compare across US counties"),
  
  # Control Panel
  div(class = "control-panel",
      fluidRow(
        column(6,
               selectInput("selected_state", 
                           "Select State:",
                           choices = unique(county_data$state),
                           selected = unique(county_data$state)[1])
        ),
        column(6,
               radioButtons("map_type",
                            "Choose Data to Display:",
                            choices = list(
                              "Cancer rate per 100k" = "cancer",
                              "Pesticides per square mile" = "pest_per_land_area"
                            ),
                            selected = "cancer",
                            inline = TRUE)
        )
      ),
      
      # Custom horizontal legend
      div(style = "margin-top: 20px; text-align: center;",
          htmlOutput("custom_legend")
      )
  ),
  
  # Map
  div(class = "map-container",
      leafletOutput("county_map", height = "600px")
  ),
  
  # Footer
  div(style = "text-align: left; padding: 20px; color: #2c3e50; font-size: 14px; margin-top: 20px;",
      "Map by Mariia Novoselia, Missouri School of Journalism"
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    county_data %>% 
      filter(state == input$selected_state)
  })
  
  # Reactive map data with geometries
  map_data <- reactive({
    req(input$selected_state)
    
    # Get county shapes for the selected state
    county_shapes <- get_county_shapes(input$selected_state)
    
    # Filter data for selected state
    state_data <- filtered_data()
    
    # FIPS-based matching - this is much more reliable!
    # The tigris package provides GEOID which matches FIPS codes
    county_shapes$GEOID <- as.character(county_shapes$GEOID)
    state_data$fips <- as.character(state_data$fips)
    
    # Merge data using FIPS codes
    merged_data <- county_shapes %>%
      left_join(state_data, by = c("GEOID" = "fips"))
    
    # Set display values based on selected map type
    merged_data$display_value <- if(input$map_type == "cancer") {
      merged_data$cancer
    } else {
      merged_data$pest_per_land_area
    }
    
    merged_data$display_name <- if(input$map_type == "cancer") {
      "Cancer rate per 100k"
    } else {
      "Pesticides per square mile (kg)"
    }
    
    # Remove counties without data (NA values will show as gray)
    return(merged_data)
  })
  
  # Create custom horizontal legend
  output$custom_legend <- renderUI({
    req(map_data())
    
    data <- map_data()
    legend_title <- if(input$map_type == "cancer") "Cancer rate per 100k" else "Pesticides per square mile (kg)"
    
    # Calculate value range
    val_range <- range(data$display_value, na.rm = TRUE)
    if(is.finite(val_range[1]) && is.finite(val_range[2])) {
      
      # Create continuous horizontal legend HTML
      continuous_gradient <- if(input$map_type == "cancer") {
        "linear-gradient(to right, #97bfeb, #135297)"
      } else {
        "linear-gradient(to right, #FFC067, #FF4D00)"
      }
      
      legend_items <- sprintf(
        '<div style="display: inline-block; width: 200px; height: 20px; background: %s; border: 1px solid #ccc; margin: 0 10px;"></div>
         <div style="display: flex; justify-content: space-between; width: 200px; margin: 5px 10px 0 10px;">
           <small>%.1f</small>
           <small>%.1f</small>
         </div>',
        continuous_gradient,
        val_range[1],
        val_range[2]
      )
      
      # Add NA indicator on same line
      na_indicator <- '<div style="display: inline-block; margin-left: 30px; vertical-align: top;">
                         <div style="width: 20px; height: 20px; background-color: #bdbdbd; border: 1px solid #ccc; display: inline-block; vertical-align: middle;"></div>
                         <small style="margin-left: 5px; vertical-align: middle;">No Data</small>
                       </div>'
      
      HTML(paste0(
        '<div style="margin: 10px 0;">
           <strong style="margin-right: 15px;">', legend_title, ':</strong>
           <div style="display: inline-block; vertical-align: top;">',
        legend_items,
        '</div>',
        na_indicator,
        '</div>'
      ))
    } else {
      HTML('<div style="margin: 10px 0;"><strong>No data available for selected state</strong></div>')
    }
  })
  
  # Create the map
  output$county_map <- renderLeaflet({
    req(map_data())
    
    data <- map_data()
    
    # Create color palette (using state-specific min/max for better contrast)
    if(input$map_type == "cancer") {
      # Get state-specific range for better color scaling
      state_range <- range(data$display_value, na.rm = TRUE)
      
      # Check if we have valid data
      if(!is.finite(state_range[1]) || !is.finite(state_range[2]) || 
         state_range[1] == state_range[2]) {
        return(leaflet() %>% 
                 setView(lng = -98, lat = 39, zoom = 4) %>%
                 addControl(html = "<div style='background: white; padding: 40px; border-radius: 5px; font-size: 18px; text-align: center; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin: auto;'>
                                      <strong>No cancer data available for this state</strong>
                                    </div>",
                            position = "topleft"))
      }
      
      pal <- colorNumeric(
        palette = c("#97bfeb", "#135297"), 
        domain = state_range,
        na.color = "#bdbdbd"
      )
      legend_title <- "Cancer rate per 100k"
    } else {
      # Get state-specific range for better color scaling  
      state_range <- range(data$display_value, na.rm = TRUE)
      
      # Check if we have valid data
      if(!is.finite(state_range[1]) || !is.finite(state_range[2]) || 
         state_range[1] == state_range[2]) {
        return(leaflet() %>% 
                 setView(lng = -98, lat = 39, zoom = 4) %>%
                 addControl(html = "<div style='background: white; padding: 40px; border-radius: 5px; font-size: 18px; text-align: center; margin: auto;'>
                                      <strong>No pesticides data available for this state</strong>
                                    </div>",
                            position = "topleft"))
      }
      
      pal <- colorNumeric(
        palette = c("#FFC067", "#FF4D00"), 
        domain = state_range,
        na.color = "#bdbdbd"
      )
      legend_title <- "Pesticides per square mile (kg)"
    }
    
    # Calculate center and zoom level
    bbox <- st_bbox(data)
    center_lng <- mean(c(bbox["xmin"], bbox["xmax"]))
    center_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
    
    # Handle Alaska's longitude wrapping issue
    if(input$selected_state == "Alaska") {
      center_lng <- -152  # Force Alaska center longitude
      center_lat <- 64    # Force Alaska center latitude
    } else if(input$selected_state == "Hawaii") {
      center_lng <- -157  # Force Hawaii center longitude
      center_lat <- 20    # Force Hawaii center latitude
    }
    
    # Set appropriate zoom level based on state size
    if(input$selected_state == "Alaska") {
      zoom_level <- 4
    } else if(input$selected_state == "Hawaii") {
      zoom_level <- 7
    } else if(input$selected_state %in% c("Texas", "California", "Montana")) {
      zoom_level <- 6
    } else if(input$selected_state %in% c("Arizona", "Florida", 
                                          "Illinois", "Michigan", 
                                          "Minnesota", "Nebraska", 
                                          "Nevada", "New Mexico", 
                                          "Utah", "Idaho")) {
      zoom_level <- 6
    } else if(input$selected_state %in% c("Rhode Island", "Connecticut")) {
      zoom_level <- 9
    } else if(input$selected_state == "Delaware") {
      zoom_level <- 8
    } else {
      zoom_level <- 7
    }
    
    # Create leaflet map (without addLegend since legend is now in UI)
    leaflet(data) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addPolygons(
        fillColor = ~pal(display_value),
        weight = 1,
        opacity = 1,
        color = "black",
        dashArray = "",
        fillOpacity = 1,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = ~paste0(
          "<strong>", NAME, " County</strong><br/>",
          "Cancer rate per 100k: ", 
          ifelse(is.na(cancer), "<em>No Data</em>", 
                 paste0(round(cancer, 1))), "<br/>",
          "Pesticide per sq mile (kg): ", 
          ifelse(is.na(pest_per_land_area), "<em>No Data</em>",
                 paste0(round(pest_per_land_area, 2))), "<br/>", "<br/>", 
          "Most prevalent pesticides: ", "<br/>", 
          ifelse(is.na(top_5_pest), "<em>No Data</em>",
                 paste0(top_5_pest)), "<br/>"
        )
      ) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level)
  })
}


# Run the application
shinyApp(ui = ui, server = server)