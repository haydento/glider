library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)

#get data
targets::tar_load("glider_trk")
pos <- glider_trk[!is.na(lat_dd) & !is.na(lon_dd)]

#change column names for shiny app
setnames(pos, "time", "timestamp_utc")
setnames(pos, "run_id", "source_serial")
setnames(pos, "lat_dd", "latitude")
setnames(pos, "lon_dd", "longitude")

#some reformatting
pos$date <- as.Date(pos$timestamp_utc, tz = "UTC")



ui <- dashboardPage(
  dashboardHeader(title = "Mobile platform explorer",
    titleWidth = 900),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(leafletOutput("glider_map", height = 800, width = 1000),
        width=9),
      box(
        title = "Display options",
        uiOutput("date_select"),
        uiOutput("source_serial_select"),
        uiOutput("time_range_slider"),
        uiOutput("focal_time_slider"),
        uiOutput("tail_size_slider"),
        actionButton("save_click", "Save Image"),
        #uiOutput("playback_speed"),
       # sliderInput("time_range", "Date", min(pos$date_time), 
         max(pos$date_time),
       #        value = range(pos$date_time), step = 1000),
        width = 3
        )
    )
  )
)



server <- function(input, output) {


  # dates select input box
  dates <- as.character(sort(unique(pos$date)))
  output$date_select <- renderUI({
    selectInput("date_select", "date", choices = dates, selected = dates,
                multiple = TRUE)
  })
  
  # animal ID select input box
  output$source_serial_select <- renderUI({
    source_serials <- sort(unique(pos$source_serial[pos$date == input$date_select]))
    selectInput("source_serial", "Source Serial", choices = source_serials, 
                selected = source_serials[1], multiple = TRUE)
  })     

  #time range subset slider
  time_range <- reactive({
    range(pos$timestamp_utc[pos$source_serial %in% input$source_serial & 
                            pos$date == input$date_select])
  })
  
  # time range input slider bar
  output$time_range_slider <- renderUI({
   sliderInput("time_range2", "Date Limits", min(time_range()), max(time_range()),
     value = time_range())
  })
  
  time_range2 <- reactive({
    input$time_range2
  })
  
  # time in focus slider
  output$focal_time_slider <- renderUI({
    sliderInput("focal_time", "Timestamp in focus", 
      min(time_range2()), 
      max(time_range2()),
      value = max(time_range2()), animate = animationOptions(interval = 1, 
        pauseButton = "||"))
  })
    
  focal_time <- reactive({
    input$focal_time
  })

  
  # tail size slider
  output$tail_size_slider <- renderUI({
    sliderInput("tail_size", "Point trail duration (seconds)", 0, 
      diff(as.numeric(time_range2())),
      value = diff(as.numeric(time_range2())))
  })
  
  tail_size <- reactive({
    input$tail_size
  })
  
  time_range_display <- reactive({
    c(focal_time()-tail_size(), focal_time())
  })

  # Reactive expression for the data subsetted to what the user selected
  pos.x <- reactive({
    pos[pos$source_serial %in%input$source_serial & findInterval(pos$timestamp_utc, 
      time_range_display()) == 1, ]
  })
  
  
  # # Reactive expression for the data subsetted to what the user selected
  # pos.x <- reactive({
  #   pos.x1()[findInterval(pos.x1()$date_time, input$time_range) == 1, ]
  # })
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  pal2 <- reactive({
    colorNumeric("Reds", as.numeric(pos.x()$timestamp_utc))
  })
  

  output$glider_map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(pos) %>% 
      addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- pal2()
    
    leafletProxy("glider_map", data = pos.x()) %>%
      clearShapes() %>%
      addPolylines(lng=~longitude, lat=~latitude, col="white", weight=1) %>% 
      addCircles(lng=~longitude, lat=~latitude, radius = 10, weight = 1, 
                 color = ~pal(as.numeric(timestamp_utc)), opacity = 1,
                 fillColor = ~pal(as.numeric(timestamp_utc)), 
                 fillOpacity = 0.8, 
                 popup = ~paste(timestamp_utc)
      )
  })
  
  
  #execute file save on click
  observeEvent(input$save_click, {

    output$glider_map <- renderLeaflet({
      pos.x <- pos[pos$source_serial %in% input$source_serial, ]
  
      #palette for elapsed time
      date_range <- range(as.numeric(as.Date(pos$timestamp_uc)))
      pal2 <- colorNumeric(
        palette = "Reds",
        domain =  date_range
      )
  
      m <- leaflet(pos.x)
      # add basemap tiles
      #m <- addTiles(m, group = "OpenStreetMap")  # Add default OpenStreetMap map tiles
      #m <- addProviderTiles(m, "Stamen.TonerLite", group = "Stamen.TonerLite")
      #m <- addProviderTiles(m, "CartoDB.Positron", group = "CartoDB.Positron")
      m <- addProviderTiles(m, "Esri.WorldImagery", group = "Esri.WorldImagery")
  
      m <- setView(m, lng=-84.34, lat=46.508, zoom=15)
  
      m <- addPolylines(m, lng=~longitude, lat=~latitude, data=pos.x)
      # for(k in 2:nrow(pos.x)){
      #   m <- addPolylines(m, lng=~lon, lat=~lat, data=pos.x[(k-1):k,],
      #     #group = ~tags.i[j], weight=4,
      #     color = ~pal2(as.numeric(date_time)[-1]), fillOpacity = 1,
      #     popup = paste0(range(pos.x$date_time), collapse="<br />"))
      # } #end k
  
      #add color legend
      # legend_dates <- seq(
      #   as.POSIXct(paste0(input$yearNum, "-", date_range[1])),
      #   as.POSIXct(paste0(input$yearNum, "-", date_range[2])), by="month")
      #
      # m <- addLegend(m, "bottomleft",
      #   colors = pal2(as.numeric(legend_dates)),
      #   labels=substr(legend_dates,6,10),
      #   opacity=1,
      #   title = "Timestamp Range",
      #   labFormat = labelFormat(prefix = "")
      # )
  
      m
    })
  })
}

shinyApp(ui, server)

