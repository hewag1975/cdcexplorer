# packages and options ---------------------------------------------------------
library(shiny)
library(semantic.dashboard)

source("R/functions.R")


# important links --------------------------------------------------------------
# citation: https://www.dwd.de/DE/service/copyright/vorlagen_quellenangabe.html?nn=450672
# metadata:


# dashboard --------------------------------------------------------------------
ui = dashboardPage(
  
  # dashboard header -----------------------------------------------------------
  dashboardHeader(
    color = "blue", 
    title = "CDC explorer", 
    inverted = TRUE,
    dropdownMenuOutput(outputId = "cdcexp")
  ),
  
  # dashboard sidebar ----------------------------------------------------------
  dashboardSidebar(
    side = "left",
    sidebarMenu(
      # for icons see: https://fontawesome.com/icons?d=gallery&q=grap
      
      # tab 1: Germany observations --------------------------------------------
      menuItem(
        tabName = "ts", 
        text = "Germany - time series", 
        icon = icon("chart bar")
      ),
      
      # tab 2: Germany extremes ------------------------------------------------
      menuItem(
        tabName = "extr", 
        text = "Germany - extreme wheather", 
        icon = icon("temperature high")
      ),
      
      # tab 3: Germany climate graphs ------------------------------------------
      menuItem(
        tabName = "climgr", 
        text = "Germany - climate graphs", 
        icon = icon("cloud sun rain")
      ),
      
      # tab 4: Inernational time series ----------------------------------------
      menuItem(
        tabName = "ts_int", 
        text = "International time series", 
        icon = icon("cloud sun rain")
      ),
      
      # tab 99: about ----------------------------------------------------------
      menuItem(
        tabName = "about", 
        text = "About", 
        icon = icon("info")
      )
      
    )
  ),
  
  # dashboard body -------------------------------------------------------------
  dashboardBody(
    tabItems(
      tabItem(
        
        # tab 1: observations --------------------------------------------------
        tabName = "ts",
        
        fluidRow(
          
          # time series --------------------------------------------------------
          column(width = 11,
                 box(title = "Time series",
                     color = "blue",
                     width = 11,
                     collapsible = FALSE,
                     div(h4("Introduction"),
                         "This application allows plotting of observed meteorologic elements ",
                         "across time for a number of DWD wheather stations. ",
                         "Studying temporal development per station may help to ", 
                         "assess overall climate characteristics (temperature, ", 
                         "humidity) or seasonal differences of a particular ", 
                         "region and to identify extreme events or long-term trends. ",
                         "Monthly data have been pulled from the ",
                         a(href = "https://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html", 
                           "CDC-OpenData-Server"),
                         "of the DWD and are available for visualization. The DWD ",
                         "publishes two levels of data, 'historical' data (quality controlled) ", 
                         "and 'recent' data which typically covers the period of ",
                         "500 days b.p. (w/o final quality conrol). Here, both data ",
                         "sets are combined to display temoral development till ", 
                         "present date. ",
                         "Long-term trends are frequently hidden in raw data plots ",
                         "(upper graph). Thus, monthly values are also compared ",
                         "to monthly reference points of the standard climate ",
                         "normal period (1961 to 1990) as defined by the ",
                         a(href = "https://public.wmo.int/en/our-mandate/climate", 
                           "World Meteorological Organization"), " (lower graph). ",
                         "For this reason, only active DWD stations with backdata ",
                         "until 1961 have been considered here.",
                         h4("Usage"),
                         "In the selection box, first select a wheather station ",
                         "from the map. Depending on the station setup, the list ",
                         "of available meteorologic elements may change. Select the one ",
                         "you want to analyse. The graph will then per default display ",
                         "a time series of raw data and normalized data (devitations ",
                         "from normal period) from 2000 to current date. The time ",
                         "range can be modified using the slider. Be aware that ",
                         "amplifying the time range may cause the rendering of ",
                         "the graph to take a few seconds. ",
                         br(),
                         br()
                     ),
                     girafeOutput(
                       outputId = "obs_trend",
                       width = "100%",
                       height = "100%"
                     ),
                     div(h4("Modify time period")),
                     uiOutput(outputId = "obs_trend_ui"),
                     div(h4("Source: Deutscher Wetterdienst", align = "right"))
                 )
          ),
          
          # selections ---------------------------------------------------------
          column(width = 5,
                 box(title = "Selection", 
                     color = "blue", 
                     width = 5,
                     collapsible = FALSE,
                     h4("Current selection"),
                     valueBox(subtitle = "",
                              value = textOutput("station_name"),
                              # icon = icon("mail"),
                              color = "blue",
                              size = "tiny"),
                     h4("Select station"),
                     leafletOutput(outputId = "obs_map"),
                     h4("Select parameter"),
                     uiOutput(outputId = "obs_param_ui")
                 )
          )
        )
      ),
      
      tabItem(
        
        # tab 99: about --------------------------------------------------------
        tabName = "about",
        column(width = 5,
               fluidRow(
                 div(h4("Data source: "),
                     "All data shown in this application are kindly provided ",
                     "free of charge through the ",
                     a(href = "https://www.dwd.de/DE/klimaumwelt/cdc/cdc_node.html", "CDC-OpenData-Server"),
                     "of the DWD (Deutscher Wetterdienst). ",
                     br(),
                     br(),
                     a(href = "https://www.dwd.de/DE/Home/home_node.html",
                       img(src = "dwd_logo_258x69.png", width = "258px", height = "69px")))
               ),
               br(),
               br(),
               fluidRow(
                 div(h4("Credits: "), 
                     "This application was built using RShiny. All code is available on ",
                     a(href = "https://github.com/hewag1975/exploRe_shiny", "Github"), ". For bug ", 
                     "reports or feedback, please file an issue! ",
                     br(),
                     "Various R-packages were involved in data preparation and ",
                     "visualisation. In particular the ",
                     a(href = "https://github.com/brry/rdwd", "rdwd-package"), " ",
                     "simplified data access and gathering a lot! Thanks you!")
               ),
               br(),
               br(),
               fluidRow(
                 div(h4("by Hendrik Wagenseil (2020)"),
                     a(href = "https://github.com/hewag1975",
                       img(src = "GitHub-Mark-32px.png", height = "32px")),
                     a(href = "https://twitter.com/hewag1975",
                       img(src = "Twitter_Logo_Blue.png", height = "32px")),
                     a(href = "https://www.linkedin.com/in/dr-hendrik-wagenseil-3a498482/",
                       img(src = "LI-In-Bug.png", height = "32px")),
                 )
               )
        )
      )
    )
  )        
)

server = function(input, output, session) {
  
  # tab 1: observations --------------------------------------------------------
  # output station name
  output$station_name = renderText({ 
    data_stations %>%
      filter(id == obs_rv$obs_station_id) %>% 
      pull(name) 
  })
  
  # output map of stations 
  output$obs_map = renderLeaflet({ m })
  
  # create initial set of reactive values
  obs_rv = reactiveValues(
    obs_station_id = 430,
    obs_parameter = coverage[id == 430][["label"]][1]
  )
  
  # observe map of stations and update obs_rv
  observeEvent(input$obs_map_marker_click, {
    
    station_sel = input$obs_map_marker_click$id[1]
    
    # update map of stations
    obs_station_select = data_stations %>% filter(id == station_sel)
    leafletProxy(mapId = "obs_map", session = session) %>%
      clearGroup(group = "obs_station_sel") %>% 
      addCircleMarkers(data = obs_station_select, 
                       radius = 10, 
                       stroke = FALSE,
                       fillColor = cpal[1], fillOpacity = 1, 
                       group = "obs_station_sel",
                       label = mapview:::makeLabels(x = obs_station_select, zcol = "name")) 
    
    # list available parameters for selected station    
    available_parameters = coverage[id == station_sel][["label"]]
    
    # update selected parameter (only if current selection not within updated list)
    if (!obs_rv$obs_parameter %in% available_parameters){
      obs_rv$obs_parameter = available_parameters[1]
    }
    
    # update id 
    obs_rv$obs_station_id = station_sel
    
  })
  
  # observe available parameters and update obs_rv
  observeEvent(input$obs_station_param, {
    if (!obs_rv$obs_parameter == input$obs_station_param){
      obs_rv$obs_parameter = input$obs_station_param
    }
  })
  
  # output time slider ui
  output$obs_trend_ui = renderUI({
    
    obs_period = melt(data = coverage[id == obs_rv$obs_station_id & 
                                        label == obs_rv$obs_parameter, 
                                      .(start, end)], 
                      measure.vars = c("start", "end"))[["value"]]
    
    # sel_period = obs_period
    # if (sel_period[1] < sel_period[2] - 20) sel_period[1] = sel_period[2] - 20
    
    sliderInput(inputId = "obs_time_period",
                label = NULL,
                min = obs_period[1],
                max = obs_period[2],
                value = c(2000, 2020),
                step = 1,
                dragRange = TRUE,
                width = '100%',
                sep ="")
    
  })
  
  # output parameter selection ui
  output$obs_param_ui = renderUI({
    
    available_parameters = coverage[id == obs_rv$obs_station_id][["label"]]
    
    radioButtons(inputId = "obs_station_param", 
                 label = "", 
                 choices = available_parameters, 
                 selected = obs_rv$obs_parameter)    
    
  })
  
  # parameter trend plot
  obs_plots = reactive({
    
    obs_plot_function(data = data_monthly, 
                      id_plot = obs_rv$obs_station_id, 
                      param_plot = obs_rv$obs_parameter, 
                      period_plot = input$obs_time_period)
    
  })
  
  output$obs_trend = renderGirafe({
    
    girafe(ggobj = cowplot::plot_grid(
      obs_plots()[["p_param"]],
      obs_plots()[["p_dev"]],
      nrow = 2),
      width_svg = 20, height_svg = 10,
      options = list(opts_hover(css = "fill:red;")))
    
  })
  
}

shinyApp(ui, server)

