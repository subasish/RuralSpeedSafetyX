### CODES DEVELOPED BY
### Subasish Das and L.D. White


library(shiny)
library(shinydashboard)
library(shinyjs)
library(sf)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(DT)
library(htmltools)

# Read State/Counties CSV File
#StateCountyData = read.csv("C:/Subasish/FHWA Rural Speed Safety Project For USDOT SDI/app/www/US_Counties/USCounties.csv")  
StateCountyData = read.csv("www/US_Counties/USCounties.csv")  

# Create State and Initial County List
StateCountyData$State <- as.character(StateCountyData$State)
StateCountyData$County <- as.character(StateCountyData$County)
### Temporarily filter for only 3 states - WA, NC, OH
#StateCountyData <- filter(StateCountyData, State == 'WA' | State == 'NC' | State == 'OH')
StateCountyData <- filter(StateCountyData, State == 'WA' | State == 'OH')
StatesList <- unique(StateCountyData$State)
StateCountyList <- subset(StateCountyData$County, StateCountyData$State == "WA")
StateNameList <- data.frame(
  unique(StateCountyData[c("State","StateID")])
)

# Read WA SHP file
#WA_shp = st_transform(st_read("www/WA_shp/new1.shp"), 4326)
#WA_shp$Fatal_Injury <- WA_shp$Fatal + WA_shp$Injury
WA_shp = st_transform(st_read("www/WA_shp2/WA_TMC_Census.shp"), 4326)

# Read OH SHP file
OH_shp = st_transform(st_read("www/OH_shp2/OH_Fin2.shp"), 4326)

# Read NC SHP file
#NC_shp = st_transform(st_read("www/NC_shp/Final1_NC.shp"), 4326)

# Read Counties SHP file
UScounties_shp = st_transform(st_read("www/US_Counties/tl_2018_us_county.shp"), 4326)



# Start Creating Dashboard layout
header <- dashboardHeader(
  title = "Rural Speed Tool (BETA)"
)

body <- dashboardBody(useShinyjs(),
                      tabsetPanel(
                        tabPanel(HTML(paste(tags$span(style="font-size: 18px", "RuralSpeedSafetyX"))), id="RuralSpeedTool",
                                 tags$h1(tags$b("Interactive Decision Support Tool to Improve Safety")),
                                 fluidRow(
                                   column(width = 8,
                                          box(width = NULL, solidHeader = TRUE,
                                              leafletOutput("MapOut", height = 500),
                                              h2()                                          )
                                   ),
                                   column(width = 3,
                                          box(width = NULL, status = "warning",
                                              selectInput("YearInput","Year",choices = list("2015" = 2015), selected = 2015),
                                              selectInput("StateInput","State",choices = StatesList,selected = 'WA'),
                                              selectInput("CountyInput","County",choices = c("All Counties",StateCountyList)),
                                              selectInput("FacilityInput","Facility",choices = c("All","Interstate/Freeway/Expressway","Multilane","Two-lane")),
                                              radioButtons("Severity", label = "Severity", choices = list("All", "Fatal and Injury"), inline=TRUE),
                                              actionButton(inputId = "RefreshMap", label = "Refresh Map", class = "butt"),
                                              tags$head(tags$style(".butt{background-color:#0000FF;} .butt{color: white;}")), # background color and font color
                                              downloadButton("downloadData",label ="Download Data")
                                          )
                                   )
                                   
                                 ),
                                 DT::dataTableOutput('outputDT'),
                                 h2(),tags$br(),
                                 h2(),tags$br()
                                 
                                 
                        )
                      )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  #header,
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body
)

server <- function(input, output, session) {
  
  StateCountyList <- eventReactive(input$StateInput, {
    subset(StateCountyData$County, StateCountyData$State == input$StateInput)
  })  
  observeEvent(input$StateInput, 
               updateSelectInput(session, "CountyInput","County",
                                 choices = c("All Counties",
                                             subset(StateCountyData$County, StateCountyData$State == input$StateInput)))
  )  
  observeEvent(input$StateInput, 
               updateSelectInput(session, "FacilityInput","Facility",
                                 choices = c("All","Interstate/Freeway/Expressway","Multilane","Two-lane"))
  )  
  
  output$MapOut <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "//cartodb-basemaps-{s}.global.ssl.fastly.net/dark_all/{z}/{x}/{y}{r}.png", layerId = 'Carto DB Dark Matter') %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)
    #setView(lng = -120.740135, lat = 47.376903, zoom = 7)
  })
  
  observeEvent(input$RefreshMap, {
    
    STATEFPin <- switch(input$StateInput,
                        "NC"= 37,
                        "OH" = 39,
                        "WA" = 53
                        )
    COUNTYin = input$CountyInput
    if (COUNTYin != "All Counties"){
      Countyin_Code <- select(filter(filter(StateCountyData, StateID == STATEFPin), (County == COUNTYin)), StateCounty)
    } else {
      Countyin_Code <- 0
    }
    FacilityIn = input$FacilityInput
    if (FacilityIn == "Interstate/Freeway/Expressway"){
      FacilityIn_Code <- "Interstate"
    } else {
      FacilityIn_Code <- FacilityIn
    }
    
    ### Will need to change MapOutputData switch to include other states
    #MapOutputData <- WA_shp
    MapOutputData <- switch(input$StateInput,
                        "NC"= WA_shp,
                        "OH" = OH_shp,
                        "WA" = WA_shp
    )
    
    if (Countyin_Code == 0){
      MapOutputDataTempCounty <- MapOutputData
      
      UScounties_shp_selected <- st_as_sf(filter(as.data.frame(UScounties_shp), STATEFP == STATEFPin))
      LATzoom <- switch(input$StateInput,
                        "NC"= 35.782169,
                        "OH" = 40.367474,
                        "WA" = 47.376903
      )
      LONzoom <- switch(input$StateInput,
                        "NC"= -80.793457,
                        "OH" = -82.996216,
                        "WA" = -120.740135
      )
      
      zoomLevel <- 6
    } else {
      MapOutputDataTempCounty <- st_as_sf(filter(as.data.frame(MapOutputData), COUNTYFP == sprintf('%03d', as.integer(Countyin_Code) %% 1000)))
      
      UScounties_shp_selected <- st_as_sf(filter(filter(as.data.frame(UScounties_shp), STATEFP == STATEFPin), COUNTYFP == sprintf('%03d', as.integer(Countyin_Code) %% 1000)))
      
      LATzoomTemp <- select(as.data.frame(UScounties_shp_selected), INTPTLAT)
      LONzoomTemp <- select(as.data.frame(UScounties_shp_selected), INTPTLON)
      
      LATzoom <- as.numeric(as.character(unlist(LATzoomTemp[[1]])))
      LONzoom <- as.numeric(as.character(unlist(LONzoomTemp[[1]])))
      
      zoomLevel <- 8
    }
    
    if (FacilityIn_Code == 'All'){
      MapOutputDataFinal <- MapOutputDataTempCounty
    } else {
      MapOutputDataFinal <- st_as_sf(filter(as.data.frame(MapOutputDataTempCounty), Facility == FacilityIn_Code))
    }
    
    DataForPal <- switch(input$Severity,
                         "All" =  MapOutputDataFinal$Total_Exp,
                         "Fatal and Injury" = MapOutputDataFinal$FI_Exp)
  
    pal_Total <- colorNumeric("YlOrRd", DataForPal)
    labelOut <- as.list(paste0('TMC: ', MapOutputDataFinal$TMC, "<br>",
                               'Road: ',MapOutputDataFinal$ROAD_NUMBE, "<br>",
                               'State: ',input$StateInput, "<br>",
                    'County: ', input$CountyInput, "<br>",
                    'Facility: ', MapOutputDataFinal$Facility, "<br>",
                    'Total Expected: ', MapOutputDataFinal$Total_Exp, "<br>",
                    'Fatal, Injury Expected: ', MapOutputDataFinal$FI_Exp, "<br>",
                    'AADT: ', round(MapOutputDataFinal$AADT, 0), "<br>",
                    #'Freeflow Speed: ', round(MapOutputDataFinal$SpdFF, 2), "<br>",
                    'Speed Variance 1: ', round(MapOutputDataFinal$SpdVarr1, 2), "<br>"
    ))
    popupOut <- paste0('TMC: ', MapOutputDataFinal$TMC, "<br>",
                       'Road: ',MapOutputDataFinal$ROAD_NUMBE, "<br>",
                       'State: ',input$StateInput, "<br>",
                       'County: ', input$CountyInput, "<br>",
                       'Facility: ', MapOutputDataFinal$Facility, "<br>",
                    'Total Expected: ', MapOutputDataFinal$Total_Exp, "<br>",
                    'Fatal, Injury Expected: ', MapOutputDataFinal$FI_Exp, "<br>",
                    'AADT: ', round(MapOutputDataFinal$AADT, 0), "<br>",
                    #'Freeflow Speed: ', round(MapOutputDataFinal$SpdFF, 2), "<br>",
                    'Speed Variance 1: ', round(MapOutputDataFinal$SpdVarr1, 2), "<br>"
    )

    leafletProxy("MapOut") %>% clearPopups() %>% clearGroup("Total/Fata/Injury") %>% clearGroup("CountiesSHP") %>% clearControls() %>%
      ###setView(lng = -120.740135, lat = 47.376903, zoom = 7) %>% 
      setView(lng = LONzoom, lat = LATzoom, zoom = zoomLevel) %>% 
      addPolylines(data=MapOutputDataFinal, 
                   color=~pal_Total(
                     switch(input$Severity,
                            "All" =  MapOutputDataFinal$Total_Exp,
                            "Fatal and Injury" = MapOutputDataFinal$FI_Exp)
                   ), 
                   group="Total/Fata/Injury",
                   popup = popupOut,
                   label = lapply(labelOut, HTML)) %>%
      addPolylines(data=UScounties_shp_selected, 
                   color='green', 
                   group="CountiesSHP", 
                   weight = 1) %>%
      addLegend("bottomright", pal = pal_Total, 
                values = switch(input$Severity,
                                "All" =  MapOutputDataFinal$Total_Exp,
                                "Fatal and Injury" = MapOutputDataFinal$FI_Exp), 
                title = paste0(input$Severity, " Crashes")
                )
if (nrow(as.data.frame(MapOutputDataFinal)) > 0){
  MapOutputDataFinalDTtemp <- cbind(input$StateInput,input$CountyInput,
                                    select(as.data.frame(MapOutputDataFinal), 
                                           c('TMC', 
                                             #'ADMIN_LE_1',
                                             #'ADMIN_LE_2',
                                             'ROAD_NUMBE',
                                             'Facility',
                                             'DISTANCE',
                                             'Population',
                                             'Household',
                                             'AADT',
                                             'NO_LANES',
                                             'SPD_LIMT',
                                             'OptSpd',
                                             #'SpdFF',
                                             'SpdVarr1',
                                             'Total_Exp',
                                             'FI_Exp'
                                           )
                                    )
  ) 
  
  MapOutputDataFinalDTtemp <- distinct(MapOutputDataFinalDTtemp, TMC, .keep_all = TRUE)
} else {
  MapOutputDataFinalDTtemp <- cbind(input$StateInput,
                                    input$CountyInput,
                                    'None', 
                                    #'None',
                                    #'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    'None',
                                    #'None',
                                    'None'
  ) 
  
}
    
    MapOutputDataFinalDT <- datatable(MapOutputDataFinalDTtemp,
                                      class = 'cell-border stripe', 
                                      colnames = c('State',
                                                   'County',
                                                   'TMC',
                                                   #'State',
                                                   #'County',
                                                   'Road',
                                                   'Facility',
                                                   'Segment Length',
                                                   'Population',
                                                   'Household',
                                                   'AADT',
                                                   'Number of Lanes',
                                                   'Speed Limit',
                                                   'Avg. Ope. Speed - Yearly',
                                                   #'Freeflow Speed',
                                                   'Std. Dev. of Ope. Speed - Hourly',
                                                   'Total - Expected',
                                                   'Fatal and Injury - Expected'
                                      )
    ) 
    
    output$outputDT = DT::renderDataTable(MapOutputDataFinalDT, options = list(lengthChange = FALSE))
    
    outputDTdowload <- MapOutputDataFinalDTtemp
    names(outputDTdowload) <- c('State',
                                'County',
                                'TMC',
                                #'State',
                                #'County',
                                'Road',
                                'Facility',
                                'Segment Length',
                                'Population',
                                'Household',
                                'AADT',
                                'Number of Lanes',
                                'Speed Limit',
                                'Avg. Ope. Speed - Yearly',
                                #'Freeflow Speed',
                                'Std. Dev. of Ope. Speed - Hourly',
                                'Total - Expected',
                                'Fatal and Injury - Expected'
    )
    
    output$downloadData <- downloadHandler(
      #filename = function() {paste("test.csv")},
      #filename = function() {gsub(" ","",paste(input$StateInput,"_",input$CountyInput,"_",input$YearInput,".csv"))},
      filename = function() {gsub(" ","",paste(input$StateInput,"_",
                                               input$CountyInput,"_",
                                               switch(input$FacilityInput,
                                                      "All"= "All",
                                                      "Interstate/Freeway/Expressway" = "IntFwayExpway",
                                                      "Multilane" = "Multilane",
                                                      "Two-lane" = "Twolane"
                                               ),
                                               "_",input$YearInput,".csv"))},
      content = function(file) {
        write.csv(outputDTdowload,file, row.names=FALSE)
      })
  })  
  
  
}

shinyApp(ui, server)
