# This app has three tabs: 
## Compare Years 
## Compare Crimes
## Table of the ages with maximum convictions

#.libPaths()
#.libPaths( c("C:/Users/u415537/Documents/OFFLINE/R/rPackages", .libPaths()) )


## app.R ##
library("tools")
library("shiny")#, lib.loc = "C:/Users/u415537/Documents/OFFLINE/R/rPackages")
library("magrittr")
library("forcats")
library("dplyr")
library("ggplot2")
library("scales")
library("stringr")
library("shiny")
library("plotly")
library("DT")

#setwd("C:/Users/u415537/Documents/OFFLINE/R/conviction")

## Load and prepare the data ##
load("convictions_data.RData")
load("tabIndex.RData")
## LOAD THE TABLE OPTIONS
tabIndex[1,"prefix"] = ""

## List the crime options and their labels
tabSelect = tabIndex %>%
  mutate(inLabel = paste(prefix," ", CrimeType,sep="")) %>% 
  select(-prefix, -Level)

tablesAre = tabSelect$CrimeType
names(tablesAre) = tabSelect$inLabel

## LOAD DATA AND LEVELS
allCrimes = allCrimes %>% 
  mutate(`Crime Type` = fct_expand(`Crime Type`,"None"))

gendersAre = levels(allCrimes$Gender)
measuresAre = levels(allCrimes$`Measurement Type`) 
limitsAre = c(min(allCrimes$Year), max(allCrimes$Year))

## App begins ##
# User interface portion of the script
ui = fluidPage(
  title = "Scottish Convictions",
  theme = "https://www.gov.scot/webfiles/1543508401163/assets/css/main.css",
  tags$style("
             body {
             -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
             zoom: 0.9; /* Other non-webkit browsers */
             zoom: 90%; /* Webkit browsers */
             }
             "),
  
  titlePanel(
    fluidRow(
      column(width = 12, h1(textOutput("titleText")))
    )
  ),
  #h1(textOutput("titleText")),
  
  ## Input commands
  sidebarLayout(
    
    sidebarPanel(
      
      ### Select table
      selectInput(inputId = "tableName1", 
                  label="Choose a crime type", 
                  choices=tablesAre, selected = tablesAre[1]),
      selectInput(inputId = "gender", 
                  label="Gender", 
                  choices=gendersAre, selected = gendersAre[3]),
      selectInput(inputId = "measure", 
                  label="Measurement Type", 
                  choices=measuresAre[c(2,1)]),
      wellPanel(
        sliderInput(inputId = "year1",
                    sep="", 
                    label="Year 1",
                    min=limitsAre[1],
                    max=limitsAre[2],
                    value = limitsAre[1],
                    step = 1,
                    animate = animationOptions(interval=500)
        ),
        div(style="text-align:right",
            "To animate press",strong("Play"),br()),
        sliderInput(inputId = "year2",
                    sep="", 
                    label="Year 2",
                    min=limitsAre[1],
                    max=limitsAre[2],
                    value = limitsAre[2],
                    step = 1,
                    animate = FALSE
        )
      ),#end of year well
      radioButtons(inputId = "scaleLim", 
                   label = "View y-axis as:", 
                   choices = list("Fixed" = 1,
                                  "Dynamic" = 3),
                   selected = 1,
                   inline = TRUE),
      uiOutput("feedback")
    ), #end of sidebar panel
    
    ## Output tab area
    
    mainPanel(
      tabsetPanel(
        ### CHART comparing years by age
        tabPanel("Compare Years",
                 h2(textOutput("yearCompTitle")),
                 plotlyOutput("yearComp"),
                 wellPanel( textOutput("yearCompDesc") )
        ), # END chart comparing years
        
        ### CHART comparing crime types by age
        tabPanel("Compare Crimes",
                 h2(textOutput("crimeCompTitle")),
                 paste("Select another crime type for comparison below the chart"),
                 plotlyOutput("crimeComp"),
                 column(width = 3, HTML('<p></p><p class="control-label"><strong>Select a crime type for comparison:</strong></p>')),
                 column(width = 9,
                        selectInput(inputId = "tableName2",
                                    label = "",
                                    width = "100%",
                                    choices=c("None",tablesAre),
                                    selected = "None"))
        ), # END chart comparing crime types by age
        
        ### CHART comparing gender by age
        tabPanel("Compare Genders",
                 h2(textOutput("genderCompTitle")),
                 plotlyOutput("genderComp")
        ), # END chart comparing crime types by age        
        ### CHART comparing timeseries trends
        tabPanel("Trend over Time",
                 h2(textOutput("tsTitle")),
                 plotlyOutput("timeseries"),
                 column(width = 3, HTML('<p></p><p class="control-label"><strong>Select a crime type for comparison:</strong></p>')),
                 column(width = 9,
                        selectInput(inputId = "tableName3",
                                    label = "",
                                    width = "100%",
                                    choices=c("None",tablesAre),
                                    selected = "None"))
        ) # END chart comparing crime types
        
        ### Uncomment next two for TABLE age of max convictions 
        #,
        #tabPanel("Table",h3(textOutput("dttitle")),paste("Sort using column controls"),dataTableOutput("datatab")            )
        
        ,
        fluidRow(),
        fluidRow(
          column(width = 4,
                 img(src="SGLogo.png",width="100%")),
          column(width = 7,
                 HTML("<p>Further information on these statistics can be found on the </br>"),
                 tags$a(href="https://www.gov.scot/publications/criminal-proceedings-scotland-2017-18/ ", 
                        "Scottish Government website")),
          column(width = 1, img(src="natstats.png",width="100%"))
        ),
        
        fluidRow(HTML("</p><p>The data contained within these representations may differ slightly from those 
                      published in the Criminal Proceedings bulletins. This may be due to the inclusion of 
                      company proceedings and convictions in the bulletins and/or the ongoing quality assurance 
                      and updating of the source data used for publication</p>")),
        
        fluidRow(HTML("<p>For more information on this publication, contact Justice Analytical Services: </br> 
                        &nbsp; &nbsp; Jamie Robertson </br>
                        &nbsp; &nbsp; Telephone: 0131 244 3040 </br>
                        &nbsp; &nbsp; e-mail: justiceanalysts@gov.scot </p>"))
      ) #end of tabsetPanel
    ) #end of mainPanel
  ) #end of sidebarLayout
  ) #end of ui

server = function(input, output){
  
  tableName = reactive({c(input$tableName1, 
                          input$tableName2, 
                          input$tableName3)}) # gather requested crime types
  
  # Title text for the page heading
  #output$titleText = renderText({toTitleCase(paste(input$measure,"of Convictions for",input$tableName1))}) # Reactive Main Title with measure and crime type
  output$titleText = renderText({"Age of Conviction in Scotland"})
  
  
  # URL and text for the feedback form
  fburl <- a("here", href="https://scotland.shinyapps.io/sg-age-crime/")
  output$feedback <- renderUI({
    tagList("An interactive analytical paper based on these statistics is available",fburl)
  })
  
  # Query data by age for table1 (and table2) requests
  agedata = reactive({
    if(input$tableName1 != input$tableName2 & 
       input$tableName2 != "None"){
      
      subset(allCrimes,
             `Crime Type` %in% c(input$tableName1, 
                                 input$tableName2) & 
               Gender == input$gender & 
               Year %in% c(input$year1,input$year2) & 
               `Measurement Type` == input$measure & 
               Age != "All")  %>% 
        #Reorder CrimeType factor to consistently plot table1 as the first line
        mutate(`Crime Type` = fct_relevel(`Crime Type`,
                                          c(input$tableName1, 
                                            input$tableName2))) 
    }
    else{
      subset(allCrimes,
             `Crime Type` %in% c(input$tableName1) & 
               Gender == input$gender & 
               Year %in% c(input$year1,input$year2) & 
               `Measurement Type` == input$measure & 
               Age != "All")
    }
    
  }) # Query the data by age based on inputs
  tsdata = reactive({
    if(input$tableName1 != input$tableName3 | 
       input$tableName3 == "None"){
      subset(allCrimes,
             `Crime Type` %in% c(input$tableName1, 
                                 input$tableName3) & 
               Gender == input$gender & 
               `Measurement Type` == input$measure & 
               Age == "All") %>% 
        #Reorder CrimeType factor to consistently plot table1 as the first line
        mutate(`Crime Type` = fct_relevel(`Crime Type`, c(input$tableName1, input$tableName3)))
    } else {
      subset(allCrimes,
             `Crime Type` %in% c(input$tableName1) & 
               Gender == input$gender & 
               `Measurement Type` == input$measure & 
               Age == "All")
    }
  }) # Query the data based on inputs
  gcompdata = reactive({
    subset(allCrimes,
           `Crime Type` == input$tableName1 & 
             Gender %in% c("Male", "Female") & 
             `Measurement Type` == input$measure & 
             Year == input$year1 &
             Age != "All")
  }) # Query the data based on inputs
  
  #Chart between-year comparison
  output$yearCompTitle = renderText({
    titleTables = c(input$tableName1, 
                    input$tableName2)
    
    if(input$year1==input$year2) {
      paste0("Convictions for ", tolower(titleTables[1]),
             ", by age, in financial year ending ",input$year1,
             " (",input$gender,")")} else  {
               paste0("Convictions for ", tolower(titleTables[1]),
                      ", by age, in financial years ending ",input$year1,
                      " & ", input$year2, 
                      " (",input$gender,")")} # Reactive subtitle with years
  })
  limiterYearComp = reactive({
    
    if(input$scaleLim == 1){
      allCrimes %>% 
        subset(`Measurement Type` == input$measure & 
                 `Crime Type` %in% c(input$tableName1)& 
                 Age != "All") %>% 
        group_by(`Crime Type`, Year) %>% 
        summarise(lim = max(Convictions)) 
    } else if(input$scaleLim == 2){
      allCrimes %>% 
        subset(`Measurement Type` == input$measure &
                 Gender == input$gender & 
                 `Crime Type` %in% c(input$tableName1)& 
                 Age != "All") %>% 
        group_by(`Crime Type`, Year) %>% 
        summarise(lim = max(Convictions))
    } else if (input$scaleLim == 3){
      allCrimes %>% 
        subset(`Crime Type` %in% c(input$tableName1) & 
                 Gender == input$gender & 
                 Year %in% c(input$year1,input$year2) & 
                 `Measurement Type` == input$measure& 
                 Age != "All") %>% 
        arrange(Convictions) %>% 
        group_by(`Crime Type`, Year) %>% 
        summarise(lim = max(Convictions))
    }
  }) # Getting the maximums for graph axes
  output$yearComp = renderPlotly({
    
    topY = limiterYearComp() %>% 
      filter(`Crime Type` == input$tableName1) %>% 
      summarise(topY = max(lim)) %>% select(topY) %>% 
      mutate(topY=ceiling(topY)) %>% 
      as.integer()
    
    agedata() %>% 
      filter(`Crime Type` == input$tableName1) %>% 
      plot_ly(x = ~Age, y = ~Convictions, split = ~Year,
              type = 'scatter', mode = 'lines',
              text = ~paste0('Crime: ', `Crime Type`,
                             '  <br>Age: ', Age,
                             '  <br>Convictions: ', round(Convictions*10,digits = 0)/10,"   "),
              hoverinfo = 'text'
      ) %>% 
      layout(xaxis = list(showline = FALSE,
                          range = c(12,90),
                          type = 'category',
                          tickfont = list(size = 14),
                          titlefont = list(size = 16), 
                          title = 'Age'),
             yaxis = list(title = paste0("Convictions (",input$measure,")"),
                          tickfont = list(size = 14),
                          titlefont = list(size = 16),
                          range = c(0,topY)),
             hovermode = 'x'
      )
  }) #line plot
  output$yearCompDesc = renderText({
    filter(tabSelect, CrimeType==input$tableName1) %>% 
      select(DescText) %>% 
      paste()
  }) # Description of selected crime type   
  
  #Chart the data reactively to compare crimes
  output$crimeCompTitle = renderText({
    if(input$tableName1==input$tableName2 | input$tableName2=="None")
    {paste0("Convictions for ", tolower(input$tableName1),
            ", by age, in financial year ending ",input$year1,
            " (",input$gender,")")} 
    else if(input$tableName1!=input$tableName2){
      paste0("Convictions for ", tolower(input$tableName1),
             " and ", tolower(input$tableName2),
             ", by age, in financial year ending ",input$year1,
             " (",input$gender,")")}
    
  }) # Reactive chart title with crime types
  limiterCrimeComp = reactive({
    
    if(input$scaleLim == 1){
      allCrimes %>% 
        subset(`Measurement Type` == input$measure &
                 Age != "All" &
                 `Crime Type` %in% c(input$tableName1, 
                                     input$tableName2)) %>%
        summarise(lim = max(Convictions)) 
    } else if(input$scaleLim == 2){
      allCrimes %>% 
        subset(`Measurement Type` == input$measure &
                 Gender == input$gender &
                 Age != "All" & 
                 `Crime Type` %in% c(input$tableName1, 
                                     input$tableName2)) %>% 
        group_by(`Crime Type`, Year) %>% 
        summarise(lim = max(Convictions))
    } else if (input$scaleLim == 3){
      allCrimes %>% 
        subset(`Crime Type` %in% c(input$tableName1, 
                                   input$tableName2) &
                 Age != "All" & 
                 Gender == input$gender & 
                 Year %in% c(input$year1,input$year2) & 
                 `Measurement Type` == input$measure) %>% 
        arrange(Convictions) %>% 
        group_by(`Crime Type`, Year) %>% 
        summarise(lim = max(Convictions))
    }
  }) # Getting the maximums for graph axes
  output$crimeComp = renderPlotly({
    
    titleTables = tableName()
    
    topY = if (input$scaleLim == 3){
      limiterCrimeComp() %>% ungroup(`Crime Type`) %>% group_by(Year) %>% 
        summarise(topY = max(lim)) %>% 
        filter(Year == input$year1) %>% 
        select(topY) %>% 
        mutate(topY=ceiling(topY)) %>% 
        as.integer()
    } else {
      limiterCrimeComp() %>% ungroup(`Crime Type`) %>% 
        summarise(topY = ceiling(max(lim))) %>% select(topY) %>% 
        mutate(topY=ceiling(topY)) %>% 
        as.integer()
    }
    
    agedata() %>% 
      subset(Year == input$year1) %>% 
      plot_ly(x = ~Age, y = ~Convictions, split = ~`Crime Type`,
              type = 'scatter', mode = 'lines',
              text = ~paste0('Crime: ', `Crime Type`,
                             '  <br>Age: ', Age,
                             '  <br>Convictions: ', round(Convictions*10,digits = 0)/10,"   "),
              hoverinfo = 'text'
      ) %>% 
      layout(xaxis = list(showline = FALSE,
                          range = c(12,90),
                          type = 'category',
                          tickfont = list(size = 14),
                          titlefont = list(size = 16), 
                          title = 'Age'),
             yaxis = list(title = paste0("Convictions (",input$measure,")"),
                          tickfont = list(size = 14),
                          titlefont = list(size = 16),
                          range = c(0,topY)),
             hovermode = 'x'
             )
  }) #line plot
  
  #Chart the data reactively to compare crimes
  output$genderCompTitle = renderText({
    paste0("Convictions for ", tolower(input$tableName1),
           ", by age and gender, in financial year ending ",input$year1)
  }) # Reactive chart title with crime types
  limiterGenderComp = reactive({
    
    if(input$scaleLim < 3){
      # fixed across all years
      allCrimes %>%
        subset(`Crime Type` == input$tableName1 & 
                 Gender %in% c("Male", "Female") & 
                 `Measurement Type` == input$measure &
                 Age != "All") %>% 
        summarise(lim = max(Convictions)) 
    } else if (input$scaleLim == 3){
      #limit for specific current years displayed
      allCrimes %>%
        subset(`Crime Type` == input$tableName1 & 
                 Age != "All" & 
                 Gender %in% c("Male", "Female") & 
                 `Measurement Type` == input$measure &
                 Year %in% c(input$year1)) %>% 
        arrange(Convictions) %>% 
        group_by(Gender) %>% 
        summarise(lim = max(Convictions)) 
    }
  }) # Getting the maximums for graph axes
  output$genderComp = renderPlotly({
    
    lim = limiterGenderComp()
    if (input$scaleLim == 3){
      topY = as.integer(ceiling(max(lim$lim)))}
    else{topY = as.integer(lim)}
    
    
    gcompdata() %>% 
      subset(Year == input$year1) %>% 
      plot_ly(x = ~Age, y = ~Convictions, split = ~Gender,
              type = 'scatter', mode = 'lines',
              text = ~paste0('Gender: ', Gender,
                             '  <br>Age: ', Age,
                             '  <br>Convictions: ', round(Convictions*10,digits = 0)/10,"   "),
              hoverinfo = 'text'
      ) %>% 
      layout(xaxis = list(showline = FALSE,
                          range = c(12,90),
                          type = 'category',
                          tickfont = list(size = 14),
                          titlefont = list(size = 16), 
                          title = 'Age'),
             yaxis = list(title = paste0("Convictions (",input$measure,")"),
                          tickfont = list(size = 14),
                          titlefont = list(size = 16),
                          range = c(0,topY)),
             hovermode = 'x'
      )
  }) #line plot
  
  #Chart the timeseries data & compare crimes
  output$tsTitle = renderText({
    titleTables = tableName()
    if(input$tableName1==input$tableName3 | input$tableName3=="None")
    {paste0("Convictions for ", tolower(input$tableName1),
            " over time (",input$gender,")")}
    
    else if(input$tableName1!=input$tableName3)
    {paste0("Convictions for ", tolower(input$tableName1),
            " and ", tolower(input$tableName3),
            " over time (",input$gender,")")}
  }) # Reactive chart title with crime types
  output$timeseries = renderPlotly({
    inData = tsdata()
    topY = max(inData$Convictions)
    xrange = c(min(inData$Year), max(inData$Year))
    
    tsdata() %>% 
      plot_ly(x = ~Year, y = ~Convictions, split = ~`Crime Type`,
              type = 'scatter', mode = 'lines',
              text = ~paste0('Crime Type: ', `Crime Type`,
                             '  <br>Year: ', Year,
                             '  <br>Convictions: ', round(Convictions*10,digits = 0)/10,"   "),
              hoverinfo = 'text'
      ) %>% 
      layout(xaxis = list(showline = FALSE,
                          range = xrange,
                          tickfont = list(size = 14),
                          titlefont = list(size = 16),
                          title = 'Year'),
             yaxis = list(title = paste0("Convictions (",input$measure,")"),
                          tickfont = list(size = 14),
                          titlefont = list(size = 16),
                          range = c(0,topY)),
             hovermode = 'x'
      )
  }) #line plot
  
} # end of server section


## Run the app ##
shinyApp(ui=ui,server=server)
