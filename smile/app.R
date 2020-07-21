
library(shiny)
library(tidyverse)
library(leaflet)
library(shinythemes)

data <- read_csv("smiley_data.csv")

# Define UI
ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
    # Application title
    titlePanel("Seneste Fødevarekontroller"),
    
    # navbar
    navbarPage("",
    
    #panel 1
    tabPanel("Kort",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 3,
                     tags$head( tags$style( type = "text/css", '
      .irs-line-mid{
        background: #428bca ;
        border: 1px solid #428bca ;
      }
      .irs-line-right{
        background: #428bca ;
      }
      .irs-bar {
        background: linear-gradient(to bottom, #DDD -50%, #FFF 150%);
        border-top: 1px solid #CCC ;
        border-bottom: 1px solid #CCC ;
      }
      .irs-bar-edge {
        background: inherit ;
        border: inherit ;
      }

    ')), 
            sliderInput("dato",
                        "Vælg datointerval",
                       # min = min(data$seneste_kontrol_dato),
                      #  max = max(data$seneste_kontrol_dato),
                          min = lubridate::today()-365,
                          max = lubridate::today(),
                          value = lubridate::today()-180,
                          step = 30),

            checkboxGroupInput("checkGroup", 
                                      "Smiley/karakter", 
                                      choices = list("1 (Glad)" = 1, 
                                                     "2" = 2, 
                                                     "3" = 3,
                                                     "4 (Sur)" = 4),
                                      selected = c(1,2,3,4)),
            h5(em("kilde: Fødevarestyrelsen")),
            
            
       ),
        
        mainPanel(
            leafletOutput("map",width = "110%", height = 800)
        ) # main panel
        
        ) # sidebar layour
    ), # tab panel 1
    
    tabPanel("Data",align="center",
            # downloadButton('download',"Download .csv"),
             
             DT::dataTableOutput("dataframe",width = "80%"))
    ) # navbar
    ) # UI
    )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # MAP #########################
    # filter data based on input from ui
    map_data_react <- reactive({
        
        data %>%
            filter(seneste_kontrol_dato >= input$dato,
                   seneste_kontrol %in% input$checkGroup)
        
    })

    output$map <- renderLeaflet({
        
        data <- map_data_react()  # Add this
        
        leaflet() %>%
            addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
            addCircleMarkers(data = data, opacity = .5,fillOpacity = .8,radius=5, stroke = T, col = ~colour,
                             label = ~navn1,
                             popup = paste(
                                 data$navn1,"<br>",
                                 "CVR: ", data$cvrnr, "<br>",
                                 data$adresse1,"<br>",
                                 data$postnr,data$By, "<br>",
                                 "Seneste kontrol: ",data$seneste_kontrol_dato, "<br>",
                                 "<a href=",data$URL,', target=\"_blank\"> Rapport</a>'))
        #,clusterOptions = markerClusterOptions())
    })
    
    # DATA #############################
    output$dataframe <- DT::renderDataTable({
         
        #DT::dataTableAjax(session, data, outputId = "dataframe")
        data <- data %>%
            select(navn1:URL) %>%
            rename("Navn" = "navn1","CVR" = "cvrnr", "Adresse" = "adresse1",
                   "Postnr" = "postnr", "Seneste smiley/karakter" = "seneste_kontrol",
                   "Dato seneste kontrol" = "seneste_kontrol_dato","Rapport" = "URL") %>%
            mutate(Rapport = paste("<a href=",data$URL,', target=\"_blank\"> Link </a>'))
        
        DT::datatable(data, escape = FALSE)
    })
    
    # output$download <- 
    #     downloadHandler(
    #         filename = "data.csv",
    #         content = function(file){
    #             write.csv(data,
    #                       file) }
    # )
}


# Run the application 
shinyApp(ui = ui, server = server)
