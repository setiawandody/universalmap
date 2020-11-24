
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(highcharter)
library(tidyr)
library(viridis)
library(colourpicker)
library(RJSONIO)
template <- read.csv("data_input/sample_dat.csv", header = T)
hc_theme <- read.csv("data_input/hc_theme2.csv", header = F)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Universal Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( 
        # "Download the csv template here",
        downloadButton("csvTemplate", label = "Download template"),
         fileInput("datInput", "Choose CSV File", accept = ".csv"),

        textInput("titleInput", "title:"),
        textInput("subtitleInput", "subtitle:"),
        textInput("legendindicatorInput", "indicator:"),
        selectInput("gradient", "select gradient:", choices = c("viridis", "inferno", "magma", "plasma")),
        checkboxInput("direction", "reverse gradient"),
        selectInput("hc_theme", "select theme: ", choices = hc_theme),
        #  colourInput("color1", "Select colour #1", "red"),
        #  colourInput("color2", "Select colour #2", "green"),
        # colourInput("color3", "Select colour #3", "blue"),        
         actionButton("plot", "Plot map")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        highchartOutput("map"),
        tableOutput("table")
         # plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$csvTemplate <- downloadHandler(
    filename = "template.csv",
    content = function(file) {
      write.table(template, file, sep = ",", row.names = FALSE)
    }
  )
  
  datInput <- observeEvent(input$datInput, {
    # Load inputs
    inFile <- input$datInput
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = T)
  })
  
  dat <- eventReactive(input$plot, {
    inFile <- input$datInput
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = T)
  })
  
  output$table <- renderTable({
    dat()
  })
  
  output$map <- renderHighchart({

    dat <- dat()
    title <- ifelse(is.null(input$titleInput), "", input$titleInput)
    subtitle <- ifelse(is.null(input$subtitleInput), "", input$subtitleInput)
    legendIndicator <- ifelse(is.null(input$legendindicatorInput), "", input$legendindicatorInput)
    direction <- ifelse(input$direction, -1, 1)
                                  
    hcmap("https://code.highcharts.com/mapdata/countries/id/id-all.js", data = dat, value = "value" ,
          joinBy = c("name", "province"), name = "Potensi Teknis",
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.05,
          tooltip = list(valueDecimals = 0, valuePrefix= '~', valueSuffix= ' MW')) %>%
      hc_title(text = title, align = "left") %>% 
      hc_subtitle(text = subtitle, align = "left") %>%
      # hc_credits(text = "credit") %>% 
      hc_legend(enabled = TRUE, reversed = TRUE) %>% 
      hc_colorAxis(stops = color_stops(colors = eval(parse(text = paste(input$gradient, "(30, direction = ", direction, ")", sep = ""))))) %>% #(30, direction = -1) #n = 30, colors = c(input$color1, input$color2, input$color3)
      hc_add_theme(eval(parse(text = paste(input$hc_theme, "()", sep = "")))) %>%
      hc_legend(title = list(text = legendIndicator), align = "right", verticalAlign = "top", floating = TRUE,
                layout = "horizontal", x = 0, y = 0) %>% 
      hc_exporting(enabled = T)
  })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

