library(shiny)
library(ggplot2)
#library(dplyr)

# THis is the Shiny App code that has both Server/Ui code.
#Rather than provide the relative code it is better to have the direct source of data. This way Shiny Server is able to identify the data globally
# Read Data.

GlobalData <- read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", stringsAsFactors = FALSE, skip=1)
NorthPoleData <- read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/SH.Ts+dSST.csv", stringsAsFactors = FALSE, skip=1)
SouthPoleData <- read.csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", stringsAsFactors = FALSE, skip=1)

print(GlobalData)

# Define UI for application that draws a plot
ui <- fluidPage(
  
  # Application title
  titlePanel(title = h3("GISS Surface Temperature Analysis", align="center")),
  br(),   br(),
  sidebarLayout(
    sidebarPanel(
      #------------------------------------------------------------------
      # Add radio button to choice for north pole or south pole data
      radioButtons("poleInput", 
                   label = "Select Data: ",
                   choices = list("North Pole data" = 'NHem', "South Pole Data" = 'SHem', "Global Data" = 'Glob'),
                   selected = 'Glob'),
      br(),   br(),
      #------------------------------------------------------------------
      # Add Variable for Year Selection
      sliderInput("YearRange", "Select Year Range : ", min=1880, max=2022, value=c(1880, 1900), step=1
                  
      ),
      
      br(),   br(),
      #------------------------------------------------------------------
      # Add Variables selection option from January to December : 
      selectInput("var", "Select Variable from Dataset", 
                  choices=c("Jan"=2, "Feb"=3, "Mar"=4, "Apr"=5, 
                            "May"=6, "Jun"=7, "Jul"=8, "Aug"=9, 
                            "Sep"=10, "Oct"=11, "Nov"=12, "Dec"=13),
                  multiple=TRUE, selected = 2
      ),
      
      br(),   br()
      #------------------------------------------------------------------
      # Change background color for body
      #tags$style("body{background-color:lightyellow; color:brown}")
    ),
    
    mainPanel(
      #------------------------------------------------------------------
      # Create tab panes
      tabsetPanel(type="tab",
                  tabPanel("Summary",verbatimTextOutput("sumry")),
                  tabPanel("Structure", verbatimTextOutput("struct")),
                  tabPanel("Data", tableOutput("displayData")),
                  tabPanel("Plot", plotOutput("mygraph"))
      )
      
      #------------------------------------------------------------------
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  cols <- reactive({
    as.numeric(c(input$var))
    
  })
  
  mylabel <- reactive({
 if(input$poleInput=='Glob'){
     lable <- "Plot for Global Data"
   }
   if(input$poleInput=='NHem'){
     lable <- "Plot for North Pole Data"
    }
  if(input$poleInput=='SHem'){
     lable <- "Plot for South Pole Data"
   }
   lable
    })
  
  myFinalData <- reactive({
    #------------------------------------------------------------------
    # Select data according to selection of radio button
    if(input$poleInput=='Glob'){
      mydata <- GlobalData
      
    }
    
    if(input$poleInput=='NHem'){
      mydata <- NorthPoleData
    }
    
    if(input$poleInput=='SHem'){
      mydata <- SouthPoleData
    }
    #------------------------------------------------------------------
    # Get data rows for selected year
    mydata1 <- mydata[mydata$Year >= input$YearRange[1], ] # From Year
    mydata1 <- mydata1[mydata1$Year <= input$YearRange[2], ] # To Year
    #------------------------------------------------------------------
    # Get Data for selected months as variable
    mydata2<- mydata1[, c(1, sort(cols()))]
    #------------------------------------------------------------------
    # Get data rows for selected year
    data.frame(mydata2)
    #------------------------------------------------------------------
    
  })
  
  # Prepare "Data tab"
   output$displayData <- renderTable({
     myFinalData()
  })
  
  # Prepare Structure Tab
 renderstr <- reactive({ str(myFinalData())})
  
 output$struct <- renderPrint({
   renderstr()
  })
  
  # Prepare Summary Tab
  rendersumry <- reactive({ summary(myFinalData())})
  
  output$sumry <- renderPrint({
    rendersumry()
  })
  
  # Prepare Plot Tab
  output$mygraph <- renderPlot({
    plotdata <- myFinalData()
    plot(plotdata, col=c(1,2,3,4,5,6,7,8,9), main=mylabel())
   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
