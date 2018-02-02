#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 
#' 

library(shiny)

ui <- fluidPage(
  #Input()
  sliderInput(
    inputId = "Year",
    label = "Year Range",
    value = 2005, min = as.integer(min(selectedYear)), max =  as.integer(max(selectedYear))
  ),
  # sliderInput(
  #   inputId = "str",
  #   label = "Country",
  #   min = unique(data[,tolower(areaVar),with=FALSE]), max =  as.integer(max(selectedYear))
  # )
  #Output()
  plotOutput("line")
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  output$line <- renderPlot({
   title <- "Test"
   hist(rnorm(input$Year), main = title)
   
   # Plot pdf 
   # x_plots <-exp(datamod$losstransf[datamod$losstransf !=0])/(1+exp(datamod$losstransf[datamod$losstransf !=0])) 
   # mean_Data <- mean(x_plots)
   # std_Data <- sd(x_plots)
   # qqnorm(x_plots)
   # qqline(x_plots)
   # plot(datamod$timepointyears,datamod$losstransf)
     
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)