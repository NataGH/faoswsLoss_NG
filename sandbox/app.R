#' Part of the FAO Loss Module
#' 
#' @author Alicia English Marco Migone
#' 
#' 
#' 
# ---
#   runtime: shiny
# output: html_document
# ---
  
GraphLosses = function(dataModeled,dataRaw,mod2res){
  
library(shiny)
library(shinythemes)
library(gridExtra)
library(ggplot2)
library(trelliscopejs)
library(rmarkdown)
#render("1-example.Rmd")

#CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
#CountryGroup$country <- tolower(CountryGroup$countryname)
#CountryGroup[,"geographicaream49":=CountryGroup$m49code]



ui <- fluidPage(theme = shinytheme("lumen"),
  sidebarLayout(
    sidebarPanel(
      #Input()
      sliderInput(
        inputId = "Year",
        label = "Year Range",
        value = c(2005,2015),step =1,sep = "", min = as.integer(min(dataRaw$timepointyears, dataModeled$timepointyears)), max =  as.integer(max(dataRaw$timepointyears,dataModeled$timepointyears))
      ),
      selectInput(
        inputId = "Commodity",
        label = "Selected Commodity",
        choices = unique(dataModeled[,"measureditemcpc",with=FALSE])
      ),
      selectInput(
        inputId = "Country",
        label = "Country",
        choices = unique(dataModeled$geographicaream49),
        selected = "360"
      ),
      selectInput(
        inputId = "Country2",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected = "392"
      ),
      selectInput(
        inputId = "Country3",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected = "454"
      ),
      selectInput(
        inputId = "Country4",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected ="484"
      ),
      selectInput(
        inputId = "Country5",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected = "686"
      ),
      selectInput(
        inputId = "Country6",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected = "716"
      ),
      selectInput(
        inputId = "Country7",
        label = "Country To Compare",
        choices = unique(dataModeled$geographicaream49),
        selected = "1248"
      )
      ),
  #Output()
  mainPanel(
    tabsetPanel(
      tabPanel("Modeled Losses",
        tags$p(""),
        tags$p("Losses for the first country"),
        plotOutput("plotgraph")
        #plotOutput("line"),
        #tags$p(""),
        #tags$p("Losses for the second country"),
        #plotOutput("line2")
        ),
      tabPanel("Residuals",  
        tags$p(""),
        tags$p("Model Residuals"),
        plotOutput("res")
        #tags$p(""),
        #tags$p("Summary stats for the raw input data for the commodity"),
        #verbatimTextOutput("stats")
        ),
      tabPanel("Raw Data for the country",  
        tags$p(""),
        tags$p("Raw input data for the commodity"),
        plotOutput("rawC"),
        tags$p(""),
        tags$p("Summary stats for the raw input data for the commodity"),
        verbatimTextOutput("statsC")),
      tabPanel("Raw Data",  
        tags$p(""),
        tags$p("Raw input data for the commodity"),
        plotOutput("raw"),
        tags$p(""),
        tags$p("Summary stats for the raw input data for the commodity"),
        verbatimTextOutput("stats"))
      )
    )
  
))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  set.seed(123)
  dataRes <- reactive({mod2res %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) )})
    
  dataR <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                          (measureditemcpc %in% input$Commodity))})
  dataRC <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                         (geographicaream49 %in% input$Country) &
                                          (measureditemcpc %in% input$Commodity))})

  
  data <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country) & 
                                              (measureditemcpc %in% input$Commodity))})
  data2 <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country2) & 
                                              (measureditemcpc %in% input$Commodity))})
  data3 <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country3) & 
                                              (measureditemcpc %in% input$Commodity))})
  data4 <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country4) & 
                                              (measureditemcpc %in% input$Commodity))})
  data5 <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country5) & 
                                              (measureditemcpc %in% input$Commodity))})
  data6 <- reactive({dataModeled %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                              (geographicaream49 %in% input$Country6) & 
                                              (measureditemcpc %in% input$Commodity))})
  output$plotgraph  <- renderPlot({
  pt1 <-  ggplot(data(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
    geom_point() +
    geom_line()+
    xlab('timePointYears') + ylab('Loss (%)') +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=12,face="bold")) +
    scale_y_continuous(labels = percent)+
    ggtitle(paste("Country", unique(data()$geographicaream49), sep= " "))
    

  pt2 <- ggplot(data2(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
     ggtitle(paste("Country", unique(data2()$geographicaream49), sep= " "))

  pt3 <- ggplot(data3(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Country", unique(data3()$geographicaream49), sep= " "))

  pt4 <- ggplot(data4(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Country", unique(data4()$geographicaream49), sep= " "))
 
  pt5 <- ggplot(data5(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Country", unique(data5()$geographicaream49), sep= " "))

  pt6 <- ggplot(data6(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Country", unique(data6()$geographicaream49), sep= " "))

    #wtlist <- c(data(),data2(),data3(),data4(),data5(),data6())
    # ptlist <- list(pt1(),pt2(),pt3(),pt4(),pt5(),pt6())
    #to_delete <- !sapply(ptlist,is.null)
    #ptlist <- ptlist[to_delete] 
    #wtlist <- wtlist[to_delete]
    #if (length(ptlist)==0) return(NULL)
    #grid.arrange(grobs=ptlist,widths=wtlist,ncol=length(ptlist))
    grid.arrange(pt1,pt2, pt3, pt4,pt5, pt6, nrow=2)
    

     
     
  })
  
  # output$line2 <- renderPlot({
  #   ggplot(data2(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) +
  #     geom_point() +
  #     geom_line()+
  #     xlab('timePointYears') + ylab('Loss (%)') +
  #     theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  #     theme(axis.text=element_text(size=12, face="bold"),
  #           axis.title=element_text(size=12,face="bold")) +
  #     scale_y_continuous(labels = percent)
  #})
  
  output$res <- renderPlot({
    ggplot(dataRes(), aes(x= timepointyears , y = mod2res)) +
    geom_point()
    
  })
  
  output$raw <- renderPlot({
    ggplot(dataR(), aes(x = timepointyears, y = loss_per_clean/100, color = fsc_location)) + 
      geom_point() +
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Country", unique(dataR()$geographicaream49), sep= " "))
    
    
  })
  
  output$stats <- renderPrint({
    title= ("The average for the commodity for all countries")
    ddply(dataR(),~timepointyears,summarise,
          mean=round(mean(loss_per_clean/100),3),
          min=round(min(loss_per_clean/100),3), 
          max=round(max(loss_per_clean/100),3),
          sd=round(sd(loss_per_clean/100),3))
  })
  
  
  output$rawC <- renderPlot({
    ggplot(dataRC(), aes(x = timepointyears, y = loss_per_clean/100, color = fsc_location)) + 
      geom_point() +
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) +
      scale_y_continuous(labels = percent)+
      ggtitle(paste("Commodity", unique(dataRC()$measureditemcpc), sep= " - "))
    
    
  })
  
  output$statsC <- renderPrint({
    title= ("The average for the commodity for all countries")
    ddply(dataRC(),~timepointyears,summarise,
          mean=round(mean(loss_per_clean/100),3),
          min=round(min(loss_per_clean/100),3), 
          max=round(max(loss_per_clean/100),3),
          sd=round(sd(loss_per_clean/100),3))
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

}
 
