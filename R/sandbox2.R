observe({
  if (input$SDG_Reg != "") {
    ctry_choices <- as.list(CountryGroup[sdg_regions == input$SDG_Reg,"geographicaream49",with=F])
    names(ctry_choices) <- CountryGroup[sdg_regions == input$SDG_Reg,"countryname",with=F]
    
    updateSelectInput(session, "Country", choices=ctry_choices)
  }
})

tabsetPanel(
  tabPanel("Raw Data for the countries",  
           tags$p(""),
           tags$p("Raw input data for the commodity"),
           plotOutput("rawC"),
           tags$p(""),
           tags$p("Summary stats for the raw input data for the commodity"),
           verbatimTextOutput("statsC"))
  
)
)
selectInput(
  inputId = "Country",
  label = "Country",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="360",countryname]
),
selectInput(
  inputId = "Country2",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="392",countryname]
),
selectInput(
  inputId = "Country3",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="454",countryname]
),
selectInput(
  inputId = "Country4",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected =CountryGroup[geographicaream49 =="484",countryname]
),
selectInput(
  inputId = "Country5",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="686",countryname]
),
selectInput(
  inputId = "Country6",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="716",countryname]
),
selectInput(
  inputId = "Country7",
  label = "Country To Compare",
  choices = unique(CountryGroup$countryname),
  selected = CountryGroup[geographicaream49 =="1248",countryname]
)


data2 <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                        (geographicaream49 %in% input$Country2) & 
                                        (measureditemcpc %in% input$Commodity))})
data3 <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                        (geographicaream49 %in% input$Country3) & 
                                        (measureditemcpc %in% input$Commodity))})
data4 <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                        (geographicaream49 %in% input$Country4) & 
                                        (measureditemcpc %in% input$Commodity))})
data5 <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                        (geographicaream49 %in% input$Country5) & 
                                        (measureditemcpc %in% input$Commodity))})
data6 <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                        (geographicaream49 %in% input$Country6) & 
                                        (measureditemcpc %in% input$Commodity))})



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





data <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) & 
                                       (geographicaream49 %in%  CountryGroup[countryname ==input$Country,geographicaream49]) & 
                                       (measureditemcpc %in% input$Commodity))})

output$plotgraph  <- renderPlot({
  pt1 <-  ggplot(data(), aes(x = timepointyears, y = loss_per_clean, color = flagcombination)) + 
    geom_point() +
    geom_line()+
    xlab('timePointYears') + ylab('Loss (%)') +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=12,face="bold")) +
    #scale_y_continuous(labels = percent)+
    ggtitle(paste("Country", unique(data()$geographicaream49), sep= " "))
  
  
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




output$rawC <- renderPlot({
  ggplot(dataRC(), aes(x = timepointyears, y = loss_per_clean/100, color = fsc_location)) + 
    geom_point() +
    xlab('timePointYears') + ylab('Loss (%)') +
    theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
    theme(axis.text=element_text(size=12, face="bold"),
          axis.title=element_text(size=12,face="bold")) +
    #scale_y_continuous(labels = percent)+
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

tabPanel("Raw Data",  
         tags$p(""),
         tags$p("Raw input data for the commodity"),
         plotOutput("raw"),
         tags$p(""),
         tags$p("Summary stats for the raw input data for the commodity"),
         verbatimTextOutput("stats"))