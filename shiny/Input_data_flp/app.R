#' Part of the FAO Loss Module
#' 
#' @author Alicia English 
#' 
#' 
#' 
# ---
#   runtime: shiny
# output: html_document
# ---



library(shiny)
library(shinythemes)
library(rmarkdown)
library(ggplot2)
library(plyr)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)
library(yaml)

load("~/faoswsLossa/shiny/Input_data_flp/Input_data_FLP.RData")

# # #----  Data In ------------------------------------------
#GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"WORLD",weights,basketn,FLIData)

ui <- fluidPage(theme = shinytheme("lumen"),
                sidebarLayout(
                  sidebarPanel(
                    #Input()
                    sliderInput(
                      inputId = "Year",
                      label = "Year Range",
                      value = c(2005,maxYear),step =1,sep = "", min = as.integer(min(selectedYear)), max =  as.integer(max(selectedYear))
                    ),
                    selectInput(
                      inputId = "aggregation",
                      label = "Aggregation",
                      choices = c("WORLD",opt2),
                      selected = "All"
                    ),
                    selectInput(
                      inputId = "Agg_options",
                      label = "Aggregation Options",
                      choices = NULL,
                      selected = NULL
                    ),
                    selectInput(
                      inputId = "Country",
                      label = "Country",
                      choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
                    ),
                    selectInput(
                      inputId = "WeightsChoice",
                      label = "Weights",
                      choices = c("International Dollar Prices (2015)","Calories"), 
                      selected ="International Dollar Prices (2015)"
                    ),
                    selectInput(
                      inputId = "BasketChoice",
                      label = "Commoditiy Aggregation",
                      choices = c('Production Value- Top 10 by country (Default SDG)','Production Value- Top 10 by World'), 
                      selected ='Production Value- Top 10 by country' 
                    ),
                    selectInput(
                      inputId = "BasketItems",
                      label = "Basket Items",
                      choices = c("All",na.omit(unique(gfli_basket[,"gfli_basket",with=FALSE]))),
                      selected = "All"
                    ),
                    # selectInput(
                    #   inputId = "itemcpc",
                    #   label = "measureditemcpc",
                    #   choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
                    # ),
                    selectInput("dataset", "Choose a dataset for download:",
                                choices = c("Index", "Weights", "Basket")),
                    
                    downloadButton("Data.csv", "Download"),
                    downloadButton("SDG12_3_Plot.jpeg", "Plots")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Graph",
                               tags$p(""),
                               tags$p("Food Loss Index"),
                               plotOutput("plotgraph", height = "900px")
                               #plotlyOutput("plotgraph")
                      ),
                      tabPanel("Index",
                               DT::dataTableOutput("IndexOut")
                      ),
                      tabPanel("Weights",
                               DT::dataTableOutput("WeightsOut")
                      ),
                      tabPanel("Basket",
                               DT::dataTableOutput("BasketsOut")
                      )
                    ))
                ))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  set.seed(122)
  observe({
    if(input$aggregation == "WORLD") {
      Agg_choices <- c("All")
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
    if(input$aggregation == "m49_code") {
      Agg_choices <- c("All")
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if(!input$aggregation  %in% c("sdgregion_code", "gfli_basket","m49_code")){
      aggregationNameC = sub("code", "region", input$aggregation  )
      Agg_choices <- c(unique(CountryGroup[[aggregationNameC]]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected =NULL)
    }
    if(length(grep("sdg+", input$aggregation  ))>0){
      Agg_choices <- c(unique(CountryGroup[["sdg_regions"]]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected =NULL)
    }
    if(input$aggregation == "gfli_basket") {
      Agg_choices <- c(unique(na.omit(gfli_basket[,gfli_basket])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected =NULL)
    }
    
  })
  observe({
    if (input$aggregation == "WORLD") {
      ctry_choices <- c("All")
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "gfli_basket") {
      ctry_choices <-  c("All",unlist(na.omit(unique(gfli_basket[,measureditemcpc]))))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (!input$aggregation  %in% c("sdgregion_code", "gfli_basket")) {
      if(input$Agg_options == 'All'){
        ctry_choices <- c("All",c(unique(CountryGroup[geographicaream49 %in% Base_Prod$geographicaream49,"country",with=F])))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "All")
      }else{
        aggregationNameB = sub("code", "region", input$aggregation )
        ctry_choices <- c("All",c(unique(CountryGroup[(CountryGroup[[aggregationNameB]]   %in% input$Agg_options)&(geographicaream49 %in% Base_Prod$geographicaream49),"country",with=F])))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "All")
      }
    }else{
      aggregationNameB = sub("code", "region", input$aggregation )
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[[aggregationNameB]] ==input$Agg_options,"country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    } 
    
    
  })
  
  # observe({
  #   # if (input$BasketItems == "All") {
  #   #   itemcpc_choices <- c("All", unique(FAOCrops[,'crop',with=F]))
  #   #   updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
  #   # }
  #   # if (input$BasketItems != "All") {
  #   #   Comm_grp <- unlist(gfli_basket[gfli_basket == input$BasketItems,measureditemcpc])
  #   #   itemcpc_choices <- c("All", unique(FAOCrops[measureditemcpc %in% Comm_grp,'crop',with=F]))
  #   #   updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
  #   # }
  #   itemcpc_choices <- c("All")
  #   updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
  # })
  WeightKeys <- reactive({
    if(input$WeightsChoice == "International Dollar Prices (2015)"){
      Weights_keys <- c("measureditemcpc")
    }
    if(input$WeightsChoice == "Calories"){
      Weights_keys <- c("geographicaream49","measureditemcpc")
    }
    Weights_keys 
  })
  
  Weights <- reactive({
    if(input$WeightsChoice == "International Dollar Prices (2015)"){
      
      pvail <- unique(intPrice$measureditemcpc)
      
      intPriceSelected <-
        intPrice %>%
        select(measureditemcpc,timepointyears ,crop, value) %>%
        filter(timepointyears  == as.numeric(BaseYear[2])-1)
      
      
      intPriceSelected$itemname <- tolower(intPriceSelected$crop)
      
      weights <- intPriceSelected[, c("itemname","measureditemcpc","value"),with=F]
      weights <- as.data.table(weights)
      weights[, weightname := input$WeightsChoice ]
      #distinct(intPriceSelected,measuredItemCPC)
    }
    if(input$WeightsChoice == "Calories"){
      Globalkcal1 <- nutrient_table %>% filter(measuredelement == 1001 &timepointyearssp==0)
      weights <- Globalkcal1[, c("geographicaream49","measureditemcpc","value"),with=F]
      weights$value <-weights$value*1/1000
      weights <- weights[!duplicated(weights)]
      weights <- as.data.table(weights)
      weights[, weightname := input$WeightsChoice ]
      
    }
    weights
    
    
  })
  
  
  DataForIndex <- reactive({
    Base_Prod1 <- join(Base_Prod,Weights(), by= WeightKeys(),type= 'left')
    Base_Prod1$p0q0 =0
    Base_Prod1[, p0q0 := qty_avey1y2*value,]
    Base_Prod1 <-Base_Prod1 %>% filter(!is.na(p0q0))
    
    DataForIndex <- merge(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , Base_Prod1, by =c("measureditemcpc","geographicaream49"),all.x=T)
    DataForIndex$l0ptqt =0
    DataForIndex[,l0ptqt:=value_measuredelement_5126*p0q0,with=T]

    DataForIndex <-  merge(DataForIndex ,gfli_basket , by = c('measureditemcpc'), all.x = T)
    DataForIndex <-  merge(DataForIndex,CountryGroup, by = c('geographicaream49'), all.x = T)
    DataForIndex <- DataForIndex[!is.na(gfli_basket),]
    #DataForIndexD <- duplicated(DataForIndex)
    
  })
  
  Top10Global <- reactive({ 
    Top10Global<-  DataForIndex()  %>%
      filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
      group_by(measureditemcpc,gfli_basket) %>%
      dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE)) %>%
      arrange(-All_p0q0)
    Top10Global<-as.data.table(Top10Global)
  })
  
  
  basketKeys <- reactive({
    if(input$BasketChoice == "Production Value- Top 10 by country (Default SDG)"){
      basketKey <- c('geographicaream49', "measureditemcpc")
    }
    if(input$BasketChoice == "Production Value- Top 10 by World"){
      basketKey <- ( "measureditemcpc")
    }
    # if(input$BasketChoice == 'Top 10 Loss Commodities - by country'){
    #   basketKeys <- c('geographicaream49', "measureditemcpc")
    # }
    # if(input$BasketChoice == 'Caloric Value- Top 10 by World' & input$WeightsChoice == "calories"){
    #   basketKeys <- c('geographicaream49', "measureditemcpc")
    # }
    # 
    basketKey
  })
  
  Basket <- reactive({
    
    if(input$BasketChoice == 'Production Value- Top 10 by country (Default SDG)'){
      Top10perctry <- DataForIndex()  %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        arrange(geographicaream49, -p0q0)
      
      Top10_VP <- DataForIndex()  %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        group_by(geographicaream49) %>%
        dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
      
      basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
      basket <- basket %>% filter(!is.na(gfli_basket))
      
      basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
      basket[,Percent_prod := p0q0/All_p0q0]
      
      
    }
    if(input$BasketChoice == 'Production Value- Top 10 by World'){
      Top10Global<-   DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        group_by(measureditemcpc,gfli_basket) %>%
        dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE)) %>%
        arrange(-All_p0q0)
      
      Top10_VP <- DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        group_by(geographicaream49) %>%
        dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
      
      ItemsBasket <- Top10Global[ ,head(.SD, 2), by= c('gfli_basket')]
      basket <-   DataForIndex() %>% filter((timepointyears == as.numeric(BaseYear[2])-1) & (measureditemcpc %in%  unlist(ItemsBasket[!is.na(gfli_basket),measureditemcpc])))
      
      basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
      basket[,Percent_prod := p0q0/All_p0q0]
      
    }
    if(input$BasketChoice == 'Top 10 Loss Commodities - by country'){
      Top10perctry <- DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        arrange(geographicaream49, -value_measuredelement_5126)
      
      Top10_VP <- DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        group_by(geographicaream49) %>%
        dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE))
      
      basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
      basket[geographicaream49 == 100,]
      basket <- basket %>% filter(!is.na(gfli_basket))
      
      basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
      basket[,Percent_prod := p0q0/All_p0q0]
      
    }
    if(input$BasketChoice == 'Caloric Value- Top 10 by World' & input$WeightsChoice == "calories"){
      Top10perctry <- DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        arrange(-p0q0)
      
      basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
      basket <- basket %>% filter(!is.na(gfli_basket))
      
      basket <- as.data.table(merge(basket,Top10_VP, by =c("geographicaream49"), all.x=TRUE))
      basket[,Percent_prod := p0q0/All_p0q0]
      
    }
    
    basket[,basketname := input$BasketChoice ]
    basket <-basket[,unique(c("basketname", "country", basketKeys(), "gfli_basket", "value","weightname","p0q0",names(CountryGroup)[seq(2,length(names(CountryGroup)), by=2)][-1])),with=FALSE]
    
    basket
  })
  FLIndex <- reactive({
    FLIndex <- GFLI_SDG_fun(BaseYear,keys_lower,input$aggregation,Basket(),basketKeys(),DataForIndex())
    FLIndex[,c("Sum_p0qt","Sum_p0q0"):=NULL]
  })
  
  
  
  dataR <- reactive({
    if((input$aggregation == "m49_code")) {
      FLIndex()%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                            (region_code %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]))
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      FLIndex()[(timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                  (unlist(FLIndex()[, "region_name", with=F]) %in%  unlist(input$Agg_options))
                ,]
    }
    else{
      FLIndex()[timepointyears %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  
  lab <- reactive({
    paste("Source: FAO", 
          paste("Date: ",as.character(Sys.time()),sep=""),
          paste("Country Aggregation: ",input$aggregation,sep=""),
          paste("Commodity Aggregation: ",input$BasketItems,sep="")
          ,sep="\n")
  }) 
  
  plotInput = function() {
    if(input$aggregation != "m49_code"){
      ggplot(dataR(), aes(x = timepointyears, y = FLP)) +
        facet_wrap(~  unlist(unique(dataR()[,"region_name",with=F])))+
        geom_line()+
        xlab('timePointYears') + ylab('FLI') +
        scale_y_continuous(labels = scales::percent)+
        labs( caption=lab())+
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold"))
    }else{
      ggplot(dataR(), aes(x = timepointyears, y = FLP)) +
        facet_wrap(~ unlist(unique(input$Country)))+
        geom_line()+
        xlab('timePointYears') + ylab('FLI') +
        labs( caption=lab())+
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold"))
    }
    
    
    
  }
  
  WeightAgg = function() {
    Wt = merge(Weights(),gfli_basket[,c("measureditemcpc" , "gfli_basket"),with=F], by = ("measureditemcpc"), all.x =T, all.y=F)
    if(input$BasketItems != 'All'){
      Wt %>% filter(gfli_basket %in% input$BasketItems)
    }else{Wt}
  }
  BasketAgg = function() {
    if((input$Country != 'All')){
      Basket()%>% filter((geographicaream49 %in% as.numeric(unlist(CountryGroup[Country %in% input$Country,"geographicaream49",with=F]))))
    }
    else if(input$BasketItems != 'All'){
      Basket()%>% filter(gfli_basket %in% input$BasketItems)
    }
    else if((input$Country != 'All') & (input$BasketItems != 'All')){
      Basket()%>% filter((gfli_basket %in% input$BasketItems) & (geographicaream49 %in% as.numeric(unlist(CountryGroup[Country %in% input$Country,"geographicaream49",with=F]))))
    }
    else{Basket()}
    
  }
  IndexAgg = function(){
    dataR()
    
  }
  ### Output Dsiplays 
  output$plotgraph  <- renderPlot({
    plotInput()
  })
  # output$plotgraph  <- renderPlotly({
  #   plot_ly(dataR(), x = ~timepointyears, y = ~FLP, type = 'scatter', mode = 'lines')
  # })
  output$WeightsOut = DT::renderDataTable({
    WeightAgg()
  })
  output$BasketsOut = DT::renderDataTable({
    BasketAgg()
  })
  output$IndexOut = DT::renderDataTable({
    IndexAgg()
  })
  
  ### R Check##
  obsB <- observe({

    print(input$aggregation)
    print(input$Agg_options)
   # print( WeightKeys())
   # print( DataForIndex())
    print( Basket())



    
   
  })
  

}
# Create Shiny app ----
shinyApp(ui = ui, server = server)



