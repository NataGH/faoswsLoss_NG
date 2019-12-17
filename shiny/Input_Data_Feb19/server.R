#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
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
library(plotly)
library(rdrop2)
library(xlsx)

#load("~/faoswsLossa/shiny/Soup2Nuts/data_InternalFAO_jan.RData")
# token <- readRDS("token.rds")
# # Then pass the token to each drop_ function
# drop_acc(dtoken = token)
# drop_download("data_InternalFAO_jan.RData" , path = "shiny", overwrite = TRUE)
load("Data_In.RData")

shinyServer(function(input, output, session) {
  ##### Aggregation Options ####
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
      Agg_choices <- c("All",unique(CountryGroup[[aggregationNameC]]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if(length(grep("sdg+", input$aggregation  ))>0){
      Agg_choices <- c("All",unique(CountryGroup[["sdg_regions"]]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if(input$aggregation == "gfli_basket") {
      Agg_choices <- c("All",unique(na.omit(gfli_basket[,gfli_basket])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    
  })
  observe({
    if (input$aggregation == "WORLD") {
      ctry_choices <- c("All")
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "gfli_basket") {
      ctry_choices <-  c("All",unlist(na.omit(unique(gfli_basket[gfli_basket %in% input$Agg_options,measureditemcpc]))))
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
    }  
    if (input$aggregation  %in% "sdgregion_code"){
      if(input$Agg_options == 'All'){
        ctry_choices <- c("All",c(unique(CountryGroup[geographicaream49 %in% Base_Prod$geographicaream49,"country",with=F])))
      }else{
        ctry_choices <- c("All",c(unique(CountryGroup[(CountryGroup[['sdg_regions']]   %in% input$Agg_options),"country",with=F])))
      }
      updateSelectInput(session, "Country", choices=ctry_choices, selected = "All")
    } 
    if (input$aggregation == "m49_code") {
      ctry_choices <- c("All",unlist(CountryGroup[["m49_region"]]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="Italy")
    }
    # else{
    #   aggregationNameB = sub("code", "region", input$aggregation )
    #   ctry_choices <- c("All",unique(CountryGroup[CountryGroup[[aggregationNameB]] ==input$Agg_options,"country",with=F]))
    #   updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    # } 
    # 
    
  })
  
  observe({
    if (input$aggregation == "m49_code") {
      mod_choices <- c("Model Estimates", "Input Data")
      updateSelectInput(session, "Model_Level", choices=mod_choices, selected ="Input Data")
    }else{
      mod_choices <- c( "Input Data")
      updateSelectInput(session, "Model_Level", choices=mod_choices, selected ="Input Data")
    }
  })
  
  observe({
    Comm_grp <- FAOCrops[["cpc"]]
    if(!"All" %in% input$Country){
      Comm_grp <-   Comm_grp[Comm_grp %in% unlist(
        production[geographicaream49 %in% unlist(
          CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]),'measureditemcpc',with=F])]
    }
    if(input$BasketItems != "All"){
      Comm_grp <- Comm_grp[Comm_grp %in% unlist(
        gfli_basket[gfli_basket %in% input$BasketItems, "measureditemcpc", with=F])]
    }
    if(input$checkbox ==TRUE){
      Comm_grp <- Comm_grp[Comm_grp %in%  Basket()[
        (geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]))
        ,"measureditemcpc",with=F]]
    }   
    
    itemcpc_choices <- c("All",unique(FAOCrops[cpc %in% unique(Comm_grp), "crop", with=F]))
    updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
  })
  
  crops <- reactive({
    if(input$aggregation == "m49_code"){
      Comm_grp <- production[geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]),'measureditemcpc',with=F]
      if(input$BasketItems != "All"){
        Comm_grp <- Comm_grp %>% filter(measureditemcpc %in% unlist(
          gfli_basket[gfli_basket%in% input$BasketItems, "measureditemcpc", with=F]))
      }
      if(input$checkbox ==TRUE){
        Comm_grp <- Comm_grp %>% filter(measureditemcpc %in%  unlist(Basket()[
          (geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]))
          ,"measureditemcpc",with=F]))
      }
      if(input$itemcpc != "All"){
        Comm_grp <- Comm_grp %>% filter(measureditemcpc %in%  unlist(
          FAOCrops[crop %in% input$itemcpc[input$itemcpc != "All"], "measureditemcpc", with=F]
        ))
      }
      
    }
    else{
      Comm_grp <- FAOCrops[["cpc"]]
    }
    unique(unlist(Comm_grp))
  })
  countries <- reactive({
      if (input$aggregation == "WORLD") {
        ctry_choices <- c(unlist(CountryGroup[["m49_region"]]))
      }
      if (input$aggregation == "gfli_basket") {
        ctry_choices <-  c(unlist(na.omit(unique(gfli_basket[gfli_basket %in% input$Agg_options,measureditemcpc]))))
      }
      if (!input$aggregation  %in% c("sdgregion_code", "gfli_basket")) {
        if(input$Agg_options == 'All'){
          ctry_choices <- c(unique(CountryGroup[geographicaream49 %in% Base_Prod$geographicaream49,"country",with=F]))
        }else{
          aggregationNameB = sub("code", "region", input$aggregation )
          ctry_choices <- c(unique(CountryGroup[(CountryGroup[[aggregationNameB]]   %in% input$Agg_options)&(geographicaream49 %in% Base_Prod$geographicaream49),"country",with=F]))
        }
      }  
      if (input$aggregation  %in% "sdgregion_code"){
        if(input$Agg_options == 'All'){
          ctry_choices <- c(unique(CountryGroup[geographicaream49 %in% Base_Prod$geographicaream49,"country",with=F]))
        }else{
          ctry_choices <- c(unique(CountryGroup[(CountryGroup[['sdg_regions']]   %in% input$Agg_options),"country",with=F]))
        }
      } 
      if (input$aggregation == "m49_code") {
        ctry_choices <- input$Country
      }
      # else{
      #   aggregationNameB = sub("code", "region", input$aggregation )
      #   ctry_choices <- c("All",unique(CountryGroup[CountryGroup[[aggregationNameB]] ==input$Agg_options,"country",with=F]))
      #   updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
      # } 
      # 
    ctry_choices 
      })

  #### SDG ### 
  WeightKeys <- reactive({
    if(input$WeightsChoice == "International Dollar Prices (2015)"){
      Weights_keys <- c("measureditemcpc")
    }
    if(input$WeightsChoice == "Calories"){
      Weights_keys <- c("geographicaream49","measureditemcpc")
    }
    Weights_keys 
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
  
  #---end of  Aggregation Options ---#
  ###### SDG data #####
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
    names(Losses)[names(Losses) == "value"] <- "value_measuredelement_5126"
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
    basket[,cpc49 := paste(geographicaream49, measureditemcpc,sep=";") ]
    basket
  })
  
  #--- End SDG data ----#
  ##################Data setting############################
  
  
  ##### ---- Model Estimate Dataset ---- ####
  dataR_loss <- reactive({
    if((input$aggregation == "m49_code") ) {
      if("All" %in% input$Country ){
        Losses_Out%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) 
        )%>% arrange(geographicaream49,-timepointyears)
      }
      if((!"All" %in% input$Country)){
        Losses_Out%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                               (geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F])) &
                               (measureditemcpc %in% crops())
        )%>%
          arrange(-timepointyears)
      }
    }
  })
  
  dataR_maggs <- reactive({
    if("All" %in% input$Country ){
      M_aggregates%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) 
      )%>% arrange(geographicaream49,-timepointyears)
    }
    if((!"All" %in% input$Country)){
      M_aggregates%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                               (geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F])) &
                               (measureditemcpc %in% crops())
      )%>%
        arrange(-timepointyears)
    }
  })
  
  ##### ---- Input Dataset ---- ####
  dataR_Input <- reactive({
    IO <- InputData_Out%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) 
       )%>% arrange(geographicaream49,-timepointyears)
    if(!"WORLD" %in% input$aggregation){
      IO <- IO %>% filter((geographicaream49 %in% unlist(CountryGroup[country %in% unlist(countries()),"m49_code", with=F]) ) &
                            (measureditemcpc %in% crops()))
     }
    if((!"All" %in% input$Country)){
      IO <- IO %>% filter((geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F])) &
                                      (measureditemcpc %in% crops())
                                    
      )%>%
        arrange(-timepointyears)
    }
    if((!"All" %in% input$BasketItems)){
      IO <- IO %>% filter( (gfli_basket %in% input$BasketItems[!input$BasketItems %in% "All"]))
    }
    if((!"All" %in% input$Stage)){
      IO <- IO %>% filter( (fsc_location1 %in% input$Stage[!input$Stage %in% "All"]))
    }
    if((!"All" %in% input$DataCollect)){
      IO <- IO %>% filter( (tag_datacollection %in% input$DataCollect[!input$DataCollect %in% "All"]))
    }
    if(input$checkboxofficial){
      IO <- IO %>% filter(protected == TRUE)
    }
    if(input$checkbox){
      IO[, cpc49 := paste(geographicaream49, measureditemcpc,sep=";")]
      IO <- IO %>% filter(cpc49 %in%  Basket()$cpc49)
    }
    IO
    #}
  })
  
  dataR_Markov <- reactive({
    #if((input$aggregation == "m49_code") ) {
    if("All" %in% input$Country ){
      IO_M <- M_aggregates%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) 
      )%>% arrange(geographicaream49,-timepointyears)
    }
    if((!"All" %in% input$Country)){
      IO_M <- M_aggregates%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                       (geographicaream49 %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F])) &
                                       (measureditemcpc %in% crops())
                                     
      )%>%
        arrange(-timepointyears)
    }
    if((!"All" %in% input$Stage)){
      IO_M <-   IO_M %>% filter( (fsc_location1 %in% input$Stage[!input$Stage %in% "All"]))
    }
    if((!"All" %in% input$DataCollect)){
      IO_M <-   IO_M %>% filter( (tag_datacollection %in% input$DataCollect[!input$DataCollect %in% "All"]))
    }
    IO_M
    #}
  })
  
  ##### ---- Plots  ---- ####
  flags <-list(
    list(target = "I;e", value = list(marker =list(color = 'orange'))),
    list(target = "T;-", value = list(marker =list(color = 'blue'))),
    list(target = "E;f", value = list(marker =list(color = 'blue'))),
    list(target = "M;-", value = list(marker =list(color = 'red'))),    
    list(target = ";-", value = list(marker =list(color = 'blue'))),  
    list(target = ";q", value = list(marker =list(color = 'blue'))),  
    list(target = "T;p", value = list(marker =list(color = 'blue'))),  
    list(target = ";p", value = list(marker =list(color = 'blue')))  
  )
  colorlist = c("Crimson","LightSeaGreen","SteelBlue",
                "MediumOrchid","OrangeRed","DarkGreen"	,
                "Tan","RosyBrown","SandyBrown",
                "Goldenrod","DarkGoldenrod",
                "Peru","Chocolate","SaddleBrown",
                "Sienna","Brown","Maroon")
  
  l1 <- unique(InputData_Out$fsc_location1)
  l2 <- unique(InputData_Out$tag_datacollection)
  tags  <- list()
  for( nn in 1:length(l2)){
    tags[[nn]] <-   list(target = l2[nn], value = list(marker =list(color =colorlist[nn])))  
  }
  
  

  
  plotly_Input2 = function(){
    #### Plots the loss estimates for different aggregations
    trace_namesa = unique(dataR_loss()[["geographicaream49"]])
    trace_namesb = unique(dataR_loss()[["measureditemcpc"]])
    
    trace1 = dataR_loss()[geographicaream49 %in% trace_namesa[1] & measureditemcpc %in% trace_namesb[1],]
    crop_name = unlist(FAOCrops[cpc ==  trace_namesb[1],"crop",with=F])
    ctry_name = unlist(CountryGroup[geographicaream49 == trace_namesa[1], "country",with=F ])
    hovertext = unlist(dataR_loss()[geographicaream49 %in% trace_namesa[1] & measureditemcpc %in% trace_namesb[1],"flagcombo",with=F])
    
    p <-plot_ly(y=trace1$value_measuredelement_5126, x= trace1$timepointyears  ,text=hovertext, showlegend = TRUE, type="scatter", mode="lines",  name = paste(ctry_name,crop_name,sep=" - "))
    if(input$checkboxflags){
      p <- p   %>% add_trace(y=trace1$value_measuredelement_5126, x= trace1$timepointyears,text=hovertext, showlegend =FALSE, type="scatter",  mode="markers", text = trace1$flagcombo,
                             transforms =list(
                               list(
                                 type = 'groupby',
                                 groups = trace1$flagcombo,
                                 styles =  flags
                               )),
                             name = paste(ctry_name,crop_name,sep=" - ") )
    }
    if(length(trace_namesa)>1){
      ## Multiple countries ###
      for (nm in 1:length(trace_namesa)){
        for (nmt in 2:length(trace_namesb)){
          trace2 = dataR_loss()[geographicaream49 %in% trace_namesa[nm] & measureditemcpc %in% trace_namesb[nmt],]
          crop_name = unlist(FAOCrops[cpc ==  trace_namesb[nmt],"crop",with=F])
          ctry_name = unlist(CountryGroup[geographicaream49 == trace_namesa[nm], "country",with=F ])
          hovertext =  unlist(dataR_loss()[geographicaream49 %in% trace_namesa[nm] & measureditemcpc %in% trace_namesb[nmt],"flagcombo",with=F])
          
          p <-p%>%  add_trace(y=trace2$value_measuredelement_5126, x= trace2$timepointyears , showlegend = TRUE, type="scatter", mode="lines",text=hovertext,name =  paste(ctry_name,crop_name,sep=" - "))
          if(input$checkboxflags){
            p <- p   %>% add_trace(y=trace2$value_measuredelement_5126, x= trace2$timepointyears,text=hovertext, showlegend = FALSE, type="scatter",  mode="markers", text = trace2$flagcombo,
                                   transforms =list(
                                     list(
                                       type = 'groupby',
                                       groups = trace2$flagcombo,
                                       styles =  flags
                                     )),
                                   name = paste(ctry_name,crop_name,sep=" - ") )
          }
        }}
    }else{
      for (nmt in 2:length(trace_namesb)){
        trace2 = dataR_loss()[geographicaream49 %in% trace_namesa[1] & measureditemcpc %in% trace_namesb[nmt],]
        crop_name = unlist(FAOCrops[cpc ==  trace_namesb[nmt],"crop",with=F])
        ctry_name = unlist(CountryGroup[geographicaream49 == trace_namesa[1], "country",with=F ])
        hovertext = unlist(dataR_loss()[geographicaream49 %in% trace_namesa[1] & measureditemcpc %in% trace_namesb[nmt],"flagcombo",with=F])
        
        
        p <-p%>%  add_trace(y=trace2$value_measuredelement_5126, x= trace2$timepointyears , showlegend = TRUE , type="scatter", mode="markers+lines",text=hovertext,name =  paste(ctry_name,crop_name,sep=" - "))
        if(input$checkboxflags){
          p <- p   %>% add_trace(y=trace2$value_measuredelement_5126, x= trace2$timepointyears,  type="scatter", showlegend = FALSE, mode="markers",  
                                 transforms =list(
                                   list(
                                     type = 'groupby',
                                     groups = trace2$flagcombo,
                                     styles =  flags
                                   )),
                                 name = paste(ctry_name,crop_name,sep=" - ") )
        }
        
      }
    }
    if(input$checkbox_input){
      if(nrow(InputData_Out)>0){
        trace3 <-   dataR_Input()
        p <- p   %>% add_trace(y= trace3$loss_per_clean/100, x= trace3$timepointyears, type="scatter", showlegend = FALSE, mode="markers",
                               text =  ~paste('Crop: ', trace3$crop,
                                              '</br> Location: ',  trace3$fsc_location1,
                                              '</br> Method of Data Collection: ', trace3$method_datacollection,
                                              '</br> Data Tag: ', trace3$tag_datacollection,
                                              '</br> Reference: ', trace3$ reference
                               ),
                               transforms =list(
                                 list(
                                   type = 'groupby',
                                   groups = trace3$tag_datacollection,
                                   styles =   tags
                                 )),
                               name = paste(ctry_name,crop_name,sep=" - ") )
        
      }
    }
    
    p
  }
  plotly_Input3 = function(){
    #### Plots the loss estimates for different aggregations
    if(nrow(InputData_Out)>0){
      trace3 <-   dataR_Input()
      p <-plot_ly(trace3 )
      p <- p   %>% add_trace(y= trace3$loss_per_clean, x= trace3$timepointyears, type="scatter", showlegend = TRUE,  mode="markers",
                             text =  ~paste('Crop: ', trace3$crop,
                                            '</br> Country: ',  trace3$country,
                                            '</br> Location: ',  trace3$fsc_location1,
                                            '</br> Method of Data Collection: ', trace3$method_datacollection,
                                            '</br> Data Tag: ', trace3$tag_datacollection,
                                            '</br> Reference: ', trace3$ reference
                             ),
                             transforms =list(
                               list(
                                 type = 'groupby',
                                 groups = trace3$tag_datacollection,
                                 styles =   tags
                                 
                               )),
                             legendgroup = trace3$tag_datacollection)
      
    }
    if(input$checkboxflags){
      trace4 <- dataR_loss()
      p <- p   %>% add_trace(y= trace4$value_measuredelement_5126, x= trace4$timepointyears, type="scatter", showlegend =  T, mode="markers",
                             text =  ~paste('Crop: ', trace4$crop,
                                            '</br> Country: ',  trace4$country,
                                            '</br> Flag: ', trace4$flagcombo
                                            
                             ),              
                             transforms =list(
                               list(
                                 type = 'groupby',
                                 groups = trace4$flagcombo,
                                 styles =  flags
                               )))
    }
    
    
    
    p
  }
  ################  Heat Map #######################
  DataAgg2 = function(){
    if("WORLD" %in% input$aggregation){
      hm <-  dataR_Input() %>% 
        group_by_(.dots = list("gfli_basket", "measureditemcpc","country")) %>%
        dplyr:: summarise(Number_Obs = n()) %>%
        arrange(- gfli_basket)
    }
    if(!"WORLD" %in% input$aggregation){
      hm <- dataR_Input() %>% 
        group_by_(.dots = list("gfli_basket",input$aggregation, "measureditemcpc","country")) %>%
        dplyr:: summarise(Number_Obs = n()) %>%
        arrange(- gfli_basket)
    }

    countT <- as.data.table(hm)
    countT <- merge(countT,FAOCrops[,c("measureditemcpc", "crop"),with=F], by= c("measureditemcpc"), all.x= T)
    countT
  }
  output$plot_ly_heat  <- renderPlotly({
    trace5 <- DataAgg2()
    p <-plot_ly(trace5, y=~crop, x= ~country, z= ~Number_Obs, type="heatmap",
                text =  ~paste('</br> Crop: ', trace5$crop,
                               '</br> Country: ',  trace5$country,
                               '</br> Number of Obs: ',  trace5$Number_Obs
                               
                ),
                hoverinfo = 'text'
    )
    
    p%>%layout(
      xaxis = list(
        title = "Geographic Area",
        titlefont = f,
        dtick = 1
        
      ),
      yaxis = list(
        title = "Commodity",
        tickformat = ".2%",
        titlefont = f
      ),
      margin = list(l =120, r = 50, t = 20, b =150),
      annotations = list(text = lab(),
                         font = list(size = 12),
                         showarrow = FALSE,
                         
                         align='right',
                         xref = 'paper', x = 0.98,
                         yref = 'paper', y = -2),
      autosize = TRUE,
      legend = list(orientation = 'h'),
      height = 800
    )
  })
  #---- end Graphs ------- ##
  lab <- reactive({
    paste("Source: FAO",
          paste("Date: ",as.character(Sys.time()),sep=""),
          paste("Aggregation: ",input$aggregation,sep=""),
          paste("Aggregation Option: ",input$Agg_options,sep=""),
          paste("Country : ",input$Country,sep=""),
          paste("Commodity Aggregation: ",input$BasketItems,sep="")
          ,sep="\n")
  })
  MetaDataOut <- reactive({
    meta <- as.data.table(rbind("Source: FAO",
                                paste("Date: ",as.character(Sys.time()),sep=""),
                                paste("Weight: ",input$WeightsChoice,sep=""),
                                paste("Aggregation: ",input$aggregation,sep=""),
                                paste("Aggregation Option: ",input$Agg_options,sep=""),
                                paste("Country : ",input$Country,sep=""),
                                paste("Commodity Aggregation: ",input$BasketItems,sep="")
    ))
    meta
  })
  
  Bmarg = -.8
  output$plot_ly  <- renderPlotly({
    if(input$Model_Level ==  "Model Estimates"){
      plotly_Input2()%>%layout(
        xaxis = list(
          title = "Years",
          titlefont = f,
          range = c(seq(input$Year[1],input$Year[2], by=1)),
          dtick = 1
          
        ),
        yaxis = list(
          title = paste0(c(rep("&nbsp;", 20),
                           "Percentage",
                           rep("&nbsp;", 20),
                           rep("\n&nbsp;", 3)),
                         collapse = ""),
          tickformat = ".2%",
          titlefont = f
        ),
        margin = list(l =80, r = 50, t = 20, b = 80),
        annotations = list(text = lab(),
                           font = list(size = 12),
                           showarrow = FALSE,
                           
                           align='right',
                           xref = 'paper', x = 0.98,
                           yref = 'paper', y = Bmarg),
        autosize = TRUE,
        legend = list(orientation = 'l')
      )
    }
    else if(input$Model_Level == "Input Data"){
      plotly_Input3()%>%layout(
        xaxis = list(
          title = "Years",
          titlefont = f,
          range = c(seq(input$Year[1],input$Year[2], by=1)),
          dtick = 1
          
        ),
        yaxis = list(
          title = paste0(c(rep("&nbsp;", 20),
                           "Percentage",
                           rep("&nbsp;", 20),
                           rep("\n&nbsp;", 3)),
                         collapse = ""),
          #tickformat = ".2%",
          titlefont = f
        ),
        margin = list(l =80, r = 50, t = 20, b = 200),
        annotations = list(text = lab(),
                           font = list(size = 12),
                           showarrow = FALSE,
                           
                           align='right',
                           xref = 'paper', x = 0.6,
                           yref = 'paper', y = Bmarg),
        autosize = TRUE,
        legend = list(legend = list(x = 100, y = 0.5)),
        height = 800
      )
    }
    else{
      plot_ly(y=0, x=0)
    }
    
    
    
  })

  ##########Other Tabs#################
  WeightAgg = function() {
    Wt = merge(Weights(),gfli_basket[,c("measureditemcpc" , "gfli_basket"),with=F], by = ("measureditemcpc"), all.x =T, all.y=F)
    if(input$BasketItems != 'All'){
      Wt %>% filter(gfli_basket %in% input$BasketItems)
    }else{Wt}
  }

  DataAgg = function(){
    countT <-  dataR_maggs() %>%
      group_by_(.dots = list("measureditemcpc")) %>%
      dplyr:: summarise(Number_Obs = n())
    countT <- as.data.table(countT)
    countT 
    
  }
  DataINOutput <- function()({
    a <- dataR_Input()
    # setnames(a, old = c("geographicaream49","isocode","country","region","measureditemcpc","crop","timepointyears","loss_per_clean","percentage_loss_of_quantity",
    #                                 "loss_quantity","loss_qualitiative","loss_monetary",
    #                                 "activity","fsc_location","periodofstorage","treatment","causeofloss","samplesize",
    #                                 "units","method_datacollection","tag_datacollection","reference","url"),
    #          new = c("geographicaream49","isocode","Country","Region","measureditemcpc","Crop","Year","loss_per_clean","Range of Quantity Loss (%)",
    #                  "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)",
    #                  "Activity","Stage","period of storage","treatment","Causes of loss","Sample Size",
    #                  "Sampling Units","Method of Data Collection","Data Collection Tag","Reference","Url"))
    # 
    # a[ ,c("geographicaream49","Country","Region","measureditemcpc","Crop","Year","Stage","Average Quantity Loss (%)",
    #       "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)","Causes of loss","Activity",         
    #       "period of storage","treatment","Sampling Units","Method of Data Collection","Data Collection Tag","Reference","Url"),with=FALSE]
    # 
    a[, c("geographicaream49","country","region","measureditemcpc","crop","timepointyears","loss_per_clean","percentage_loss_of_quantity",
          "loss_quantity","loss_qualitiative","loss_monetary",
          "activity","fsc_location","periodofstorage","treatment","causeofloss","samplesize",
          "units","method_datacollection","tag_datacollection","reference","url"),with=F]
  })
  
  
  output$DataTab = DT::renderDataTable({
    DataINOutput()
    
  })
  
  output$WeightsOut = DT::renderDataTable({
    WeightAgg()
  })
  output$BasketsOut = DT::renderDataTable({
    BasketAgg()
  })
  output$IndexOut = DT::renderDataTable({
    IndexAgg()
  })
  output$Maggregrates = DT::renderDataTable({
    DataAgg()
  })
  ################Output#################
  

  
  
  
  ################################
  obsB <- observe({
    
   # print(input$aggregation)
   # print(input$Agg_options)
   # print(unlist(countries()))
    #print( Base_Prod)
   # print(  DataAgg2())
    print("complete")
    
  })
  
})
