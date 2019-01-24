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


library(faoswsLoss)
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


suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(lme4)
  library(data.table)
  library(magrittr)
  library(reshape2)
  library(plyr)
  library(dplyr)

})


############# Computation Parameters #####################################
savesws <- TRUE
LocalRun <- TRUE # For if you are running the model on a local environment and loading data tables from local fiiles

if(CheckDebug()){
  maxYear <- format(Sys.Date(), "%Y")
  selectedYear <- as.character(1991:maxYear)
  ReportingYear<-  as.character(c(2015))
  aggregation <-  "geographicaream49"
  weights <- "intl_prices"
  basketn <- "top2perhead_byCtry"
  ComparisonYear <- (c(2005,2016))
  gfli_Reporting <- TRUE
  gfli_compare <- TRUE
}

#####################

BaseYear = as.character(c(2014,2016)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)


##### Load Data ######
## These two tables are constantly needing to be merged - country groups and food groups
if(CheckDebug()){
  message("Not on server, so setting up environment...")
  USER <- if_else(.Platform$OS.type == "unix",
                  Sys.getenv('USER'),
                  Sys.getenv('USERNAME'))


  library(faoswsModules)
  settings <- ReadSettings(file = file.path(paste(getwd(),"sws.yml", sep='/')))
  SetClientFiles(settings[["certdir"]])


  GetTestEnvironment(
    baseUrl = settings[["server"]],
    token = settings[["token"]]
  )



}else if(CheckDebug() & LocalRun){
  #Load local last dataset
  load("InputData.RData")

}else{
  # Remove domain from username
  USER <- regmatches(
    swsContext.username,
    regexpr("(?<=/).+$", swsContext.username, perl = TRUE)
  )

  options(error = function(){
    dump.frames()

    filename <- file.path(Sys.getenv("R_SWS_SHARE_PATH"), USER, "PPR")

    dir.create(filename, showWarnings = FALSE, recursive = TRUE)

    save(last.dump, file = file.path(filename, "last.dump.RData"))
  })
}

#### Data In #####
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar,selectedYear) # Value_measuredElement_5510
imports <- getImportData(areaVar,itemVar,yearVar, selectedYear)
nutrient_table <- getNutritionData(areaVar,itemVar,yearVar,elementVar,selectedYear, protected = FALSE)

#fbsTree <- ReadDatatable("fbs_tree")
gfli_basket <- ReadDatatable("gfli_basket")

CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
intPrice <-  ReadDatatable("int_dollar_prices_all") #int_$_prices_2005

names(Losses) <- tolower(names(Losses))
names(production) <- tolower(names(production))
names(imports) <- tolower(names(imports))
names(nutrient_table) <- tolower(names(nutrient_table))

production$geographicaream49 <- as.character(production$geographicaream49)
Losses$geographicaream49 <- as.character(Losses$geographicaream49)
nutrient_table$geographicaream49 <- as.character(nutrient_table$geographicaream49)
production$timepointyears <- as.numeric(production$timepointyears)
imports$timepointyears<- as.numeric(imports$timepointyears)

prod_imports <- merge(production,imports, by= keys_lower, all.x = TRUE)
prod_imports[,prod_imports := rowSums(.SD, na.rm = TRUE), .SDcols=c("value.x","value.y")]

CountryGroup$geographicaream49 <- CountryGroup$m49_code
CountryGroup$country <- CountryGroup$m49_region

opt <- as.data.table(cbind( c("m49_code","iso2code","isocode","m49_region","sdgregion_code","sdg_regions","m49_level1_code",
                              "m49_level1_region","m49_level2_code","m49_level2_region","mdgregions_code","mdgregions_region","ldcs_code","ldcs_region",
                              "lldcssids_code","lldcssids_region","fao_region","fao_operational_agg", "worldbank_income2018_agg", "sofa_agg"),
                            c("m49_code","ISO2","ISO3","Country","sdgregion_code","SDG Regions","m49_level1_code",
                              "Geographic Regions(m49) Level1","m49_level2_region_code","Geographic Regions(m49) Level2","mdgregions_code","MDG Regions","ldcs_code","Least Developed Countries (LDC)",
                              "lldcssids_code","Land Locked Developing Countries (LLDC)","FAO Operational Region","FAO Operational Coverage", "World Bank Income Groups", "SOFA Aggregations")))
names(opt) <- c("code", "Aggregates")
opt2 <- c("m49_code","sdgregion_code","m49_level1_code",
          "m49_level2_code","mdgregions_code","ldcs_code",
          "lldcssids_code","fao_operational_agg", "worldbank_income2018_agg", "sofa_agg","gfli_basket")

FAOCrops$measureditemcpc <- FAOCrops$cpc
FAOCrops[, "crop" := FAOCrops$description]
#FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]
intPrice <- merge(intPrice, FAOCrops[,c("measureditemcpc","crop"),with=F], by= c("measureditemcpc"))

gfli_basket[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]

names(Losses)[names(Losses) =="value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measureditemsuafbs"] <- "measureditemcpc"
#intPrice <- merge(intPrice, FAOCrops[,c("measureditemcpc","crop"),with=F], by= c("measureditemcpc"))

# ProdQtySWS <- subset(prod_imports,
#                      select = c(keys_lower,"prod_imports")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])


ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- Base_Prod[timepointyears == as.numeric(BaseYear[2])-1,]
Base_Prod <- Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F]

## Remove working system packagesto upload to shinyapps ###########
if(shinyapps){
    rm( swsContext.computationParams,swsContext.datasets,swsContext.executionId,swsContext.token,swsContext.userEmail,
        swsContext.userId,swsContext.username,swsContext.baseRestUrl,settings,USER)
    
    
    detach("package:faoswLosss", unload=TRUE)
    detach("package:faosws", unload=TRUE)
    detach("package:faoswsUtil", unload=TRUE)
    detach("package:faoswsModules", unload=TRUE)
    
    ###
    load(file.path(paste(getwd(),"shiny", "Input_Data_flp", "Input_data_FLP.RData", sep='/')))
    settings <- yaml.load_file(file.path(paste(getwd(),"shiny", "Shiny.yml", sep='/')))
    rsconnect::setAccountInfo(name = settings$shinyio$name, token= settings$shinyio$token, secret=settings$shinyio$secret)
    rsconnect::deployApp(file.path(paste(getwd(),"shiny/Input_Data_flp", sep='/')))
    ## Remove working system packagesto upload to shinyapps ###########
}

f <- list(
  family = "Times New Roman",
  size = 12,
  face="bold",
  color = "#7f7f7f"
)



# # #----  Data In ------------------------------------------
#GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"WORLD",weights,basketn,FLIData)
##### UI #######
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
                      selected = NULL,
                      multiple = FALSE,
                      selectize=TRUE
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
                    selectInput(
                      inputId = "Model_Level",
                      label = "Model level",
                      choices = c("SDG", "Model Estimates", "Input Data"),
                      selected = "SDG",
                      multiple = TRUE,
                      selectize=TRUE
                    ),
                    selectInput("dataset", "Choose a dataset for download:",
                                choices = c("Index", "Weights", "Basket")),
                    
                    downloadButton("Data.csv", "Download"),
                    downloadButton("SDG12_3_Plot.jpeg", "Plots")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("plotly_test",
                               plotlyOutput("plot_ly")
                      )
                    ))
                ))

# Define server logic to summarize and view selected dataset ----
##### Server #######
server <- function(input, output, session) {
  set.seed(122)
  
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
  
  #### end of  Aggregation Options ####
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
    
    basket
  })
  FLIndex <- reactive({
    FLIndex <- GFLI_SDG_fun(BaseYear,keys_lower,input$aggregation,Basket(),basketKeys(),DataForIndex())
    FLIndex[,c("Sum_p0qt","Sum_p0q0"):=NULL]
  })
  
  
  
  dataR <- reactive({
    if((input$aggregation == "m49_code")) {
      FLIndex()%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                            (region_code %in% unlist(CountryGroup[country %in% unlist(input$Country),"m49_code", with=F]))%>%
                            arrange(-timepointyears)
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      FLIndex()[(timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                  (unlist(FLIndex()[, "region_name", with=F]) %in%  unlist(input$Agg_options))
                ,]%>%
        arrange(-timepointyears)
    }
    else{
      FLIndex()[timepointyears %in% seq(input$Year[1],input$Year[2], by=1),]%>%
        arrange(-timepointyears)
    }
  
  })
  ############### End SDG data #####################
  dataR_loss <- reactive({
   if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      aggregationNameB = sub("code", "region", input$aggregation )
      ctry_choices <- unique(CountryGroup[CountryGroup[[aggregationNameB]] ==input$Agg_options,"m49_code",with=F])
      
      Losses[(timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
               (geographicaream49 %in%  ctry_choices),]%>%
        arrange(-timepointyears)
    }
    else{
      Losses[timepointyears %in% seq(input$Year[1],input$Year[2], by=1),]%>%
        arrange(-timepointyears)
    }
  })
  
  lab <- reactive({
    paste("Source: FAO", 
          paste("Date: ",as.character(Sys.time()),sep=""),
          paste("Country Aggregation: ",input$aggregation,sep=""),
          paste("Commodity Aggregation: ",input$BasketItems,sep="")
          ,sep="\n")
  }) 
  
  # plotly_Input = function(){
  #   x_label = "timepointyears"
  #  
  #   if(input$Model_Level %in% "SDG"){
  #     y_label = "FLP"
  #     p <-plot_ly(dataR(),y=~FLP, x= ~timepointyears  , type="scatter", mode="markers+lines" )
  #     p
  #     }
  #   if(input$Model_Level %in% "Model Estimates"){
  #     y_label = "value_measuredelement_5126"
  #     dataR_loss() 
  #     db_y = dataR_loss() [[y_label]]
  #     db_x = dataR_loss() [[agg_c]]
  #     
  # 
  # 
  #     
  #     p <-plot_ly(y=db_y, x=db_x  , type="scatter", mode="markers+lines",name = opt_y1 ) %>%
  #       layout(
  #         xaxis = list(
  #                    title = "Years",
  #                    titlefont = f,
  #                    range = c(seq(input$Year[1],input$Year[2], by=1)),  
  #                    dtick = 1
  #                  
  #         ),
  #         yaxis = list(
  #                    title = paste0(c(rep("&nbsp;", 20),
  #                            "Percentage",
  #                            rep("&nbsp;", 20),
  #                            rep("\n&nbsp;", 3)),
  #                          collapse = ""),
  #           tickformat = ".2%",
  #           titlefont = f
  #         ),
  #         margin = list(l =80, r = 50, t = 60, b = 150),
  #         annotations = list(text = lab,
  #                            font = list(size = 12),
  #                            showarrow = FALSE,
  #                            textposition= "bottom right",
  #                            align='right',
  #                            xref = 'paper', x = 0.98,
  #                            yref = 'paper', y = -0.5)
  #       )
  # 
  #     
  #     # Add 5 trace to this graphic with a loop!
  #     for(i in 1:nrow(opt_y)){
  #       opt_y1 <-  unlist(opt_y[i])
  #       dataB<-  dataR_loss() [(test[[agg_a]]  %in% opt_z1) & (test[[agg_b]] %in% opt_y1)& (test[[agg_c]] %in% seq(input$Year[1],input$Year[2], by=1)),]
  #       dataB<-  dataB %>% arrange(timepointyears)
  #       db_y = dataB[[y_label]]
  #       
  #       p<-p%>% 
  #         add_trace(y=db_y, x=db_x  , type="scatter", mode="markers+lines" ,name = opt_y1 )
  # 
  #     }
  #     p
  #    } 
  # }   
  # 
  # output$plot_ly  <- renderPlotly({
  #   plotly_Input()
  # 
  # })  


  ### R Check##
  obsB <- observe({

    print(input$aggregation)
    print(input$Agg_options)
    print( "af")
   # print( DataForIndex())
   # print( Basket())



    
   
  })
  

}
# Create Shiny app ----
options(shiny.autoreload.pattern = glob2rx("ui.R"))
shinyApp(ui = ui, server = server)




