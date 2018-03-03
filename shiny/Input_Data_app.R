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
# 
# library(faosws)
# library(faoswsUtil)
# library(faoswsLoss)
# library(shiny)
# library(shinythemes)
# library(shinydashboard)
# library(rmarkdown)
# library(gtools)
# library(ggplot2)
# library(dplyr)
# library(dtplyr)
# library(DT)
# library(magrittr)
# remove.packages(pkgs, lib, version)
#
#
# suppressMessages({
#   library(faosws)
#   library(faoswsUtil)
#   library(faoswsFlag)
#   library(lme4)
#   library(data.table)
#   library(magrittr)
#   library(reshape2)
#   library(plyr)
#   library(dplyr)
#
# })


# if(CheckDebug()){
#   message("Not on server, so setting up environment...")
#   USER <- if_else(.Platform$OS.type == "unix",
#                   Sys.getenv('USER'),
#                   Sys.getenv('USERNAME'))
#   
#   
#   library(faoswsModules)
#   settings <- ReadSettings(file = file.path(paste(dirmain,"sws.yml", sep='/')))
#   #SetClientFiles(settings[["certdir"]])
#   
#   GetTestEnvironment(
#     baseUrl = settings[["server"]],
#     token = settings[["token"]]
#   )
#   
# }
# 
BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"


# ###----  Data In ----------############
# ConvFactor1 <- ReadDatatable('flw_lossperfactors')
# fbsTree <- ReadDatatable("fbs_tree")
# CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
# FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
# 
# names(CountryGroup)[names(CountryGroup) =="countryname"] <- "Country"
# names(CountryGroup)[names(CountryGroup) =="m49code"] <- "geographicaream49"
# 
# names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
# names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
# FAOCrops[, "crop" := FAOCrops$description]
# names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"
# 
# #SDG Headings
# fbsTree[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
# fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
# fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
# fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
# fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",
# 
# ConvFactor1[,"percentage_loss_of_quantity" := NULL]
# ConvFactor1[,"month" := NULL]
# ConvFactor1[,"countryalt" := NULL]
# setnames(ConvFactor1, old = c("isocode","country","region","measureditemcpc","crop","year","periodofstorage","treatment","causeofloss","loss_per_clean",
#                              "loss_quantity","loss_qualitiative","loss_monetary","activity","fsc_location",         
#                              "samplesize","totalsize","method_datacollection","tag_datacollection","reference","url","notes"),
#           new = c("isocode","Country","Region","measureditemcpc","Crop","timepointyears","period of storage","treatment","causes of loss","Quantity Loss (%)",
#                  "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)","Activity","Food Value Chain Stage",         
#                  "Sampling Units","Quantity Coverage","Method of Data Collection","Data Collection Tag","Reference","Url","Notes"))
# ConvFactor1$fsc_location1 = sapply(strsplit(ConvFactor1$"Food Value Chain Stage","/"), '[', 1)
# ConvFactor1 <- merge(ConvFactor1,CountryGroup, by=c("isocode"))
# ConvFactor1[, "Country.y" := NULL]
# names(ConvFactor1)[names(ConvFactor1) =="Country.x"] <- "Country"

ui <- dashboardPage(
  dashboardHeader(title = "Post-Harvest Loss Data, Studies and Resources"),
  dashboardSidebar(
    sliderInput(
      inputId = "Year",
      label = "Year Range",
      value = c(2005,2015),step =1,sep = "", min = as.integer(min(selectedYear)), max =  as.integer(max(selectedYear))
    ),
    selectInput(
      inputId = "aggregation",
      label = "Aggregation",
      choices = c("WORLD",names(CountryGroup)[seq(2,length(names(CountryGroup)), by=2)][-8]),
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
      inputId = "BasketItems",
      label = "Basket Items",
      choices = c("All",na.omit(unique(fbsTree[,"gfli_basket",with=FALSE]))), 
      selected = "All"
    ),
    selectInput(
      inputId = "itemcpc",
      label = "measureditemcpc",
      choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
    ),
    
    downloadButton("Data.csv", "Download")
    
  ),

  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title = "Introduction", width = 12, solidHeader = TRUE, status = "info",
          tags$p("This data include raw data from the FAO Statistical Working System for loss calculations. 
          Data that was imputed, estimated, based on trends without a priori knowledge, was excluded, 
          essentially leaving only protected and official data. Additional information was gathered from 
          300+ publications and reports from various sources (subnational reports, academic institutions, 
          reports from international organizations, including but not limited to, the World Bank, GIZ, FAO, IFPRI)."),

          tags$p("Studies on losses published in academic journals and by governments and donor communities may also over-represent
          segments (e.g. storage) or segments of the population (e.g. small holder farmers) or supply chain that
          meet development objectives. Although there is much to be desired in terms of the specific methods 
          and definition of loss estimates in these studies, they provide a fuzzy baseline of snapshots and 
          causes of losses over several commodities, countries and years. Often these studies illuminate
          microcosms of economic, system and environmental factors that need to be incorporated in a more robust study.") 
       )
      ),
    fluidRow(
      box(status = "info",
      valueBoxOutput("HarvestBox"),
      "These values are not included (yet) but are important for
      focusing losses that occur due to harvesting practices or market failures "
      )
    ),
    fluidRow(
      box(title ="Global Food Loss Index",status = "primary",
      valueBoxOutput("FarmBox"),
      valueBoxOutput("TransportBox"),
      valueBoxOutput("StorageBox"),
      valueBoxOutput("TraderBox"),
      valueBoxOutput("WholesaleBox"),
      valueBoxOutput("ProcessingBox")
    ),
    box(
      title = "Aggregates",status = "primary",
      valueBoxOutput("WholeSupplyChainBox"),
      valueBoxOutput("SWSBox")
    )),
    fluidRow(
      box(title = "Boundary",status = "warning",
      valueBoxOutput("RetailBox")
    )),
    fluidRow(
      box(width=12, title = "Data Available",  solidHeader = TRUE,
           collapsible = TRUE,
          div(style = 'overflow-x: scroll', DT::dataTableOutput("DataTab"))
           
      )
     
    )
  )
)


server <- function(input, output, session) {
  set.seed(122)
  observe({
    if (input$aggregation == "Country") {
      Agg_choices <- c("All",unlist(unique(CountryGroup[,sdg_regions])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "sdg_regions") {
      Agg_choices <- c("All",unlist(unique(CountryGroup[,sdg_regions])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "m49_level1_region") {
      Agg_choices <- c("All",unlist(unique(CountryGroup[,m49_level1_region])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "m49_level2_region") {
      Agg_choices <- c("All",unlist(unique(CountryGroup[,m49_level2_region])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "mdgregions") {
      Agg_choices <- c("All",unlist(unique(CountryGroup[,mdgregions])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "ldcs") {
      Agg_choices <- c("All",unlist(na.omit(unique(CountryGroup[,ldcs]))))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "lldcssids") {
      Agg_choices <- c("All",unlist(na.omit(unique(CountryGroup[,lldcssids]))))
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "WORLD") {
      Agg_choices <- c("All")
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
  })
  observe({
    if (input$aggregation == "WORLD") {
      ctry_choices <- c("All")
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "Country") {
      if(input$Agg_options == 'All'){
        ctry_choices <- c(unique(CountryGroup[geographicaream49 %in% Losses$geographicaream49,"Country"]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "Italy")
      }
      if(input$Agg_options != 'All'){
        ctry_choices <- c(unique(CountryGroup[(sdg_regions ==input$Agg_options)&(geographicaream49 %in% Losses$geographicaream49),"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = NULL)
      }
    }
    if (input$aggregation == "sdg_regions") {
      ctry_choices <- c(unique("All",CountryGroup[sdg_regions ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "m49_level1_region") {
      ctry_choices <- c("All",unique(CountryGroup[m49_level1_region ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "m49_level2_region") {
      ctry_choices <- c("All",unique(CountryGroup[m49_level2_region ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "mdgregions") {
      ctry_choices <- c("All",unique(CountryGroup[mdgregions ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "ldcs") {
      ctry_choices <- c("All",unique(CountryGroup[ldcs ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "lldcssids") {
      ctry_choices <- c("All",unique(CountryGroup[lldcssids ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
  })
  
  observe({
    if (input$BasketItems == "All") {
      itemcpc_choices <- c("All", unique(FAOCrops[,'crop',with=F]))
      updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
    }
    if (input$BasketItems != "All") {
      Comm_grp <- unlist(fbsTree[gfli_basket == input$BasketItems,measureditemcpc])
      itemcpc_choices <- c("All", FAOCrops[measureditemcpc %in% Comm_grp,'crop',with=F])
      updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
    }
    
  })

  dataR1 <- reactive({
    options(show.error.messages = FALSE)
    if((input$aggregation == "Country")) {
      ConvFactor1 %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                            (Country %in% unlist(input$Country)) 
                            
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      ConvFactor1[(timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                  (unlist(ConvFactor1 [,input$aggregation, with=F]) %in%  unlist(input$Agg_options))
                ,]
    }
    else{
      ConvFactor1 [timepointyears %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  dataR <- reactive({
    if((input$aggregation != "All")) {
      dataR1()[MeasuredItemCPC %in% unlist(fbsTree[gfli_basket %in% input$BasketItems,"measureditemcpc"]) ,]
    }
    else{
      dataR1()
    }
  })

  output$HarvestBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 == "Harvest") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
         paste0(round(sum(dataR()[fsc_location1 == "Harvest", "Quantity Loss (%)",with=F], na.rm=T)/
                nrow(dataR()[(fsc_location1 == "Harvest") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
              , "%")}else{"No Data"}, 
      "Harvest", icon = icon("grain", lib = "glyphicon"),
      color = "yellow"
      )
  })

  
  output$FarmBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 == "Farm") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "Farm", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 == "Farm") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"}
        , "Farm", icon = icon("chevron-right"),
      color = "green"
    )
  })
  output$TransportBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 == "Transport") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "Transport", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 == "Transport") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"}
      ,"Transport", icon = icon("chevron-right"),
      color = "olive"
    )
  })
  output$StorageBox <- renderValueBox({
    valueBox(
    if(nrow(dataR()[(fsc_location1 == "Storage") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "Storage", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 == "Storage") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"}
    ,"Storage", icon = icon("chevron-right"),
      color = "teal"
    )
  })
  output$TraderBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 == "Trader") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "Trader", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 == "Trader") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"}
      ,"Trader", icon = icon("chevron-right"),
      color = "aqua"
    )
  })
  output$WholesaleBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Wholesale") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])){
      paste0(round(sum(dataR()[fsc_location1 == "Wholesale", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 =="Wholesale") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"},"Wholesale", icon = icon("chevron-right"),
      color = "light-blue"
    )
  })
  output$ProcessingBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Processing") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])){
      paste0(round(sum(dataR()[fsc_location1 == "Processing", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 =="Processing") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"},"Processing", icon = icon("chevron-right"),
      color = "blue"
    )
  })
  output$RetailBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Retail") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "Retail", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 =="Retail") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"},"Retail", icon = icon("cultery", lib = "glyphicon"),
      color = "navy"
    )
  })
  output$WholeSupplyChainBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="WholeSupplyChain") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "WholeSupplyChain", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 =="WholeSupplyChain") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"},"Whole Supply Chain", icon = icon("option-horizontal", lib = "glyphicon"),
      color = "maroon"
    )
  })
  output$SWSBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 =="SWS_Total") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F])>0){
      paste0(round(sum(dataR()[fsc_location1 == "SWS_Total", "Quantity Loss (%)",with=F], na.rm=T)/
                     nrow(dataR()[(fsc_location1 =="SWS_Total") & (!is.na("Quantity Loss (%)")), "Quantity Loss (%)",with=F]),2)
             , "%")}else{"No Data"},"Official Estimates", icon = icon("option-horizontal", lib = "glyphicon"),
      color = "purple"
    )
  })
  
  DataOutput <- function()({
    dataR()[ ,c("isocode","Country","Region","MeasuredItemCPC","Crop","timepointyears","Food Value Chain Stage","Quantity Loss (%)",
                "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)","causes of loss","Activity",         
                "period of storage","treatment","Sampling Units","Quantity Coverage","Method of Data Collection","Data Collection Tag","Reference","Url","Notes"),with=FALSE]
  })


  output$DataTab = DT::renderDataTable({
    DataOutput()
    
  })
  #### Downloadable csv of selected dataset ####
  
  output$Data.csv <- downloadHandler(
    filename = function(){
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DataOutput(), file, row.names = FALSE)
    }
  )
  obsB <- observe({
    dataR()
  })
  
  
}

shinyApp(ui, server)