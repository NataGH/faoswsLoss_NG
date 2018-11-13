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
#library(faosws)
#library(faoswsUtil)
#library(faoswsLoss)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(rmarkdown)
library(ggplot2)
library(grid)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)



suppressMessages({
  library(dplyr)
  
})


BaseYear = as.character(c(2004,2006)) ## This is not an option to choose after the movement to the SDG base yr
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
LocalRun <- FALSE



# ###----  Data In ----------############
if(!LocalRun){
  library(faosws)
  library(faoswsUtil)
  library(faoswsLoss)
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
  
  LossFactorRaw <- ReadDatatable('flw_lossperfactors_')
  AggregateLoss <- ReadDatatable('aggregate_loss_table')
  fbsTree <- ReadDatatable("fbs_tree")
  CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
  FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
  LossFactorRaw$measureditemcpc <- addHeadingsCPC(LossFactorRaw$measureditemcpc)
  
  setnames(CountryGroup, old = c("m49code","iso2code","isocode","countryname","sdgregion_code","sdg_regions","m49_level1_code",        
                                 "m49_level1_region","m49_level2_region_code","m49_level2_region","mdgregions_code","mdgregions","ldcs_code","ldcs",                   
                                 "lldcssids_code","lldcssids","fao_region","fao_operationalcoverage"),
           new = c("geographicaream49","ISO2code","isocode","Country","sdgregion_code","SDG Regions","m49_level1_code",        
                   "Geographic Regions(m49) Level1","m49_level2_region_code","Geographic Regions(m49) Level2","mdgregions_code","MDG Regions","ldcs_code","Least Developed Countries (LDC)",                   
                   "lldcssids_code","Land Locked Developing Countries (LLDC)","FAO Operational Region","FAO Operational Coverage"))
  
  
}else{
  setwd(paste(getwd(),"/shiny/Input_Data",sep=""))
  load("Inputs.RData")
  
}

CountryGroup$fao_operationalcoverage<-as.character(CountryGroup$fao_operationalcoverage)
CountryGroup[fao_operationalcoverage %in% c("1"),fao_operationalcoverage := "Yes"]
CountryGroup[fao_operationalcoverage %in% c("0"),fao_operationalcoverage := "No"]


names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
FAOCrops[, "crop" := FAOCrops$description]
names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"
FAOCrops <- FAOCrops[order(FAOCrops$measureditemcpc),]

#SDG Headings
fbsTree[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",

LossFactorRaw[fsc_location =="SWS","fsc_location" ] <- "Official/Semi-Official - National"
LossFactorRaw[fsc_location =="sws_total","fsc_location" ] <- "Official/Semi-Official - National"
LossFactorRaw[fsc_location =="Calc","fsc_location" ] <- "Aggregated from multiple sources"


AggregateLoss[fsc_location =="SWS","fsc_location" ] <- "Official/Semi-Official - National"
AggregateLoss[fsc_location =="sws_total","fsc_location" ] <- "Official/Semi-Official - National"
AggregateLoss[fsc_location =="Calc","fsc_location" ] <- "Aggregated from multiple sources"

LossFactorRaw[,"analyst" := NULL]
LossFactorRaw[,"notes" := NULL]
setnames(LossFactorRaw, old = c("geographicaream49","isocode","country","region","measureditemcpc","crop","timepointyears","loss_per_clean","percentage_loss_of_quantity",
                                "loss_quantity","loss_qualitiative","loss_monetary",
                                "activity","fsc_location","periodofstorage","treatment","causeofloss","samplesize",
                                "units","method_datacollection","tag_datacollection","reference","url"),
         new = c("geographicaream49","isocode","Country","Region","measureditemcpc","Crop","Year","loss_per_clean","Range of Quantity Loss (%)",
                 "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)",
                 "Activity","Stage","period of storage","treatment","Causes of loss","Sample Size",
                 "Sampling Units","Method of Data Collection","Data Collection Tag","Reference","Url"))

setnames(AggregateLoss, old = c("geographicaream49","isocode","timepointyears","country","measureditemcpc","crop",
                                "loss_per_clean","fsc_location"),
         new = c("geographicaream49","isocode","Year","Country","measureditemcpc","Crop",
                 "loss_per_clean","Stage"))



LossFactorRaw[,"Average Quantity Loss (%)":=loss_per_clean]
AggregateLoss[,"Average Quantity Loss (%)":=loss_per_clean]

LossFactorRaw$fsc_location1 = sapply(strsplit(LossFactorRaw$Stage,"/"), '[', 1)
LossFactorRaw <- merge(LossFactorRaw,CountryGroup, by=c("isocode", "geographicaream49"))
AggregateLoss <- merge(AggregateLoss,CountryGroup, by=c("isocode", "geographicaream49"))
LossFactorRaw[, "Country.y" := NULL]
names(LossFactorRaw)[names(LossFactorRaw) =="Country.x"] <- "Country"
names(AggregateLoss)[names(AggregateLoss) =="Country.x"] <- "Country"
datatags <- sort(unlist(unique(LossFactorRaw$"Data Collection Tag")),decreasing=F)
dataStages <- sort(unlist(unique(LossFactorRaw$fsc_location1)))
LossFactorRaw[Stage =="SWS_Total","Stage"] = "Official - Whole chain Estimate"

LossFactorRaw2 <- merge(LossFactorRaw,fbsTree, by=c("measureditemcpc"))
LossFactorRaw_descriptivestat <- LossFactorRaw2 %>%
  filter(loss_per_clean >0 & Reference != "SWS") %>%
  group_by(gfli_basket) %>%
  do(data.frame(t(quantile(.$loss_per_clean, probs = c(0.25, 0.50, 0.75)))))

LossFactorRaw_descriptivestat2 <- LossFactorRaw2 %>%
  filter(loss_per_clean >0 & Reference != "SWS") %>%
  group_by(gfli_basket) %>%
  dplyr::summarise(n=n())

# # # #-------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Post-Harvest Loss Data, Studies and Resources"),
  dashboardSidebar(
    sliderInput(
      inputId = "Year",
      label = "Year Range",
      value = c(1990,2016),step =1,sep = "", min = as.integer(min(na.omit(unique(LossFactorRaw$Year)))), max =  as.integer(max(na.omit(unique(LossFactorRaw$Year))))
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
    selectInput(
      inputId = "Stage",
      label = "Value Chain Stage(s)",
      choices = c("All",dataStages), selected ="All", multiple=TRUE, selectize=TRUE
    ),
    selectInput(
      inputId = "DataCollect",
      label = "Method of Data Collection",
      choices = c("All",datatags) , selected ="All", multiple=TRUE, selectize=TRUE
    ),
    
    
    downloadButton("Data.csv", "Download"),
    downloadButton("ValueChain_Stages.jpeg", "Plots")
    
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
      ),
      box(
        title = "Aggregates",status = "primary",
        valueBoxOutput("WholeSupplyChainBox"),
        valueBoxOutput("SWSBox"),
        valueBoxOutput("ModelBox")
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
      box(title = "Boundary",status = "warning",
          valueBoxOutput("RetailBox")
      )),
    fluidRow(
      box(width=12,  collapsible = TRUE,
          title = "Graph",status = "primary",
          plotOutput("PointGraphs")
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
      Agg_choices <- c("All",unlist(unique(CountryGroup[,"SDG Regions"])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "SDG Regions") {
      Agg_choices <- c("All",unique(CountryGroup[,"SDG Regions"]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Geographic Regions(m49) Level1") {
      Agg_choices <- c("All",unique(CountryGroup[,"Geographic Regions(m49) Level1"]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Geographic Regions(m49) Level2") {
      Agg_choices <- c("All",unique(CountryGroup[,"Geographic Regions(m49) Level2"]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "MDG Regions") {
      Agg_choices <- c("All",unique(CountryGroup[,"MDG Regions"]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Least Developed Countries (LDC)") {
      Agg_choices <- c("All",na.omit(unique(CountryGroup[,"Least Developed Countries (LDC)"])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Land Locked Developing Countries (LLDC)") {
      Agg_choices <- c("All",na.omit(unique(CountryGroup[,"Land Locked Developing Countries (LLDC)"])))
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "FAO Operational Coverage") {
      Agg_choices <- c("All",na.omit(unique(CountryGroup[,"FAO Operational Coverage"])))
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
        ctry_choices <- c(unique(CountryGroup[geographicaream49 %in% LossFactorRaw$geographicaream49,"Country"]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "Italy")
      }
      if(input$Agg_options != 'All'){
        ctry_choices <- c(unique(CountryGroup[(CountryGroup[["SDG Regions"]]   %in% input$Agg_options)&(geographicaream49 %in% LossFactorRaw$geographicaream49),"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = NULL)
      }
    }
    if (input$aggregation == "SDG Regions") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["SDG Regions"]]  %in% input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "Geographic Regions(m49) Level1") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["Geographic Regions(m49) Level1"]] %in%  input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "Geographic Regions(m49) Level2") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["Geographic Regions(m49) Level2"]] %in% input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "MDG Regions") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["MDG Regions"]]  %in% input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "Least Developed Countries (LDC)") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["Least Developed Countries (LDC)"]] ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "Land Locked Developing Countries (LLDC)") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["Land Locked Developing Countries (LLDC)"]] ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation ==  "FAO Operational Coverage") {
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[["FAO Operational Coverage"]] ==input$Agg_options,"Country",with=F]))
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
      itemcpc_choices <- c("All", unique(FAOCrops[measureditemcpc %in% Comm_grp,'crop',with=F]))
      updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
    }
    
  })
  
  dataR1 <- reactive({
    #options(show.error.messages = FALSE)
    if((input$aggregation == "Country")) {
      LossFactorRaw %>% filter((Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                                 (Country %in% unlist(input$Country)) 
                               
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      LossFactorRaw[(Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                      (unlist(LossFactorRaw [,input$aggregation, with=F]) %in%  unlist(input$Agg_options))
                    ,]
    }
    else{
      LossFactorRaw[Year %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  dataR1_agg <- reactive({
    options(show.error.messages = FALSE)
    if((input$aggregation == "Country")) {
      AggregateLoss %>% filter((Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                                 (Country %in% unlist(input$Country)) 
                               
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      AggregateLoss[(Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                      (unlist(AggregateLoss[,input$aggregation, with=F]) %in%  unlist(input$Agg_options))
                    ,]
    }
    else{
      AggregateLoss[Year %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  dataR2 <- reactive({
    dataR1() %>% filter((measureditemcpc %in%
                           if(input$BasketItems %in%  'All'){unlist(unique(LossFactorRaw[,"measureditemcpc",with=F]))}
                         else if(any(input$itemcpc %in%  c('All'))){unlist(unique(fbsTree[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
                         else if(!(is.null(input$itemcpc))){unlist(unique(dataR1()[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                         else{unlist(unique(dataR1()[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
    ))
    
  })
  dataR2_agg <- reactive({
    dataR1_agg() %>% filter((measureditemcpc %in%
                               if(input$BasketItems %in%  'All'){unlist(unique(LossFactorRaw[,"measureditemcpc",with=F]))}
                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(fbsTree[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
                             else if(!(is.null(input$itemcpc))){unlist(unique(LossFactorRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                             else{unlist(unique(LossFactorRaw[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
    ))
    
  })
  
  dataR3 <- reactive({
    if(any(input$DataCollect %in% c('All'))){dataR2()}
    else if(!any(input$DataCollect %in% c('All'))){dataR2() %>% filter(dataR2()[['Data Collection Tag']] %in% input$DataCollect)}
    
    
  })
  dataR3_agg <- reactive({
    if(any(input$DataCollect %in% c('All'))){dataR2_agg()}
    else if(!any(input$DataCollect %in% c('All'))){dataR2_agg() %>% filter(dataR2_agg()[['Data Collection Tag']] %in% input$DataCollect)}
    
    
  })
  dataR <- reactive({
    if(any(input$Stage %in% c('All'))){dataR3()}
    else if(!any(input$Stage %in% c('All'))){dataR3() %>% filter(fsc_location1 %in% input$Stage)}
    
    
  })
  dataR_agg <- reactive({
    if(any(input$Stage %in% c('All'))){dataR3_agg()}
    else if(!any(input$Stage %in% c('All'))){dataR3_agg() %>% filter(fsc_location1 %in% input$Stage)}
    
    
  })
  
  
  output$HarvestBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 == "Harvest") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Harvest", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 == "Harvest") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"}, 
      "Harvest", icon = icon("grain", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  output$FarmBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 == "Farm") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Farm", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 == "Farm") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"}
      , "Farm", icon = icon("chevron-right"),
      color = "green"
    )
  })
  output$TransportBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 == "Transport") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Transport", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 == "Transport") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"}
      ,"Transport", icon = icon("chevron-right"),
      color = "olive"
    )
  })
  output$StorageBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 == "Storage") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Storage", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 == "Storage") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"}
      ,"Storage", icon = icon("chevron-right"),
      color = "teal"
    )
  })
  output$TraderBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 == "Trader") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Trader", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 == "Trader") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"}
      ,"Trader", icon = icon("chevron-right"),
      color = "aqua"
    )
  })
  output$WholesaleBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Wholesale") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])){
        paste0(round(sum(dataR()[fsc_location1 == "Wholesale", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 =="Wholesale") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"},"Wholesale", icon = icon("chevron-right"),
      color = "light-blue"
    )
  })
  output$ProcessingBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Processing") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])){
        paste0(round(sum(dataR()[fsc_location1 == "Processing", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 =="Processing") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"},"Processing", icon = icon("chevron-right"),
      color = "blue"
    )
  })
  output$RetailBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="Retail") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "Retail", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 =="Retail") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"},"Retail", icon = icon("cultery", lib = "glyphicon"),
      color = "navy"
    )
  })
  output$WholeSupplyChainBox <- renderValueBox({
    valueBox(
      if(nrow(dataR()[(fsc_location1 =="WholeSupplyChain") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "WholeSupplyChain", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 =="WholeSupplyChain") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"},"Whole Supply Chain - Studies", icon = icon("option-horizontal", lib = "glyphicon"),
      color = "maroon"
    )
  })
  output$SWSBox <- renderValueBox({
    valueBox(
      if( nrow(dataR()[(fsc_location1 =="SWS_Total") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR()[fsc_location1 == "SWS_Total", "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR()[(fsc_location1 =="SWS_Total") & (!is.na("Average Quantity Loss (%)")), "Average Quantity Loss (%)",with=F]),2)
               , "%")}else{"No Data"},"Official and Semi-Official Estimates", icon = icon("option-horizontal", lib = "glyphicon"),
      color = "purple"
    )
  })
  output$ModelBox <- renderValueBox({
    valueBox(
      if( nrow(dataR_agg()[, "Average Quantity Loss (%)",with=F])>0){
        paste0(round(sum(dataR_agg()[, "Average Quantity Loss (%)",with=F], na.rm=T)/
                       nrow(dataR_agg()[, "Average Quantity Loss (%)",with=F]),2)*100
               , "%")}else{"No Data"},"National Estimates (Model Aggregates)", icon = icon("option-horizontal", lib = "glyphicon"),
      color = "black"
    )
  })
  
  DataOutput <- function()({
    dataR()[ ,c("isocode","Country","Region","measureditemcpc","Crop","Year","Stage","Average Quantity Loss (%)",
                "Loss (quantity, tons)","Loss (Qualitative, tons)","Loss (Monetary, LCU)","Causes of loss","Activity",         
                "period of storage","treatment","Sampling Units","Method of Data Collection","Data Collection Tag","Reference","Url"),with=FALSE]
  })
  
  
  output$DataTab = DT::renderDataTable({
    DataOutput()
    
  })
  
  lab <- reactive({
    paste("Source: FAO", 
          paste("Date: ",as.character(Sys.time()),sep=""),
          paste("Country Aggregation: ",input$aggregation,sep=""),
          paste("Commodity Aggregation: ",input$BasketItems,sep=""),
          paste("Data Tag: ",input$DataCollect,sep="")
          ,sep="\n")
  })  
  
  
  
  plotInput = function() {
    if(length(dataR())>1){
      ggplot(dataR(), aes(x = Year, y = loss_per_clean, colour = Stage)) +
        geom_point()+
        xlab('Year') + ylab('Average Quantity Loss (%)') +
        labs( caption=lab())+
        scale_x_continuous(labels = function(x) round(as.numeric(x), digits=0)) +
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold"))+ 
        theme(plot.margin = unit(c(1,1,3,1), "cm"))+
        theme(plot.caption=element_text(vjust= -0.6))
      
      
      
    }else{
      print("Please Choose Another Option")
    }
    
  }
  
  
  
  output$PointGraphs <- renderPlot({
    plotInput()
    
  })
  #### Downloadable csv of selected dataset ####
  
  output$Data.csv <- downloadHandler(
    filename = function(){
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(DataOutput(), file, row.names = FALSE, na = "")
    }
  )
  
  
  
  output$ValueChain_Stages.jpeg <- downloadHandler(
    filename = function(){paste("ValueChain_Stages_",input$filename, ".jpeg", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInput(),   scale = .8, width = 450, height = 200, dpi = 300, units = "mm", device =  "jpeg")
    }
  )
  
  obsB <- observe({
    #print(plotInput())
    print( dataR())
  })
  
  
}

shinyApp(ui, server)
