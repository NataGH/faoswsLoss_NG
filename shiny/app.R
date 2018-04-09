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
library(shiny)
library(shinythemes)
library(gridExtra)
library(ggplot2)
library(trelliscopejs)
library(rmarkdown)
library(gtools)
library(ggplot2)
library(dplyr)
library(dtplyr)

library(magrittr)
remove.packages(pkgs, lib, version)

library(faosws)
library(faoswsUtil)
library(faoswsLoss)

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

# 
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
# 
# areaVar = "geographicAreaM49"
# yearVar = "timePointYears"
# itemVar = "measuredItemCPC"
# elementVar = "measuredElement"
# selectedYear = as.character(1991:2016)
# #----  Data In ------------------------------------------
# dataRaw <- ReadDatatable("aggregate_loss_table")
# dataRaw[,country :=NULL ]
# dataRaw[fsc_location =="SWS","fsc_location" ] <- "Official/Semi-Official - National"
# dataRaw[fsc_location =="sws_total","fsc_location" ] <- "Official/Semi-Official - National"
# dataRaw[fsc_location =="Calc","fsc_location" ] <- "Aggregated from multiple sources"
# setnames(dataRaw,"fsc_location", "Source"  )
# dataModel <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
# names(dataModel) <- tolower(names(dataModel))
# names(dataModel)[names(dataModel) =='measureditemsuafbs'] <- "measureditemcpc"
# dataModel$geographicaream49 <- as.character(dataModel$geographicaream49)
# #----
# CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
# fbsTree <- ReadDatatable("fbs_tree")
# FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
# #----
# CountryGroup$country <- tolower(CountryGroup$countryname)
# CountryGroup[,"geographicaream49":=CountryGroup$m49code]
# 
# names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
# names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
# FAOCrops[, "crop" := FAOCrops$description]
# names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"
# 
# fbsTree[foodgroupname %in% c(2905), gfli_basket :='Cereals',]
# fbsTree[foodgroupname %in% c(2911), gfli_basket :='Pulses',]
# fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
# fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
# fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
# fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |fo
# #----
# Crops <- merge(fbsTree,FAOCrops, by=("measureditemcpc"), all.x =T)
# dataRaw <-merge(dataRaw, fbsTree, by=("measureditemcpc"), all.x =T)
# dataRaw <-merge(dataRaw, CountryGroup, by=("geographicaream49"), all.x =T)
# dataModel <-merge(dataModel, fbsTree, by=("measureditemcpc"), all.x =T)
# dataModel <-merge(dataModel, CountryGroup, by=("geographicaream49"), all.x =T)
# # #-------------------------------------------------------

ui <- fluidPage(theme = shinytheme("lumen"),
                sidebarLayout(
                  sidebarPanel(
                    #Input()
                    sliderInput(
                      inputId = "Year",
                      label = "Year Range",
                      value = c(2005,2015),step =1,sep = "", min = as.integer(min(dataRaw$timepointyears)), max =  as.integer(max(dataRaw$timepointyears))
                    ),
                    selectInput(
                      inputId = "CommodityGroup",
                      label = "Selected Commodity",
                      choices = c("All",na.omit(unique(fbsTree[,"gfli_basket",with=FALSE]))),
                      selected = "All"
                    ),
                    selectInput(
                      inputId = "itemcpc",
                      label = "measureditemcpc",
                      choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
                    ),
                    selectInput(
                      inputId = "SDG_Reg",
                      label = "SDG Regions",
                      choices = c("All",unique(CountryGroup$sdg_regions)),
                      selected = "All"
                    ),
                    selectInput(
                      inputId = "Country",
                      label = "Country",
                      choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
                    ),
                    selectInput(
                      inputId = "Source",
                      label = "Source",
                      choices = c("All",unique(dataRaw$Source)), selected ="All", multiple=TRUE, selectize=TRUE
                    ),
                    selectInput("dataset", "Choose a dataset for download:",
                                choices = c("National Raw Aggregated Data","Modeled Estimates","Descriptive Stats")),
                    downloadButton("Data.csv", "Download"),
                    selectInput("datasetPlot", "Choose a Plot for download:",
                                choices = c("National Raw Aggregated Data","By CPC","By Country")),
                    downloadButton("MeasuredElement5126.jpeg", "Plots")
                  ),

                  mainPanel(
                    tabsetPanel(
                      tabPanel("Raw Data for the countries",  
                               tags$p(""),
                               tags$p("Raw input data for the commodity"),
                               plotOutput("rawC"),
                               tags$p(""),
                               tags$p("Summary stats for the raw input data for the commodity"),
                               verbatimTextOutput("statsC")),
                      tabPanel("Modeled Losses by Country",
                               tags$p(""),
                               tags$p("Losses for the by country"),
                               plotOutput("plotgraph", height = "900px")
                      ),
                      tabPanel("Modeled Losses by Commodity",
                               tags$p(""),
                               tags$p("Losses for the by Commodity"),
                               plotOutput("plotgraph2", height = "900px")
                      )
                    ))
                ))

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  observe({
    if (input$CommodityGroup != "") {
      CommodityGroup_item <- unlist(fbsTree[gfli_basket == input$CommodityGroup ,"measureditemcpc", with=FALSE])
      Data_CommodityGroup_item <- unlist(unique(dataRaw[measureditemcpc %in%  CommodityGroup_item,"measureditemcpc",with=F]))
      cpc_choices <- c("All",unique(FAOCrops[measureditemcpc %in% Data_CommodityGroup_item,"crop",with=F]))
      updateSelectInput(session, "itemcpc", choices=cpc_choices, selected ="All")
     
    }
  })
    observe({
    if (input$SDG_Reg != "") {
      Sdg_m49 <- unlist(CountryGroup[sdg_regions ==input$SDG_Reg,"geographicaream49",with=F])
      Data_Sdg_m49 <- unlist(unique(dataRaw[geographicaream49 %in%  Sdg_m49,"geographicaream49",with=F]))
      ctry_choices <- c("All",CountryGroup[geographicaream49 %in%  Data_Sdg_m49,"countryname",with=F])
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
  })

    
    dataR <- reactive({dataRaw %>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                            (measureditemcpc %in%
                                             if(input$CommodityGroup %in%  'All'){unlist(unique(dataRaw[,"measureditemcpc",with=F]))}
                                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(dataRaw[gfli_basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             else if(!(is.null(input$itemcpc))){unlist(unique(dataRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                                             else{unlist(unique(dataRaw[gfli_basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             ) &
                                          (geographicaream49 %in%
                                             if(input$SDG_Reg %in%  'All'){unlist(dataRaw[,"geographicaream49",with=F])}
                                             else if(any(input$Country %in%  c('All'))){unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])}
                                             else if(!is.null(input$Country)){unlist(dataRaw[countryname %in% input$Country,"geographicaream49",with=F])}
                                             else{unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])})&
                                          (Source %in%
                                             if(any(input$Source == 'All')){unlist(dataRaw[,"Source",with=F])}
                                             else{unlist(dataRaw[Source %in% input$Source,"Source",with=F])}
                                           )
                                        )
    })



  dataMI <- reactive({dataModel%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                            (measureditemcpc %in%
                                               if(input$CommodityGroup %in%  'All'){unlist(unique(dataRaw[,"measureditemcpc",with=F]))}
                                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(dataRaw[gfli_basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             else if(!(is.null(input$itemcpc))){unlist(unique(dataRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                                             else{unlist(unique(dataRaw[gfli_basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                            ) &
                                            (geographicaream49 %in%
                                               if(input$SDG_Reg %in%  'All'){unlist(dataRaw[,"geographicaream49",with=F])}
                                             else if(any(input$Country %in%  c('All'))){unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])}
                                             else if(!is.null(input$Country)){unlist(dataRaw[countryname %in% input$Country,"geographicaream49",with=F])}
                                             else{unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])})
  )
  })

  plotRawData = function(){
    ggplot(dataR(), aes(x = timepointyears, y = loss_per_clean, color = Source)) +
      geom_point() +
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold"))+
      scale_x_continuous(limits = c(input$Year[1],input$Year[2]), breaks = seq(input$Year[1],input$Year[2], 2))
  }
  plotModelData = function(){
      ggplot(dataMI(), aes(x = timepointyears, y = value, color = measureditemcpc)) +
      facet_wrap(~ country)+
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold"))
  }
  plotModelData1 = function(){
      ggplot(dataMI(), aes(x = timepointyears, y = value, color = geographicaream49)) +
        facet_wrap(~ measureditemcpc)+
        geom_point() +
        geom_line()+
        xlab('timePointYears') + ylab('Loss (%)') +
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold"))
  }
  
  DescriptiveStats = function(){
    title= ("The average for the commodity for all countries")
    ddply(dataR(),~timepointyears,summarise,
        N_Country = length(unique(geographicaream49)),
        N_Crops = length(unique(measureditemcpc)),
        N_CtryCropCombo = length(unique(paste(measureditemcpc,geographicaream49,sep="_"))),
        mean=round(mean(loss_per_clean),3),
        min= min(loss_per_clean),
        max= max(loss_per_clean),
        sd=round(sd(loss_per_clean),3))

  }
  output$rawC <- renderPlot({
    plotRawData()
  })
  output$statsC <- renderPrint({
    DescriptiveStats() 
  })

  output$plotgraph  <- renderPlot({
    plotModelData()
    })

  output$plotgraph2  <- renderPlot({
    plotModelData1()
  })
  

  #### Downloadable csv of selected dataset ####
  datasetPlot <- reactive({
    switch(input$datasetPlot,
           "National Raw Aggregated Data" = plotRawData()  ,
           "By CPC" =  plotModelData1(),
           "By Country" =  plotModelData()
    )
  })
  dataset <- reactive({
    switch(input$dataset,
           "National Raw Aggregated Data" = dataR(),
           "Modeled Estimates" =  dataMI(),
           "Descriptive Stats" =  DescriptiveStats() 
    )
  })
 
  output$table <- renderTable({
    datasetInput()
  })
  output$plot <- renderPlot({
    plotInput()
  })
  
  output$Data.csv <- downloadHandler(
    filename = function(){
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  output$MeasuredElement5126.jpeg <- downloadHandler(
    filename = function(){paste("MeasuredElement5126",input$filename, ".jpeg", sep = "")},
    content = function(file) {
      ggsave(file, plot =   datasetPlot(),  scale = .8, width = 450, height = 200, dpi = 300, units = "mm", device =  "jpeg")
    }
  )
  ### R Check##
  obsB <- observe({
    print(plotModelData())

  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



