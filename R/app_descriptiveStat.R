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


#render("1-example.Rmd")

#################### For deletion #####################################
# For Local
# SWS Connection

githubsite <- '~/faoswsLoss/data-raw/'
dirmain <-  '~/faoswsLossa'
SetClientFiles(dir = "C:\\Users\\ENGLISHA\\Documents\\certificates\\qa")
files = dir("~/Github/faoswsLoss/R",
            full.names = TRUE)


token4 = "9b7a17f1-03e9-48c7-871c-115d8f20c4a3" # saved Loss % data

GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token4
)



areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
#----  Data In ------------------------------------------
dataRaw <- ReadDatatable("aggregate_loss_table")
dataModel <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
names(dataModel) <- tolower(names(dataModel))
names(dataModel)[names(dataModel) =='measureditemsuafbs'] <- "measureditemcpc"
dataModel$geographicaream49 <- as.character(dataModel$geographicaream49)
#----
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
fbsTree <- ReadDatatable("fbs_tree")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")
#----
CountryGroup$country <- tolower(CountryGroup$countryname)
CountryGroup[,"geographicaream49":=CountryGroup$m49code]
names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
FAOCrops[, "crop" := FAOCrops$description]
names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"

fbsTree[foodgroupname %in% c(2905), GFLI_Basket :='Cereals',]
fbsTree[foodgroupname %in% c(2911), GFLI_Basket :='Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), GFLI_Basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), GFLI_Basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), GFLI_Basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), GFLI_Basket :='Animals Products & Fish and fish products',] # |fo
#----
Crops <- merge(fbsTree,FAOCrops, by=("measureditemcpc"), all.x =T)
dataRaw <-merge(dataRaw, fbsTree, by=("measureditemcpc"), all.x =T)
dataRaw <-merge(dataRaw, CountryGroup, by=("geographicaream49"), all.x =T)
dataModel <-merge(dataModel, fbsTree, by=("measureditemcpc"), all.x =T)
dataModel <-merge(dataModel, CountryGroup, by=("geographicaream49"), all.x =T)
#-------------------------------------------------------

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
                      choices = c("All",na.omit(unique(fbsTree[,"GFLI_Basket",with=FALSE]))),
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
                    )
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
      CommodityGroup_item <- unlist(fbsTree[GFLI_Basket == input$CommodityGroup ,"measureditemcpc", with=FALSE])
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
                                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(dataRaw[GFLI_Basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             else if(!(is.null(input$itemcpc))){unlist(unique(dataRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                                             else{unlist(unique(dataRaw[GFLI_Basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             ) &
                                          (geographicaream49 %in% 
                                             if(input$SDG_Reg %in%  'All'){unlist(dataRaw[,"geographicaream49",with=F])}
                                             else if(any(input$Country %in%  c('All'))){unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])}
                                             else if(!is.null(input$Country)){unlist(dataRaw[countryname %in% input$Country,"geographicaream49",with=F])}
                                             else{unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])})
                                        )
    })

  dataMI <- reactive({dataModel%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                                            (measureditemcpc %in% 
                                               if(input$CommodityGroup %in%  'All'){unlist(unique(dataRaw[,"measureditemcpc",with=F]))}
                                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(dataRaw[GFLI_Basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                             else if(!(is.null(input$itemcpc))){unlist(unique(dataRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc"])),"measureditemcpc",with=F]))}
                                             else{unlist(unique(dataRaw[GFLI_Basket %in%  input$CommodityGroup,"measureditemcpc",with=F]))}
                                            ) &
                                            (geographicaream49 %in% 
                                               if(input$SDG_Reg %in%  'All'){unlist(dataRaw[,"geographicaream49",with=F])}
                                             else if(any(input$Country %in%  c('All'))){unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])}
                                             else if(!is.null(input$Country)){unlist(dataRaw[countryname %in% input$Country,"geographicaream49",with=F])}
                                             else{unlist(dataRaw[sdg_regions %in%  input$SDG_Reg,"geographicaream49",with=F])})
  )
  })
    
  output$rawC <- renderPlot({
    ggplot(dataR(), aes(x = timepointyears, y = loss_per_clean/100, color = fsc_location)) + 
      geom_point() +
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold"))+
      scale_x_continuous(limits = c(input$Year[1],input$Year[2]), breaks = seq(input$Year[1],input$Year[2], 2))
  })
  
  output$statsC <- renderPrint({
    title= ("The average for the commodity for all countries")
    ddply(dataR(),~timepointyears,summarise,
          N_Country = length(unique(geographicaream49)),
          N_Crops = length(unique(measureditemcpc)),
          N_CtryCropCombo = length(unique(paste(measureditemcpc,geographicaream49,sep="_"))),
          mean=round(mean(loss_per_clean/100),3),
          min= min(loss_per_clean/100),
          max= max(loss_per_clean/100),
          sd=round(sd(loss_per_clean/100),3))
  })

  output$plotgraph  <- renderPlot({
    ggplot(dataMI(), aes(x = timepointyears, y = value, color = measureditemcpc)) + 
      facet_wrap(~ geographicaream49)+
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) 
    })
  
  output$plotgraph2  <- renderPlot({
    ggplot(dataMI(), aes(x = timepointyears, y = value, color = geographicaream49)) + 
      facet_wrap(~ measureditemcpc)+
      geom_point() +
      geom_line()+
      xlab('timePointYears') + ylab('Loss (%)') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold")) 
  })
  

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



