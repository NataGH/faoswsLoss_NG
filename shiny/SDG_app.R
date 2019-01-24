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
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
library(data.table)



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
  selectedYear <- as.character(1991:2016)
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
CountryGroup$Country <- CountryGroup$m49_region

opt <- as.data.table(cbind( c("m49_code","iso2code","isocode","m49_region","sdgregion_code","sdg_regions","m49_level1_code",        
                               "m49_level1_region","m49_level2_code","m49_level2_region","mdgregions_code","mdgregions_region","ldcs_code","ldcs_region",                   
                               "lldcssids_code","lldcssids_region","fao_region","fao_operational_agg", "worldbank_income2018_agg", "sofa_agg"),
          c("m49_code","ISO2","ISO3","Country","sdgregion_code","SDG Regions","m49_level1_code",        
                 "Geographic Regions(m49) Level1","m49_level2_region_code","Geographic Regions(m49) Level2","mdgregions_code","MDG Regions","ldcs_code","Least Developed Countries (LDC)",                   
                 "lldcssids_code","Land Locked Developing Countries (LLDC)","FAO Operational Region","FAO Operational Coverage", "World Bank Income Groups", "SOFA Aggregations")))
names(opt) <- c("code", "Aggregates")


FAOCrops[, "crop" := FAOCrops$description]
FAOCrops[, "measureditemcpc" := addHeadingsCPC(FAOCrops$cpc)]


gfli_basket[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
names(fbsTree) <- tolower(names(fbsTree) )

names(Losses)[names(Losses) =="value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measureditemsuafbs"] <- "measureditemcpc"

# ProdQtySWS <- subset(prod_imports,
#                      select = c(keys_lower,"prod_imports")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])


ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- Base_Prod[timepointyears == as.numeric(BaseYear[2])-1,]
Base_Prod <- Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F]

#### Weights ####


## Production multiplied by the weighting scheme
FLIData <-  merge(Base_Prod,Weights, by = Weights_keys,all.x = F, all.y = F)
FLIData[, p0q0 := qty_avey1y2*value,]




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
                      choices = c("WORLD",opt[! Aggregates %in% grep("+_code", opt$Aggregates, value=TRUE),"Aggregates",with=F]),
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
                      choices = c("International Dollar Prices (2015)"),
                      #"Calories"
                      selected ="International Dollar Prices (2015)"
                    ),
                    selectInput(
                      inputId = "BasketChoice",
                      label = "Commoditiy Aggregation",
                      choices = c('Production Value- Top 10 by country (Default SDG)',
                                  'Production Value- Top 10 by World',
                                  'Top 10 Loss Commodities - by country'),
                               #   'Caloric Value- Top 10 by World'), 
                      selected ='Production Value- Top 10 by country' 
                    ),
                    selectInput(
                      inputId = "BasketItems",
                      label = "Basket Items",
                      choices = c("All",na.omit(unique(gfli_basket[,"gfli_basket",with=FALSE]))), 
                      selected = "All"
                    ),
                    selectInput(
                      inputId = "itemcpc",
                      label = "measureditemcpc",
                      choices = NULL, selected =NULL, multiple=TRUE, selectize=TRUE
                    ),
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
  
  agg <-reactive({
    opt[input$aggregation %in% opt$Aggregates ,"code",with=F]
  })
  
  observe({
    if (input$aggregation == "WORLD") {
      Agg_choices <- c("All")
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Country") {
      Agg_choices <- c("All",(unique(CountryGroup[,"sdgregion_code",with=F])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }else{
      Agg_choices <- c("All",unique(CountryGroup[, unlist(opt[input$aggregation == opt$Aggregates ,"code",with=F]),with=F]))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }
    
  })
  observe({
    if (input$aggregation == "WORLD") {
      ctry_choices <- c("All")
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    }
    if (input$aggregation == "Country") {
      if(input$Agg_options == 'All'){
        ctry_choices <- c(unique(CountryGroup[m49_code %in% DataForIndex$geographicaream49,"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "Italy")
      }
      if(input$Agg_options != 'All'){
        ctry_choices <- c(unique(CountryGroup[(CountryGroup[, unlist(opt[input$aggregation == opt$Aggregates ,"code",with=F]),with=F]   %in%  input$Agg_options)&(geographicaream49 %in% DataForIndex$geographicaream49),"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = NULL)
      }
      #input$Agg_options
     }
    #  else{
    #   ctry_choices <- c("All",unique(CountryGroup[CountryGroup[, unlist(opt[input$aggregation == opt$Aggregates ,"code",with=F]),with=F] == input$Agg_options,"Country",with=F]))
    #   updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    # }


  })
 # input$Agg_options
  ### R Check##
  obsB <- observe({
    print(  input$aggregation )
    print(input$Agg_options)
    print(CountryGroup[, unlist(opt[input$aggregation == opt$Aggregates ,"code",with=F]),with=F] %in% "Central Asia (M49) and Southern Asia (MDG=M49)")

  })
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)




