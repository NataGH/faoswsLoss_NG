#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(DT)
library(magrittr)
library(data.table)
library(plotly)



#### Need to change out the xlsx for openxlsx


# token <- readRDS("token.rds")
# # Then pass the token to each drop_ function
# drop_acc(dtoken = token)
# drop_download("data_InternalFAO_jan.RData" , path = "shiny", overwrite = TRUE)

#load("~/faoswsLossa/shiny/External_web/Visualization/data_InternalFAO_marc26.RData")

load("data_InternalFAO_marc26.RData")

## Pretify the data ###
dataStages <- c(na.omit(dataStages[!dataStages %in% c("SWS_Total", "WholeSupplyChain", 'Havest',"Trader","Farm")]),"Total Supply Chain Estimate", "Producer")
dataStages <- sort(dataStages)

datatags <-  c(na.omit(datatags[!datatags %in% c("FBS/APQ", "LitReview", "NationalStatsYearbook", "SWS", "NonProtected", "NP", "", "NationalAcctSys","Crop-Cutting")]),
               "Food Balance Sheet/Ag. Production Questionnaire", 
               "Secondary Sources cited in Documents","FAO Sources","National Statistics Yearbook","National Acctounts")

datatags <- sort(datatags)



opt2 <- c("Country (M49)" = "m49_code", "SDG Region" = "sdgregion_code", "Geographic Region (M49-L1)" ="m49_level1_code", "Geographic Region (M49-L2)" ="m49_level2_code", 
          "MDG Regions" ="mdgregions_code", "Least Developed Countries" ="ldcs_code"  , "Land Locked Developing Countries" = "lldcssids_code", 
          "FAO Operational Countries" ="fao_operational_agg","World Bank Income Groups (2018)" ="worldbank_income2018_agg", 
          "FAO SoFA Aggregates" ="sofa_agg")


InputData_Out[fsc_location1 == "Havest", "fsc_location1" ] <- "Harvest"
InputData_Out[fsc_location1 == "Trader", "fsc_location1" ] <- "Traders"
InputData_Out[fsc_location1 == "Farm", "fsc_location1" ] <- "Producer"
InputData_Out[fsc_location1 == "SWS_Total", "fsc_location1" ] <- "Total Supply Chain Estimate"
InputData_Out[fsc_location1 == "WholeSupplyChain", "fsc_location1" ] <- "Total Supply Chain Estimate"


InputData_Out[tag_datacollection == "FBS/APQ", "tag_datacollection" ] <- "Food Balance Sheet/Ag. Production Questionnaire"
InputData_Out[tag_datacollection == "LitReview", "tag_datacollection" ] <- "Secondary Sources cited in Documents"
InputData_Out[tag_datacollection == "SWS", "tag_datacollection" ] <- "FAO Sources"
InputData_Out[tag_datacollection == "NationalStatsYearbook", "tag_datacollection" ] <- "National Statistics Yearbook"
InputData_Out[tag_datacollection == "NonProtected", "tag_datacollection" ] <- "FAO Sources"
InputData_Out[tag_datacollection == "NP", "tag_datacollection" ] <- "Secondary Sources cited in Documents"
InputData_Out[tag_datacollection == "", "tag_datacollection" ] <- "-"
InputData_Out[tag_datacollection == "NationalAcctSys", "tag_datacollection" ] <- "National Acctounts"
InputData_Out[tag_datacollection == "Crop-Cutting", "tag_datacollection" ] <- "Crop Cutting Field Experiment"
InputData_Out[reference == "SWS", "reference"] <- "FAO Sources"
InputData_Out[grep("WRAP",reference), "reference"] <- "WRAP, 2011"

# Define UI for application that draws a histogram

shinyUI(fluidPage(sidebarLayout(
                    sidebarPanel(
                      #Input()
                      sliderInput(
                        inputId = "Year",
                        label = "Year Range",
                        value = c(2000,maxYear),step =1,sep = "", min = as.integer(min(ConvFactor1$timepointyears, na.rm=T)), max =  as.integer(max(selectedYear))
                      ),
                      # textInput(inputId ="min_ax",
                      #           label = "min Y-Axis",
                      #           value = "Enter decimal"),
                      # textInput(inputId ="max_ax",
                      #           label = "max Y-Axis",
                      #           value = "Enter decimal"),
                    
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
                        inputId = "Model_Level",
                        label = "Model level",
                        choices = c("Input Data"),
                        selected = "Input Data",
                        multiple = FALSE,
                        selectize=TRUE
                      ),
                      conditionalPanel(
                        condition = "input.Model_Level == 'SDG-Food Loss Percentage'",
                        "SDG Related",
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
                      )),
                      conditionalPanel(
                        condition = "input.Model_Level != 'SDG-Food Loss Percentage'",
                      "Estimates Related",
                      checkboxInput("checkbox_basket", label = "Only Top 10 SDG Baskets", value = FALSE),
                      checkboxInput("checkboxflags", label = "Show Data Collection Tags", value = FALSE),
                     # checkboxInput("checkbox_input", label = "Add Input Data", value = FALSE),
                      #checkboxInput("checkbox_Markovs", label = "Add Input Data- Aggregated", value = FALSE),
                     selectInput(
                       inputId = "BasketChoice",
                       label = "Commoditiy Basket Aggregation",
                       choices = c('Production Value- Top 10 by country (Default SDG)'), 
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
                        choices = NULL, selected ="All", multiple=TRUE, selectize=TRUE
                      ),
                      conditionalPanel(
                        condition = "input.Model_Level == 'Input Data'",
                      selectInput(
                        inputId = "Stage",
                        label = "Value Chain Stage(s)",
                        choices = c("All",dataStages), selected ="All", multiple=TRUE, selectize=TRUE
                      ),
                      selectInput(
                        inputId = "DataCollect",
                        label = "Method of Data Collection",
                        choices = c("All",datatags) , selected ="All", multiple=TRUE, selectize=TRUE
                      ))),

                      #selectInput("dataset", "Choose a dataset for download:",
                      #            choices = c("Index", "Weights", "Basket")),
                      
                      downloadButton("Data.csv", "Download Data"),
                      downloadButton("SDG12_3_Plot.jpeg", "Plots")
                     #downloadButton("FLP.xlsx", "FLP")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        # tabPanel("Graph",
                        #       tags$p(""),
                        #       tags$p("Food Loss Percentage by Value fo Domestic Production"),
                        #       plotlyOutput("plot_ly")
                        # 
                        # ),
  
                        # tabPanel("Weights",
                        #          DT::dataTableOutput("WeightsOut")
                        # ),
                        # tabPanel("Basket",
                        #          DT::dataTableOutput("BasketsOut")
                        # ),
                        navbarMenu("Data",
                                   tabPanel("Filtered Data",
                                            fluidPage(title = "Data Available- Filtered from the previous page",
                                                      DT::dataTableOutput("DataTab")
                                                      
                                            )
                                   ))
                      )
                )
                  ))
)
