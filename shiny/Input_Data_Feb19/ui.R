#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# token <- readRDS("token.rds")
# # Then pass the token to each drop_ function
# drop_acc(dtoken = token)
# drop_download("data_InternalFAO_jan.RData" , path = "shiny", overwrite = TRUE)

load("Data_In.RData")
# Define UI for application that draws a histogram

shinyUI(fluidPage(theme = shinytheme("lumen"),
                  sidebarLayout(
                    sidebarPanel(
                      #Input()
                      sliderInput(
                        inputId = "Year",
                        label = "Year Range",
                        value = c(2005,maxYear),step =1,sep = "", min = as.integer(min(selectedYear)), max =  as.integer(max(selectedYear))
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
                       # condition = "input.Model_Level == 'SDG-Food Loss Percentage'",
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
                        checkboxInput("checkbox", label = "Only Top 10 SDG Baskets", value = FALSE),
                        checkboxInput("checkboxofficial", label = "Official Data Only", value = FALSE),
                        checkboxInput("checkboxflags", label = "Show Flags", value = FALSE),
                        checkboxInput("checkboxstage", label = "By Stage of Value Chain", value = FALSE),
                        #checkboxInput("checkbox_input", label = "Add Input Data", value = FALSE),
                        #checkboxInput("checkbox_Markovs", label = "Add Input Data- Aggregated", value = FALSE),
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
                      
                      # downloadButton("Data.csv", "Download"),
                      #downloadButton("SDG12_3_Plot.jpeg", "Plots")
                      downloadButton("FLP.xlsx", "FLP")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        tabPanel("HeatMap",
                                 tags$p(""),
                                 tags$p("Food Loss Percentage"),
                                 plotlyOutput("plot_ly_heat")
                                 
                        ),
                        tabPanel("Graph",
                                 tags$p(""),
                                 tags$p("Food Loss Percentage"),
                                 plotlyOutput("plot_ly")
                                 
                        ),
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
