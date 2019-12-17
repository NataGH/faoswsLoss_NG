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

library(faosws)
library(faoswsUtil)
library(faoswsLoss)
library(shiny)
library(shinythemes)
library(rmarkdown)
library(gtools)
library(ggplot2)
library(dplyr)
library(dtplyr)
library(DT)
library(magrittr)
remove.packages(pkgs, lib, version)

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
Losses <- getLossData_LossDomain(areaVar,itemVar,yearVar,elementVar,selectedYear,'5126')
production <- getProductionData(areaVar,itemVar,yearVar,elementVar) # Value_measuredElement_5510
fbsTree <- ReadDatatable("fbs_tree")
CountryGroup <- ReadDatatable("a2017regionalgroupings_sdg_feb2017")
FAOCrops <- ReadDatatable("fcl2cpc_ver_2_1")

names(CountryGroup)[names(CountryGroup) =="countryname"] <- "Country"
names(CountryGroup)[names(CountryGroup) =="m49code"] <- "geographicaream49"
CountryGroup[,c("iso2code","isocode") :=NULL]

names(Losses)[names(Losses) =="Value"] <- "value_measuredelement_5126"
names(Losses)[names(Losses) =="measuredItemSuaFbs"] <- "measureditemcpc"
names(Losses) <- tolower(names(Losses))
names(production) <- tolower(names(production))
names(fbsTree)[names(fbsTree)== "id3"] <- "foodgroupname"
names(fbsTree)[names(fbsTree)== "measureditemsuafbs"| names(fbsTree)== "item_sua_fbs" ] <- "measureditemcpc"
FAOCrops[, "crop" := FAOCrops$description]
names(FAOCrops)[names(FAOCrops) =='cpc'] <- "measureditemcpc"

#SDG Headings
fbsTree[foodgroupname %in% c(2905,2911), gfli_basket :='Cereals & Pulses',]
fbsTree[foodgroupname %in% c(2919,2918), gfli_basket :='Fruits & Vegetables',]
fbsTree[foodgroupname %in% c(2907,2913), gfli_basket :='Roots, Tubers & Oil-Bearing Crops',]
fbsTree[foodgroupname %in% c(2914,2908,2909,2912,2922,2923), gfli_basket :='Other',]
fbsTree[foodgroupname %in% c(2943, 2946,2945,2949,2948), gfli_basket :='Animals Products & Fish and fish products',] # |foodGroupName == "PRODUCTS FROM FISH",

###Weight Options
intPrice2005 <-  ReadDatatable("int_$_prices_2005")
pvail <- unique(intPrice2005$itemcode)
intPrice2005Selected =
  intPrice2005 %>%
  select(itemcode,itemname, value) %>%
  filter(itemcode %in% as.numeric(unlist(na.omit(pvail))))

intPrice2005Selected$intprice <- intPrice2005Selected$value
#intPrice2005Selected$measureditemfclname <- intPrice2005Selected$ItemName

intPrice2005Selected$measureditemfcl <- addHeadingsFCL(intPrice2005Selected$itemcode)
intPrice2005Selected$measureditemcpc <- fcl2cpc(intPrice2005Selected$measureditemfcl,version = "2.1")
intPrice2005Selected$itemname <- tolower(intPrice2005Selected$itemname)

############ Index Calculations #############
ProdQtySWS <- subset(production,
                     select = c(keys_lower,"value_measuredelement_5510")) %>% filter(timepointyears >= BaseYear[1] & timepointyears <= BaseYear[2])

Base_Prod <- ProdQtySWS[,qty_avey1y2 := mean(value_measuredelement_5510),by = c("geographicaream49",'measureditemcpc')]
Base_Prod <- unique(Base_Prod[,c("geographicaream49",'measureditemcpc','qty_avey1y2'),with=F])



# # #----  Data In ------------------------------------------
#GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,BaseYear,keys_lower,"WORLD",weights,basketn,FLIData)

ui <- fluidPage(theme = shinytheme("lumen"),
                sidebarLayout(
                  sidebarPanel(
                    #Input()
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
                      inputId = "WeightsChoice",
                      label = "Weights",
                      choices = c("intl_prices"), 
                      selected ="intl_prices"
                    ),
                    selectInput(
                      inputId = "BasketChoice",
                      label = "Commoditiy Aggregation",
                      choices = c('Production Value- Top 10 by country','Production Value- Top 10 by World'), 
                      selected ='Production Value- Top 10 by country' 
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
                    selectInput("dataset", "Choose a dataset for download:",
                                choices = c("Index", "Weights", "Basket")),
                   
                    downloadButton("Data.csv", "Download"),
                    downloadButton("SDG12_3_Plot.pdf", "Plots")
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
  
  
  
  Weights <- reactive({
    if(input$WeightsChoice == "intl_prices"){
    intPrice2005Selected[, c("itemname","measureditemcpc","intprice")]
    }
  
  })
  DataForIndex <- reactive({
    Base_Prod1 <- merge(Base_Prod,Weights(), by.x = c('measureditemcpc'), by.y = c('measureditemcpc'),all.x = F, all.y = F)
    Base_Prod1[, p0q0 := qty_avey1y2*intprice,]

    DataForIndex <- join(Losses[,c(keys_lower,"value_measuredelement_5126"),with=F] , Base_Prod1, by =c("measureditemcpc","geographicaream49"),type= 'left', match='all')
    DataForIndex$l0ptqt =0
    DataForIndex[,l0ptqt:=value_measuredelement_5126*p0q0,with=T]
    
    DataForIndex <- join(DataForIndex ,fbsTree , by = c('measureditemcpc'),type= 'left', match='all')
    DataForIndex <- join(DataForIndex,CountryGroup, by = c('geographicaream49'),type= 'left', match='all')
    
    
  })
  Top10Global <- reactive({ 
  Top10Global<-  DataForIndex()  %>%
    filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
    group_by(measureditemcpc,gfli_basket) %>%
    dplyr:: summarise(All_p0q0 = sum(p0q0, na.rm = TRUE)) %>%
    arrange(-All_p0q0)
  })
  Basket <- reactive({
    if(input$BasketChoice == 'Production Value- Top 10 by country'){
      Top10perctry <- DataForIndex() %>%
        filter(timepointyears == as.numeric(BaseYear[2])-1) %>%
        arrange(geographicaream49, -p0q0) 
      
      basket <- Top10perctry[ ,head(.SD, 2), by= c('geographicaream49','gfli_basket')]
      basket <- basket %>% filter(!is.na(gfli_basket))
      basketKeys <- c('geographicaream49', "measureditemcpc")
      ComBasketN  <- 'Production Value- Top 10 by country'
      basket[,basketname := ComBasketN]
      basket <-basket[,unique(c("basketname","Country", basketKeys, "gfli_basket", "intprice","p0q0",names(CountryGroup)[seq(2,length(names(CountryGroup)), by=2)][-1])),with=FALSE]

    }
    if(input$BasketChoice == 'Production Value- Top 10 by World'){
      ItemsBasket <- Top10Global()[ ,head(.SD, 2), by= c('gfli_basket')]
      ItemCPCS <- ItemsBasket[!is.na(gfli_basket),measureditemcpc]
      basket <-Top10Global()[measureditemcpc %in%  ItemCPCS,]
      basketKeys <- ( "measureditemcpc")
      ComBasketN  <- 'Production Value- Top 10 by World'
      basket[,basketname := ComBasketN]
    }
    if(input$BasketChoice == 'Caloric Value- Top 10 by World'){
     ###
    }
    basket
    })
  basketKeys <- reactive({
    c('Caloric Value- Top 10 by World')
    if(input$BasketChoice == 'Production Value- Top 10 by country'){
      basketKeys <- c('geographicaream49', "measureditemcpc")
    }
    if(input$BasketChoice == 'Production Value- Top 10 by World'){
      basketKeys <- ( "measureditemcpc")
    }
    basketKeys
  })
  
  FLIndex <- reactive({
    FLIndex <- GFLI_SDG_fun(BaseYear,keys_lower,input$aggregation,Basket(),basketKeys(),DataForIndex())
    FLIndex[,c("Sum_p0qt","Sum_p0q0"):=NULL]
  })

  
  dataR <- reactive({
    if((input$aggregation == "Country")) {
      FLIndex()%>% filter((timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                            (Country %in% unlist(input$Country))
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      FLIndex()[(timepointyears %in% seq(input$Year[1],input$Year[2], by=1)) &
                (unlist(FLIndex()[, input$aggregation, with=F]) %in%  unlist(input$Agg_options))
    ,]
    }
    else{
      FLIndex()[timepointyears %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  
  plotInput = function() {
    if(input$aggregation != "Country"){
    ggplot(dataR(), aes(x = timepointyears, y = Index)) +
      facet_wrap(~ unlist(unique(dataR()[,input$aggregation,with=F])))+
      geom_line()+
      xlab('timePointYears') + ylab('FLI') +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
      theme(axis.text=element_text(size=12, face="bold"),
            axis.title=element_text(size=12,face="bold"))
    }else{
      ggplot(dataR(), aes(x = timepointyears, y = Index)) +
        facet_wrap(~ unlist(unique(input$Country)))+
        geom_line()+
        xlab('timePointYears') + ylab('FLI') +
        theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
        theme(axis.text=element_text(size=12, face="bold"),
              axis.title=element_text(size=12,face="bold"))
    }
    
  }


  WeightAgg = function() {
    Wt = merge(Weights(),fbsTree[,c("measureditemcpc" , "gfli_basket")], by = ("measureditemcpc"), all.x =T, all.y=F)
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
  output$WeightsOut = DT::renderDataTable({
    WeightAgg()
  })
  output$BasketsOut = DT::renderDataTable({
    BasketAgg()
      })
  output$IndexOut = DT::renderDataTable({
    IndexAgg()
  })
  
  #### Downloadable csv of selected dataset ####
  datasetInput <- reactive({
    switch(input$dataset,
           "Index" = FLIndex(),
           "Weights" = WeightAggre(),
           "Basket" = Basket()
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
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  output$SDG12_3_Plot.pdf <- downloadHandler(
    filename = function(){paste("SDG12_3_Plot_",input$filename, ".pdf", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInput(),  width = 10, height = 8, dpi = 150, units = "in", device =  "pdf")
    }
  )
  

  
  ### R Check##
  obsB <- observe({
    print(FLIndex())
    
    print(  dataR())
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


