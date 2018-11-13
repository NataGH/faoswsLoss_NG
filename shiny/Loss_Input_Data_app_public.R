#' Displays the existing data that is available for the loss model 
#' Additionally stores all other datasets that are available, including those not used by the model
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
library(rsconnect)
library(yaml)
library(shiny)
library(ggplot2)
library(grid)
library(dplyr)
library(DT)
library(magrittr)
library(data.table)

# ###----  Data In ----------############
##### Load Data ######
load("~/faoswsLossa/shiny/Input_Data/Inputs.RData")
settings <- yaml.load_file(file.path(paste(getwd(),"shiny", "Shiny.yml", sep='/')))
rsconnect::setAccountInfo(name = settings$shinyio$name, token= settings$shinyio$token, secret=settings$shinyio$secret)
rsconnect::deployApp(file.path(paste(getwd(),"shiny/Input_Data", sep='/')))

box <- {"position: relative; border-radius: 3px; background: #fff; border-top: 3px solid #d2d6de;margin-bottom: 20px;width: 100%;box-shadow: 0 1px 1px rgba(0,0,0,.1);"}
boxbody <- {"border-top-left-radius: 0; border-top-right-radius: 0;border-bottom-right-radius: 3px;border-bottom-left-radius: 3px;padding: 10px;"}

smallbox <- {"border-radius: 2px;position: relative;display: block;margin-bottom: 20px;"}
smallboxiconlrge <- {'position: absolute;top: auto;bottom: 5px;right: 5px;font-size: 20px;color: rgba(0, 0, 0, 0.15);'}

# ###----  Functions ----------############
html_boxesGen <- function(width,...){
  tags$div(class = paste("col-sm-", as.character(width),sep=""),  style= smallbox, 
           HTML(paste(...)))
}  
  
Htmlboxes <- function(title,width,status,...){
  if(status=="info"){
    tags$div(class = "container-fluid",
      tags$div(class = paste("col-sm-", as.character(width),sep=""), 
           tags$div(class = "panel panel-primary",
                    tags$div(class = "panel-heading",style='background: #00c0ef',title),
                    tags$div(class = "panel-body",HTML(paste(...)))
           )))
  }
  if(status=="bright"){
    tags$div(class = "container-fluid",
             tags$div(class = paste("col-sm-", as.character(width),sep=""), 
                      tags$div(class = "panel panel-primary",
                               tags$div(class = "panel-heading",style='background: #f39c12',title),
                               tags$div(class = "panel-body",HTML(paste(...)))
                      )))
  }

}

ValueBoxes <- function(titles,number){
  tags$div(class = paste("col-sm-", as.character(2),sep=""), style= paste(smallboxiconlrge,"background-color: #f39c12!important;") , 
           
    tags$div(class= "inner",     
      tags$h3(number),
      tags$p(titles)
      ),
    tags$div(class ="icon-large",
        tags$i(class="glyphicon glyphicon-chevron-right", 
                 style="position: relative; top: 1px; display: inline-block; font-family:'Glyphicons Halflings';
                       font-style: normal; font-weight: 400")
    ))
  
}
#ValueBoxes(titles,number)


# # # #-------------------------------------------------------


ui <- navbarPage(
  "Post-Harvest Loss Data, Studies and Resources",
  tabPanel("Main",
    fluidRow(
      sidebarPanel(
        sliderInput(
          inputId = "Year",
          label = "Year Range",
          value = c(1990,maxYear),step =1,sep = "", min = as.integer(min(na.omit(unique(LossFactorRaw$Year)))), max =  as.integer(max(na.omit(unique(LossFactorRaw$Year))))
        ),
        selectInput(
          inputId = "aggregation",
          label = "Aggregation",
          choices = c("WORLD",agglist),
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
          choices = c("All",na.omit(unique(gfli_basket[,"gfli_basket",with=FALSE]))), 
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
        
        checkboxInput("checkbox", "With Model Estimates", value = FALSE),
       
        
        #downloadButton("Data.csv", "Download"),
        downloadButton("ValueChain_Stages.jpeg", "Plots")
        
      ),
      mainPanel(
        fluidRow(
          Htmlboxes(title = "Introduction",width =12, status = "bright",
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
              )),
        fluidRow(
          column(3,
          html_boxesGen(
          ValueBoxes("harvest", '3.2')
          )
              
          )),
        fluidRow(
              plotOutput("PointGraphs")
          )

          

        
    ))
  ), 
   navbarMenu("Data",
     tabPanel("Filtered Data",
              fluidPage(title = "Data Available- Filtered from the previous page",
                 DT::dataTableOutput("DataTab")
                    
                )
              ))

  )
        
   

server = function(input, output, session) { 
  set.seed(122)
  observe({
    if (input$aggregation == "WORLD") {
      Agg_choices <- c("All")
      updateSelectInput(session, "Agg_options",choices=Agg_choices, selected ="All")
    }
    if (input$aggregation == "Country") {
      Agg_choices <- c("All",(unique(CountryGroup[,"SDG Regions",with=F])))
      updateSelectInput(session, "Agg_options", choices=Agg_choices, selected ="All")
    }else{
      Agg_choices <- c("All",unique(CountryGroup[[input$aggregation]]))
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
        ctry_choices <- c(unique(CountryGroup[geographicaream49 %in% LossFactorRaw$geographicaream49,"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = "Italy")
      }
      if(input$Agg_options != 'All'){
        ctry_choices <- c(unique(CountryGroup[(CountryGroup[["SDG Regions"]]   %in% input$Agg_options)&(geographicaream49 %in% LossFactorRaw$geographicaream49),"Country",with=F]))
        updateSelectInput(session, "Country", choices=ctry_choices, selected = NULL)
      }
    }else{
      ctry_choices <- c("All",unique(CountryGroup[CountryGroup[[input$aggregation]] ==input$Agg_options,"Country",with=F]))
      updateSelectInput(session, "Country", choices=ctry_choices, selected ="All")
    } 
    
    
  })
  
  observe({
    if (input$BasketItems == "All") {
      itemcpc_choices <- c("All", unique(FAOCrops[,'crop',with=F]))
      updateSelectInput(session, "itemcpc", choices=itemcpc_choices, selected ="All")
    }
    if (input$BasketItems != "All") {
      Comm_grp <- unlist(gfli_basket[gfli_basket == input$BasketItems,measureditemcpc])
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
                      (unlist(LossFactorRaw[[input$aggregation]]) %in%  unlist(input$Agg_options))
                    ,]
    }
    else{
      LossFactorRaw[Year %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  dataR1_agg <- reactive({
    #options(show.error.messages = FALSE)
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
  #### Modeled Estimates # --------
  data_L1 <- reactive({
    #options(show.error.messages = FALSE)
    if((input$aggregation == "Country")) {
      Losses %>% filter((Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                                 (Country %in% unlist(input$Country)) 
                               
      )
    }
    else if((input$aggregation != "WORLD") & (input$Agg_options != "All")){
      Losses[(Year %in% seq(input$Year[1],input$Year[2], by=1)) &
                      (unlist(Losses[[input$aggregation]]) %in%  unlist(input$Agg_options))
                    ,]
    }
    else{
      Losses[Year %in% seq(input$Year[1],input$Year[2], by=1),]
    }
  })
  #####

  dataR2 <- reactive({
    dataR1() %>% filter((measureditemcpc %in%
                           if(input$BasketItems %in%  'All'){unlist(unique(LossFactorRaw[,"measureditemcpc",with=F]))}
                         else if(any(input$itemcpc %in%  c('All'))){unlist(unique(gfli_basket[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
                         else if(!(is.null(input$itemcpc))){unlist(unique(dataR1()[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc",with=F])),"measureditemcpc",with=F]))}
                         else{unlist(unique(dataR1()[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
    ))
    
  })
  dataR2_agg <- reactive({
    dataR1_agg() %>% filter((measureditemcpc %in%
                               if(input$BasketItems %in%  'All'){unlist(unique(LossFactorRaw[,"measureditemcpc",with=F]))}
                             else if(any(input$itemcpc %in%  c('All'))){unlist(unique(gfli_basket[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
                             else if(!(is.null(input$itemcpc))){unlist(unique(LossFactorRaw[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc",with=F])),"measureditemcpc",with=F]))}
                             else{unlist(unique(LossFactorRaw[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
    ))
    
  })
  data_L2 <- reactive({
    data_L1() %>% filter((measureditemcpc %in%
                           if(input$BasketItems %in%  'All'){unlist(unique(Losses[,"measureditemcpc",with=F]))}
                         else if(any(input$itemcpc %in%  c('All'))){unlist(unique(gfli_basket[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
                         else if(!(is.null(input$itemcpc))){unlist(unique(data_L1()[measureditemcpc %in%  unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc",with=F])),"measureditemcpc",with=F]))}
                         else{unlist(unique(data_L1()[gfli_basket %in%  input$BasketItems,"measureditemcpc",with=F]))}
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
  data_L3 <- reactive({
    if(any(input$DataCollect %in% c('All'))){data_L2()}
    else if(!any(input$DataCollect %in% c('All'))){data_L2() %>% filter(data_L2()[['Data Collection Tag']] %in% input$DataCollect)}
    
    
  })
  dataR <- reactive({
    if(any(input$Stage %in% c('All'))){dataR3()}
    else if(!any(input$Stage %in% c('All'))){dataR3() %>% filter(fsc_location1 %in% input$Stage)}
    
    
  })
  dataR_agg <- reactive({
    if(any(input$Stage %in% c('All'))){dataR3_agg()}
    else if(!any(input$Stage %in% c('All'))){dataR3_agg() %>% filter(fsc_location1 %in% input$Stage)}
    
    
  })
  dataL <- reactive({
    if(any(input$Stage %in% c('All'))){data_L3 ()}
    else if(!any(input$Stage %in% c('All'))){data_L3 () %>% filter(fsc_location1 %in% input$Stage)}
    
    
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
  
  
  
  plotInput = function(modelest) {
    if(length(dataR())>1){


      
      
    if(modelest){
      ggplot(Losses[geographicaream49==204 & measureditemcpc %in% c("0111", "0112"),], aes(x = Year, y = loss_per_clean, colour = combo)) +
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
    }
      
    }else{
      print("Please Choose Another Option")
    }
    
  }
  
  
  
  output$PointGraphs <- renderPlot({
    plotInput(input$checkbox)
    
  })
  #### Downloadable csv of selected dataset ####
  
  # output$Data.csv <- downloadHandler(
  #   filename = function(){
  #     paste("Data", ".csv", sep = "")
  #   },S
  #   content = function(file) {
  #     write.csv(DataOutput(), file, row.names = FALSE, na = "")
  #   }
  # )
  # 
  
  
  output$ValueChain_Stages.jpeg <- downloadHandler(
    filename = function(){paste("ValueChain_Stages_",input$filename, ".jpeg", sep = "")},
    content = function(file) {
      ggsave(file, plot = plotInput(),   scale = .8, width = 450, height = 200, dpi = 300, units = "mm", device =  "jpeg")
    }
  )
  
  obsB <- observe({
    #print(plotInput())
    print( dataR())
    print( unlist(unique(FAOCrops[crop %in%  input$itemcpc,"measureditemcpc",with=F])))
    print( data_L2())
  })
  
  
}
shinyApp(ui = ui, server = server)
