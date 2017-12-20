#' Module for estimating Global Food Loss Index
#' @docType package
#' @name faoswsLoss-package
#' @aliases faoswsLoss
#' 
#' @author Alicia English Marco Mingione 
#' 
#' 
######### Load all libraries ###########
#install.packages('faosws')
#install_github(repo = "SWS-Methodology/faoswsFlag")
#install_github(repo = "SWS-Methodology/faoswsUtil")
#install.packages("faoswsLoss", repo = "http://hqlprsws1.hq.un.fao.org/fao-sws-cran/") 


library(XML)
library(httr)
library(stats4)
library(plm)
library(gtools)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(rpart)
library(gridExtra)
library(scales)
library(reshape)
library(devtools)
library(jsonlite)
library(readr)

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


##################### For deletion #####################################
## For Local 
## SWS Connection

githubsite <- '~/faoswsLoss/data-raw/'
dirmain <-  '~/faoswsLoss'
SetClientFiles(dir = "C:\\Users\\ENGLISHA\\Documents\\certificates\\qa")
files = dir("~/Github/faoswsLoss/R",
            full.names = TRUE) 

token = "84fdac88-f975-4f81-95a1-7dd3cbfdedc5" #Production 2004-06
token2 = '72832c23-6650-4454-ac7e-a2d1d926353a' #Loss Data
token3 = "1fff0cf2-0d75-46df-a63d-d99744f8f927"


GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token3
) 


######### Options ############
GFLI_calc <- 1
aggregation <- "geographicAreaM49" #"sdg_region", "geographicAreaM49"
weights <- "intl_prices" 
basketN <- "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"
#####################


if(GFLI_calc){
  GlobalfoodLoss <- GFLI_SDG_fun(keys_lower,aggregation,weights,basketN,production)
  
  ### Table for   
  FLI_13 <- foodLossIndex %>% filter(timepointyears == 2013)
  FLI_13[, FLI_13:= GFLI_N]
  FLI_15 <- foodLossIndex %>% filter(timepointyears == 2015)
  FLI_15[, FLI_15:= GFLI_N]
  delta13_15 <- merge(FLI_13,FLI_15, by= c("geographicaream49"),   all.x = TRUE)
  delta13_15[,delta13_15:= FLI_15/FLI_13]
  delta13_15[, c("geographicaream49","delta13_15" )]
  date = "_18Dec17"
  write.table(delta13_15,paste(githubsite, 'General/delta13_16',date, '.csv', sep=''),sep=',' )
  
  write.table(foodLossIndex,paste(githubsite, 'General/foodLossIndex',date, '.csv', sep=''),sep=',' )
  write.table(   basket ,paste(githubsite, 'General/Commodbasket',date, '.csv', sep=''),sep=',' )
  write.table(GFLI_Weight,paste(githubsite, 'General/weights',date, '.csv', sep=''),sep=',' )
  
  
}  
