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




library(data.table)
library(plyr)
library(dplyr)
library(dtplyr)
library(devtools)

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


token3 = "ec71e788-c7fa-44bc-a732-278d84f071fd"


GetTestEnvironment(
  baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
  token = token3
) 

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
}

######### Options ############
selectedYear = as.character(1991:2015)
ComparisonYear = as.character(c(2013,2015))

GFLI_calc <- 1
aggregation <- "geographicAreaM49" #"sdg_region", "geographicAreaM49"
weights <- "intl_prices" 
basketN <- "top2perhead_byCtry" # "top2perhead_Globatop10","top2_calories"
#####################

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
keys =c(areaVar,yearVar,itemVar)
keys_lower =tolower(keys)


if(GFLI_calc){
  GlobalfoodLoss <- GFLI_SDG_fun(selectedYear,keys_lower,aggregation,weights,basketN,production)
  
  ### Table for   
  FLI_y1 <- foodLossIndex %>% filter(timepointyears == min(ComparisonYear))
  FLI_y1 [, FLI_y1 := GFLI_N]
  FLI_y2  <- foodLossIndex %>% filter(timepointyears ==  max(ComparisonYear))
  FLI_y2[, FLI_y2:= GFLI_N]
  deltay1_y2 <- merge(FLI_y1,FLI_y2, by= c("geographicaream49"),   all.x = TRUE)
  deltay1_y2[,deltay1_y2:= FLI_y2/FLI_y1]
  deltay1_y2[, c("geographicaream49","deltay1_y2" )]

  
}  

date = "_18Dec17"
write.table(delta13_15,paste(githubsite, 'General/delta13_16',date, '.csv', sep=''),sep=',' )
write.table(foodLossIndex,paste(githubsite, 'General/foodLossIndex',date, '.csv', sep=''),sep=',' )
write.table(   basket ,paste(githubsite, 'General/Commodbasket',date, '.csv', sep=''),sep=',' )
write.table(GFLI_Weight,paste(githubsite, 'General/weights',date, '.csv', sep=''),sep=',' )
