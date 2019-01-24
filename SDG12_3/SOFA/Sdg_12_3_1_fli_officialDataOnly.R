

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

fli_officialDataOnly <- read_excel("~/faoswsLossa/SDG12_3/SOFA/Sdg_12_3_1_fli_officialDataOnly.xlsx")
fli_officialDataOnly <-fli_officialDataOnly %>%
                 arrange( region_name,timepointyears)

f <- list(
  family = "Times New Roman",
  size = 12,
  face="bold",
  color = "#7f7f7f"
)

data <- fli_officialDataOnly %>% filter(region_code == 1)

p <-plot_ly(y=data$FLP, x=data$timepointyears, mode="lines")
p %>%layout(
    xaxis = list(
      title = "Years",
      titlefont = f,
      range = c(seq(1990,2016, by=1)),
      dtick = 1
      
    ),
    yaxis = list(
      title = paste0(c(rep("&nbsp;", 20),
                       "Percentage",
                       rep("&nbsp;", 20),
                       rep("\n&nbsp;", 3)),
                     collapse = ""),
      tickformat = ".2%",
      titlefont = f
    ),
    margin = list(l =80, r = 50, t = 20, b = 100),
  
    autosize = TRUE,
    legend = list(orientation = 'l')
  )

