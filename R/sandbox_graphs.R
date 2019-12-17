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

markov_stageEst <- FSC_Markov(RawData=ConvFactor2,opt="aveatFSP",modelEst=T,selectedYear,CountryGroup,fbsTree)
markovW <- FSC_Markov(RawData=ConvFactor2,opt="aveatFSP",modelEst=T,selectedYear,CountryGroup,fbsTree)
markovWo <- FSC_Markov(RawData=ConvFactor2,opt="aveatFSP",modelEst=F,selectedYear,CountryGroup,fbsTree)

markov <- markov[timepointyears < 2020 & fsc_location != "sws_total",]

rawData <- merge(ConvFactor2[timepointyears < 2020 & fsc_location != "sws_total",],fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)
Stagests <- merge(markov_stageEst[timepointyears < 2020 & fsc_location1 != "sws_total",],fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)



Graphs_withoutmodel <- merge(markovWo[timepointyears < 2020 & fsc_location != "sws_total",],fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)

Graphs_withmodel <- merge(markovW[timepointyears < 2020 & fsc_location != "sws_total",],fbsTree[,c("measureditemcpc","gfli_basket"),with=F], by = c("measureditemcpc"), all.x = TRUE)


p <-plot_ly(Graphs_withmodel, y=~loss_per_clean, x= ~timepointyears, type="box", mode="markers", color=~gfli_basket) %>% layout(title= "Modeled Stages then aggregated")
p
q <-plot_ly(Graphs_withoutmodel, y=~loss_per_clean, x= ~timepointyears, type="box", mode="markers", color=~gfli_basket) %>% layout(title= "Stages only aggregated")
q

s <-plot_ly(rawData, y=~loss_per_clean, x= ~timepointyears, type="box", mode="markers", color=~gfli_basket)  %>% layout(title= "Raw Data")
s
t <-plot_ly(Stagests, y=~loss_per_clean, x= ~timepointyears, type="box", mode="markers", color=~gfli_basket)  %>% layout(title= "Stage estimates")
t