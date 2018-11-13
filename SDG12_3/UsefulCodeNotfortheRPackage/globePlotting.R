### Global Plot of Data
library(plotly)
library(dplyr)
library(data.table)


df <- as.data.table(read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv'))
df[,GDP..BILLIONS. := NULL]
CtryWork_SDG123 <- read.csv("~/faoswsLossa/data-raw/CtryWork_SDG123.csv")
MapMe <- merge(df,CtryWork_SDG123, by.x= c("CODE"), by.y = c("isocode"))
MapMe[is.na(PresentSDG_0),"PresentSDG_0"] = 0 
ld<- MapMe %>%
  group_by(PresentSDG) %>%
  dplyr:: summarise(Count = length(m49code))
ld<-as.data.table(ld)
ld<-ld[PresentSDG != "",]
MapMe2 <- MapMe[PresentSDG != 0,]
# light grey boundaries
l <- list(color = "grey", width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

colorsB <- c('rgb(247,251,255)','rgb(172,208,230)','rgb(57,135,192)', 'rgb(115,172,211)')


p <- plot_geo(MapMe) %>%
  add_trace(z = ~PresentSDG_0, color = ~PresentSDG_0,  colors = 'Blues',
            text = ~COUNTRY, locations = ~CODE,  showscale = F,  reversescale = F, marker = list(line = l)) %>%
  
  add_trace(data=MapMe[PresentSDG == ld[1,"PresentSDG"],],z = ~PresentSDG_0, color = ~PresentSDG_0,  colors = 'Blues',
            text = ~COUNTRY, locations = ~CODE,  showscale = F,  reversescale = F, marker = list(line = l)) %>%
  
  
  add_trace(data=MapMe[PresentSDG == "Presentation",],z = ~PresentSDG_0, color = ~PresentSDG_0,  colors = 'Blues',
            text = ~COUNTRY, locations = ~CODE,  showscale = F,  reversescale = T, marker = list(line = l)) %>%
  
  
  add_markers(
    data = ld[1], x = ~PresentSDG,y = ~Count , color =~PresentSDG, 
    marker = list(
      color = colorsB[2],
      size = 5,      
      line = list(
        color = colorsB[2],
        width = 5
      )
  ))%>%
  add_markers(
    data = ld[3], x = ~PresentSDG,y = ~Count , color =~PresentSDG, 
    marker = list(
      color = colorsB[3],
      size = 5,      
      line = list(
        color = colorsB[3],
        width = 5
      )
    ))%>%
  add_markers(
    data = ld[2], x = ~PresentSDG,y = ~Count , color =~PresentSDG, 
    marker = list(
      color = colorsB[4],
      size = 5,      
      line = list(
        color = colorsB[4],
        width = 5
      )
    ))%>% 
  layout(title = '',legend = list(x = 100, y = 0.5,  bgcolor = "#E2E2E2",
                                  bordercolor = "#FFFFFF"), geo = g)

print(p)

