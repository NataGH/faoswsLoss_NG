## ----load-libraries, cache=FALSE, echo=FALSE-----------------------------
## Load libraries
library(faoswsFlag)
library(faoswsUtil)
library(FAOSTAT)
library(data.table)
library(lattice)
library(splines)
library(RColorBrewer)

## time = 1:1000
## myBs = bs(time, df = 20, degree = 10, intercept = TRUE)
## plot.new()
## plot.window(xlim = c(0, max(time)), ylim = c(0, 1))
## for(i in 1:ncol(myBs)){
##     lines(time, myBs[, i], col = rgb(i/ncol(myBs), 0, 0))
## }


## time = 1:1000
## myBs = bs(time, df = 10, degree = 1, intercept = TRUE)
## ## plot.new()
## ## plot.window(xlim = c(0, max(time)), ylim = c(0, 1))
## par(mfrow = c(ncol(myBs), 1), mar = c(0, 5, 0, 0))
## for(i in 1:ncol(myBs)){
##     plot(time, myBs[, i], col = rgb(i/ncol(myBs), 0, 0), axes = FALSE,
##          ylab = paste0("Spline ", i), type = "l", lwd = 2)
## }

## Function to compute the yield as a ratio of production and area harvested.
computeRatio = function(numerator, denominator){
    as.numeric(ifelse(numerator == 0 | denominator == 0, NA,
                      numerator/denominator))
}

## A temporary flag table for the old flag system
oldFlagTable = 
    data.frame(flagObservationStatus = 
               c(" ", "*", "T", "P", "E", "F", "C", "M"),
               flagObservationWeights = 
               seq(1, 0, length = 8), 
               stringsAsFactors = FALSE)

## temporary function to read the data
read.example = function(file){
    tmp = read.csv(file = file, stringsAsFactors = FALSE)
    tmp2 = data.table(merge(tmp, 
        FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")], 
        by.x = "areaCode", by.y = "FAOST_CODE"))
    setnames(tmp2, old = "FAO_TABLE_NAME", new = "areaName")
    tmp2[, productionSymb := ifelse(productionSymb == "", " ", productionSymb)]
    tmp2[, areaHarvestedSymb := ifelse(areaHarvestedSymb == "", " ",
                                       areaHarvestedSymb)]
    tmp2[productionSymb == "/", productionSymb := " "]
    tmp2[is.na(productionSymb), productionSymb := "M"]
    tmp2[productionSymb %in% c("P", "X", "*"), productionSymb := "T"]
    tmp2[productionSymb %in% c("C", "F"), productionSymb := "E"]
    tmp2[is.na(areaHarvestedSymb), areaHarvestedSymb := "M"]
    tmp2[areaHarvestedSymb == "/", areaHarvestedSymb := " "]
    tmp2[areaHarvestedSymb %in% c("P", "X", "*"), areaHarvestedSymb := "T"]
    tmp2[areaHarvestedSymb %in% c("C", "F"), areaHarvestedSymb := "E"]

    tmp2[, methodFlag := ifelse(productionSymb == "M", "u", "")]
    removeImputation(data = tmp2, value = "productionValue", 
                     observationFlag = "productionSymb",
                     methodFlag = "methodFlag",
                     imputedFlag = c("E", "T", "F"),
                     missingObservationFlag = "M")
    tmp2[, methodFlag := ifelse(areaHarvestedSymb == "M", "u", "")]
    removeImputation(data = tmp2, value = "areaHarvestedValue", 
                     observationFlag = "areaHarvestedSymb",
                     methodFlag = "methodFlag",
                     imputedFlag = c("E", "T", "F"),
                     missingObservationFlag = "M")
    remove0M(data = tmp2, value = "productionValue", 
             flag = "productionSymb", naFlag = "M")
    remove0M(data = tmp2, value = "areaHarvestedValue", 
             flag = "areaHarvestedSymb", naFlag = "M")    
    tmp2[, yieldValue := 
         computeRatio(productionValue, areaHarvestedValue)]
    setkeyv(tmp2, c("areaCode", "year"))
    tmp2[, yieldSymb := 
         aggregateObservationFlag(productionSymb, 
                                  areaHarvestedSymb,
                                  flagTable = oldFlagTable)]
    removeNoInfo(tmp2, value = "yieldValue", observationFlag = "yieldSymb",
                 byKey = "areaCode")
    tmp2[areaHarvestedSymb == "/", areaHarvestedSymb :=" "]
    tmp2[is.na(areaHarvestedSymb), areaHarvestedSymb :="M"]
    tmp2[is.na(productionSymb), productionSymb :="M"]
    tmp2[productionSymb == "X", productionSymb == "*"]
    tmp2[areaHarvestedSymb == "X", areaHarvestedSymb == "*"]
    tmp2[, productionSymb2 := ""]
    tmp2[, areaHarvestedSymb2 := ""]
    tmp2[, yieldSymb2 := ""]
    tmp2[, methodFlag := NULL]
    
    tmp3 = tmp2[areaCode != 357 & year >= 1992, ]
    ## tmp3 = tmp2[areaCode != 357, ]
    tmp3
}

wheat.dt = read.example("wheatSUA.csv")
grape.dt = read.example("grapesSUA.csv")
beef.dt = read.example("beefSUA.csv")
okra.dt = read.example("okraSUA.csv")

## ----wheat-yield-explore, fig.width=15, fig.height=12, fig.cap='This figure illustrates the yield of wheat accross all countries, it provide strong support to the facts stated. First of all, we can observe the concordant increasing trend accross all country where technological innovation such as improved seed, and synthetic nitrogen fertilizer contributed to the increase in productivity. Yet at the same time, we can also observe that the rate of growth differ between countries. The single yield spike in Zambia raises concern on data quality.', fig.pos='!ht', echo = FALSE----
xyplot(yieldValue ~ year|areaName, data = wheat.dt, type = c("g", "l"),
       xlab = "", ylab = "Wheat Yield (1000 Hg/Ha)")

## ----grape-yield-explore, fig.width=15, fig.height=12, fig.cap='Unlike the yield of wheat, the yield for grape has remain rather constant over time except a few selective country such as Peru and Azerbaijian. There are a few spikes observed, namely Iraq, the invasion of Iraq may have contributed to the negative shock. The considerable fall in the yield for India is of unknown cause, and potentially a data entry error.', fig.pos='!ht', echo = FALSE----
xyplot(yieldValue ~ year|areaName, data = grape.dt, type = c("g", "l"),
       xlab = "", ylab = "Grape Yield (1000 Hg/Ha)")

## ----okra-yield-explore, fig.width=15, fig.height=12, fig.cap='Shown in this graph are the yield of Okra over time. We can observe that the data is extremely sparse, further the quality of the data is questionable. Yield growth from less than 10 Hg/Ha to greater than 30 Hg/Ha in a single year for both Bahrain and Senegal is deemed suspicious.', fig.pos='!ht', echo = FALSE----
xyplot(yieldValue ~ year|areaName, data = okra.dt, type = c("g", "l"),
       xlab = "", ylab = "Okra Yield (1000 Hg/Ha)")

## ----beef-yield-explore, fig.width=15, fig.height=12, fig.cap='Similar to grape, the carcass weight per animal of beef also shows the same pattern of a very modest improvement in yield with a handful of exceptions.', fig.pos='!ht', echo = FALSE----
xyplot(yieldValue ~ year|areaName, data = beef.dt, type = c("g", "l"),
       xlab = "", ylab = "Beef Yield (1000 Hg/An)")


## ----wheat-production-area-explore, fig.width=15, fig.height=20, fig.cap='Wheat production and area harvested by country. The figure shows that excluding several producers such as India, Nepal, and Pakistan which has a stable trend in production, both the production and area of most countries display erratic behavior.', fig.pos='!ht', echo = FALSE----
xyplot(productionValue + areaHarvestedValue ~ year|areaName, 
       data = wheat.dt, type = c("g", "l"),
       xlab = "", ylab = "", 
       scales = list(y = list(relation = "free", draw = FALSE)),
       auto.key = 
       list(text = c("Production", "Area harvested")))

## ----grape-production-area-explore, fig.width=15, fig.height=20, fig.cap='In contrast to wheat, the area for grape is much more stable, a character of tree which takes year to plant and nuture and the alteration of the land use is much more difficult. Nonetheless, the production also display different trends over different time period', fig.pos='!ht', echo = FALSE----
xyplot(productionValue + areaHarvestedValue ~ year|areaName, 
       data = grape.dt, type = c("g", "l"),
       xlab = "", ylab = "", 
       scales = list(y = list(relation = "free", draw = FALSE)),
       auto.key = 
       list(text = c("Production", "Area harvested")))

## ----okra-production-area-explore, fig.width=15, fig.height=20, fig.cap='Even more so than both wheat and grape, the production of okra appears to demonstrate unpredictable trends and shocks.', fig.pos='!ht', echo = FALSE----
xyplot(productionValue + areaHarvestedValue ~ year|areaName, 
       data = okra.dt, type = c("g", "l"),
       xlab = "", ylab = "", 
       scales = list(y = list(relation = "free", draw = FALSE)),
       auto.key = 
       list(text = c("Production", "Area harvested")))

## ----beef-production-area-explore, fig.width=15, fig.height=19, fig.cap='Of all the production, livestock meat such as beef and veal may have been the easiest to predict and impute. There is continuing demand around the world for meat, while shift in production is usually difficult due to the high expenditure in machinery capital. With this being said, we can likewise observe shocks and or period of contraction or expansion over time.', fig.pos='!ht', echo = FALSE----
xyplot(productionValue + areaHarvestedValue ~ year|areaName, 
       data = beef.dt, type = c("g", "l"),
       xlab = "", ylab = "", 
       scales = list(y = list(relation = "free", draw = FALSE)),
       auto.key = 
       list(text = c("Production", "Area harvested")))

