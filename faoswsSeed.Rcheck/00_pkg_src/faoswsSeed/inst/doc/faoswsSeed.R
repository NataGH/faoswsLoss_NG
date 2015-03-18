## ----setup, include=FALSE, cache=FALSE, echo=FALSE----------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/', fig.align = 'center', fig.show = 'hold',
               warning = FALSE, message = FALSE, error = FALSE, tidy = FALSE,
               results = 'markup', eval = TRUE, echo = TRUE, cache = FALSE)
options(replace.assign = TRUE, width = 80)
assign("depthtrigger", 80, data.table:::.global)

## ----loda-library-------------------------------------------------------------
## Load libraries
library(faosws)
library(faoswsSeed)
library(faoswsImputation)
library(faoswsUtil)
library(data.table)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  ## Pull in necessary swsContext parameters, see faosws documentation
#  GetTestEnvironment(
#      baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
#      token = "ec5a4b0e-0ffa-432e-9db0-ba08072c924b"
#  )
#  swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys = c("40")
#  data = getAreaData(dataContext = swsContext.datasets[[1]],
#                     areaSownElementCode = "5212",
#                     areaHarvestedElementCode = "5312",
#                     seedElementCode = "5525")

## -----------------------------------------------------------------------------
seedData

## -----------------------------------------------------------------------------
seedData[geographicAreaM49 == 348 & measuredItemCPC == "01330",
         .(time = timePointYears, areaSown = Value_measuredElement_5212,
           areaHarvested = Value_measuredElement_5312)]
temp = copy(seedData)
imputeAreaSown(data = temp)
temp[geographicAreaM49 == 348 & measuredItemCPC == "01330",
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312, Value_areaSownRatio)]

## -----------------------------------------------------------------------------
temp = copy(seedData)
imputeAreaSown(data = temp, valueAreaSown = "Value_measuredElement_5212",
               byKey = "geographicAreaM49")
temp[geographicAreaM49 == 348 & measuredItemCPC == "01330",
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312, Value_areaSownRatio)]

## -----------------------------------------------------------------------------
imputationParams = defaultImputationParameters(variable = "seed")
## Coerce type to character instead of default factor type
imputationParams$flagTable$flagObservationStatus =
    as.character(imputationParams$flagTable$flagObservationStatus)
imputationParams$ensembleModels = list(
    defaultMean = ensembleModel(model = defaultMean, extrapolationRange = Inf,
                                level = "countryCommodity"),
    globalMean = ensembleModel(model = defaultGlobalMean,
                               extrapolationRange = Inf, level = "commodity"),
    defaultLm = ensembleModel(model = defaultLm, extrapolationRange = Inf,
                              level = "countryCommodity"))
temp = seedData[measuredItemCPC == "01330", ]
imputeAreaSown(data = temp, imputationParameters = imputationParams)
temp[geographicAreaM49 == 348 & measuredItemCPC == "01330",
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312, Value_areaSownRatio)]

## -----------------------------------------------------------------------------
data = seedData[measuredItemCPC == "0111", ]
data[geographicAreaM49 == 100,
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312)]
imputeAreaSown(data = data, imputationParameters = imputationParams)
data[geographicAreaM49 == 100,
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312, Value_areaSownRatio)]
imputeAreaSown(data = data)
data[geographicAreaM49 == 100,
     .(time = timePointYears, areaSown = Value_measuredElement_5212,
       areaHarvested = Value_measuredElement_5312, Value_areaSownRatio)]

## -----------------------------------------------------------------------------
# countrySpecificData = getCountrySpecificSeedRate()
countrySpecificData = data.table(
    geographicAreaM49 = c("100", "348", "400"),
    measuredItemCPC = "0111",
    Value_seedRate = c(222, 213, 115),
    flagObservationStatus_seedRate = c("E", "E", ""))
setkeyv(countrySpecificData, c("geographicAreaM49", "measuredItemCPC"))
fillCountrySpecificSeedRate(data = data,
                            countrySpecificData = countrySpecificData)
head(data, 1)
data[, Value_seedRate]

## -----------------------------------------------------------------------------
# generalSeedData = getCountryGeneralSeedRate()
generalSeedData = data.table(measuredItemCPC = "0111", Value_seedRate = 151.14,
                             flagObservationStatus_seedRate = "")
setkeyv(generalSeedData, "measuredItemCPC")
fillGeneralSeedRate(data = data,
                    generalSeedData = generalSeedData)
head(data, 1)
data[, Value_seedRate]

## -----------------------------------------------------------------------------
data[, oldSeed := Value_measuredElement_5525]
imputeSeed(data)
data[geographicAreaM49 == 100, .(timePointYears,
                                 AreaSown = Value_measuredElement_5212,
                                 AreaHarvested = Value_measuredElement_5312,
                                 Seed = Value_measuredElement_5525,
                                 oldSeed)]

## ----eval=FALSE---------------------------------------------------------------
#  saveSeedData(data)

## ----echo=FALSE---------------------------------------------------------------
# Reset the depthtrigger to 3 so normal printing works fine again.
assign("depthtrigger", 3, data.table:::.global)

