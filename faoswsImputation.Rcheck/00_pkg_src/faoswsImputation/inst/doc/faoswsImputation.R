## ----setup, include=FALSE, cache=FALSE----------------------------------------
library(knitr)
opts_chunk$set(fig.path='figure/', fig.align='center', fig.show='hold',
               warning=FALSE, message=FALSE, error=FALSE, tidy=FALSE, 
               results='markup', eval=TRUE, echo=TRUE, cache=FALSE, dpi=200)
options(replace.assign=TRUE,width=80)
assign("depthtrigger", 10, data.table:::.global)

## ----loda-library-------------------------------------------------------------
## Load libraries
library(faoswsImputation)
library(faoswsUtil)
library(data.table)

## -----------------------------------------------------------------------------
library(faoswsProduction)

## ----read-data, results='markup'----------------------------------------------
str(okrapd)

## ----create-flagt-table-------------------------------------------------------
swsOldFlagTable = faoswsFlagTable
faoswsFlagTable$flagObservationStatus =
    as.character(faoswsFlagTable$flagObservationStatus)
swsOldFlagTable

## ----remove-prior-imputation--------------------------------------------------
okraProcessed = copy(okrapd)

## Removing prior imputation for production
table(okraProcessed$flagObservationStatus_measuredElement_5510)
removeImputation(data = okraProcessed,
                 value = "Value_measuredElement_5510",
                 observationFlag =
                     "flagObservationStatus_measuredElement_5510",
                 methodFlag = "flagMethod_measuredElement_5510",
                 imputedFlag = "E",
                 missingObservationFlag = "M",
                 missingMethodFlag = "u")
table(okraProcessed$flagObservationStatus_measuredElement_5510)

## Removing prior imputation for area harvested
table(okraProcessed$flagObservationStatus_measuredElement_5312)
removeImputation(data = okraProcessed,
                 value = "Value_measuredElement_5312",
                 observationFlag =
                     "flagObservationStatus_measuredElement_5312",
                 methodFlag = "flagMethod_measuredElement_5312",
                 imputedFlag = "E",
                 missingObservationFlag = "M",
                 missingMethodFlag = "u")
table(okraProcessed$flagObservationStatus_measuredElement_5312)

## Removing prior imputation for yield
table(okraProcessed$flagObservationStatus_measuredElement_5416)
removeImputation(data = okraProcessed,
                 value = "Value_measuredElement_5416",
                 observationFlag =
                     "flagObservationStatus_measuredElement_5416",
                 methodFlag = "flagMethod_measuredElement_5416",
                 imputedFlag = "E",
                 missingObservationFlag = "M",
                 missingMethodFlag = "u")
table(okraProcessed$flagObservationStatus_measuredElement_5416)

## ----remove-zero-values-------------------------------------------------------
okraProcessed[geographicAreaM49 == 12 & timePointYears >= 2005,
              .(Value_measuredElement_5312,
                flagObservationStatus_measuredElement_5312)]
remove0M(data = okraProcessed,
         value = "Value_measuredElement_5312",
         flag = "flagObservationStatus_measuredElement_5312",
         naFlag = "M")
okraProcessed[geographicAreaM49 == 12 & timePointYears >= 2005,
              .(Value_measuredElement_5312,
                flagObservationStatus_measuredElement_5312)]

## ----more-remove-zero-values--------------------------------------------------
remove0M(data = okraProcessed,
         value = "Value_measuredElement_5416",
         flag = "flagObservationStatus_measuredElement_5416",
         naFlag = "M")

remove0M(data = okraProcessed,
         value = "Value_measuredElement_5510",
         flag = "flagObservationStatus_measuredElement_5510",
         naFlag = "M")

## ----remove-info--------------------------------------------------------------
okraProcessed[geographicAreaM49 == 245,
              .(Value_measuredElement_5416,
                flagObservationStatus_measuredElement_5416)]
removeNoInfo(data = okraProcessed,
             value = "Value_measuredElement_5416",
             observationFlag = "flagObservationStatus_measuredElement_5416",
             byKey = "geographicAreaM49")
okraProcessed[geographicAreaM49 == 245,
              .(Value_measuredElement_5416,
                flagObservationStatus_measuredElement_5416)]

## ----processingParams---------------------------------------------------------
processingParams = defaultProcessingParameters()
processingParams

## ----processProductionDomain--------------------------------------------------
okraProcessed = copy(okrapd)
processProductionDomain(data = okraProcessed,
                        processingParameters = processingParams)

## ----imputationParams---------------------------------------------------------
okraProcessed = okraProcessed[geographicAreaM49 <= 60, ]
imputationParams = defaultImputationParameters(variable = "yield")
sapply(imputationParams, class)

## ----ensembleModels-----------------------------------------------------------
names(imputationParams$ensembleModels)
imputationParams$ensembleModels =
    imputationParams$ensembleModels[1:3]
names(imputationParams$ensembleModels)

## -----------------------------------------------------------------------------
newModel = ensembleModel(
    model = function(data){
        rep(10, length(data))
    },
    extrapolationRange = 5,
    level = "countryCommodity")
is(newModel)
imputationParams$ensembleModels = c(imputationParams$ensembleModels,
                                    newModel = newModel)
names(imputationParams$ensembleModels)

## ----impute-yield-------------------------------------------------------------
imputationParams$newImputationColumn = "test"
imputeVariable(data = okraProcessed, imputationParameters = imputationParams)
colnames(okraProcessed)

## -----------------------------------------------------------------------------
imputationParams$ensembleModels = imputationParams$ensembleModels[-4]
names(imputationParams$ensembleModels)
imputeVariable(data = okraProcessed, imputationParameters = imputationParams)

## -----------------------------------------------------------------------------
imputationParams$newImputationColumn = ""
imputeVariable(data = okraProcessed, imputationParameters = imputationParams)
okraProcessed[, c("Value_test", "flagObservationStatus_test",
                  "flagMethod_test") := NULL]

## -----------------------------------------------------------------------------
imputeVariable(data = okraProcessed, imputationParameters = imputationParams)

## ----impute-production--------------------------------------------------------
balanceProduction(data = okraProcessed,
                  imputationParameters = imputationParams,
                  processingParameters = processingParams)
imputationParams = defaultImputationParameters("production")
imputationParams$ensembleModels =
    imputationParams$ensembleModels[4:9]
names(imputationParams$ensembleModels)
imputeVariable(data = okraProcessed,
               imputationParameters = imputationParams)

## -----------------------------------------------------------------------------
for(model in imputationParams$ensembleModels)
    print(model@extrapolationRange)

## ----balance-area-harvested---------------------------------------------------
balanceAreaHarvested(data = okraProcessed,
                     imputationParameters = imputationParams,
                     processingParameters = processingParams)

## ----one-step-imputation------------------------------------------------------
yieldParams = defaultImputationParameters("yield")
yieldParams$ensembleModels = yieldParams$ensembleModels[1:3]
productionParams = defaultImputationParameters("production")
productionParams$ensembleModels = productionParams$ensembleModels[1:3]
okraProcessed = okrapd[geographicAreaM49 <= 55, ]
system.time(
    {        
        imputeProductionDomain(data = okraProcessed,
                               processingParameters = processingParams,
                               yieldImputationParameters = yieldParams,
                               productionImputationParameters =
                                   productionParams)
    })

## ----default-linear-----------------------------------------------------------
defaultLm

## ----ensemble-model-----------------------------------------------------------
mod = ensembleModel(model = defaultLm, extrapolationRange = Inf,
                    level = "countryCommodity")
is(mod)

## ----default-models-----------------------------------------------------------
names(allDefaultModels())
sapply(allDefaultModels(), is)

## ----ensemble-illustration----------------------------------------------------
bahrainExample = okrapd[areaName == "Bahrain", ]
bahrainExample[1:4, .(areaName, timePointYears,
                      production = Value_measuredElement_5510,
                      productionFlag =
                          flagObservationStatus_measuredElement_5510)]
remove0M(data = bahrainExample, value = "Value_measuredElement_5510",
         flag = "flagObservationStatus_measuredElement_5510")
bahrainExample[1:4, .(areaName, timePointYears,
                      production = Value_measuredElement_5510,
                      productionFlag =
                          flagObservationStatus_measuredElement_5510)]

## -----------------------------------------------------------------------------
## Compute fit for all component models
imputationParameters = defaultImputationParameters("production")
modelFits = computeEnsembleFit(data = bahrainExample,
                               imputationParameters = imputationParameters)
modelFits[1:3]
length(modelFits)

## -----------------------------------------------------------------------------
## Calculate the weight for each component model
cvGroup = makeCvGroup(data = bahrainExample,
                      imputationParameters = imputationParameters)
cvGroup
modelWeights = computeEnsembleWeight(data = bahrainExample,
                                     cvGroup = cvGroup,
                                     fits = modelFits,
                                     method = "inverse",
                                     imputationParameters =
                                         imputationParameters)
modelWeights[, .(defaultArima, defaultExp, defaultLm)]
dim(modelWeights)

## ----dpi=100------------------------------------------------------------------
## Combine the models to obtain the ensemble
ensemble = bahrainExample[, Value_measuredElement_5510]
imputationFit = computeEnsemble(modelFits, modelWeights)
ensemble[is.na(ensemble)] = imputationFit[is.na(ensemble)]
plotEnsemble(data = bahrainExample, modelFits = modelFits,
             modelWeights = modelWeights, ensemble = ensemble,
             imputationParameters = imputationParameters)

## ----ensemble-imputation, dpi=100---------------------------------------------
bahamasExample = okrapd[areaName == "Bahamas", ]
remove0M(data = bahamasExample, value = "Value_measuredElement_5510",
         flag = "flagObservationStatus_measuredElement_5510")
ensembleFit = ensembleImpute(data = bahamasExample,
                             imputationParameters = imputationParameters)

## ----example-dataset----------------------------------------------------------
exampleData = okrapd[geographicAreaM49 < 30, ]
processProductionDomain(exampleData,
        processingParameters = defaultProcessingParameters())
imputationParameters = defaultImputationParameters("yield")
imputationParameters$newImputationColumn = "test"
invisible(exampleData[timePointYears %in% 2005:2007 &
                          geographicAreaM49 == "91",
                      c("Value_measuredElement_5416",
                        "flagObservationStatus_measuredElement_5416") :=
                          list(NA, "M")])

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultArima, extrapolationRange = Inf,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultExp, extrapolationRange = 1,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultGlobalMean, extrapolationRange = Inf,
                      level = "commodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultGlobalMedian, extrapolationRange = Inf,
                      level = "commodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultLm, extrapolationRange = Inf,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultLoess, extrapolationRange = 1,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultLogistic, extrapolationRange = 1,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultMars, extrapolationRange = Inf,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultMean, extrapolationRange = Inf,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultMedian, extrapolationRange = Inf,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
mixedModelData = okrapd[geographicAreaM49 < 100, ]
processProductionDomain(mixedModelData,
        processingParameters = defaultProcessingParameters())
updateMissingFlags = function(data, value, flag, naFlag = "M"){
    missingIndex = which(is.na(data[[value]]))
    invisible(data[missingIndex, `:=`(c(flag), list(naFlag))])
}
updateMissingFlags(data = mixedModelData, value = "Value_measuredElement_5416",
         flag = "flagObservationStatus_measuredElement_5416")

## ----fig.height=3-------------------------------------------------------------
newParameters = defaultImputationParameters("yield")
newParameters$newImputationColumn = "test"
newParameters$estimateNoData = TRUE
model = ensembleModel(model = defaultMixedModel, extrapolationRange = Inf,
                      level = "commodity")
newParameters$ensembleModels = list(model)
imputeVariable(data = mixedModelData, imputationParameters = newParameters)

## ----fig.height=3-------------------------------------------------------------
mixedModelData[geographicAreaM49 == "66", Value_measuredElement_5416 := NA]
mixedModelData[geographicAreaM49 == "66",
               flagObservationStatus_measuredElement_5416 := "M"]
mixedModelData[,region := factor(ifelse(geographicAreaM49 < 15, 1,
                                 ifelse(geographicAreaM49 < 50, 2, 3)))]
formals(defaultMixedModel)$modelFormula = Value_measuredElement_5416 ~
    timePointYears*region + (timePointYears|geographicAreaM49/region)
hierarchical = ensembleModel(model = defaultMixedModel, level = "commodity",
                             extrapolationRange = Inf)
globalMean = ensembleModel(model = defaultGlobalMean, level = "commodity",
                           extrapolationRange = Inf)
globalMedian = ensembleModel(model = defaultGlobalMedian, level = "commodity",
                             extrapolationRange = Inf)
newParameters$ensembleModels = list(hierarchical = hierarchical,
                                    globalMean = globalMean,
                                    globalMedian = globalMedian)
imputeVariable(data = mixedModelData, imputationParameters = newParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultNaive, extrapolationRange = 0,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

## ----fig.height=3-------------------------------------------------------------
model = ensembleModel(model = defaultSpline, extrapolationRange = 0,
                      level = "countryCommodity")
imputationParameters$ensembleModels = list(model)
imputeVariable(data = exampleData, imputationParameters = imputationParameters)

