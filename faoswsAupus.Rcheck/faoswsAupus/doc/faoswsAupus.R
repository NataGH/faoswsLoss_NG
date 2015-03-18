## ----setup, include=FALSE, cache=FALSE----------------------------------------
library(knitr)
opts_chunk$set(fig.path='figure/', fig.align='center', fig.show='hold',
               warning=FALSE, message=FALSE, error=FALSE, tidy=FALSE, 
               results='markup', eval=TRUE, echo=TRUE, cache=FALSE, dpi=200)
options(replace.assign=TRUE, width=80)
assign("depthtrigger", 10, data.table:::.global)

## ----load-packages------------------------------------------------------------
library(faoswsAupus)
library(faosws)
library(igraph)
library(data.table)

## -----------------------------------------------------------------------------
is(US)
names(US)
sapply(US, class)
sapply(US, dim)
is(usAupusParam)
names(usAupusParam)

## ----eval=FALSE---------------------------------------------------------------
#  GetTestEnvironment(
#      baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
#      token = "66984d62-6add-4ad4-bdf3-5d8538bb2b70")
#  usAupusParam = getAupusParameter(areaCode = "231", assignGlobal = FALSE,
#                                   yearsToUse = 2009:2013)
#  US = getAupusDataset(aupusParam = usAupusParam)

## -----------------------------------------------------------------------------
plotCommodityTree(US$shareData)

## -----------------------------------------------------------------------------
US = subsetAupus(aupusData = US, itemKeys = c(71, 72, 73),
                 aupusParam = usAupusParam)

## -----------------------------------------------------------------------------
aupusNetwork = suaToNetworkRepresentation(dataList = US,
                                          aupusParam = usAupusParam)
names(aupusNetwork)
sapply(aupusNetwork, class)
sapply(aupusNetwork, dim)

## -----------------------------------------------------------------------------
plotCommodityTree(US$shareData, edge.arrow.size = 2, vertex.size = 25)
aupusNetwork$nodes[, .(geographicAreaFS, timePointYearsSP, measuredItemFS)]
colnames(aupusNetwork$nodes)[1:10]
aupusNetwork$edges[, .(geographicAreaFS, timePointYearsSP,
                       measuredItemParentFS, measuredItemChildFS)]
colnames(aupusNetwork$edges)

## -----------------------------------------------------------------------------
nodes = aupusNetwork$nodes
edges = aupusNetwork$edges
nodes = coerceColumnTypes(aupusParam = usAupusParam, data = nodes)
edges = coerceColumnTypes(aupusParam = usAupusParam, data = edges)
from = usAupusParam$keyNames$itemParentName
to = usAupusParam$keyNames$itemChildName
processingLevelData = edges[, findProcessingLevel(.SD, from = from, 
    to = to, aupusParam = usAupusParam),
    by = c(usAupusParam$keyNames$areaName, usAupusParam$keyNames$yearName)]
setkeyv(processingLevelData, key(nodes))
invisible(nodes[processingLevelData, `:=`(processingLevel, i.processingLevel)])
invisible(nodes[is.na(processingLevel), processingLevel := 0])
nodes[, c(key(nodes), "processingLevel"), with = FALSE]

## -----------------------------------------------------------------------------
toProcess = nodes[processingLevel == 0, ]

## -----------------------------------------------------------------------------
toProcess[, Value_measuredElementFS_11]
toProcess[, Value_measuredElementFS_161]
toProcess[, flagFaostat_measuredElementFS_11]
calculateEle11(data = toProcess, aupusParam = usAupusParam)
toProcess[, Value_measuredElementFS_11]
toProcess[, flagFaostat_measuredElementFS_11]

## -----------------------------------------------------------------------------
calculateEle21(data = toProcess, aupusParam = usAupusParam)
calculateEle41(data = toProcess, aupusParam = usAupusParam)
calculateEle51(data = toProcess, aupusParam = usAupusParam)
calculateEle314151(data = toProcess, aupusParam = usAupusParam)
calculateEle63(data = toProcess, aupusParam = usAupusParam)
calculateEle71(data = toProcess, aupusParam = usAupusParam)
calculateEle93(data = toProcess, aupusParam = usAupusParam)
calculateTotalSupply(data = toProcess, aupusParam = usAupusParam)
tail(colnames(toProcess))
toProcess$TOTAL_SUPPLY

## -----------------------------------------------------------------------------
calculateEle101(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle111(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle121(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle131(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle141(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle144(population11Num = "Value_population_11",
                data = toProcess, aupusParam = usAupusParam)
calculateEle151(stotal = "TOTAL_SUPPLY", data = toProcess,
                aupusParam = usAupusParam)
calculateEle161(data = toProcess, aupusParam = usAupusParam)
calculateEle171(data = toProcess, aupusParam = usAupusParam)
calculateEle174(population11Num = "Value_population_11",
                                   data = toProcess,
                                   aupusParam = usAupusParam)

## -----------------------------------------------------------------------------
calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_261",
                            elementNum = 261, data = toProcess,
                            aupusParam = usAupusParam)
calculateDailyNutritive(population11Num = "Value_population_11",
                        population21Num = "Value_population_21",
                        dailyElement = 264, totalElement = 261,
                        data = toProcess, aupusParam = usAupusParam)
calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_271",
                            elementNum = 271, data = toProcess,
                            aupusParam = usAupusParam)
calculateDailyNutritive(population11Num = "Value_population_11",
                        population21Num = "Value_population_21",
                        dailyElement = 274, totalElement = 271,
                        data = toProcess, aupusParam = usAupusParam)
calculateTotalNutritive(ratioNum = "Ratio_measuredElementFS_281",
                            elementNum = 281, data = toProcess,
                            aupusParam = usAupusParam)
calculateDailyNutritive(population11Num = "Value_population_11",
                        population21Num = "Value_population_21",
                        dailyElement = 284, totalElement = 281,
                        data = toProcess, aupusParam = usAupusParam)

## -----------------------------------------------------------------------------
calculateEle541(data = toProcess, aupusParam = usAupusParam)
calculateEle546(data = toProcess, aupusParam = usAupusParam)
calculateTotalUtilization(data = toProcess, aupusParam = usAupusParam)
calculateBalance(supply = "TOTAL_SUPPLY", utilization = "TOTAL_UTILIZATION",
                 data = toProcess, aupusParam = usAupusParam)
tail(colnames(toProcess))

## -----------------------------------------------------------------------------
toProcess[, c("timePointYearsSP", "Value_measuredElementFS_131",
              "Value_measuredElementFS_41"), with = F]
edges[, c("timePointYearsSP", "measuredItemChildFS", "Value_share",
          "Value_input", "Value_extraction"), with = FALSE]
updateEdges(nodes = toProcess, edges = edges, aupusParam = usAupusParam)
edges[, c("timePointYearsSP", "measuredItemChildFS", "Value_share",
          "Value_input", "Value_extraction"), with = FALSE]

## -----------------------------------------------------------------------------
nodesNextLevel = nodes[processingLevel == 1, ]
nodesNextLevel[, c("timePointYearsSP", "measuredItemFS",
                   "Value_measuredElementFS_31"), with = FALSE]
updateInputFromProcessing(nodes = nodesNextLevel,
                          edges = edges,
                          aupusParam = usAupusParam)
nodesNextLevel[, c("timePointYearsSP", "measuredItemFS",
                   "Value_measuredElementFS_31"), with = FALSE]

## -----------------------------------------------------------------------------
FBSelements =
    c("Value_measuredElementFS_51", "Value_measuredElementFS_61",
      "Value_measuredElementFS_91", "Value_measuredElementFS_101",
      "Value_measuredElementFS_111", "Value_measuredElementFS_121",
      "Value_measuredElementFS_141", "Value_measuredElementFS_151")
standardizationGraph = 
    constructStandardizationGraph(aupusNetwork = aupusNetwork,
                                  standardizeElement = FBSelements,
                                  aupusParam = usAupusParam)
is(standardizationGraph)
sapply(standardizationGraph, class)

## -----------------------------------------------------------------------------
standardization(standardizationGraph[[1]], standardizeElement = FBSelements,
                plot = TRUE, aupusParam = usAupusParam,
                vertex.size = 20, edge.arrow.size = 2, vertex.label.cex = 1)

## -----------------------------------------------------------------------------
vertex.attributes(standardizationGraph[[1]])[1:2]
edge.attributes(standardizationGraph[[1]])[c(3, 4)]
E(standardizationGraph[[1]])

## -----------------------------------------------------------------------------
177630 +                           # The initial value
    67040 * 10000/8000 * 100/100 + # Standardizing element 72
    13408 * 10000/1600 * 100/100   # Standardizing element 73

## -----------------------------------------------------------------------------
fbsStandardization(graph = standardizationGraph, 
                   standardizeElement = FBSelements,
                   plot = FALSE, aupusParam = usAupusParam)

