library(faosws)

GetTestEnvironment(
#     baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
#     token = "d0e1f76f-61a6-4183-981c-d0fec7ac1845"
    baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
    token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
)

## Define the keys
key3 <- DatasetKey(domain = "population", dataset = "population", dimensions = list(
    Dimension(name = "geographicAreaM49", keys = c("12", "28", "51", "31", "44")),
    Dimension(name = "measuredElementPopulation", keys = "21"),
    Dimension(name = "timePointYears", keys = as.character(2005:2010))
))

key4 = DatasetKey(domain = "agriculture", dataset = "agriculture", dimensions = list(
    ## All countries (needed for models)
    Dimension(name = "geographicAreaM49", keys = c("4", "8", "10")),
    Dimension(name = "measuredElement", keys = c("5113", "5025", "5312")),
    Dimension(name = "measuredItemCPC", keys = c("0111", "0112", "0115")),
    Dimension(name = "timePointYears", keys = as.character(2005:2010))
))

key5 = DatasetKey(domain = "agriculture", dataset = "aupus_share", dimensions = list(
    Dimension(name = "geographicAreaM49", keys = c("4", "8", "10")),
    Dimension(name = "measuredShare", keys = c("1")),
    Dimension(name = "measuredItemParentCPC", keys = c("0111", "01112", "0112", "01141")),
    Dimension(name = "measuredItemChildCPC", keys = c("0111", "01112", "0112", "01141")),
    Dimension(name = "timePointYearsSP", keys = as.character(2005:2010))
))


GetData(key3, normalized = TRUE, flags = FALSE)
GetData(key3, normalized = TRUE, flags = TRUE)
GetData(key3, normalized = FALSE, flags = FALSE)
GetData(key3, normalized = FALSE, flags = TRUE)
dimNames = lapply(key3@dimensions, function(x) x@name)
randomPivot = sample(lapply(dimNames, function(x){
    Pivoting(code = x, ascending = sample(c(T,F), size = 1))
}))
GetData(key3, normalized = TRUE, flags = FALSE, pivoting = randomPivot)
GetData(key3, normalized = TRUE, flags = TRUE, pivoting = randomPivot)
GetData(key3, normalized = FALSE, flags = FALSE, pivoting = randomPivot)
GetData(key3, normalized = FALSE, flags = TRUE, pivoting = randomPivot)


GetData(key4, normalized = TRUE, flags = FALSE)
GetData(key4, normalized = TRUE, flags = TRUE)
GetData(key4, normalized = FALSE, flags = FALSE)
GetData(key4, normalized = FALSE, flags = TRUE)
dimNames = lapply(key4@dimensions, function(x) x@name)
randomPivot = sample(lapply(dimNames, function(x){
    Pivoting(code = x, ascending = sample(c(T,F), size = 1))
}))
GetData(key4, normalized = TRUE, flags = FALSE, pivoting = randomPivot)
GetData(key4, normalized = TRUE, flags = TRUE, pivoting = randomPivot)
GetData(key4, normalized = FALSE, flags = FALSE, pivoting = randomPivot)
GetData(key4, normalized = FALSE, flags = TRUE, pivoting = randomPivot)


GetData(key5, normalized = TRUE, flags = FALSE)
GetData(key5, normalized = TRUE, flags = TRUE)
GetData(key5, normalized = FALSE, flags = FALSE)
GetData(key5, normalized = FALSE, flags = TRUE)
dimNames = lapply(key5@dimensions, function(x) x@name)
randomPivot = sample(lapply(dimNames, function(x){
    Pivoting(code = x, ascending = sample(c(T,F), size = 1))
}))
GetData(key5, normalized = TRUE, flags = FALSE, pivoting = randomPivot)
GetData(key5, normalized = TRUE, flags = TRUE, pivoting = randomPivot)
GetData(key5, normalized = FALSE, flags = FALSE, pivoting = randomPivot)
GetData(key5, normalized = FALSE, flags = TRUE, pivoting = randomPivot)

key5@dimensions[[1]]@keys = c("4", "8", "12")
key5@dimensions[[2]]@keys = "1"
key5@dimensions[[3]]@keys = c("01324", "01330", "01341", "0231")
key5@dimensions[[4]]@keys = c("21439.04", "21434", "21435.02", "23993.02")

GetData(key5, normalized = TRUE, flags = FALSE)
GetData(key5, normalized = TRUE, flags = TRUE)
GetData(key5, normalized = FALSE, flags = FALSE)
GetData(key5, normalized = FALSE, flags = TRUE)
dimNames = lapply(key5@dimensions, function(x) x@name)
randomPivot = sample(lapply(dimNames, function(x){
    Pivoting(code = x, ascending = sample(c(T,F), size = 1))
}))
GetData(key5, normalized = TRUE, flags = FALSE, pivoting = randomPivot)
GetData(key5, normalized = TRUE, flags = TRUE, pivoting = randomPivot)
GetData(key5, normalized = FALSE, flags = FALSE, pivoting = randomPivot)
GetData(key5, normalized = FALSE, flags = TRUE, pivoting = randomPivot)
