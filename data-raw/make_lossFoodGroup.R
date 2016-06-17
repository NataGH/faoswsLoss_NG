library(data.table)

## Read the production file
lossFoodGroupData <- data.table(read.csv(file.path("data-raw", "foodPerishableGroup.csv"))) %>%
  select(FCL..Item..code.,FCL..Title,Group.Name,P.D,FBS..GROUP.Number,PERISHABLE) %>%
  filter(PERISHABLE != "")

devtools::use_data(lossFoodGroupData, overwrite = TRUE)
