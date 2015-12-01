##' Get Loss Food Group
##'
##' Function to load the loss food group classification
##'


getLossFoodGroup = function(){
  lossFoodGroup = GetTableData(schemaName = "ess", tableName = "loss_food_group")
  setnames(lossFoodGroup, old = colnames(lossFoodGroup),
           new = c("measuredItemFCL", "measuredItemNameFS", "foodGroupName",
                   "foodGroup", "foodGeneralGroup", "foodPerishableGroup",
                   "measuredItemCPC"))
  lossFoodGroup = 
    lossFoodGroup[, list(measuredItemFCL, foodGroupName,
                         foodGeneralGroup, foodPerishableGroup)]
  
  ## Adding headings to FS codes
  lossFoodGroup[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  
  lossFoodGroup
}
