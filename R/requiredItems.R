##' Get Required Items
##' 
##' Table to obtain all primary item with CPC and FCL codes
##' 
##' Table to check

getRequiredItems <- function(){
  
  requiredItems = GetTableData(schemaName = "ess", tableName = "loss_food_group") %>%
  select(measured_item_fs,food_general_group,measured_item_cpc) %>%
  filter(food_general_group == "primary")
}

# requiredItems = ReadDatatable(table = "loss_food_group", readOnly = FALSE, limit=1e3) %>%
#   select(measured_item_fs,food_general_group,measured_item_cpc) %>%
#   filter(food_general_group == "primary")