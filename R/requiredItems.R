##' Get Required Items
##' 
##' Table to obtain all primary item with CPC and FCL codes
##' 
##' Table to check
##' 
##' @export

getRequiredItems <- function(){
  
#   requiredItems = ReadDatatable(table = "loss_food_group") %>%
#   select(measured_item_fs,food_general_group,measured_item_cpc) %>%
#   filter(food_general_group == "primary")
  
  lossFoodGroup = data.table(read.csv("foodPerishableGroup.csv")) %>%
    select(FCL..Item..code.,FCL..Title,Group.Name,P.D,FBS..GROUP.Number,PERISHABLE) %>%
    filter(PERISHABLE != "")
  
  setnames(lossFoodGroup, 
           old = colnames(lossFoodGroup),
           new = c("measuredItemFCL", "measuredItemNameFCL", "foodGroupName",
                   "foodGeneralGroup", "measuredItemFBS", "foodPerishableGroup"))
#   lossFoodGroup = 
#     lossFoodGroup[, list(measuredItemFCL, foodGeneralGroup)] %>%
#     filter(foodGeneralGroup == "P")
  
  ## Adding headings to FCL codes
  lossFoodGroup[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  lossFoodGroup[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
  
}

# requiredItems = ReadDatatable(table = "loss_food_group", readOnly = FALSE, limit=1e3) %>%
#   select(measured_item_fs,food_general_group,measured_item_cpc) %>%
#   filter(food_general_group == "primary")