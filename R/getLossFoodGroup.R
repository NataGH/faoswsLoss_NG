##' Get Loss Food Group
##'
##' Function to load the loss food group classification
##' 
##' @export


getLossFoodGroup = function(){
  ## update in SWS to match contents of CSV file
  ## see functions decribed using ?faosws::SaveDatatable
  ## lossFoodGroup = ReadDatatable(table = "loss_food_group")
  ## lossFoodGroup = data.table(read.csv(file.path("data-raw", "foodPerishableGroup.csv"))) %>%
  ##   select(FCL..Item..code.,FCL..Title,Group.Name,P.D,FBS..GROUP.Number,PERISHABLE) %>%
  ##   filter(PERISHABLE != "")
  ## data("lossFoodGroupData", package = "faoswsLoss", envir = environment())
  lossFoodGroup <- lossFoodGroupData
        
  setnames(lossFoodGroup, 
           old = colnames(lossFoodGroup),
           new = c("measuredItemFCL", "measuredItemNameFCL", "foodGroupName",
                   "foodGeneralGroup", "measuredItemFBS", "foodPerishableGroup"))
  lossFoodGroup = 
    lossFoodGroup[, list(measuredItemFCL, foodGroupName,
                         foodGeneralGroup, foodPerishableGroup)]
  
  ## Adding headings to FCL codes
  lossFoodGroup[, measuredItemFCL := addHeadingsFCL(measuredItemFCL)]
  lossFoodGroup[, measuredItemCPC := faoswsUtil::fcl2cpc(as.character(measuredItemFCL))]
  
}
