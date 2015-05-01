##' Get Item Information Data
##'
##' The name and type of the commodity corresponding to the item code
##' is returned.
##' 
##' @return A data.table with the codes for each measured item as well as a
##' name describing the item and an item type.
##' 
##' @export
##' 

getItemInfoData = function(){
    itemCodeList =
        GetCodeList(domain = "faostat_one",
                    dataset = "FS1_SUA",
                    dimension = "measuredItemFS")
    itemCodeList[, startDate := as.Date(faosws::NullToNa(startDate))]
    itemCodeList[, endDate := as.Date(faosws::NullToNa(endDate))]
    setnames(itemCodeList,
             old = c("code", "description", "type"),
             new = c("measuredItemFS", "measuredItemNameFS",
                     "measuredItemTypeFS"))
    swsItemTable =
        itemCodeList[, list(measuredItemFS, measuredItemNameFS,
                            measuredItemTypeFS)]
    setkeyv(swsItemTable, "measuredItemFS")
    swsItemTable
}
