##' Example AUPUS Parameters
##' 
##' This object is an example of the type of object created by the
##' getAupusParameter function.  It contains codes/dimensions of the data
##' as well as column name descriptions.
##' 
##' @docType data
##' @keywords aupus, parameter, dimension
##' @name usAupusParam
##' @usage data(usAupusParam)
##' @format A list object with 5 elements: areaCode, itemCode, elementCode,
##' year, and keyNames.  The first four elements are numeric vectors providing
##' the dimensions of the data to be queried (country, commodity, variable, and
##' year, respectively).  The last element is another list containing various
##' parameters:
##' \itemize{
##'     \item areaName Column name corresponding to the variable
##'     containing the location codes.
##'     \item itemName Column name corresponding to the variable
##'     containing the item codes.
##'     \item itemParentName Used in an edge dataset (i.e. shares), this column
##'     name corresponds to the variable containing the parent item code.
##'     \item itemChildName Used in an edge dataset (i.e. shares), this column
##'     name corresponds to the variable containing the child item code.
##'     \item itemTypeName Column name corresponding to the variable
##'     containing the item type, see ?faoswsAupus::itemTypeDescription.
##'     \item elementName Column name corresponding to the variable
##'     containing the element codes.
##'     \item extractionRateName Column name corresponding to the variable
##'     containing extraction rates, see getExtractionRateData.
##'     \item balanceElementName Column name corresponding to the variable
##'     containing the balancing element, see getBalanceElementData.
##'     \item inputName Column name corresponding to the variable
##'     containing the input from processing data, see getInputFromProcessData.
##'     \item shareName Column name corresponding to the variable
##'     containing the share data, see getShareData.
##'     \item yearName Column name corresponding to the variable
##'     containing the year.
##'     \item valuePrefix To construct variables corresponding to certain
##'     elements, the valuePrefix is combined with the itemName element and
##'     the element code of interest.  For example,
##'     paste0(aupusParam$keyName$valuePrefix, aupusParam$keyName$itemName, "_", 11)
##'     gives the variable corresponding to element 11.
##'     \item flagPrefix See above description for valuePrefix, the same
##'     applies for the flag variables.
##'     \item ratioPrefix See above description for valuePrefix, the same
##'     applies for the ratio variables.
##' }
##'
NULL