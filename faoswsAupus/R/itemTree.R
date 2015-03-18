##' Commodity Item Tree
##' 
##' This dataset provides each of the individual item codes/names for all the
##' commodities and also specifies whether that particular commodity is a
##' target commodity (i.e. is aggregated up to in the standardization) as
##' well as the equivalent FBS code.
##' 
##' A short description of each column is below:
##' \itemize{
##'     \item itemCode: The Supply and Utilization Account (SUA) ID number
##'     for the commodity.
##'     \item itemName: The name of the commodity
##'     \item incTot: (More documentation needed here...)
##'     \item aggCom: (More documentation needed here...)
##'     \item weight: (More documentation needed here...)
##'     \item target: Specifies if the particular commodity is a target.  If
##'     so, then all the children of this commodity will be aggregated up to
##'     this commodity (i.e. expressed in this commodity) during the
##'     standardization proceedure.
##'     \item convType: If this column takes the value "(cal.)", then it is
##'     standardized only for caloric purposes.  For example, when wheat (15)
##'     is processed, it generates flour wheat (16), bran wheat (17), and germ
##'     wheat (19).  If each of these items is standardized to wheat in terms
##'     of quantities, we'll be overcounting.  However, calories can be
##'     standardized, and so one commodity is chosen for the quantity
##'     standardization (in this case flour wheat) and all three are used for
##'     caloric standardization.  The (cal.) indicates which commodities are
##'     not standardized with quantities but only with calories.
##'     \item targetCode: Each commodity is standardized to some commodity 
##'     (possibly itself).  The targetCode specifies the itemCode of the
##'     "target", or the commodity it is standardized to.
##'     \item targetName: The name of the target commodity, see targetCode.
##'     \item baseExtraction: 
##'     \item fbsCode: The corresponding code for this commodity in the Food
##'     Balance Sheets (FBS).
##'     \item fbsName: The corresponding name for this commodity in the Food
##'     Balance Sheets (FBS).
##' }
##' 
##' @docType data
##' @keywords item, commodity groups, fbs, codes
##' @name itemTree
##' @usage data(itemTree)
##' @format A data.table object of 12 columns by 808 rows.
##'
NULL