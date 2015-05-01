##' AUPUS Groups
##' 
##' The AUPUS logic has many groupings of elements that require specialized
##' processing.  The numbers of these groupings could be hard-coded in the
##' code, but to increase visibility of these groups (and for clearer
##' understanding of the code) these groups have been defined in this object.
##' This object is a dataset of the faoswsAupus package, and contains
##' defintions for all the various groups used within this package.
##' 
##' For an example, consider initialAsSupply.  These four item types
##' (tobacco, tea, cocoa, coffee) consider initial existence as a supply in the
##' balance, where all other elements do not consider initial existence in the
##' balance equation at all.
##' 
##' A list and short description of each element is below:
##' \itemize{
##'     \item initialAsSupply A vector of the item types which consider initial
##'     existence as a supply in the balance equation.
##'     \item reemploymentNotAsUtilization Most item types consider element 151
##'     (Reemployment other sector) as utilization.  However, some item groups
##'     do not, and those are specified in this vector.
##'     \item excludedFromBalance This vector gives item types which are not
##'     balanced.
##'     \item specialBalanceSugar Instead of computing the balance as the
##'     difference between supply and utilization, the balance is computed as
##'     1000 times the ratio of element 161 (final existence) divided by
##'     element 171 (consumption).
##'     \item specialBalanceNegate A vector of all item types where the
##'     negative of the computed value in the balance is used.
##'     \item escrDivisionFactor: A division factor of 1000 is used in
##'     reporting production values (instead of 10000).
##'     \item sugarDivisionFactor: A division factor of 1 is used in
##'     reporting production values (instead of 10000).
##'     \item escrSugarProduction: These cases are handled differently in the
##'     calculation of element 51 (Production).
##'     \item escrSugarExistence: These cases 
##' }
##' 
##' @docType data
##' @keywords AUPUS, commodity groups
##' @name aupusGroups
##' @usage data(aupusGroups)
##' @format A list object containing 1 numeric vector.  Each vector specifies
##' item type groups which are special scenarios for certain elements.
##'
NULL

# # The code below generates the data object
# aupusGroups = list(
#     initialAsSupply = c(51, 58, 59, 61),
#     reemploymentNotAsUtilization = c(53),
#     excludedFromBalance = c(4, 15, 16, 20, 21, 25, 32, 33, 37, 49, 50, 55, 56),
#     specialBalanceSugar = 57,
#     specialBalanceNegate = 71,
#     escrDivisionFactor = c(58, 59, 61),
#     sugarDivisionFactor = 55,
#     escrSugarProduction = c(55, 56),
#     escrSugarExistence = 57:61
# )
# 
# save(aupusGroups, file = "~/Documents/SVN/RModules/faoswsAupus/data/aupusGroups.RData")
