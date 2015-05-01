##' Get Share Data
##' 
##' This function extracts the shares data from the data base.  The shares data
##' contains information on children are shared amongst parents.  For example,
##' some children of wheat flour (item = "16") are: macaroni (18), bread (20),
##' and pastry (22).  In Algeria (geographicAreaFS = "4"), the shares are 95,
##' 3, and 2 which means that 95% of wheat flour is converted to macaroni, 3%
##' to bread and 2% to pastry.  A default share is also available with the 0
##' wildcard, and values can vary by year (also with a wildcard of 0).
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.  Only required
##' if database = "old".
##' 
##' @return A list of three items: specific, yearWildCard, and
##' areaYearWildCard.  Each of these elements has the same general structure:
##' a data.table with zero to two dimension columns (area and year), the parent
##' and child items, and then the Value_share column (representing the
##' proportion allocated to each child).
##' 
##' @export
##' 

getShareData = function(aupusParam, database = c("new", "old"), conn){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(database %in% c("new", "old"))
    if(database == "old")
        stopifnot(!missing(conn))
    
    database = match.arg(database)
    if(database == "old"){
        shareQuery =
            paste0("SELECT area, item_parent, item_child, yr, aupus_share
                FROM aupus_item_tree_shares
                WHERE area in (0, ", areaCode, ")")
        share =
            data.table(dbGetQuery(conn = conn, shareQuery))
        setnames(share,
                 old = c("AREA", "ITEM_PARENT", "ITEM_CHILD", "YR",
                     "AUPUS_SHARE"),
                 new = c("areaCode", "itemCode", "itemChildCode", "Year",
                     "SHARE"))
        specific = share[areaCode != 0 & Year != 0, ]
        setkeyv(specific, c("areaCode", "itemCode", "itemChildCode",
                            "Year"))
        yearWildCard = share[areaCode != 0 & Year == 0,
            !"Year", with = FALSE]
        setkeyv(yearWildCard, c("areaCode", "itemCode", "itemChildCode"))
        areaYearWildCard = share[areaCode == 0 & Year == 0,
            !c("areaCode", "Year"), with = FALSE]
        setkeyv(areaYearWildCard, c("itemCode", "itemChildCode"))
        finalShare = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){
        shareDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c("0", aupusParam$areaCode))),
                 Dimension(name = "measuredItemParentFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "measuredItemChildFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", aupusParam$year))))

        shareDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "aupus_share_fs",
                       dimensions = shareDimension)

        sharePivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemParentFS", ascending = TRUE),
            Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
            Pivoting(code = "timePointYearsSP", ascending = FALSE)
        )

        fullShare =
            GetData(key = shareDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = sharePivot)
        fullShare[, flagShare := NULL]
        setnames(fullShare,
                 old = c("Value"),
                 new = c("Value_share"))

        ## Partition the data into three groups: shares that apply only to
        ## specific years and locations, shares applying to all years but
        ## specific locations, and shares applying to all years and all
        ## locations
        specific = fullShare[geographicAreaFS != "0" &
                             timePointYearsSP != 0, ]
        setkeyv(specific, c("geographicAreaFS", "measuredItemParentFS",
                            "measuredItemChildFS", "timePointYearsSP"))
        yearWildCard = fullShare[geographicAreaFS != "0" &
                                 timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS",
                                "measuredItemParentFS",
                                "measuredItemChildFS"))
        areaYearWildCard =
            fullShare[geographicAreaFS == "0" & timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, c("measuredItemParentFS",
                                    "measuredItemChildFS"))
        finalShare = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    }
    finalShare
}
