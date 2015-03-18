##' Get Ratio Data
##' 
##' This function extracts the ratio data from the data base.  The ratio data
##' contains information on ratios that exist between certain "elements" and
##' "items."  For example, how many calories (element = "261") are in wheat
##' flour (item = "16")?  The answer according to the database is 364, but 
##' better estimates may exist for specific years and specific countries.  This
##' dataset contains the overall average rates and the specific rates.
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
##' a data.table with one to three dimension columns (area, year, and item) and
##' then many columns containing the ratios (or conversion factor) from that
##' item into elements in the FBS.  If such values don't make sense or don't
##' apply, an NA is in the data.
##' 
##' @export
##' 

getRatioData = function(aupusParam, database = c("new", "old"), conn){

    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(database %in% c("new", "old"))
    if(database == "old")
        stopifnot(!missing(conn))
    
    database = match.arg(database)
    if(database == "old"){
        ratioQuery =
            paste0("SELECT area, item, ele, yr, ratio
                FROM aupus_ratios
                WHERE area in (0, ", areaCode, ")")
        ratio =
            data.table(dbGetQuery(conn = conn, ratioQuery))
        ratio[, ELE := paste0("RATIO_", ELE)]
        castedRatio =
            dcast.data.table(data = ratio,
                             formula = AREA + ITEM + YR ~ ELE,
                             value.var = "RATIO")
        setnames(castedRatio,
                 old = c("AREA", "ITEM", "YR"),
                 new = c("areaCode", "itemCode", "Year"))

        specific = castedRatio[areaCode != 0 & Year != 0, ]
        setkeyv(specific, c("areaCode", "itemCode", "Year"))
        yearWildCard = castedRatio[areaCode != 0 & Year == 0,
            !"Year", with = FALSE]
        setkeyv(yearWildCard, c("areaCode", "itemCode"))
        areaYearWildCard = castedRatio[areaCode == 0 & Year == 0,
            !c("areaCode", "Year"), with = FALSE]
        setkeyv(areaYearWildCard, "itemCode")
        finalRatio = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)
    } else if(database == "new"){

        ratioDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(c("0",
                               aupusParam$areaCode))),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(c("0", aupusParam$year))),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(aupusParam$elementCode)))

        ratioDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "aupus_ratio_fs",
                       dimensions = ratioDimension)

        ratioPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYearsSP", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )

        fullRatio =
            GetData(key = ratioDataContext, flags = TRUE,
                    normalized = FALSE,
                    pivoting = ratioPivot)

        ## Remove the symbol since they are the indicator whether it
        ## is the balancing element
        flagColumns = grep("flag", colnames(fullRatio))
        fullRatio[, `:=`(c(flagColumns), NULL)]

        ## Coerce logical to numeric
        warning("This step should no longer be necessary upon resolution of ",
                "SWS-797")
        isLogical = (sapply(fullRatio, typeof) == "logical")
        logicalColumns = names(which(isLogical))
        for(name in logicalColumns)
            fullRatio[, c(name) := as.numeric(get(name))]

        oldNames = grep("Value", colnames(fullRatio), value = TRUE)
        setnames(fullRatio,
                 old = oldNames,
                 new = gsub("Value", "Ratio", oldNames))

        ## Partition the data into three groups: ratios that apply only to
        ## specific years and locations, ratios applying to all years but
        ## specific locations, and ratios applying to all years and all
        ## locations
        specific = fullRatio[geographicAreaFS != "0" &
                             timePointYearsSP != 0, ]
        setkeyv(specific,
                cols = c("geographicAreaFS", "measuredItemFS",
                    "timePointYearsSP"))
        yearWildCard = fullRatio[geographicAreaFS != "0" &
                                 timePointYearsSP == 0,
            !"timePointYearsSP", with = FALSE]
        setkeyv(yearWildCard, c("geographicAreaFS", "measuredItemFS"))
        areaYearWildCard = fullRatio[geographicAreaFS == "0" &
                                     timePointYearsSP == 0,
            !c("geographicAreaFS", "timePointYearsSP"), with = FALSE]
        setkeyv(areaYearWildCard, "measuredItemFS")

        finalRatio = list(specific = specific, 
            yearWildCard = yearWildCard,
            areaYearWildCard = areaYearWildCard)

    }
    finalRatio
}
        
