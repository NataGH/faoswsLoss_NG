##' Get AUPUS Data
##' 
##' This function extracts the AUPUS data from the data base.
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system, only required
##' if database = "old".
##' 
##' @return A data.table object containing the AUPUS data.  The first three
##' columns contain the dimensions (geographicAreaFS, measuredItemFS, and
##' timePointYears) and the remaining columns are values and flags for each of
##' the elements required by AUPUS.
##' 
##' @export
##' 

getAupusData = function(aupusParam, database = c("new", "old"), conn){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(database %in% c("new", "old"))
    if(database == "old")
        stopifnot(!missing(conn))
    
    database = match.arg(database)
    if(database == "old"){
        aupusQuery =
            paste0("SELECT *
                FROM tsv_ics_work_yr
                WHERE area =", areaCode)
        aupus =
            data.table(dbGetQuery(conn = conn, aupusQuery))
        meltedAupus =
            suppressWarnings(melt(aupus,
                                  id.var = c("AREA", "ITEM", "ELE")))
        meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedAupus[, type := gsub("[0-9|_]", "", variable)]
        meltedAupus[, variable := NULL]
        finalAupus =
            dcast.data.table(meltedAupus, AREA + ITEM + Year ~ type + ELE,
                             value.var = "value")
        valueCol = grep("NUM", colnames(finalAupus), value = TRUE)
        finalAupus[, (valueCol) :=
                       lapply(valueCol, function(x)
                           as.numeric(finalAupus[[x]]))]
        for(i in valueCol){
            remove0M(data = finalAupus, value = i,
                     flag = gsub("NUM", "SYMB", i), naFlag = "M")
        }
        setnames(finalAupus,
                 old = c("AREA", "ITEM"),
                 new = c("areaCode", "itemCode"))
        setkeyv(finalAupus, cols = c("areaCode", "itemCode", "Year"))
    } else if(database == "new"){
        ## NOTE (Michael): Population is not included in this set, use
        ##                 getPopulationData.
        aupusDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(aupusParam$areaCode)),
                 Dimension(name = "measuredItemFS",
                           keys =
                               as.character(aupusParam$itemCode[aupusParam$itemCode != 1])),
                 Dimension(name = "timePointYears",
                           keys = as.character(aupusParam$year)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(aupusParam$elementCode)))

        aupusDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "FS1_SUA",
                       dimensions = aupusDimension)

        aupusPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYears", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )
        finalAupus =
            GetData(key = aupusDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = aupusPivot)

        ## Convert list of NULL to vector of NA
        for(i in colnames(finalAupus)){
            if(typeof(finalAupus[, i, with = FALSE]) == "list"){
                finalAupus[, c(i) := faosws::NullToNa(get(i))]
            }
            if(grepl("Value", i)){
                finalAupus[, c(i) := as.numeric(get(i))]
            } else if(grepl("flag", i)){
                finalAupus[, c(i) := as.character(get(i))]
            }
        }
        
        setnames(finalAupus, "timePointYears", "timePointYearsSP")
        finalAupus[, timePointYearsSP := as.numeric(timePointYearsSP)]
        finalAupusKey = c("geographicAreaFS", "measuredItemFS",
            "timePointYearsSP")
        ## Filling in missing columns
        ## fillMissingColumn(finalAupus,
        ##                   allColumn =
        ##                       paste0("Value_measuredElementFS_",
        ##                              aupusParam$elementCode))
        ## fillMissingColumn(finalAupus,
        ##                   allColumn =
        ##                       paste0("flagFaostat_measuredElementFS_",
        ##                              aupusParam$elementCode))
        setkeyv(finalAupus, cols = finalAupusKey)        
    }
    finalAupus
}
