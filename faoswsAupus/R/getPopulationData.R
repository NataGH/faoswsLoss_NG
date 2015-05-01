##' Get Population Data
##' 
##' This is the function to obtain the population separately.
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.  Only required
##' if database = "old".
##' 
##' @return A data.table containing the country and year values specified in
##' aupusParam and value and flags for the population (elements 11 and 21).
##' 
##' @export
##' 


## TODO (Michael): Apply calculation of 11 and 21 here.

getPopulationData = function(aupusParam, database = c("new", "old"), conn){
    
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
                WHERE area =", areaCode,
                "AND item = '1'")
        aupus =
            data.table(dbGetQuery(conn = conn, aupusQuery))
        meltedAupus =
            suppressWarnings(melt(aupus,
                                  id.var = c("AREA", "ITEM", "ELE")))
        meltedAupus[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedAupus[, type := gsub("[0-9|_]", "", variable)]
        meltedAupus[, `:=`(c("variable", "ITEM"), NULL)]
        finalPopulation =
            dcast.data.table(meltedAupus, AREA + Year ~ type + ELE,
                             value.var = "value")
        valueCol = grep("NUM", colnames(finalPopulation), value = TRUE)
        finalPopulation[, (valueCol) :=
                       lapply(valueCol, function(x)
                           as.numeric(finalPopulation[[x]]))]
        for(i in valueCol){
            remove0M(data = finalPopulation, value = i,
                     flag = gsub("NUM", "SYMB", i), naFlag = "M")
        }
        setnames(finalPopulation,
                 old = c("AREA"),
                 new = c("areaCode"))
        setkeyv(finalPopulation, cols = c("areaCode", "Year"))
    } else if(database == "new"){
        populationDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(aupusParam$areaCode)),
                 Dimension(name = "measuredItemFS",
                           keys = as.character(1)),
                 Dimension(name = "timePointYears",
                           keys = as.character(aupusParam$year)),
                 Dimension(name = "measuredElementFS",
                           keys = as.character(c(11, 21))))

        populationDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "FS1_SUA",
                       dimensions = populationDimension)

        populationPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemFS", ascending = TRUE),
            Pivoting(code = "timePointYears", ascending = FALSE),
            Pivoting(code = "measuredElementFS", ascending = TRUE)
        )
        
        finalPopulation =
            GetData(key = populationDataContext, flags = TRUE,
                    normalized = FALSE, pivoting = populationPivot)

        ## Convert list of NULL to vector of NA
        for(i in colnames(finalPopulation)){
            if(typeof(finalPopulation[, i, with = FALSE]) == "list"){
                finalPopulation[, c(i) := faosws::NullToNa(get(i))]
            }
            if(grepl("Value", i)){
                finalPopulation[, c(i) := as.numeric(get(i))]
            } else if(grepl("flag", i)){
                finalPopulation[, c(i) := as.character(get(i))]
            }
        }

        oldNames = grep(aupusParam$keyNames$elementName, colnames(finalPopulation),
                          value = TRUE)
        setnames(finalPopulation,
                 old = c("timePointYears", oldNames),
                 new = c("timePointYearsSP",
                     gsub(aupusParam$keyNames$elementName, "population", oldNames)))
        finalPopulation[, measuredItemFS := NULL]
        finalPopulation[, timePointYearsSP := as.numeric(timePointYearsSP)]
        finalPopulationKey = c("geographicAreaFS", "timePointYearsSP")
        setkeyv(finalPopulation, finalPopulationKey)
    }
    finalPopulation
}
        
