##' Get Input from Processing Data
##' 
##' Some data used in AUPUS will be specified by the users prior to the AUPUS
##' routine (for example, official data provided by some countries).  This
##' function provides a way for this input data to be specified and utilized
##' in AUPUS.
##'
##' @param aupusParam A list of running parameters to be used in pulling the data.
##' Typically, this is generated from getAupusParameter (see that function for
##' a description of the required elements).
##' @param database Whether to use the new or the old statistical
##' working system.
##' @param conn The RJDBS connection to the old working system.  Only required
##' if database = "old".
##' 
##' @return This function returns a data.table object containing the input from
##' processing data.
##' 
##' @export
##' 

getInputFromProcessData = function(aupusParam, database = c("new", "old"), conn){
    
    ## Data Quality Checks
    if(!exists("aupusParameterEnsured") || !aupusParameterEnsured)
        ensureAupusParameter(aupusParam)
    stopifnot(database %in% c("new", "old"))
    if(database == "old")
        stopifnot(!missing(conn))
    
    database = match.arg(database)
    if(database == "old"){
        inputQuery =
            paste0("SELECT *
                FROM input_from_procv
                WHERE area = ", areaCode)
        input =
            data.table(dbGetQuery(conn, inputQuery))
        meltedInput =
            suppressWarnings(melt(input, id.var = c("AREA", "ITEM_PARENT",
                                             "ITEM_CHILD")))
        meltedInput[, Year := as.numeric(gsub("[^0-9]", "", variable))]
        meltedInput[, type := gsub("[0-9|_]", "", variable)]
        meltedInput[, variable := NULL]
        finalInput =
            dcast.data.table(meltedInput, AREA + ITEM_PARENT +
                                 ITEM_CHILD + Year ~ type,
                             value.var = "value")
        finalInput[, NUM := as.numeric(NUM)]
        remove0M(input, value = "NUM", flag = "SYMB", naFlag = "M")
        setnames(finalInput,
                 old = c("AREA", "ITEM_PARENT", "ITEM_CHILD", "NUM",
                     "SYMB"),
                 new = c("areaCode", "itemParentCode", "itemCode",
                     "NUM_INPUT", "SYMB_INPUT"))
        setkeyv(finalInput,
                cols = c("areaCode", "itemParentCode", "itemCode",
                    "Year"))
        finalInput
    } else if(database == "new"){
        
        inputDimension =
            list(Dimension(name = "geographicAreaFS",
                           keys = as.character(aupusParam$areaCode)),
                 Dimension(name = "measuredItemParentFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "measuredItemChildFS",
                           keys = as.character(aupusParam$itemCode)),
                 Dimension(name = "timePointYearsSP",
                           keys = as.character(aupusParam$year)))

        inputDataContext =
            DatasetKey(domain = "faostat_one",
                       dataset = "input_from_proc_fs",
                       dimensions = inputDimension)

        inputPivot = c(
            Pivoting(code = "geographicAreaFS", ascending = TRUE),
            Pivoting(code = "measuredItemParentFS", ascending = TRUE),
            Pivoting(code = "measuredItemChildFS", ascending = TRUE),    
            Pivoting(code = "timePointYearsSP", ascending = FALSE)
        )

        finalInput =
            GetData(key = inputDataContext, flags = TRUE,
                    normalized = TRUE, pivoting = inputPivot)

        setnames(finalInput,
                 old = c("Value", "flagFaostat"),
                 new = c("Value_input", "flagFaostat_input"))

        ## NOTE (Michael): Why does the year has 'SP' suffix?
        inputKey = c("geographicAreaFS", "measuredItemParentFS",
            "measuredItemChildFS", "timePointYearsSP")
        setkeyv(finalInput, cols = inputKey)
    }
    finalInput
}
        
