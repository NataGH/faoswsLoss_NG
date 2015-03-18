##' Reassign Global Variable
##' 
##' Parameters used in running the imputation or processing algorithms are
##' assigned to the global environment.  To protect these parameters from being
##' accidentally overwritten, we call lockBinding.  This prevents reassignment
##' of these variables, but occassionally these variables do need to be
##' reassigned.  This function allows this reassignment by temporarily
##' unlocking the variable, reassigning the value, and relocking it. This
##' function should be used with caution: values that have already been ensured
##' can be changed and not checked, which could be disastrous.
##' 
##' @param var The name of the variable to be reassigned.
##' @param value The value to be reassigned to var.
##' 
##' @return No value is returned, instead this function changes the global
##' variable var.
##' 
##' @export
##' 

reassignGlobalVariable = function(var, value){
    
    ### Data Quality Checks
    stopifnot(exists(var, envir = .GlobalEnv))
    
    ### Unlock, reassign, relock
    unlockBinding(var, .GlobalEnv)
    assign(x = var, value = value, envir = .GlobalEnv)
    lockBinding(var, .GlobalEnv)
}