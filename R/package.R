#' Tools providing access to the FAO Statistical Working System
#' 
#' This package allows access to the FAO SWS via its API when the appropriate 
#' client certificates are present. The main workflow is to acquire a token from
#' the SWS web interface and use the \code{\link{GetTestEnvironment}} function 
#' to open a session. After this, use the \code{\link{GetData}} function. To
#' read the vignette: \code{vignette("R_API_2014")}
#' 
#' @docType package
#' @name faosws-package
#' @aliases faosws
#' @author Engineering Ingegneria Informatica
#'   
#' @include SetClientFiles.R
#'   
#' @import data.table

# prevent roxygen2 marking this as dataset
NULL

# Set environment in which authentication variables reside
.swsenv <- new.env()
# Populates it with default variables
SetClientFiles()

# Add params for changeset
assign("changeset",  
       list(pagesize = 5000),
       envir = .swsenv)