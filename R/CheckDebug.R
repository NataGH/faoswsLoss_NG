##' Check if the script is being run on Rserve or being debugged
##' 
##' This functions is a small utility function which allows checking if the 
##' script is running on the server or being debugged by a developer. The server
##' has an environment variable called R_DEBUG_MODE which is present if the 
##' script is running there.
##' 
##' This function thus returns TRUE when not on the server and FALSE when on the
##' server.

CheckDebug <- function(){
  
  debug <- Sys.getenv("R_DEBUG_MODE")
  return(debug == "")
  
}