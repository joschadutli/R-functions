#' Set up multiple job system
#' 
#' @author Chenyu Li
#' 
#' @param operation description
#' 
#' @param path A folder path used to save the file
#' 
#' 
read_jobLog <- function(path = NULL) {
  
  if (is.null(path)){
    file_path = "./job_log.rds"
  } else {
    file_path = paste0(path, "job_log.rds", sep="")
  }
  
  Table_status <- readRDS(file_path)
  
  return(Table_status)
  
}