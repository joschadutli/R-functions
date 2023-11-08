#' Initialize job log.
#' 
#' @description
#' This function will create a data frame including model information and store it in local disk
#' 
#' @param path A folder path used to save the file
#' 
init_jobLog <- function(path = NULL) {
  
  Table_status <- data.frame(
    index = 0,
    core = 0,
    status = "completed",
    priority = 1)
  
  if (is.null(path)){
    file_path = "./job_log.rds"
  } else {
    file_path = paste0(path, "job_log.rds", sep="")
  }
  
  write_rds(Table_status, file_path)
  
}
