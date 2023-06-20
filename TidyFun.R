

#' Download data from JATOS
#' 
#' This function downloads result data from a JATOS server using an API token and specified UUID and batch ID.
#' It then unzips and extracts the relevant data, including metadata, file names, and file contents.
#' The function returns a data frame with the relevant data.
#'
#' @param token An API token, you can create a new one on JATOS. Details see: https://www.jatos.org/JATOS-API.html#personal-access-tokens
#' @param url Server address. The default value is the address of our lab server.
#' @param uuId A unique code of study, you can find it on JATOS
#' @param batchId A unique code of a batch session in the study.
#' @param dataPath A path used to save data. If NUll, data will be saved in working directory.
#'
#' @return data.frame
#' @export
#'
get_JATOS_data <- function(token, url = "https://coglab.xyz/jatos/api/v1/results", uuId, batchId, dataPath = NULL) {
  
  require(httr) # Load the httr library
  
  if (is.null(dataPath)) dataPath = "./" # Set the default data path
  
  # Create the authorization header with the specified token
  headers = headers = c(`Authorization` = str_glue("Bearer {token}"))
  
  # Send an HTTP GET request to the specified URL, passing in the UUID and batch ID
  # Also pass in the authorization header and write the response to a temporary file
  res <- httr::GET(
    url = str_glue("{url}?studyUuid={uuId}&batchId={batchId}"),
    httr::add_headers(.headers=headers), 
    write_disk(str_glue("{dataPath}tmp.jrzip"), overwrite = TRUE)
  )
  
  # Unzip the downloaded file and extract the file names into a list
  filelist = unzip(str_glue("{dataPath}tmp.jrzip"), exdir=str_glue("{dataPath}JATOS_DATA"), overwrite = TRUE)
  
  # Extract relevant data from the file names and store in a data frame
  file_table <- data.frame(filelist) %>% 
    rename(file = filelist) %>% 
    mutate(resultID = str_extract(file,"study_result_([0-9]+)"),
           componentID = str_extract(file,"comp-result_([0-9]+)"),
           resultID = as.numeric(str_extract(resultID,"([0-9]+)")),
           componentID = as.numeric(str_extract(componentID,"([0-9]+)")))
  
  # Remove the temporary file
  file.remove(str_glue("{dataPath}tmp.jrzip"))
  
  # Read the metadata from the last file in the list (assuming it is in JSON format)
  metaData <- jsonlite::read_json(filelist[length(filelist)])$data[[1]]$studyResults
  
  # Extract relevant metadata from the metadata list and store in a data frame
  info_table <- data.frame()
  for (a in 1:length(metaData)) {
    info_table[a, "resultID"] = metaData[[a]]$id
    info_table[a, "componentID"] = metaData[[a]]$componentResults[[1]]$id
    info_table[a, "duration"] = ifelse(is.null(metaData[[a]]$duration),
                                       metaData[[a]]$lastSeenDate - metaData[[a]]$startDate,
                                       sapply(strsplit(metaData[[a]]$duration, ":"), function(n) as.numeric(n) %*% c(3600, 60, 1))
    )
    info_table[a, "studyState"] = metaData[[a]]$studyState
  }
  
  # Combine the file data and metadata into a single data frame
  outcome <- info_table %>% 
    right_join(file_table) %>% 
    filter(!is.na(resultID))
  
  # Return the combined data frame
  return(outcome)
}


# Read JATOS data
read_JATOS_files <- function(files) {
  
  # Create an empty string to store the data
  combined_text = ""
  
  # Iterate over each file in the list of files to read
  combined_text <- map_vec(files, .f = read_file, .progress = TRUE)
  single_text <- str_c(combined_text, collapse = "")
  modified_text <- gsub("\\}\\]\\s{0,}\\[\\{", "\\},\\{", single_text, perl = TRUE)
  data <- jsonlite::fromJSON(modified_text)

  # Return the combined data frame
  return(data)
  
}



filterOutlier <- function(.data, vars, std = 3) {

  for (var in vars) {
    df <- .data %>%
      mutate(zscore = scale(.data[[var]])) %>%
      mutate(across({{var}}, .fns = ~ ifelse(abs(zscore) > std, NA, .data[[var]]))) %>%
      mutate(across({{var}}, .fns = ~ ifelse(
        zscore > std,
        mean(.data[[var]], na.rm =
               T) + std * sd(.data[[var]], na.rm = T),
        .data[[var]]
      )),
      across({{var}}, .fns = ~ ifelse(
        zscore < -std,
        mean(.data[[var]], na.rm =
               T) - std * sd(.data[[var]], na.rm = T),
        .data[[var]]
      ))) %>%
      select(!zscore)
  }
  return (df)
}