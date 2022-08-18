

# Merge results into a data frame

MergeFiles <- function(folder, pattern, format="csv") {
  
  # get the list of result files' name
  file_names <- list.files(path = folder, pattern = pattern, full.names = TRUE)  
  file_names <- sort(file_names, decreasing = TRUE)
  
  # read file sequentially and combine all of them as a single data frame.
  for (i in 1:length(file_names)){ 
    
    if (format == "excel"){ 
      # the format is excel.
      data <- readxl::read_excel(file_names[i])
    } else if (format == "csv") { 
      # for the format csv.
      data <- readr::read_csv(file_names[i])
    } else { 
      # for other data types
      data <- readr::read_delim(file_names[i])
    }
    
    if(i == 1) DAT <- data # if the first file, create the variable
    if(i >  1) DAT <- rbind(DAT,data) # if not the first file, merge data
  } 
  
  return(DAT)
  
}