
full_path <- "./data/input/pateretais/"
df <- read_in_consumption_files(full_path = full_path)
mutate_consumption_files(df)

read_in_consumption_files <- function(full_path){
  require(readxl)
  require(dplyr)

  read_xls_file <- function(file){
    df_out <- readxl::read_excel(path=paste0(full_path, file), 
                                 skip=8) %>%
      as.data.frame() %>%
      dplyr::filter(grepl("^[0-9]{1,2}.[0-9]{1,2}.*$", Time))
    return(df_out)
  }
  
  xls_files <- list.files(full_path, pattern = ".xls")
  df_out <- do.call(
    rbind, lapply(xls_files, function(x) read_xls_file(file=x)))
  return(df_out)
}


mutate_consumption_files <- function(data){
  data$Time <- as.character(data$Time)
  
  df_split <- stringr::str_match(
    df$Time, 
    paste0("([0-9]{1,2}.[0-9]{1,2}.[0-9]{4}) ([0-9]{1,2}:[0-9]{1,2})",
           " - ([0-9]{1,2}:[0-9]{1,2})")) %>%
    as.data.frame(.)
  names(df_split) <- c("Time", "Date", "Time_from", "Time_to")
  df_split$Time <- as.character(df_split$Time)
  
  data <- data %>%
    dplyr::left_join(df_split, by="Time")
  
  return(data)
}

