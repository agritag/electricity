
full_path <- "./data/input/"
df <- read_sales_files(full_path = full_path)
mutate_price_files(df) %>% View()

read_sales_files <- function(full_path){
  require(readxl)
  require(dplyr)
  
  read_xls_file <- function(file){
    df_out <- readxl::read_excel(
      path=paste0(full_path, file), skip=1, 
      .name_repair = "minimal") %>%
      setNames(., c("Time", tail(names(.), -1))) %>%
      as.data.frame() %>%
      dplyr::filter(grepl("^[0-9]{1,2} ?- ?[0-9].*$", Time))
    return(df_out)
  }
  
  xls_files <- list.files(full_path, pattern = ".xls")
  xls_files_read_in <- lapply(xls_files, function(x) read_xls_file(file=x))
  df_all <- Reduce(function(x, y) merge(x, y, by="Time"), xls_files_read_in)
  return(df_all)
}

mutate_price_files <- function(data){
  data <- data %>%
    reshape2::melt(id="Time") %>%
    stats::setNames(., c("Time", "Date", "mhw")) %>%
    dplyr::mutate(Time = as.character(Time)) %>%
    dplyr::mutate(khw = 1000 * mhw)
  
  df_split <- stringr::str_match(
    data$Time, 
    "([0-9]{1,2}) ?- ?([0-9]{1,2})") %>%
    as.data.frame(.) %>%
    stats::setNames(., c("Time", "Time_from", "Time_to")) %>%
    dplyr::mutate_all(.funs = as.character)
  
  data <- data %>%
    dplyr::left_join(df_split, by="Time")
  
  return(data)
}
