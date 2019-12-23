### Dependecies ====
require(dplyr, quietly = TRUE)
require(readxl, quietly = TRUE)
require(stringr, quietly = TRUE)

### Functions ====
read_consumption_files <- function(full_path){
  
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
    as.data.frame(.) %>%
    dplyr::mutate_all(.funs = as.character)
  names(df_split) <- c("Time", "Date", "Time_from", "Time_to")
  df_split$Time <- as.character(df_split$Time)
  
  data <- data %>%
    dplyr::left_join(df_split, by="Time") %>%
    dplyr::mutate(Date = as.Date(Date, format="%d.%m.%Y")) %>%
    dplyr::select(Date, Time_from, Time_to, `Total (A+)`)
  
  return(data)
}

read_sales_files <- function(full_path){

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
  xls_files_read_in <- lapply(xls_files, 
                              function(x) read_xls_file(file=x))
  df_all <- Reduce(function(x, y) 
    merge(x, y, by="Time"), xls_files_read_in)
  return(df_all)
}

mutate_price_files <- function(data){
  data <- data %>%
    reshape2::melt(id="Time") %>%
    dplyr::distinct() %>%
    stats::setNames(., c("Time", "Date", "mwh")) %>%
    dplyr::mutate(Time = as.character(Time)) %>%
    dplyr::mutate(kwh = 1000 * mwh)
  
  df_split <- stringr::str_match(
    data$Time, 
    "([0-9]{1,2}) ?- ?([0-9]{1,2})") %>%
    as.data.frame(.) %>%
    stats::setNames(., c("Time", "Time_from", "Time_to")) %>%
    dplyr::distinct() %>%
    dplyr::mutate_all(.funs = as.character) %>%
    dplyr::mutate(Time_from = as.character(paste0(Time_from, ":00")),
                  Time_to = as.character(paste0(Time_to, ":00")))
  
  data <- data %>%
    dplyr::left_join(df_split, by="Time") %>% 
    dplyr::mutate(Date = as.Date(Date, format="%d-%m-%Y")) %>%
    dplyr::select(Date, Time_from, Time_to, mwh, kwh)
  
  return(data)
}

# Main ====
## Consumption files
full_path <- "./data/input/pateretais/"
df <- read_consumption_files(full_path = full_path)
df_consumption <- mutate_consumption_files(df)

full_path <- "./data/input/"
df <- read_sales_files(full_path = full_path)
df_sales <- mutate_price_files(df)

df_consumption %>%
  dplyr::left_join(df_sales, by=c("Date", "Time_from", "Time_to")) %>%
  data.table::fwrite(
    paste0("./calculated_prices_", as.character(Sys.Date()), "_",
           as.character(as.numeric(Sys.time())), ".csv")
    )

