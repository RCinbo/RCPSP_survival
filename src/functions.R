get_dataset <- function(files, subset = "J30"){
  assertthat::is.string(subset)
  files_to_read <- files[stringr::str_starts(files, subset)]
  data <- files_to_read %>% purrr::map(read_and_clean_data)
  if(length(files_to_read > 1)){
    data <- lapply(seq(length(data)), FUN = function(i){
      x <- data[[i]]
      return(x)}
      )
    data <- do.call(rbind, data)
  }
  return(data)
}


read_and_clean_data <- function(filename){
  read_all <- read_csv2(
    file = find_root_file(
      "data", filename,
      criterion = has_file("RCPSP_survival.Rproj")
    ),
    progress = FALSE,
    show_col_types = FALSE)
  author <- stringr::str_split_1(filename, pattern = "_")[2]
  author <- stringr::str_split_1(author, pattern = "\\.")[1]
  skip <- which(read_all[,2] == "Type")
  data <- read_csv2(
    file = find_root_file(
      "data", filename,
      criterion = has_file("RCPSP_survival.Rproj")
    ),
    skip = skip,
    progress = FALSE,
    show_col_types = FALSE) %>%
    as.data.frame() %>%
    dplyr::select(1:4) %>%
    mutate(author = author)
  if(!("ID" %in% colnames(data))){
    data <- data_PSBLIB %>%
      dplyr::select(`ID set`, FileName) %>%
      mutate(FileName = str_remove(FileName, ".sm")) %>%#stringr::str_split_1(FileName, pattern = ".")[1]) %>%
      right_join(data %>%
                   mutate(FileName = str_remove(FileName, ".mm"))) %>% #stringr::str_split_1(FileName,pattern = ".")[1]))
      rename(ID = `ID set`) %>%
      dplyr::select(-FileName)
  }
  return(data)
}