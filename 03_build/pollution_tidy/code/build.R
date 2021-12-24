main <- function(){
  my_folder <- "pollution"
  file_name <- "CO2"
  
  raw_data <- read_raw(my_folder, file_name)
  
  tidy_data <- raw_data %>% 
    prep_shape() %>% 
    prep_nonnumeric() %>% 
    ##check data type
    prep_asserts()
  
  save_interim(tidy_data, my_folder, extension = "tidy")
}


##read the raw data
read_raw <- function(my_folder, file_name){
  file_path <- here::here("02_raw", my_folder, "data", paste0(file_name, ".xlsx"))
  ## skip the first two rows
  data_output <- readxl::read_excel(file_path, skip = 2)
  
  return(data_output)
}


prep_shape <- function(data_input){
  data_output <- data_input %>% 
    
    ##列名を一部変更
    dplyr::rename(country = ...1) %>% 
    
    dplyr::mutate(across(everything(), as.character)) %>% 
    
    ## https://heavywatal.github.io/rstats/tidyr.html
    tidyr::pivot_longer(
      cols = !country,
      names_to = "year",
      names_prefix = "pollution_",
      values_to = "pollution_original"
    ) 
  
  return(data_output)
}


##change pollution_original into numeric from character
prep_nonnumeric <- function(data_input){
  data_output <- data_input %>% 
    ## https://kazutan.github.io/kazutanR/hands_on_170730/mutate.html
    dplyr::mutate(
      missing_dummy = dplyr::if_else(pollution_original =="missing",true = 1, false = 0, missing = 0),
      pollution_numeric = replace(pollution_original,which(pollution_original == "missing"),NA),
      pollution_numeric = as.numeric(pollution_numeric),
      year = as.numeric(year))
  
  return(data_output)
}


prep_asserts <- function(data_input){

  testthat::test_that(desc = "data type correct",{
    testthat::expect_true(is.numeric(data_input$pollution_numeric))
    testthat::expect_true(is.numeric(data_input$year))
  })
  
  return(data_input)
}


##import a module or package
box::use(`functions`/basics)

main()
