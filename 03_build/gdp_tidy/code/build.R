main <- function(){
  my_folder <- "gdp"
  name_list <- c("Japan", 
                 "United States")
  
  ##read raw data
  raw_data <- read_raw(my_folder, name_list)
  
  tidy_data <- raw_data 
  
  save_interim(tidy_data, my_folder, extension = "tidy")
}


##read raw data
read_raw <- function(my_folder, name_list){
  
  data_list <- name_list %>%  
    
    purrr::map(function(name) 
      here::here("02_raw", my_folder,"data", paste0(name, ".csv"))
    ) %>% 
    
    purrr::map(readr::read_csv)

  data_output <- data_list %>%
    ## 縦結合
    dplyr::bind_rows(.id = "country_num") %>% 
    dplyr::mutate(country = name_list[as.numeric(country_num)]) %>% 
    dplyr::select(-country_num)
  
  return(data_output)
}


##box::use(`functions`/basic)
source("01_admin/functions/basics.R")

main()