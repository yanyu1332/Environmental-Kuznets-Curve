main <- function(){
  my_folder <- "gdp"
  name_list <- c("Japan", 
                 "United States")
  
  ##read raw data
  raw_data <- read_raw(my_folder, name_list)
  
  tidy_data <- raw_data %>% 
    prep_duplictae_count %>% 
    check_duplication
  
  save_interim(tidy_data, my_folder, extension = "tidy")
}


##read raw data
read_raw <- function(my_folder, name_list){
  
  data_list <- name_list %>%  
    ## purr::map https://qiita.com/kilometer/items/b4977df268d2c21211fc
    purrr::map(function(name) 
      here::here("02_raw", my_folder,"data", paste0(name, ".csv"))
    ) %>% 
    
    purrr::map(readr::read_csv)

  data_output <- data_list %>%
    ## bind_rows https://dplyr.tidyverse.org/reference/bind.html
    dplyr::bind_rows(.id = "country_num") %>% 
    dplyr::mutate(country = name_list[as.numeric(country_num)]) %>% 
    dplyr::select(-country_num)
  
  return(data_output)
}

##check duplication
prep_duplictae_count <- function(data_input){
  # data_input <- data_output #test用
  data_output <- data_input %>% 
    dplyr::group_by(country, year) %>% 
    dplyr::mutate(duplicate_id = dplyr::row_number()) %>% 
    dplyr::ungroup()
  
  return(data_output)
}

check_duplication <- function(data_input){
  data_input <- data_output #test
  num_row <- nrow(data_input)
  dup_id <- data_input %>% dplyr::select(duplicate_id)
  
  ## setequal http://cse.naro.affrc.go.jp/takezawa/r-tips/r/14.html
  if(setequal(dup_id, as.data.frame(rep(1, num_row)))){
    data_output <- data_input %>% dplyr::select(-duplicate_id)
    return(data_output)
  }else{
    print("Error : There is duplication !!")
  }
}


##box::use(`functions`/basic)
source("01_admin/functions/basics.R")

main()