main <- function(){
  my_folder <- "pollution"
  tidy_data <- read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    ##generate data table with  year interval 5 
    interval_data(interval = 5) 
  
  save_interim(ready_data, my_folder, extension = "ready")
}


interval_data <- function(data_input, interval){
  
  # data_input <- tidy_data #test用
  # interval <- 5
  
  ## 2. ５年の間の年を、より近い年に含めて、平均値を計算しなさい
  data_summarized <- data_input %>%
    ##interval 年を基準に年を割り振る
    dplyr::mutate(group = round(year/interval) *
                    dplyr::if_else(country == "JPN",
                                   true = 1,
                                   false = 2,
                                   missing = 3)) %>%
    dplyr::mutate(every_5_years = group * interval )%>% 
    dplyr::group_by(every_5_years,country) %>%
    dplyr::summarise(pollution_mean = mean(pollution_numeric, na.rm = TRUE),
                     missing_rate = mean(missing_dummy)) %>%
    dplyr::ungroup() 
    # dplyr::mutate(years = every_5_years / dplyr::if_else(country == "JPN",
    #                                                      true = 1,
    #                                                      false = 2,
    #                                                      missing = 3)) %>% 
    # dplyr::select(-c(every_5_years))
  
  ## 1. ５年おきのデータ(例: 1960, 1965, 1970, ...) のみを抽出しなさい
  data_output <- data_input %>%
    dplyr::filter(year %% interval == 0) %>% 
    dplyr::mutate(every_5_years = year *
                    dplyr::if_else(country == "JPN",
                                   true = 1,
                                   false = 2,
                                   missing = 3)) %>%
    dplyr::left_join(data_summarized, by = "every_5_years") %>% 
    dplyr::select(-c(pollution_original,missing_dummy,every_5_years,country.y)) %>% 
    dplyr::rename(country = country.x)
  return(data_output)  
}


source("01_admin/functions/basics.R")

main()