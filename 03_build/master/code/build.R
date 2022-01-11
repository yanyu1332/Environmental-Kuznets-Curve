main <- function(){
  gdp_data <- read_interim("gdp", extension = "ready")
  pollution_data <- read_interim("pollution", extension = "ready")
  
  ##プライマリーキーの設定
  gdp_data_for_merge <- prep_gdp(gdp_data)
  
  ##マージする
  master_data <- prep_merge(
    gdp_data_for_merge,
    pollution_data) %>% 
    sort
  
  save_interim(master_data, "master")
}


prep_gdp <- function(data_input){
  data_output <- data_input %>% 
    dplyr::mutate(country = dplyr::if_else(country == "Japan", "JPN", country),
                  country = dplyr::if_else(country == "United States", "USA", country),
    ) 
  return(data_output)
}


prep_merge <- function(gdp_data, pollution_data){
  data_output <- gdp_data %>% 
    ##by引数でプライマリーキーを対応付ける
    dplyr::left_join(pollution_data, by = c("country" = "country",
                                            "year" = "year"))
  
  return(data_output)
}

sort <- function(data_input){
  list = c("year","country","population","GDP","gdp_per_cap","pollution_single","pollution_mean","missing_rate")
  data_output <- data_input %>% 
    dplyr::select(list)
}


source("01_admin/functions/basics.R")

main()