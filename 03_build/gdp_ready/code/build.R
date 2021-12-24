main <- function(){
  my_folder <- "gdp"
  tidy_data <- read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    ##generate GDP per capita
    gen_gdp_per_capita()
  
  save_interim(ready_data, my_folder, extension = "ready")
}