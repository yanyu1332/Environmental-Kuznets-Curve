main <- function(){
  my_folder <- "initial"
  ##データの読み込み
  data_master <- read_interim("master")
  #main_varnames <- c(population_mean)
  output_folder <- "initial"
  
  #(a)
  data_master %>% 
    gen_table %>% 
    print ##何らかの形式で保存することはできませんでした...
  
  #(b)
  data_master %>%  
    plot_kuznets_curve(x_var = gdp_per_cap,
                       y_var = pollution_mean,
                       group_var = country,
                       label_var = year) %>% 
  save_my_plot(var_name = "Environmenta Kuznets Curve",
               folder_name = my_folder)
  
  #(c)
  data_master %>% 
    run_regression() %>% 
    print ##何らかの形式で保存することはできませんでした...
}

data_master[data_master$country == "JPN"]

## ==============================
# (a) 記述統計の作成
## (1)度数分布表の作成
gen_table <- function(data_input){
  table_output <- data_input %>% 
  psych::describeBy(group = "country") 
  return(table_output)
}

save_table <- function(data_input,file_name,folder_name){
  file <- paste0(file_name, ".png")
  file_path <- here::here("04_analyze", folder_name,"table", file)
  save(data_input,file = file_path)
}

## ==============================
# (b) 図の作成
# 環境クズネッツ曲線の仮説を視覚的に検証するための図を作成しなさい
# 他者と共有できるように図を整理しなさい

plot_kuznets_curve <- function(data_input,x_var,y_var,group_var,label_var){
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  group_var <- rlang::enquo(group_var)
  label_var <- rlang::enquo(label_var)
  
  plot_output <- ggplot2::ggplot(data = data_input,mapping = aes(x=!!x_var,y=!!y_var,color = !!group_var,label = !!label_var)) +
    labs(x = "gdp_per_capita", y = "CO2 pollution") +
    ggtitle("Environmental Kuznets Curve for USA and Japan CO2 emissions") + 
    geom_point(position="identity", size=2, alpha = 0.8) +
    geom_text_repel() +
    geom_line()
  #library("gridExtra")
  #gridExtra::grid.arrange(p1, p2, nrow = 1)
  
  return(plot_output)
}



## ==============================
# (c) 回帰分析
# 回帰分析で環境クズネッツ曲線の仮説を検証するには、どのような回帰式を推定すればよいですか
# 回帰分析の結果を表としてまとめ、議論しなさい

run_regression <- function(data_input){
  summary_list <- list(
    "linear_regression" = estimatr::lm_robust(
      pollution_mean ~ gdp_per_cap,
      clusters = country,
      se_type = "stata",
      data = data_input
    ),
    "Nonlinear_regression" = estimatr::lm_robust(
      pollution_mean ~ gdp_per_cap + I(gdp_per_cap^2),
      clusters = country,
      se_type = "stata",
      data = data_input
    )
  )
  return(summary_list)
}


## ==============================


source("01_admin/functions/basics.R")
library(ggplot2)
library(ggrepel)
library(dplyr)
library(psych)
library(gridExtra)
main()