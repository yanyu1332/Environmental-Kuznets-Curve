main <- function(){
  my_folder <- "initial"
  ##データの読み込み
  data_master <- read_interim("master")
  main_varnames <- c()
  output_folder <- "initial"
  
  #(a)
  
  #(b)
  data_master %>%  
    plot_kuznets_curve(x_var = gdp_per_cap,
                       y_var = pollution_mean,
                       group_var = country,
                       label_var = year) %>% 
  save_my_plot(var_name = "Environmenta Kuznets Curve",
               folder_name = my_folder)

}

## ==============================
# (a) 記述統計の作成
## (1)度数分布表の作成
gen_quantile <- function(data_input){
  data_input <- data_master %>% 
  group_by(data_input, country) %>% 
  quantile(data_input$gdp_per_cap)
  
}
## (2)代表値の作成(mean,median,variance)
gen_central_tendency <- function(){
  
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
    labs(x = "gdp_per_capita", y = "pollution") +
    ggtitle("Environmental Kuznets Curve for USA and Japan sulfur dioxide emissions") + 
    geom_point(position="identity", size=2, alpha = 0.8) +
    geom_text_repel() +
    geom_line()
  
  return(plot_output)
}

## ==============================
# (c) 回帰分析
# 回帰分析で環境クズネッツ曲線の仮説を検証するには、どのような回帰式を推定すればよいですか
# 回帰分析の結果を表としてまとめ、議論しなさい

plot_regression <- function(){
  
}

## ==============================


source("01_admin/functions/basics.R")
library(ggplot2)
library(ggrepel)
library(dplyr)
main()