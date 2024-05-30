
# clear workspace
  rm(list = ls())

# ----load-packages-------------

requireNamespace('gtsummary')
requireNamespace('flextable')
  

requireNamespace('dplyr')
requireNamespace('ggplot2')
requireNamespace('rstatix')
requireNamespace('ggpubr')
requireNamespace('ggpmisc')
requireNamespace('car')
requireNamespace('systemfonts')

library(ggplot2)

## -- User-defined-functions

# Function to remove leading zeros
remove_leadingzero <- function(x) {
  sub("^0+", "", as.numeric(x)) 
}

# modify the default function
estimate_fun_modified <-  function(x) {
  x <- gtsummary::style_sigfig(x, digits = 2)
  x <- remove_leadingzero(x)
}

pvalue_stars <- function(p) {
  p <- as.numeric(p)  # Ensure p is numeric
  
  styled_p <- gtsummary::style_pvalue(p, digits = 3)
  formatted_p <- sub("^0+", "", as.character(styled_p))
  
  dplyr::case_when(
    p < 0.001 ~ paste0(formatted_p, "***"),
    p < 0.01  ~ paste0(formatted_p, "**"),
    p < 0.05  ~ paste0(formatted_p,"*"),
    TRUE      ~ formatted_p
  )
  
}

# -----load-data----------------
 # generate data
 # Setting a seed for reproducibility
  set.seed(123)  

 # Generates random variables
  child_age      = sample(seq(1,  7), 100, replace = TRUE)
  mom_age        = sample(seq(17, 45), 100, replace = TRUE)
  income         = sample(seq(10000, 150000), 100, replace = TRUE)
  child_gender   = sample(c(0, 1), 100, replace = TRUE, prob = c(0.4, 0.6))
  mom_education  = sample(c("secondary","high school diploma","college degree"), 100, replace = TRUE)
  mom_smoking    = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.3, 0.7))
  mom_drinking   = sample(c(TRUE, FALSE), 100, replace = TRUE, prob = c(0.6, 0.4))
  screen_time    = sample(seq(1,  6), 100, replace = TRUE)
  child_autism_diagnosis = sample(c( TRUE, FALSE), 100, replace = TRUE, prob = c(0.2, 0.8))
  
  # Assuming mom_education is a factor variable with levels "High School", "Bachelor's", "Master's", and "PhD"
  mom_education_dummy <- model.matrix(~ mom_education - 1)  # Generate dummy variables
  
  # generate outcome var based on a linear function
  child_sleep_quality = round( 2  * scale(child_age)       +
                               1  * scale(mom_age)         +
                              .1  * scale(income)          +
                              .1  * child_gender           +
                              # 1  * mom_education_dummy    +
                               5  * mom_smoking            +
                              .5  * mom_drinking           +
                               3  * scale(screen_time)     +
                               8  * child_autism_diagnosis +
                              rnorm(100, mean = 0, sd = 5)  # noise
                              ,0)
  
 #  create a data frame  
    
  df = data.frame("child_age"      = child_age    ,
                  "mom_age"        = mom_age      ,
                  'income'         = income       ,
                  'child_gender'   = child_gender ,
                  'mom_education'  = mom_education,
                  'mom_smoking'    = mom_smoking  ,
                  "mom_drinking"   = mom_drinking ,
                  "screen_time"    = screen_time ,
                  "child_autism_diagnosis" = child_autism_diagnosis,
                  "child_sleep_quality"    = child_sleep_quality  )
 
    
  # view data
  # head(df)
  
  # create a data frame with missing values
  df_missing = df
  # create missing values in mom_age
  df_missing$mom_age[sample(seq(1:100),10,replace =TRUE)] <-NA
  
  
  library(ggplot2)
#----descriptive-univariate-table1----------- 
  
  # summary table
 df |>
    gtsummary::tbl_summary()  # Q1: What issues observed? 
  
#----descriptive-univariate-table2-----------    
  # summary table - specification & modification
 df |>
   gtsummary::tbl_summary(
    
     # summary table - Specify variable type   
     type = list(
       # continuous
       child_age    ~ "continuous",
       mom_age      ~ "continuous",
       screen_time  ~ "continuous",
       
       # categorical
       child_gender ~ 'categorical',  # display both levels
       mom_smoking  ~ 'categorical',
       mom_drinking ~ 'categorical'
     ),
     
  # summary table - Specify statistic 
     # statistic = list(
     #   child_age ~ "{mean} ({sd})",
     #   mom_age  ~ "{mean} ({sd})" ,
     #   child_gender ~ "{n} / {N} ({p}%)"
     # ) ,
   statistic = list(
     #gtsummary::all_continuous() ~ "{mean} ({sd})",
     gtsummary::all_continuous() ~ "{median} ({IQR})",
     gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
   ), 
 
   # summary table - Specify label 
    label = 
       list(
         child_age ~ 'Child Age',
         mom_age   ~ 'Mom Age'
       ),
   
   # summary table - Specify missing value 
    missing_text = "(Missing)",
   
   # summary table - Specify digits
    digits = gtsummary::all_continuous() ~ 2,
   ) |>
    gtsummary::as_flex_table() |>
    flextable::line_spacing(space =.5)
 
 
 
# ----descriptive-univariate-bygroup-table--------- 
 # by group
 
   df |>
   gtsummary::tbl_summary(
     type = list(
       child_age    ~ "continuous",
       mom_age      ~ "continuous",
       screen_time  ~ "continuous",
       child_gender ~ 'categorical',  
       mom_smoking  ~ 'categorical',
       mom_drinking ~ 'categorical'
     ),
     statistic = list(
       gtsummary::all_continuous() ~ "{mean} ({sd})",
       gtsummary::all_categorical() ~ "{n} / {N} ({p}%)"
     ), 
     label = 
       list(
         child_age ~ 'Child Age',
         mom_age   ~ 'Mom Age'
       ),
     missing_text = "(Missing)",
     digits = gtsummary::all_continuous() ~ 2,
     
     by = child_gender
   ) |> gtsummary::add_p()  # add p value
 
 # Turn off the PNG device
 # dev.off()
 

 
# ----descriptive-bivariate1--------------------------
 
 # # correlation plot
 # df |>
 #   dplyr::select(- mom_education) |>
 #   GGally::ggpairs(
 #     #mapping = ggplot2::aes(color = as.factor(child_gender)),
 #     upper = list( discrete = "count"),
 #     lower = list(discrete = "facetbar"), 
 #     diag  = list(discrete = "barDiag"),
 #     labeller   = "label_value",
 #     axisLabels = "show", 
 #     progress   = FALSE) #+
 #   # ggplot2::scale_fill_manual(values=c('red','blue' )) +
 #   # ggplot2::scale_colour_manual(values=c('red','blue'))
 # 
 # 
 # df |>
 #   dplyr::select(- mom_education) |>
 #   GGally::ggpairs(
 #     #mapping = ggplot2::aes(color = as.factor(child_gender)),
 #     upper = list( discrete = "count"),
 #     # lower = list(discrete = "facetbar"), 
 #     diag  = list(discrete = "barDiag"),
 #     labeller   = "label_value",
 #     axisLabels = "show", 
 #     progress   = FALSE)+
 #   ggplot2::scale_fill_manual(values=c('red','blue' )) +
 #   ggplot2::scale_colour_manual(values=c('red','blue'))
 # 
 # 
 
 
    # cont-cont-graph 
  df |>
   ggplot2::ggplot(ggplot2::aes(x = child_age, y = child_sleep_quality 	)) +
   geom_point() +
   #geom_smooth(method = 'lm', formula = y ~ poly(x, 1)) +
   ggpmisc::stat_poly_line() +
   ggpmisc::stat_poly_eq(ggpmisc::use_label(c("eq", "adj.R2", "f", "p", "n"))) +
   theme_minimal()  

# ----descriptive-bivariate2--------------------------  
 # by group
 df |>
   ggplot2::ggplot(ggplot2::aes(x = child_age, y = child_sleep_quality, group = child_gender	)) +
   geom_point() +
   geom_smooth(method = 'lm', formula = y ~ poly(x, 2))  

# ----descriptive-bivariate3--------------------------
 ## cont-cat-graph  -- BOXPLOT
 # install.packages('car')
 stat.test <- df |>
   rstatix::t_test(child_sleep_quality ~ child_gender) |>
   rstatix::adjust_pvalue(method = "bonferroni") |>
   rstatix::add_significance("p.adj") |>
   rstatix::add_xy_position(x = "child_gender", dodge = 0.9) |>
   dplyr::mutate(
     stat.p = paste0('t = ',round(statistic,3), ', p = ',p) )
 
 df |>
   dplyr::mutate(child_gender = as.factor(child_gender)) |>
   ggplot2::ggplot(ggplot2::aes( x = child_gender,y = child_sleep_quality	)) +
   geom_boxplot(  width = .5  ) +
   stat_summary(ggplot2::aes(shape = "Mean"),   fun = mean, geom = "point", size = 2, fill = "blue")   +
   stat_summary(ggplot2::aes(shape = "Median"), fun = median, geom = "point", size = 2, fill = "blue") +
   ggpubr::stat_pvalue_manual(
     stat.test,  label =  'stat.p',tip.length = .01,y.position = 20
   )   +
   scale_shape_manual(name = "Statistics", values = c(Mean = 23, Median = 25)) +
   theme(
     plot.title = element_text(hjust = 0.5, margin = margin(b = 10), size = 12),  # Adjust title font size
     axis.title = element_text(size = 12),  # Adjust axis label font size
     axis.text = element_text(size = 11),   # Adjust axis tick label font size
     legend.title = element_text(size = 12),  # Adjust legend title font size
     legend.text = element_text(size = 11),   # Adjust legend label font size,
     plot.margin = margin(t = 11),            # Add space to the top of the plot,
     panel.grid.major = element_blank(),  # Remove major grid lines
     panel.grid.minor = element_blank()   # Remove minor grid lines
   ) +
   ggplot2::ggtitle('Child Sleep Quality by Gender')+
   ylab('Child sleep quality')+
   scale_x_discrete(labels = c("0" = "Female", "1" = "Male")) 
 # geom_text(ggplot2::aes(label=n),color="black",position=position_dodge(width=1),vjust=-0.5)
 

# ----descriptive-bivariate4--------------------------
 ## cont-cat-graph  -- BARPLOT
 stat.test <- df |>
   #dplyr::mutate(child_gender = as.factor(child_gender)) |>
   rstatix::t_test(child_sleep_quality ~ child_gender) |>
   rstatix::adjust_pvalue(method = "bonferroni") |>
   rstatix::add_significance("p.adj") |>
   rstatix::add_xy_position(x = "child_gender", dodge = .3) |>
   dplyr::mutate(
     stat.p = paste0('t = ',round(statistic,3), ', p = ',p) )
 
 df |>
   dplyr::mutate(child_gender = as.factor(child_gender)) |>
   dplyr::group_by(child_gender) |>
   dplyr::summarize(m=mean(child_sleep_quality),sd=sd(child_sleep_quality),n=dplyr::n()) |>
   ggplot( ggplot2::aes(x = child_gender, y = m )  ) +
   geom_bar(stat = "identity", position = "dodge", width = .4 )   + # Mean bars
   geom_errorbar(ggplot2::aes( #ymin = m - sd/sqrt(n), 
                               ymin = m  , 
                              ymax = m + sd/sqrt(n)), 
                 width = 0.05, 
                 position = position_dodge(width = 0.3)) +        # Error bars
   ggpubr::stat_pvalue_manual(
     stat.test,  label =  'stat.p',tip.length = .03, y.position = 5
   )  + 
   labs(x = "Child Gender", 
        y = "Mean Child Sleep Quality", 
        title = "Mean Child Sleep Quality by Gender with Error Bars") +
   theme(
     plot.title = element_text(hjust = 0.5, margin = margin(b = 10), size = 14),  # Adjust title font size
     axis.title = element_text(size = 11),  # Adjust axis label font size
     axis.text = element_text(size = 11),   # Adjust axis tick label font size
     legend.title = element_text(size = 11),  # Adjust legend title font size
     legend.text = element_text(size = 11),   # Adjust legend label font size,
     plot.margin = margin(t = 11),            # Add space to the top of the plot,
     panel.grid.major = element_blank(),  # Remove major grid lines
     panel.grid.minor = element_blank()   # Remove minor grid lines
   ) +
   scale_x_discrete(labels = c("0" = "Female", "1" = "Male"))    # Modify x-axis tick labels
  # scale_fill_manual(values = c("0" = "pink", "1" = "blue"))     # Change bar colors

# ----descriptive-bivariate5--------------------------
 ## cat-cat- BARPLOT
 stat.test_chisquare <- 
   rstatix::chisq_test(df$mom_smoking , df$child_gender) |>
   rstatix::adjust_pvalue(method = "bonferroni") |>
   rstatix::add_significance("p.adj") |>
   #rstatix::add_xy_position(x = "child_gender", dodge = .8) |>
   dplyr::mutate(
     stat.p = paste0('t = ',round(statistic,3), ', p = ',p) )
 
 df |>
   dplyr::count(child_gender,  mom_smoking) |>
   dplyr::group_by(child_gender) |>
   dplyr::mutate(pct = n/sum(n)) |>
   ggplot2::ggplot(ggplot2::aes(x = factor(child_gender), 
                       y = pct , 
                       fill=mom_smoking,
                       label = paste0(round(pct*100, 1), "%", '\n (n = ', n, ")"))) +
   geom_bar(stat = "identity", position = "dodge")  +
   geom_text(position = position_dodge(width = 0.9), vjust = 0.5) +
   annotate("text", x = 1.5, y = 1 + 0.05, 
            label = stat.test_chisquare$stat.p, size = 4, hjust = 0.5) +
   labs(x = "Child gender", y = "Percentage", fill = "Mom smoking") +
   scale_y_continuous(labels = scales::percent) +
   ggplot2::scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("False", "True")) +  # Set custom fill colors and labels
   theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 5))) +
   theme(
     plot.title = element_text(hjust = 0.5, margin = margin(b = 20), size =14 ),  # Adjust title font size
     axis.title = element_text(size = 11),  # Adjust axis label font size
     axis.text  = element_text(size = 11),   # Adjust axis tick label font size
     legend.title = element_text(size = 11),  # Adjust legend title font size
     legend.text = element_text(size = 11) ,   # Adjust legend label font size
     plot.margin = margin(t = 11), # Add space to the top of the plot
     panel.grid.major = element_blank(),  # Remove major grid lines
     panel.grid.minor = element_blank(),   # Remove minor grid lines
    # axis.line = element_line(color = "black", linewidth = 1),
     #panel.border = element_rect(color = "black", linewidth  = 1, fill = NA)
   ) +
   ggtitle('Mom smoking by child gender')+
   scale_x_discrete(labels = c("0" = "Female", "1" = "Male"))   # Modify x-axis tick labels
 
 
  

# ----Multivariate-Multiple-linear-regression-----------  
 stats::glm( child_sleep_quality ~ 
               child_age     +
               mom_age       +
               income        +
               as.factor(child_gender)  +
               mom_education +
               mom_smoking   +
               mom_drinking  +
               screen_time   +
               child_autism_diagnosis, data =df) |>
   gtsummary::tbl_regression() |>
   gtsummary::as_flex_table()  |>
   flextable::line_spacing(space =.2) |> # adjust space between lines of text
   #flextable::padding(padding.top = .1, padding.bottom = .1) |> # adjust space between lines of text
   flextable::width(j = 1, width = 2)       |> # adjust width between columns
   flextable::width(j = 3, width = 1.5)     |> 
   #flextable::font(font ='Courier')  |>  #   Courier
   #flextable::font(font ='Elephant')  |>  # Elephant
   flextable::font(font ="Times New Roman") |> # change font # unique(systemfonts::system_fonts())$family
   flextable::fontsize(size = 11)           |> # change fontsize
   flextable::add_header_lines ('Multiple Linear Regression: Factors Affecting Children Sleep') |>
   flextable::align(align = 'center', part = 'body')    |>
   flextable::align(j= 1, align = 'left', part = 'body')|>
   #flextable::align(align = 'center', part = 'header')  |>
   flextable::align(i= 1,  align = 'center', part = 'header') |>
   flextable::fontsize(i = 1,size = 11, part = 'header')  |>           # change fontsize
   flextable::italic(j = 4, part = 'body')
 #flextable::add_header_lines (values = c("Line 1", "Line 2"))
 # flextable::add_header_row(values = c("Col 1", "Col 2", "Col 3", "Col 4")) |>
 # flextable::add_header_row(values = c( "Col 1", "Col 2",'Col3'), 
 #                           colwidths = c(1,2,1 ) )  
 
 

 

# ----Multivariate-Logistic-regression----------- 
 
 stats::glm( mom_smoking ~ 
               child_age     +
               mom_age       +
               income        +
               child_gender  +
               mom_education            +
               child_sleep_quality      +
               mom_drinking  +
               screen_time   +
               child_autism_diagnosis, 
             data = df,
             family = 'binomial') |>
   #gtsummary::tbl_regression() |>
   gtsummary::tbl_regression(
     exponentiate  = TRUE,
     pvalue_fun = pvalue_stars ,
     estimate_fun = estimate_fun_modified
     ) |>
   gtsummary::as_flex_table()  |>
   flextable::padding(padding.top = .1, padding.bottom = .1) |> # adjust width between rows
   flextable::width(j = 1, width = 2)       |> # adjust width between columns
   flextable::width(j = 3, width = 1.5)     |> 
   #flextable::font(font ='Courier')  |>  # Georgia; Courier
   flextable::font(font ="Times New Roman") |> # chaneg font
   flextable::fontsize(size = 11)           |> # change fontsize
   flextable::add_header_lines ('Logistic Regression: predicting mom smoking') |>
   flextable::align(align = 'center', part = 'body')    |>
   flextable::align(j= 1, align = 'left', part = 'body')|>
   #flextable::align(align = 'center', part = 'header')  |>
   flextable::align(i= 1,  align = 'center', part = 'header') |>
   flextable::fontsize(i = 1,size = 11, part = 'header') 
 