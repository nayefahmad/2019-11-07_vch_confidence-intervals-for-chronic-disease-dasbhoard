
#'--- 
#' title: "CI for average deaths per quarter at a hospital"
#' author: "Nayef Ahmad"
#' date: "2019-11-07"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: 
#'       collapsed: false 
#'     toc_folding: false
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(Hmisc)
library(broom)
library(DT)

#+ data
#' # Data 
#' 
#' This is dummy data.  
#' 

df1.test_discharges <- 
  tibble::tribble(
      ~fyear, ~quarter, ~is_q4, ~site, ~total_discharges, ~alos_days, ~total_deaths,
    "fy2015",     "Q1",   "no", "VGH",              7193,        9.5,           282,
    "fy2015",     "Q2",   "no", "VGH",              7064,        9.7,           301,
    "fy2015",     "Q3",   "no", "VGH",              7365,        9.8,           300,
    "fy2015",     "Q4",  "yes", "VGH",              7190,        9.8,           300,
    "fy2016",     "Q1",   "no", "VGH",              7246,        9.6,           319,
    "fy2016",     "Q2",   "no", "VGH",              6897,       10.2,           268,
    "fy2016",     "Q3",   "no", "VGH",              7241,         10,           292,
    "fy2016",     "Q4",  "yes", "VGH",              7011,       10.2,           319,
    "fy2017",     "Q1",   "no", "VGH",              6898,       10.2,           283,
    "fy2017",     "Q2",   "no", "VGH",              6756,       10.2,           283,
    "fy2017",     "Q3",   "no", "VGH",              6739,       10.1,           300,
    "fy2017",     "Q4",  "yes", "VGH",              6931,       10.3,           330,
    "fy2018",     "Q1",   "no", "VGH",              6900,       10.1,           270,
    "fy2018",     "Q2",   "no", "VGH",              6784,         10,           303,
    "fy2018",     "Q3",   "no", "VGH",              6943,        9.9,           318,
    "fy2018",     "Q4",  "yes", "VGH",              7160,        9.8,           326,
    "fy2019",     "Q1",   "no", "VGH",              7094,        9.8,           312,
    "fy2019",     "Q2",   "no", "VGH",              6980,        9.8,           312,
    "fy2019",     "Q3",   "no", "VGH",              7132,        9.9,           338,
    "fy2019",     "Q4",  "yes", "VGH",              7149,       10.1,           335
    ) %>% 
  mutate_if(is.character, as.factor)

# str(df1.test_discharges)
# summary(df1.test_discharges)

df1.test_discharges %>%
  datatable(extensions = 'Buttons',
          options = list(dom = 'Bfrtip',
                         buttons = c('excel', "csv")))


# df1.test_discharges %>%
#   ggplot(aes(x = total_deaths)) +
#   geom_density()
#    
# df1.test_discharges %>%
#   ggplot(aes(x = alos_days)) +
#   geom_density()
#  
# df1.test_discharges %>% 
#   ggplot(aes(x = total_discharges)) +
#   geom_density()



#' # Models - total deaths 
#' ## Total deaths vs quarter 
#'  

m1.deaths <- lm(total_deaths ~ quarter, 
                data = df1.test_discharges)

summary(m1.deaths)

#' **Note that the F-stat is not significant. This model may not be useful.**
#'  

m1.deaths %>% 
  tidy() %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' ### Confidence intervals 
#' 

df2.nested <-                            
  df1.test_discharges %>% 
  group_by(quarter) %>% 
  nest() %>% 
  mutate(conf = map(data, 
                       function(df){
                         mean <- smean.cl.normal(df$total_deaths)[1]
                         lwr <- smean.cl.normal(df$total_deaths)[2]
                         upr <- smean.cl.normal(df$total_deaths)[3]
                         
                         return(data.frame(lwr_death = lwr, 
                                           mean_death = mean, 
                                           upr_death = upr))
                       })) # %>% View("confint")

df2.nested %>% 
  unnest(conf) %>% 
  select(quarter, 
         lwr_death:upr_death) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           


df1.test_discharges %>% 
  ggplot(aes(x = quarter, 
             y = total_deaths)) + 
  stat_summary(fun.y = mean, 
               geom = "point") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar") + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               col = "red") + 
  
  labs(title = "Dummy data - Estimates of average total deaths per quarter", 
       subtitle = "Black - CI based on t-dist \nRed - CI based on bootstrap \n\nModel: deaths ~ quarter") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))

#' ** Q. Are shorter CIs necessarily better?**
#'
#' **Ans.** No. See [Hesterberg, 2015](https://arxiv.org/abs/1411.5279):
#'
#' > "...the common combination of nonparametric bootstrapping and bootstrap
#' percentile CIs is less accurate than using t-intervals for small
#' samples, though more accurate for larger samples." 
#' 
#' 

#' ## Total deaths vs is_q4
#' 

m2.deaths <- lm(total_deaths ~ is_q4, 
                data = df1.test_discharges)

summary(m2.deaths)

m2.deaths %>% 
  tidy() %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' ### Confidence intervals 
#' 

df2.nested <-                            
  df1.test_discharges %>% 
  group_by(is_q4) %>% 
  nest() %>% 
  mutate(conf = map(data, 
                    function(df){
                      mean <- smean.cl.normal(df$total_deaths)[1]
                      lwr <- smean.cl.normal(df$total_deaths)[2]
                      upr <- smean.cl.normal(df$total_deaths)[3]
                      
                      return(data.frame(lwr_death = lwr, 
                                        mean_death = mean, 
                                        upr_death = upr))
                    })) # %>% View("confint")

df2.nested %>% 
  unnest(conf) %>% 
  select(is_q4, 
         lwr_death:upr_death) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))



df1.test_discharges %>% 
  ggplot(aes(x = is_q4, 
             y = total_deaths)) + 
  stat_summary(fun.y = mean, 
               geom = "point") + 
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar") + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", 
               col = "red") + 
  
  labs(title = "Dummy data - Estimates of average total deaths per quarter", 
       subtitle = "Black - CI based on t-dist \nRed - CI based on bootstrap \n\nModel: deaths ~ is_q4") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))



