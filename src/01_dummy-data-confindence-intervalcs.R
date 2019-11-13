
#'--- 
#' title: "Confidence intervals for average deaths per quarter "
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
library(kableExtra)
library(glmnet)

#+ data
#' # Data 
#' 
#' This is dummy data. 
#' 
#' Let's say we're looking at the total deaths by quarter at a large hospital. 
#' 

#' The question we're interested in is whether there are meaningful differences
#' across quarters. For instance, anecdotal evidence indicates that deaths often
#' rise in the 4th quarter of the year.
#'
#' We also want to set standards for what to expect in future quarters. If there
#' are no longer-term trends over time, then the confidence interval for Q1 that
#' we establish from the historical data can be used to indicate wheter the next
#' Q1 has a higher/lower than expected number of deaths.
#' 
#' Here's what the data looks like: 
#' 

# paste data using {datapasta} add-in 
# Data ----------
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

# Models ------
m1.deaths <- lm(total_deaths ~ quarter, 
                data = df1.test_discharges)

summary(m1.deaths)

#' **Note that the F-stat is not significant. This model may not be useful.**
#'  

# m1.deaths %>% 
#   tidy() %>% 
#   datatable(extensions = 'Buttons',
#             options = list(dom = 'Bfrtip', 
#                            buttons = c('excel', "csv")))

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
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))



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

#' **Q. Are shorter CIs necessarily better?**
#'
#' **Ans.** No. See [Hesterberg, 2015](https://arxiv.org/abs/1411.5279):
#'
#' > "...the common combination of nonparametric bootstrapping and bootstrap
#' percentile CIs is less accurate than using t-intervals for small
#' samples, though more accurate for larger samples." 
#' 
#' > "(In small samples) bootsrap distributions tend to be too narrow on average ... "
#' 

#' ## Total deaths vs is_q4
#' 

m2.deaths <- lm(total_deaths ~ is_q4, 
                data = df1.test_discharges)

summary(m2.deaths)

# m2.deaths %>% 
#   tidy() %>% 
#   datatable(extensions = 'Buttons',
#             options = list(dom = 'Bfrtip', 
#                            buttons = c('excel', "csv")))

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
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))
            



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


#' ## LASSO variable selection
#'
#' Previous models show that not every level of the `quarter` variable is likely
#' to have a significant coefficient. We could manually try to find which levels
#' to include, but that doesn't scale - what if we need to do this for dozens of
#' different patient groups?
#' 
#' Instead, let's try automated variable selection with LASSO. See [here for more on this](http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/). 
#' 
#'
#' `?glmnet`: "Fit a generalized linear model via penalized maximum
#' likelihood... Fits linear, logistic and multinomial, poisson, and Cox
#' regression models."
#'
#' `?cv.glmnet`: "Does k-fold cross-validation for glmnet, produces a plot, and
#' returns a value for lambda (and gamma if relax=TRUE)"
#'
#' Well start with cross-validation to select a value for lambda. (In actual
#' analysis, we would do this on training dataset, not the full dataset.)
#' 

# predictors: 
x <- model.matrix(total_deaths ~ ., 
                  data = df1.test_discharges %>% 
                    select(quarter, 
                           total_deaths))

# drop the first column of all 1s (glmnet will recreate this)
x <- x[, -1]

y <- df1.test_discharges$total_deaths

# Find the best lambda using cross-validation
set.seed(123) 
cv.lasso <- cv.glmnet(x, y)

#' Results of cross-validation: 

cv.lasso

#' If we use `cv.lasso$lambda.min`, there will be 2 nonzero coefficients. If we
#' use `cv.lasso$lambda.1se`, there will be only 1 nonzero coefficient.
#' 

#' Now we fit final models with the values of `lambda` that we got from cross-validation. 
#' 

# Fit the final model with 2 coeffs:  
m3.deaths_lasso <- glmnet(x, y, lambda = cv.lasso$lambda.min)
coef(m3.deaths_lasso)

tidy(m3.deaths_lasso) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                "condensed", 
                "responsive"))
              

#' Note how similar these results are to those of the first model we fit, `m1.deaths`

# Fit the final model with 1 coeffs:  
m4.deaths_lasso <- glmnet(x, y, lambda = cv.lasso$lambda.1se)
coef(m4.deaths_lasso)

tidy(m4.deaths_lasso) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))

#' In this case, results are quite different from previous models. Might be best to 
#' stick with using `lambda.min`. 
#' 
#' **Overall, using LASSO for this purpose seems very promising**. 












