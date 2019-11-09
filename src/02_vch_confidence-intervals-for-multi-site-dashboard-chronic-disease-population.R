
#'--- 
#' title: "LGH, SPH & VGH - CIs for key metrics, by chronic disease population"
#' author: "Nayef Ahmad"
#' date: "2019-11-09"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     toc_folding: false
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(kableExtra)
library(DT)
library(Hmisc)

source(here::here("src", 
                  "ci_function.R"))

#+ data 
#' # Data 
#' 

df1.raw <- 
  read_csv(here::here("data", 
                      "02_lgh-vgh-sph_discharges-mortality-alos-readmit-metrics.csv")) %>% 
  mutate_if(is.character, 
            as.factor)

str(df1.raw)  
summary(df1.raw)


df2.nest <- 
  df1.raw %>% 
  group_by(site, 
           subpop, 
           qrtr) %>% 
  nest()

# df2.nest$data[1]



#' # Adding CIs with mapped function 
#' 

df3.add_ci <- 
  df2.nest %>% 
  mutate(disch_interval = map(data, 
                              ci_function, 
                              response_col = y_discharges), 
         mort_interval = map(data, 
                             ci_function, 
                             response_col = y_mortality), 
         alos_interval = map(data, 
                             ci_function, 
                             response_col = y_alos),
         readmit_interval = map(data, 
                                ci_function, 
                                response_col = y_readmit_rate)
         
  )

# df3.add_ci$disch_interval[2]
# df3.add_ci

df3.add_ci %>% 
  unnest(disch_interval, 
         mort_interval,
         alos_interval,
         readmit_interval) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(4:99, 
              digits = 3)


                           
#' # Join data with intervals 
#' 

df4.data_and_ci <- 
  df2.nest %>% 
  unnest(data) %>% 
  
  left_join(df3.add_ci %>% 
              unnest(disch_interval, 
                     mort_interval,
                     alos_interval,
                     readmit_interval) %>% 
              select(-data)) %>% 
  arrange(site, 
          subpop, 
          fyear, 
          qrtr) %>% 
  mutate(time = rep(1:20, times = 18)) 

df4.data_and_ci %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' # Plots 
#'
#' Just to confirm things look okay: 
#' 

site_param <- "LGH"
subpop_param <- "diabetes"

# discharges 
df4.data_and_ci %>% 
  filter(site == site_param, 
         subpop == subpop_param) %>% 
  select(site:fyear, 
         time, 
         y_discharges, 
         y_discharges_lwr,
         y_discharges_upr) %>% 
  
  ggplot(aes(x = time, 
             y = y_discharges)) + 
    geom_line() + 
    geom_ribbon(aes(ymin = y_discharges_lwr, 
                    ymax = y_discharges_upr), 
                alpha = .2) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# mortality 
df4.data_and_ci %>% 
  filter(site == site_param, 
         subpop == subpop_param) %>% 
  select(site:fyear,
         time, 
         y_mortality, 
         y_mortality_lwr,
         y_mortality_upr) %>% 
  
  ggplot(aes(x = time, 
             y = y_mortality)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = y_mortality_lwr, 
                  ymax = y_mortality_upr), 
              alpha = .2) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# ALOS 
df4.data_and_ci %>% 
  filter(site == site_param, 
         subpop == subpop_param) %>% 
  select(site:fyear, 
         time, 
         y_alos, 
         y_alos_lwr,
         y_alos_upr) %>% 
  
  ggplot(aes(x = time, 
             y = y_alos)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = y_alos_lwr, 
                  ymax = y_alos_upr), 
              alpha = .2) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))



#' # Appendix 
#' 
#' ## Outputs 
#' 

# write_csv(df4.data_and_ci,
#           here::here("results", 
#                      "dst", 
#                      "2019-11-09_lgh-sph-vgh_chronic-disease-metrics-with-conf-intervals.csv"))
             
