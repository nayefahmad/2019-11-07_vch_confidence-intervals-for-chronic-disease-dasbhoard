
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
           subpop)