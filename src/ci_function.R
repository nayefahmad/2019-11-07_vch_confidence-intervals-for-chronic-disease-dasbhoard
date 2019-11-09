

#*********************************************************
# Function for returning t-dist based CI 
# 2019-11-08
# Nayef 

#*********************************************************

# fn definition: 
ci_function <- function(df,
                        response_col){
  
  # Inputs: 
  # df is a df for specific site and subpop
  # response_col is the y-variable that we are calculating CIs for 
  
  y <- enquo(response_col)
  df <- as.data.frame(df)
  
  y <- df %>% pull(!!y)
  
  mean <- smean.cl.normal(y)[1]
  lwr <- smean.cl.normal(y)[2]
  upr <- smean.cl.normal(y)[3]
  
  df2 <- 
    data.frame(lwr = lwr, 
               mean = mean, 
               upr = upr) 
  
  rownames(df2) <- NULL 
  
  colnames(df2)[1] <- paste0(enexpr(response_col), "_lwr")
  colnames(df2)[2] <- paste0(enexpr(response_col), "_mean")
  colnames(df2)[3] <- paste0(enexpr(response_col), "_upr")
  
  return(df2)
}



#*********************************************************
# fn test: 
ci_function(df = df2.nest$data[1], 
            response_col = y_deaths) 

ci_function(df = df2.nest$data[1], 
            response_col = y_discharges) 
