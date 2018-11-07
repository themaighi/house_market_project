### simple model
### 

simple_model <- function(model_data , x, y){ #x <- 'c'; y <- 'd'
  results_lm <- lm(as.formula(paste(y, ' ~ ',paste(x, collapse = ' + '))), model_data)
  return(results_lm)
}