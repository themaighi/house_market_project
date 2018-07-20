## Data exploration
## Goal of the script : Understand the relation between variables




org_list<-ls()
houseprice_data <- fread("Housemarket/data/kaggle_house_training_data.csv")

## Define function 

names_like <- function(input, pattern){#input <- sapply(houseprice_data, class) %>% names(.);pattern <- "x_"
  if(class(input) %like% "data.table"){
   output <- names(input)[names(input) %like% pattern]
  }else{
   output <- input[input %like% pattern]
  }
  return(output)
}


## Check the summary of the data

summary(houseprice_data)


## Transforming the variables

setnames(houseprice_data, names(houseprice_data),tolower(names(houseprice_data)))
setnames(houseprice_data, names(houseprice_data)[!names(houseprice_data) %like% "saleprice|id"],
         paste0("x_",names(houseprice_data)[!names(houseprice_data) %like% "saleprice|id"]))
setnames(houseprice_data, names(houseprice_data)[names(houseprice_data) %like% "saleprice"],
         paste0("y_",names(houseprice_data)[names(houseprice_data) %like% "saleprice"]))
## Check distrribution of price
## Reason : If the dependent variable is not normal it means that your error will not be normally distributed
hist(houseprice_data$y_saleprice) ##  Conclusion : it is a long tail
houseprice_data$y_log_saleprice <- log(houseprice_data$y_saleprice) ## By taking the log it became more regular

## check relation between variables and the dependent variable
numerical_variable <- sapply(houseprice_data[,.SD, .SDcols = names_like(names(houseprice_data), "x_")], class) %>% .[. %like% "integer|numeric"]
character_variable <- sapply(houseprice_data[,.SD, .SDcols = names_like(names(houseprice_data), "x_")], class) %>% .[!. %like% "integer|numeric"]

numerical_house_variables <- melt(houseprice_data[, .SD, .SDcols = c(names(numerical_variable), "id", "y_log_saleprice")],
                                  id.vars = c("id", "y_log_saleprice"), value.name = "x_value",
                                  variable.name = "x_variable")

character_house_variables <- melt(houseprice_data[, .SD, .SDcols = c(names(character_variable),"id", "y_log_saleprice")],
                                  id.vars = c("id", "y_log_saleprice"), value.name = "x_value",
                                  variable.name = "x_variable")


ggplot(numerical_house_variables, aes(y = y_log_saleprice, x = x_value)) + 
  geom_point() +
  facet_wrap(c("x_variable"), scales = "free_x")

ggplot(character_house_variables, aes(y = y_log_saleprice, x = x_value)) + 
  geom_point() +
  facet_wrap(c("x_variable"), scales = "free_x")

rm(list = ls()[!ls() %in% org_list])
