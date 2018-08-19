## Data exploration
## Goal of the script : Understand the relation between variables

source("Housemarket/l - libraries.R")
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

define_structure_variable <- function(data){
  #data <- houseprice_data
  loop_over_structure <- c("numeric|integer",
                           "factor",
                           "character")
  if(all(!class(data) %like% "data.table")){
    data <- as.data.table(data) ## change to data table
  }
  list_variable_by_sructure <-
    lapply(loop_over_structure,function(x){sapply(data[,.SD], class) %>% 
             .[. %like% x]})
  names(list_variable_by_sructure) <- loop_over_structure
  return(list_variable_by_sructure)
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

structure_variables <- define_structure_variable(houseprice_data[,.SD, .SDcols = names_like(names(houseprice_data), "x_")])

numerical_house_variables <- melt(houseprice_data[, .SD, .SDcols = c(names(structure_variables[["numeric|integer"]]), "id", "y_log_saleprice")],
                                  id.vars = c("id", "y_log_saleprice"), value.name = "x_value",
                                  variable.name = "x_variable")

character_house_variables <- melt(houseprice_data[, .SD, .SDcols = c(names(structure_variables[["character"]]),"id", "y_log_saleprice")],
                                  id.vars = c("id", "y_log_saleprice"), value.name = "x_value",
                                  variable.name = "x_variable")


ggplot(numerical_house_variables, aes(y = y_log_saleprice, x = x_value)) + 
  geom_point() +
  facet_wrap(c("x_variable"), scales = "free_x")

ggplot(character_house_variables, aes(y = y_log_saleprice, x = x_value)) + 
  geom_point() +
  facet_wrap(c("x_variable"), scales = "free_x")


## Check the NA in the features

houseprice_data[, lapply(.SD, is.na)] %>% colSums() %>% .[. != 0]
## Changing the msvn
houseprice_data[is.na(x_masvnrarea), x_masvnrarea := 0 ]
houseprice_data[is.na(x_masvnrtype),  x_masvnrtype := "None"]

## Changing the lotFrontage

lot.by.nbrh <- houseprice_data[,.(median = median(x_lotfrontage,na.rm = T )), by = .(x_neighborhood)]
houseprice_data <- merge(houseprice_data, lot.by.nbrh, by = "x_neighborhood")
houseprice_data[is.na(x_lotfrontage), x_lotfrontage := median]
houseprice_data[, median := NULL]

## Changing Alley
houseprice_data[is.na(x_alley),x_alley := "None"]


## CHanging basement

for (col in names_like(names(houseprice_data), 'x_bsm')){ #col <- "x_bsmtqual"
  if (sapply(houseprice_data[, .SD, .SDcols = col], is.numeric) == TRUE){
    houseprice_data[,(col):= lapply(.SD, function(x){ifelse(is.na(x), 0, x)}),.SDcol = col]
  }
  else{
    houseprice_data[,(col) := lapply(.SD, function(x){ifelse(is.na(x), 'None', x)}),.SDcol = col] 
  }
}

rm(list = ls()[!ls() %in% org_list])
