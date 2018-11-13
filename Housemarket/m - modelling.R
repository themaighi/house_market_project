## Start making the model
## 



modelled_dataset <- function(house){
  #h2o.init()
house[,id := .I]

##recode askingprice  ##
model_house <- house[!is.na(`Asking price`),.SD,.SDcols = c(#"Accessibility",
                                              #"Acceptance",
                                              "Asking price",
                                              #"Back garden",
                                              "Building insurance",
                                              #"Bathroom facilities",
                                              "Energy label",
                                              "Living area",
                                              #"Kind of house",
                                              #"Location",
                                              #"Number of bath rooms",
                                              "Number of rooms",
                                              "id")]

##recode askingprice  ##
model_house[,y_price := gsub("k\\.k\\.|v\\.o\\.n\\.", "",`Asking price`)]
model_house[,y_price := gsub(substring(y_price, 1,2), "",y_price)]
print(model_house)##Understand what von means
model_house[,y_price := gsub(",", "",y_price) %>% as.numeric()]

##Recoding the Building insurance
model_house[,x_buildin_ins := ifelse(`Building insurance` == "Yes",1,0 )]



##Recoding the energy label
model_house[,x_energy_label := substring(`Energy label`, 1, 1  )]
model_house[x_energy_label %>% is.na(),x_energy_label := "not_defined"]

##Recoding the  living area
model_house[,x_living_area := gsub("m²", "",`Living area`) %>% as.numeric()] ##Understand what von means

##recoding the number of rooms
model_house[,c("x_rooms", "x_bedrooms") := tstrsplit(`Number of rooms`, "\\(")]
model_house[, x_rooms := substring(x_rooms, 1,1)]
model_house[, x_bedrooms := substring(x_bedrooms, 1,1)]




model.frame <- as.h2o(model_house[,.SD, .SDcols = names(model_house)[names(model_house) %like% "x_|y_"]])
print(model.frame)

model <- h2o.randomForest(x = 2:6,
                   #mode_house[,.SD, .SDcols = names(model_house)[names(model_house) %like% "x_"]],
                 y = 1,
                 training_frame = model.frame)
                   #model_house[,.SD, .SDcols = names(model_house)[names(model_house) %like% "y_"]])
prediction <- h2o.predict(model,model.frame)

model_house[,residual := y_price - as.data.table(prediction)[,predict]]

output_house <- merge(house, model_house[,.(id,residual)], by = "id")

return(output_house)
h2o.shutdown()
}


map_data <- function(house){
  
  return(house)
}

## Linear model
simple_model <- function(model_data , x, y){ #x <- 'c'; y <- 'd'
  results_lm <- lm(as.formula(paste(y, ' ~ ',paste(x[x %in% names(model_data)], collapse = ' + '))), model_data)
  return(results_lm)
}


