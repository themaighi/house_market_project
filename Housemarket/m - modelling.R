## Start making the model
## 


modelled_dataset <- function(house){

house[,id := .I]

##recode askingprice  ##
model_house <- house[!is.na(`Asking price`),.SD,.SDcols = c(
                                              "Asking price",
                                              "lat",
                                              "lon",
                                              "id",
                                              "LotArea",
                                              "YearBuilt",
                                              "SaleCondition")]

##recode askingprice  ##

model_house[,y_price := `Asking price`]
model_house[, `Asking price`:= NULL]


model.frame <- as.data.table(model_house)

rf_model = randomForest(y_price ~ LotArea + YearBuilt + SaleCondition, data=model.frame)


prediction <- predict(rf_model, model.frame)

model_house[,residual := y_price - prediction]

output_house <- merge(house, model_house[,.(id,residual)], by = "id")
print(model.frame)
return(output_house)

}


