# step 1. Clean data for analysis data 

setwd("/Users/skychi/Desktop/5200 Framework I/PAC/usedcars2023")
data=read.csv("/Users/skychi/Desktop/5200 Framework I/PAC/usedcars2023/analysisData(clean++).csv",stringsAsFactors = T)

library(readr)
data$torque_lbft=parse_number(sub("@.*", " ", data$torque))
data <- data[, -which(names(data) == "torque")]
data$RPM=parse_number(sub(" ", ".*@(.*)",data$power_RPM))
data <- data[, -which(names(data) =="power_RPM")]

#parsing numerical data
library(lubridate)
data$listed_date=as.Date(data$listed_date)
data$listed_year=year(data$listed_date)
data$listed_year=factor(data$listed_year, levels = unique(data$listed_year), ordered = TRUE)
data$listed_month=month(data$listed_date)
data$listed_month<- factor(data$listed_month, levels = unique(data$listed_month), ordered = TRUE)
data <- data[, -which(names(data) == "listed_date")]

#imputation 
columns_to_impute <- c("highway_fuel_economy", "city_fuel_economy", "engine_displacement", "mileage", "daysonmarket","seller_rating","owner_count","price","fuel_tank_volume_gallons", "wheelbase_inches", "width_inches" , "height_inches","horsepower", "maximum_seating" , "year", "back_legroom_inches" , "front_legroom_inches", "length_inches","torque_lbft","RPM") 
imputed_means <- vector("list", length(columns_to_impute))

for (i in seq_along(columns_to_impute)) {
  col_name <- columns_to_impute[i]
  imputed_means[[col_name]] <- mean(data[[col_name]], na.rm = TRUE)
  data[[col_name]][is.na(data[[col_name]])] <- imputed_means[[col_name]]
} #missing numerical into mean

columns_to_impute <- c("make_name", "wheel_system", "listing_color","fleet","franchise_make","has_accidents","isCab","is_cpo","is_new","salvage","franchise_dealer","major_options","frame_damaged","listed_month")
imputed_mode <- vector("list", length(columns_to_impute))

for (i in seq_along(columns_to_impute)) {
  col_name <- columns_to_impute[i]
  imputed_mode[[col_name]] <- names(sort(table(data[[col_name]]), decreasing = TRUE))[1]
  data[[col_name]][is.na(data[[col_name]])] <- imputed_mode[[col_name]]
}#missing catergoical into mode

data$has_accidents=factor(data$has_accidents)
data$franchise_dealer=factor(data$franchise_dealer)
data$isCab=factor(data$isCab)
data$is_cpo=factor(data$is_cpo)
data$is_new=factor(data$is_new)
data$salvage=factor(data$salvage)
data$fleet=factor(data$fleet)
data$frame_damaged=factor(data$frame_damaged)
data$year=factor(data$year)

# Mutate a new column 'has_bluetooth' based on the presence of 'Bluetooth' in 'major_options'
install.packages("dplyr")
library(dplyr)
data$bluetooth_state <-ifelse(grepl("Bluetooth",data$major_options),"yes","no" )
data$carplay_state <- ifelse(grepl("CarPlay",data$major_options ),"yes","no" )
data$roof_state <- ifelse(grepl('Sunroof/Moonroof',data$major_options ),"yes","no" )
data$remote_state <- ifelse(grepl('Remote Start',data$major_options),"yes","no" )
data$parking <- ifelse(grepl('Backup Camera|Parking Sensors', data$major_options),"yes","no" )
data$alloy_wheel <- ifelse(grepl( 'Alloy Wheels',data$major_options ),"yes","no" )
data$premium_seat <- ifelse(grepl( 'Heated Seats|Leather Seats',data$major_options ),"yes","no" )
data$premium <- ifelse(grepl( 'Premium Package|Elite Package|SE Package',data$major_options ),"yes","no" )
data$sport<- ifelse(grepl( 'Sport Package',data$major_options ),"yes","no" )
data$adapive_control<- ifelse(grepl( 'Adaptive Cruise Control',data$major_options ),"yes","no" )

#delete unnecessary variables from feature selection 

data <- data[, -which(names(data) == "id")]
data <- data[, -which(names(data) == "is_cpo")]
data <- data[, -which(names(data) == "major_options")]
data <- data[, -which(names(data) == "model_name")]#cannot handle more than 53 levels categorical
data <- data[, -which(names(data) == "isCab")]
data <- data[, -which(names(data) == "fleet")]
data <- data[, -which(names(data) == "adapive_control")]
data <- data[, -which(names(data) == "bluetooth_state")]

#split data
library  (caret)
set.seed(1031)
split=createDataPartition(y=data$price, p=0.8,list=F,groups=100)
train=data[split,]
test=data[-split,]

#2. Clean data for scoring data 
score=read.csv("/Users/skychi/Desktop/5200 Framework I/PAC/usedcars2023/scoringData(clean++).csv",stringsAsFactors = T)
score$torque_lbft=parse_number(sub("@.*", " ", score$torque))
score <- score[, -which(names(score) == "torque")]
score$RPM=parse_number(sub(" ", ".*@(.*)",score$power_RPM))
score <- score[, -which(names(score) =="power_RPM")]
library(lubridate)
score$listed_date=as.Date(score$listed_date)
score$listed_year=year(score$listed_date)
score$listed_year=factor(score$listed_year, levels = unique(score$listed_year), ordered = TRUE)
score$listed_month=month(score$listed_date)
score$listed_month<- factor(score$listed_month, levels = unique(score$listed_month), ordered = TRUE)
score <- score[, -which(names(score) == "listed_date")]

columns_to_impute <- c("highway_fuel_economy", "city_fuel_economy", "engine_displacement", "mileage", "daysonmarket","seller_rating","owner_count","fuel_tank_volume_gallons", "wheelbase_inches", "width_inches" , "height_inches","horsepower", "maximum_seating" , "year", "back_legroom_inches" , "front_legroom_inches", "length_inches","torque_lbft","RPM") 
imputed_means <- vector("list", length(columns_to_impute))

for (i in seq_along(columns_to_impute)) {
  col_name <- columns_to_impute[i]
  imputed_means[[col_name]] <- mean(score[[col_name]], na.rm = TRUE)
  score[[col_name]][is.na(score[[col_name]])] <- imputed_means[[col_name]]
} #missing numerical into mean

columns_to_impute <- c("make_name", "wheel_system", "listing_color","fleet","franchise_make","has_accidents","isCab","is_cpo","is_new","salvage","franchise_dealer","major_options","frame_damaged","listed_month")
imputed_mode <- vector("list", length(columns_to_impute))

for (i in seq_along(columns_to_impute)) {
  col_name <- columns_to_impute[i]
  imputed_mode[[col_name]] <- names(sort(table(score[[col_name]]), decreasing = TRUE))[1]
  score[[col_name]][is.na(score[[col_name]])] <- imputed_mode[[col_name]]
}#missing caterical into mode

score$has_accidents=factor(score$has_accidents)
score$franchise_dealer=factor(score$franchise_dealer)
score$isCab=factor(score$isCab)
score$is_cpo=factor(score$is_cpo)
score$is_new=factor(score$is_new)
score$salvage=factor(score$salvage)
score$fleet=factor(score$fleet)
score$frame_damaged=factor(score$frame_damaged)
score$year=factor(score$year)

score$bluetooth_state <- ifelse(grepl("Bluetooth",score$major_options ),"yes","no")
score$carplay_state <- ifelse(grepl("CarPlay",score$major_options ),"yes","no")
score$roof_state <- ifelse(grepl('Sunroof/Moonroof',score$major_options ),"yes","no")
score$remote_state <- ifelse(grepl('Remote Start',score$major_options),"yes","no")
score$parking <- ifelse(grepl('Backup Camera|Parking Sensors', score$major_options),"yes","no")
score$alloy_wheel <- ifelse(grepl( 'Alloy Wheels',score$major_options ),"yes","no")
score$premium_seat <- ifelse(grepl( 'Heated Seats|Leather Seats',score$major_options ),"yes","no")
score$premium <- ifelse(grepl( 'Premium Package|Elite Package|SE Package',score$major_options ),"yes","no")
score$sport<- ifelse(grepl( 'Sport Package',score$major_options ),"yes","no")
score$adaptive_control<- ifelse(grepl( 'Adaptive Cruise Control',score$major_options ),"yes","no")

score$year=factor(score$year)
score <- score[, -which(names(score) == "id")]
score <- score[, -which(names(score) == "is_cpo")]
score <- score[, -which(names(score) == "major_options")]
score <- score[, -which(names(score) == "model_name")]  #cannot handle more than 53 levels categorical 
score <- score[, -which(names(score) == "isCab")]
score <- score[, -which(names(score) == "fleet")]
score <- score[, -which(names(score) == "bluetooth_state")]
score <- score[, -which(names(score) == "adaptive_control")]

#Step 3. handle different levels between analysis data and scoring data

# Identify the variables with extra levels
extra_levels_vars <- c("franchise_make","body_type", "make_name","year","listed_year")

# Loop through each variable
for (var in extra_levels_vars) {
  # Extract the desired levels from the corresponding variable in your test score
  desired_levels <- levels(data[[var]])
  
  # Create a factor variable in your scoring score with the desired levels
  score[[var]] <- factor(score[[var]], levels = desired_levels)
}

#Step 4. Feature Selection 
#forwardstepwise
start_mod=lm(price~1,data=data)
empty_mod=lm(price~1,data=data)
full_mod=lm(price~.,data=data)
forwardStepwise=step(start_mod,scope=list(upper=full_mod, lower=empty_mod), direction="forward")

#lasso shrinkage

x= model.matrix(price ~ horsepower + year + make_name + franchise_make + fuel_type + 
                  roof_state + wheel_system + mileage + height_inches + body_type + 
                  is_new + engine_displacement  + wheelbase_inches + 
                  maximum_seating + remote_state + front_legroom_inches + listed_month + 
                  transmission + parking + seller_rating + city_fuel_economy + 
                  highway_fuel_economy + length_inches + back_legroom_inches + 
                  listing_color + width_inches + bluetooth_state + 
                  salvage + fuel_tank_volume_gallons + daysonmarket + has_accidents,data=data)

y = data$price#lasso shrinkage 
cv_lasso = cv.glmnet(x = x, 
                     y = y, 
                     alpha = 1,
                     type.measure = 'mse')
coef(cv_lasso, s = cv_lasso$lambda.1se) |>
  round(4) #city fuel not important 

#important features:horsepower+year+make_name+franchise_make+fuel_type+roof_state+wheel_system+mileage+height_inches+body_type+is_new+engine_displacement+isCab+maximum_seating+remote_state+front_legroom_inches+listed_month+transmission+parking+seller_rating+city_fuel_economy+length_inches+listing_color+salvage+fuel_tank_volume_gallons+has_accidents

#Step 5.Fit cleaned data into different models
#linear regression
model0=lm(price~.,data=train)
pred0=predict(model0)
rmse0=sqrt(mean((pred0-train$price)^2))
pred0_test=predict(model0, newdata=test)
rmse0_test=sqrt(mean((pred0_test-test$price)^2))

#random forest
library(randomForest)
set.seed(1031)
forest = randomForest(price~.,
                      train, 
                      ntree = 200)

pred1=predict(forest)
head(pred1)
rmse1=sqrt(mean((pred1-train$price)^2))
pred1_test=predict(forest, newdata=test)
rmse1_test=sqrt(mean((pred1_test-test$price)^2))

#boost
library(gbm)
set.seed(1031)
boost = gbm(price ~.,
            data=train,
            distribution="gaussian",
            n.trees = 200,
            interaction.depth = 10,
            shrinkage = 0.01)

pred_boosted_train= predict(boost,n.trees = 200)
rmse_boost_train = sqrt(mean((pred_boosted_train-train$price)^2)) 

pred_boosted_test= predict(boost,newdata=test,n.trees = 200)
rmse_boost_test = sqrt(mean((pred_boosted_test-test$price)^2))

#tuned ranger
library(caret)
library(randomForest)
trControl = trainControl(method = 'cv', number = 5)
tuneGrid=expand.grid(mtry=1:42, 
                     splitrule = c('variance','extratrees','maxstat'), 
                     min.node.size = c(2,5,10,15,20,25))
set.seed(1031)
tuned_forest_ranger_cv = train(price~.,
                               data = train, 
                               method = 'ranger', 
                               num.trees=200,
                               trControl = trControl, 
                               tuneGrid=tuneGrid)
results=tuned_forest_ranger_cv$results$RMSE
which.min(results)

  library (ranger)
model_tuned_ranger=ranger(price~.,
                          data=train,
                          num.trees = 200, 
                          mtry=tuned_forest_ranger_cv$bestTune$mtry, 
                          min.node.size = tuned_forest_ranger_cv$bestTune$min.node.size, 
                          splitrule = tuned_forest_ranger_cv$bestTune$splitrule)

pred_tuned_ranger_test = predict(model_tuned_ranger, data =test, num.trees = 200)
rmse_cv_forest_ranger_test = sqrt(mean((pred_tuned_ranger_test$predictions-test$price)^2))

pred_tuned_ranger_score = predict(model_tuned_ranger, data =score, num.trees = 200)

#xgboost
install.packages("vtreat")
library(vtreat)
varlist_without_price <- setdiff(names(data), c("price"))
varlist_without_price_data <- setdiff(names(data), c("price"))
trt <- designTreatmentsZ(dframe = train, varlist = varlist_without_price)

newvars = trt$scoreFrame[trt$scoreFrame$code%in% c('clean','lev'),'varName']

train_input = prepare(treatmentplan = trt, 
                      dframe = train,
                      varRestriction = newvars)

test_input = prepare(treatmentplan = trt, 
                     dframe = test,
                     varRestriction = newvars)

score_input = prepare(treatmentplan = trt, 
                      dframe = score,
                      varRestriction = newvars)

data_input = prepare(treatmentplan = trt, 
                     dframe = data,
                     varRestriction = newvars)
head(train_input)
install.packages("xgboost")
library(xgboost)
xgboost = xgboost(data=as.matrix(train_input), 
                  label = train$price,
                  nrounds=10000,
                  verbose = 0,
                  early_stopping_rounds = 100)

xgboost$best_iteration

pred_test = predict(xgboost, 
                    newdata=as.matrix(test_input))

rmse_test_xgboost = sqrt(mean((pred_test - test$price)^2))


pred = predict(xgboost, 
               newdata=as.matrix(score_input))

submissionFile=data.frame(id=score$id,price=pred)
write.csv(submissionFile,'final_submission.csv',row.names = F)
