#open data
file_path="../data/maindata.csv"
monthly_file_path="../data/data_monthly.csv"
data<-read.csv(file_path)
monthly_data<-read.csv(monthly_file_path)

#Preprocessing data
monthly_data<- monthly_data[, !names(monthly_data) %in% c("X","tempmax", "tempmin", "feelslikemax","feelslikemin","solarradiation", "solarenergy", "uvindex", "severerisk", "snowdepth", "snow" )]
monthly_data[is.na(monthly_data)]<-0
head(monthly_data)


#one hot vector for the month column
library(caret)

monthly_data$month<-as.factor(monthly_data$month)
one_hot_matrix<-dummyVars(~month,data=monthly_data)
one_hot_data_df<-data.frame(predict(one_hot_matrix,newdata=monthly_data))
prepro_monthly_data<-cbind(monthly_data,one_hot_data_df)
prepro_monthly_data<-prepro_monthly_data[,!(names(prepro_monthly_data) %in% c("month"))]

head(prepro_monthly_data)


#Spliting data
df_len<-nrow(prepro_monthly_data)
train_data_indices=as.integer(df_len*0.7)
valid_data_indices=as.integer(df_len*0.85)
train_data<-prepro_monthly_data[1:train_data_indices,]
valid_data<-prepro_monthly_data[train_data_indices:valid_data_indices,]
valid_data<-valid_data[complete.cases(valid_data),]
test_data<-prepro_monthly_data[valid_data_indices+1:df_len,]
test_data<-test_data[complete.cases(test_data),]


#Making a target value
train_y<-subset(train_data,select="temp")
valid_y<-subset(valid_data,select="temp")
test_y<-subset(test_data,select="temp")
train_data<-subset(train_data,select=-temp)
valid_data<-subset(valid_data,select=-temp)
test_data<-subset(test_data,select=-temp)


colSums(is.na(train_data))
nrow(train_data)
nrow(valid_data)
nrow(test_data)


#normalizing data columns
normalize_col <-c("humidity","windspeed","cloudcover","sealevelpressure","precip") 

train_data[normalize_col] <- scale(train_data[normalize_col])
valid_data[normalize_col] <- scale(valid_data[normalize_col])
test_data[normalize_col] <- scale(test_data[normalize_col])
train_y_array=unlist(train_y)
valid_y_array=unlist(valid_y)
data_ncols=ncol(train_data)
head(train_data)


#making dataframe to save the result
train_loss=c()
val_loss=c()
train_mae=c()
val_mae=c()
columns=c()
result_df_feel<-data.frame(columns=columns,train_loss=train_loss,val_loss=val_loss,train_mae=train_mae,val_mae=val_mae)

#feature selections
fixed_columns=c("year","month.1","month.2","month.3","month.4","month.5","month.6","month.7","month.8","month.9","month.10","month.11","month.12")
train_data[fixed_columns]
add_columns<-c('Clear','Overcast','Partially.cloudy','Rain..Partially.cloudy','Rain..Overcast','Rain','windspeed','winddir','sealevelpressure','cloudcover','visibility','moonphase','sunrise_time','sunset_time'
,"dew","humidity","precip","precipprob","precipcover","windgust", )

library(keras)
train_fs=train_data[fixed_columns]
valid_fs=valid_data[fixed_columns]
test_fs=test_data[fixed_columns]
for (col in add_columns){
  print(col)
  train_fs=cbind(train_fs,train_data[col])
  valid_fs=cbind(valid_fs,valid_data[col])
  test_fs=cbind(test_fs,test_data[col])
  
  model <- keras_model_sequential() %>%
    layer_lstm(units =32, activation = "relu", input_shape = c(ncol(train_fs),1),return_sequences=TRUE) %>%
    layer_lstm(units =16, activation = "relu",return_sequences=FALSE) %>%
    layer_dense(units = 1,activation="linear")
  
  optimizer <- keras$optimizers$legacy$Adam(learning_rate = 0.001)
  model %>% compile(
    loss='mean_squared_error',
    optimizer=optimizer,
    metrics=c('mean_absolute_error')
  )
  
  history<-model %>% fit(
    as.matrix(train_fs),
    train_y_array,
    epochs=100,
    batch_size=4,
    validation_data=list(as.matrix(valid_fs),valid_y_array)
  )
  
  val_mae <- history$metrics$val_mean_absolute_error[[length(history$metrics$val_mean_absolute_error)]]
  train_mae <- history$metrics$mean_absolute_error[[length(history$metrics$mean_absolute_error)]]
  val_loss <- history$metrics$val_loss[[length(history$metrics$val_loss)]]
  train_loss <- history$metrics$loss[[length(history$metrics$loss)]]
  new_row<-data.frame(columns=col,train_loss=train_loss,val_loss=val_loss,train_mae=train_mae,val_mae=val_mae)
  result_df<-rbind(result_df,new_row)
}


write.csv(result_df ,file="feature_selection.csv",row.names=TRUE)


#Parameter selection
batch=c(4,8,16,32)
epochs=c(100,200,300)

best_features<-c("dew", "humidity","precip","precipprob","precipcover")
best_train_data<-cbind(train_data[fixed_columns],train_data[best_features])
best_valid_data<-cbind(valid_data[fixed_columns],valid_data[best_features])
best_test_data<-cbind(test_data[fixed_columns],test_data[best_features])
ncol(best_train_data)

#making a dataframe to save the result of parameter selection

b_train_loss=c()
b_val_loss=c()
b_train_mae=c()
b_val_mae=c()
batch=c()
epoch=c()
parameters_df<-data.frame(batch=batch,epoch=epoch,train_loss=b_train_loss,val_loss=b_val_loss,train_mae=b_train_mae,val_mae=b_val_mae)


for (ep in epochs){
  model <- keras_model_sequential() %>%
    layer_lstm(units =32, activation = "relu", input_shape = c(ncol(best_train_data),1),return_sequences=TRUE) %>%
    layer_lstm(units =16, activation = "relu",return_sequences=FALSE) %>%
    layer_dense(units = 1,activation="linear")
  
  optimizer <- keras$optimizers$legacy$Adam(learning_rate = 0.001)
  model %>% compile(
    loss='mean_squared_error',
    optimizer=optimizer,
    metrics=c('mean_absolute_error')
  )
  
  history<-model %>% fit(
    as.matrix(best_train_data),
    train_y_array,
    epochs=ep,
    batch_size=4,
    validation_data=list(as.matrix(best_valid_data),valid_y_array)
  )
  
  b_val_mae <- history$metrics$val_mean_absolute_error[[length(history$metrics$val_mean_absolute_error)]]
  b_train_mae <- history$metrics$mean_absolute_error[[length(history$metrics$mean_absolute_error)]]
  b_val_loss <- history$metrics$val_loss[[length(history$metrics$val_loss)]]
  b_train_loss <- history$metrics$loss[[length(history$metrics$loss)]]
  
  b_new_row<-data.frame(batch=16,epoch=ep,train_loss=b_train_loss,val_loss=b_val_loss,train_mae=b_train_mae,val_mae=b_val_mae)
  parameters_df<-rbind(parameters_df,b_new_row)
}

write.csv(parameters_df,file="parameters_selection.csv",row.names=TRUE)


#Prediction by using the best model
test_pred=model %>% predict(
  as.matrix(best_test_data)
)  


#Plotting the prediction and true value to check the difference

plot(test_pred,type='l',col="blue")
lines(test_y,type='l',col="red")
legend("topright",lengend=c("prediction","true_value"),col=c("blue","red"))
