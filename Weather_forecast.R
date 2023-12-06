file_path="C:/Users/권예은/Documents/Academic/Fall2023/Data mining/Final PJ/Fall2023_Datamining_Group3/data/maindata.csv"
monthly_file_path="C:/Users/권예은/Documents/Academic/Fall2023/Data mining/Final PJ/Fall2023_Datamining_Group3/data/data_monthly.csv"
data<-read.csv(file_path)
monthly_data<-read.csv(monthly_file_path)
monthly_data<-subset(monthly_data,select=c(year,month))
data<-subset(data,select=c(-X,-solarradiation , -uvindex))

#missing value 
main_data<-cbind(monthly_data,data)
main_data$Clear[is.na(main_data$Clear)]<-0
main_data$Overcast[is.na(main_data$Overcast)]<-0
main_data$Partially.cloudy[is.na(main_data$Partially.cloudy)]<-0
main_data$Rain..Partially.cloudy[is.na(main_data$Rain..Partially.cloudy)]<-0
main_data$Rain..Overcast[is.na(main_data$Rain..Overcast)]<-0
main_data$Rain[is.na(main_data$Rain)]<-0
main_data$precip[is.na(main_data$precip)]<-0

#Train data 1990/1/1-2014/12/31
#Valid data 2015/1/1-2019/12/31
#Test data  2020/1/1-2023/11/31

train_data<-subset(main_data,year<2015)
valid_data<-subset(main_data,year<2020 & year >2014)
test_data<-subset(main_data,year>=2020)
train_y<-subset(train_data,select="temp")
valid_y<-subset(valid_data,select="temp")
test_y<-subset(test_data,select="temp")
train_data<-subset(train_data,select=-temp)
valid_data<-subset(valid_data,select=-temp)
test_data<-subset(test_data,select=-temp)
colSums(is.na(train_data))

#option 1 z-score
z_score_normalization <- function(x) {(x-mean(x))/sd(x)}
train_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-lapply(train_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")],z_score_normalization)
valid_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-lapply(valid_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")],z_score_normalization)
test_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-lapply(test_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")],z_score_normalization)

# option 2 scaling min_loss =1.8530
train_scale_data=train_data
valid_scale_data=valid_data
test_scale_data=test_data
train_scale_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-scale(train_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")])
valid_scale_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-scale(valid_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")])
test_scale_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")]<-scale(test_data[,c("humidity","windspeed","cloudcover","sealevelpressure","precip")])
# option 3 normalize keras


#Making a model
install.packages("/"{{}})
install_keras()
library(keras)
install_tensorflow()
normalize(train_data[,0])

#Change the input type
train_data_matrix<-matrix(unlist(train_scale_data),ncol=length(train_scale_data))
dim(train_data_matrix)
train_data<-setNames(train_scale_data,c('lstm_1_input'))
data_ncols<-ncol(train_scale_data)
train_y_array=unlist(train_y)
typeof(train_y_array)


valid_data_matrix<-matrix(unlist(valid_scale_data),ncol=length(valid_scale_data))
valid_y_array=unlist(valid_y)

test_data_matrix<-matrix(unlist(test_scale_data),ncol=length(test_scale_data))
test_y_array=unlist(test_y)

model <- keras_model_sequential() %>%
  layer_lstm(units = 16, activation = "relu", input_shape = c(13,1)) %>%
  layer_flatten() %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss='mean_squared_error',
  optimizer=optimizer_adam()
)
model %>% fit(
  train_data_matrix,
  train_y_array,
  epochs=100,
  batch_size=8
)

valid_pred=model %>% predict(
  valid_data_matrix,batch_size=8
)
test_pred=model %>% predict(
  test_data_matrix,batch_size=8
)
#plotting graphs

plot(test_pred,type='l',col="blue")
lines(test_y_array,type='l',col="red")
legend("topright",lengend=c("prediction","true_value"),col=c("blue","red"))


library(ggplot2)
ggplot(aes(x=seq(1,valid_pred)))+
  geom_line(aes(y = valid_pred), color = "blue", linetype = "solid") +
  geom_line(aes(y = valid_y_array), color = "red", linetype = "dashed") +
  labs(title = "Two Line Graphs", x = "X-axis", y = "Y-axis") +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal()
