
library(keras)
library(tfdatasets)


boston_housing <- dataset_boston_housing()
c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

#여기서 라벨은 집값을 천달러단위로 표현 한 것이다.(데이터셋은 1970년대 중반이라고 한다)


paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))
train_data[1, ] # Display sample features, notice the different scales
library(dplyr)

column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')


train_df <- train_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = train_labels)

test_df <- test_data %>% 
  as_tibble(.name_repair = "minimal") %>% 
  setNames(column_names) %>% 
  mutate(label = test_labels)

train_labels[1:10] # Display first 10 entries
spec <- feature_spec(train_df, label ~ . ) %>%  # recipe 랑비슷한데.. 환경설정? 데이터 프레임 및 종속변수 등록 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()

spec
# 스킵해보기 -------------------------------------------------------------------

 # layer <- layer_dense_features(
 #   feature_columns = dense_features(spec), 
 #   dtype = tf$float32
 # )  #뭘 하는지 모르겠습니다..아마 spec 누르면 나오는 세세한 부분들 조정하는 것 같습니다.
 

# 데이터프레임으로 부터 넣기 
input <- layer_input_from_dataset(train_df %>% select(-label))
input
#dense_features(spec) %>% View()


output <- input %>%  # 인풋에 데이터 변수들 집어넣고, 다음레이어들 구성
  layer_dense_features(dense_features(spec)) %>%  #첫번째는 변수들끼리의 dense( 교호작용항으로 이해해도 될까요?)
  layer_dense(units = 64, activation = "relu") %>% # 은닉층 형성  +use_bias = F 할수 있음
  layer_dense(units = 64, activation = "relu") %>% #은닉층 형성
  layer_dense(units = 1) #최종 결과 값 output

model <- keras_model(input, output)

summary(model)
#64*13+64(bias항)
#64*13*5 ?? 왜 parameter가 4160..........
model %>% 
  compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error"
                   #,"mean_absolute_percentage_error" 여러개 가능
                   )
    
  )
model
#compile을 해도 어떤 변화가 보이지는 않습니다.

# build_model <- function() {
#   input <- layer_input_from_dataset(train_df %>% select(-label))
# 
#   output <- input %>%
#     layer_dense_features(dense_features(spec)) %>%
#     layer_dense(units = 64, activation = "relu") %>%
#     layer_dense(units = 64, activation = "relu") %>%
#     layer_dense(units = 1)
# 
#   model <- keras_model(input, output)
# 
#   model %>%
#     compile(
#       loss = "mse",
#       optimizer = optimizer_rmsprop(),
#       metrics = list("mean_absolute_error")
#     )
# 
#   model
# }


# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 20 == 0) cat("\n") # 20번 반복할떄마다 .출력
    cat(".")
  }
)    

# callback_learning_rate_scheduler() :schedule을 정해주면, 어느정도 학습되고 있는지 보여주는듯..

#model <- build_model()

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 100,
  validation_split = 0.2,
  verbose = F,
  callbacks = list(print_dot_callback)
)

library(ggplot2)
plot(history)
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()

history <- model %>% fit(
  x = train_df %>% select(-label),
  y = train_df$label,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop)
)

plot(history)
c(loss, mae) %<-% (model %>% evaluate(test_df %>% select(-label), test_df$label, verbose = F))
loss


paste0("Mean absolute error on test set: $", sprintf("%.2f", mae * 1000))
#sprintf는 C스타일로 문자나 숫자 출력 할 수 있는 함수
test_predictions <- model %>% predict(test_df %>% select(-label))
test_predictions[ , 1]


lm_model<-lm(label~.,data=train_df)

mean((test_df$label-predict(lm_model,test_df %>% select(-label)))^2)
loss


