library(tidyverse)
library(tidymodels)
library(skimr)

train <- read_csv("housingprice/train.csv")
test <- read_csv("housingprice/test.csv")

dim(train)
dim(test)
train %>% skim()
all_data <- bind_rows(train, test)

names(all_data)

# target variable: "SalePrice"
ggplot(all_data, aes(x = SalePrice)) + 
    geom_histogram(bins = 50)

glimpse(all_data)
all_data %>% names()

house_recipe <- 
    recipe(SalePrice ~ 
               Neighborhood + 
               GrLivArea + 
               YearBuilt +
               BldgType,
           data = all_data) %>%
    step_log(GrLivArea, base = 10) %>% 
    step_other(Neighborhood, threshold = 0.05) %>% 
    step_dummy(all_nominal())


house_recipe <- prep(house_recipe, training = all_data)
house_recipe

all_data2 <- bake(house_recipe, new_data = all_data)
names(all_data2)
View(all_data2)

# train, test
train_index <- seq_len(nrow(train))
train2 <- all_data2[train_index,]
test2 <- all_data2[-train_index,]

train2 %>% dim()
test2 %>% dim()
all_data2 %>% dim()

# Fitting
lm_model <- 
    linear_reg() %>% 
    set_engine("lm")

lm_form_fit <- 
    lm_model %>% 
    fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

predict(lm_form_fit, new_data = ames_test)
