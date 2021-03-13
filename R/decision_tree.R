library(tidymodels)
library(tidyverse)
library(palmerpenguins)
library(rpart)
library(rpart.plot)
library(magrittr)

set.seed(1234)

penguins %<>% 
    drop_na() %>% 
    initial_split(prop = 0.7,
                  strata = species)

train_data <- penguins %>% training()
test_data <- penguins %>% testing()

penguins_recipe <- train_data %>% 
    recipe(species ~ .) %>% 
    step_naomit(all_numeric(), all_nominal()) %>% 
    prep()

train_data <- juice(penguins_recipe)
test_data <- penguins_recipe %>% bake(test_data)

dt_model <-
    decision_tree(
        cost_complexity = 0.01, # cp
        tree_depth = 10, # max depth
        min_n = 5
    ) %>% 
    set_engine("rpart") %>%     
    set_mode("classification")

dt_model_fit <- 
    dt_model %>% 
    fit(species ~ ., data = train_data)

result <- dt_model_fit$fit

rpart.plot(result)
printcp(initial_fit)
plotcp(initial_fit)

# ==============================================

model_f <- penguins %>% 
    select(-species) %>% 
    names() %>% paste(collapse = " + ") %>%
    paste("species~", .) %>% 
    as.formula()
    
# fitting
initial_fit <- rpart(model_f,
             data = penguins, 
             method = "class", # regression = "anova"
             control = rpart.control(
                 minsplit = 5,
                 minbucket = 2,
                 cp = 0,
                 maxdepth = 20),
             parms = list(split = "information")
)

rpart.plot(initial_fit)
printcp(initial_fit)
plotcp(initial_fit)


pdt1 <- prune(initial_fit, cp = initial_fit$cptable[which.min(initial_fit$cptable[, "xerror"]), "CP"])

# Plot the tree
rpart.plot(pdt1)



