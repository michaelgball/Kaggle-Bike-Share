library(stacks)
library(tidymodels)
library(tidyverse)
library(vroom)
library(glmnet)
library(poissonreg)

## Read in the data
bikeTrain <- vroom("./train.csv")
bikeTest <- read.csv("./test.csv")
## Remove casual and registered because we can't use them to predict
bikeTrain <- bikeTrain %>%
  select(-casual, - registered)
bikeTrain$datetime <- as_datetime(bikeTrain$datetime)
bikeTest$datetime <- as_datetime(bikeTest$datetime)

my_recipe <- recipe(count~., data=bikeTrain) %>%
  step_mutate(weather=ifelse(weather==4, 3, weather)) %>% #Relabel weather 4 to 3
  step_mutate(weather=factor(weather, levels=1:3, labels=c("Sunny", "Mist", "Rain"))) %>%
  step_mutate(season=factor(season, levels=1:4, labels=c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday=factor(holiday, levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_mutate(workingday=factor(workingday,levels=c(0,1), labels=c("No", "Yes"))) %>%
  step_time(datetime, features="hour") %>%
  step_rm(datetime)%>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())
my_recipe <- prep(my_recipe)

folds <- vfold_cv(bikeTrain, v = 5, repeats=1)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

#Poisson Model
lin_model <- linear_reg() %>%
  set_engine("lm")
## Set up the whole workflow
lin_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(lin_model) %>%
  fit(data=bikeTrain)
lin_reg_model <-fit_resamples(pois_wf,resamples = folds,
              metrics = metric_set(rmse), control = tunedModel)

#penalized regression
preg_model <- linear_reg(penalty = tune(), mixture=tune())%>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(my_recipe)%>%
  add_model(preg_model)
preg_tuning_grid <- grid_regular(penalty(), mixture(), levels=4)
preg_models <- preg_wf %>%
  tune_grid(resamples=folds,
          grid=preg_tuning_grid,
          metrics=metric_set(rmse, mae, rsq),
          control = untunedModel)

##Random Forest
rf_model <- rand_forest(mtry = tune(),
                          min_n=tune(),
                          trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")
rf_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(rf_model)
rf_tuning_grid <- grid_regular(mtry(c(1,20)), min_n(), levels = 3)
rf_models <- rf_wf %>%
  tune_grid(resamples=folds,
            grid=rf_tuning_grid,
            metrics=metric_set(rmse, mae, rsq),
            control = untunedModel)


## Specify with models to include
my_stack <- stacks() %>%
add_candidates(lin_reg_model) %>%
add_candidates(preg_models) %>%
add_candidates(rf_models)

## Fit the stacked model
stack_mod <- my_stack %>%
blend_predictions() %>% # LASSO penalized regression meta-learner
  fit_members() ## Fit the members to the dataset

## If you want to build your own metalearner you'll have to do so manually using
stackData <- as_tibble(my_stack)

## Use the stacked data to get a prediction
stack_mod_preds <- stack_mod %>% predict(new_data=bikeTest)%>% 
  bind_cols(., bikeTest) %>% 
  select(datetime, .pred) %>% 
  rename(count=.pred) %>% 
  mutate(count=pmax(0, count)) %>% 
  mutate(datetime=as.character(format(datetime)))
## Write prediction file to CSV
vroom_write(x=stack_mod_preds, file="./StackModPreds.csv", delim=",")
