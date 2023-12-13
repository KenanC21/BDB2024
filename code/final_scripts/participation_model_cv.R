library(svDialogs)
library(tidymodels)
library(caret)

path <- dlg_input(message = "Input your working directory:",
                  default = "C:/DIRECTORY/BDB2024")$res

setwd(path)
source("code/util/create_and_standardize_week_data.R")
# source("code/modeling/lowo_modeling.R")

# load in supplementary data
plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")

tictoc::tic()
all_data <- pmap_dfr(.l = list(1:9),
                     .f = create_and_standardize_week_data,
                     .progress = T)
tictoc::toc()

# Cross validation #####
# - first find the "optimal" hyperparameter values under assumption that optimal
# decision threshold is 0.5 (it will likely be higher)
# - then use the "optimal" hyperparameters to find optimal decision threshold
# - check to see if optimal hyperparameters still hold
set.seed(30)
cv_split <- all_data %>% 
  initial_split(all_data, prop = 0.8, strata = week)

train_x <- training(cv_split) %>% 
  select(
    # TODO - input the predictor variables we find useful
  ) %>% 
  as.matrix()

train_y <- training(cv_split) %>% 
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

validation_x <- testing(cv_split) %>% 
  select(
    # TODO - input the predictor variables we find useful
  ) %>% 
  as.matrix()

validation_y <- testing(cv_split) %>% 
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

param_grid <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.01, 0.1, 0.15, 0.2),
  max_depth = c(2, 4, 6),
  gamma = 1
)

model_cv_helper <- function(param_grid, iteration, threshold = 0.5)
{
  print(paste0("--------------- ITERATION ", iteration, " ---------------"))
  print("TRAINING MODEL WITH FOLLOWING PARAMETERS:")
  print(paste("MAX DEPTH:", param_grid$max_depth[iteration]))
  print(paste("N ROUNDS:", param_grid$nrounds[iteration]))
  print(paste("ETA:", param_grid$eta[iteration]))
  print(paste("GAMMA:", param_grid$gamma[iteration]))
  
  temp_model <- xgboost(data = train_x,
                        max.depth = param_grid$max_depth[iteration],
                        nrounds = param_grid$nrounds[iteration],
                        eta = param_grid$eta[iteration],
                        gamma = param_grid$gamma[iteration],
                        params = list(objective = "binary:logistic"),
                        eval_metric = 'auc')

  pred <- predict(temp_model, validation_x)

  pred_factor = factor(ifelse(pred_test >= threshold, 1, 0))

  pred <- testing(prelim_split) %>%
    bind_cols(pred = pred_test_fact)

  conf_mat <- confusionMatrix(as.factor(as.numeric(test_y)), pred_test_fact)
  
  # evaluation metrics
  # calculate error rate, specificity and sensitivity
  
  return(data.frame(max_depth = param_grid$max_depth[iteration],
                    nrounds = param_grid$nrounds[iteration],
                    eta = param_grid$eta[iteration],
                    gamma = param_grid$gamma[iteration],
                    accuracy = conf_mat[["overall"]][["Accuracy"]],
                    sensitivity = conf_mat[["byClass"]][["Sensitivity"]],
                    specificity = conf_mat[["byClass"]][["Specificity"]]))
}

cv_results <- pmap_dfr(.l = list(1:nrow(param_grid)),
                       .f = model_cv_helper,
                       param_grid = param_grid)

write_csv(cv_results, "misc/participation_model_cv_results.csv")

# now using the optimal hyperparameter values found above, find the optimal 
# decision threshold

# We already do some threshold cutoffs to get a better idea above, but use an ROC
# curve with the optimal parameters above to get a better



