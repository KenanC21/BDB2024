library(svDialogs)
library(tidymodels)
library(xgboost)
library(caret)
library(tidyverse)

# path <- dlg_input(message = "Input your working directory:",
#                   default = "C:/DIRECTORY/BDB2024")$res

path <- "C:/Users/Michael Egle/BDB2024"

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
  initial_split(prop = 0.8)

# Split training and testing
train_x <- training(cv_split) %>% #Independent variables for train
  select(
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    min_distance_to_ball_carrier,
    dir,
    distance_to_ball_carrier,
    s,
    possible_blockers_within_3_yards,
    ball_carrier_distance_to_sideline,
    ball_carrier_distance_to_endzone,
    ball_carrier_dir_difference,
    ball_carrier_s_difference
  ) %>% 
  as.matrix()

train_y <- training(cv_split) %>% #Dependent variable for train
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

test_x <- testing(cv_split) %>% #Independent vars for test
  select(
    difference_min_distance_to_ball_carrier,
    max_angle_formed_by_blocker_and_ball_carrier,
    min_distance_to_ball_carrier,
    dir,
    distance_to_ball_carrier,
    s,
    possible_blockers_within_3_yards,
    ball_carrier_distance_to_sideline,
    ball_carrier_distance_to_endzone,
    ball_carrier_dir_difference,
    ball_carrier_s_difference
  ) %>% 
  as.matrix()

test_y <- testing(cv_split) %>% #Dependent var for test
  select(will_have_chance_to_make_tackle) %>% 
  as.matrix()

xgboost_train = xgb.DMatrix(data=train_x, label=train_y)
xgboost_test = xgb.DMatrix(data=test_x, label=test_y)

param_grid <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.01, 0.1, 0.15, 0.2),
  max_depth = c(2, 4, 6),
  gamma = 1
)

model_cv_helper <- function(param_grid, iteration, threshold = 0.274)
{
  print(paste0("--------------- ITERATION ", iteration, " ---------------"))
  print("TRAINING MODEL WITH FOLLOWING PARAMETERS:")
  print(paste("MAX DEPTH:", param_grid$max_depth[iteration]))
  print(paste("N ROUNDS:", param_grid$nrounds[iteration]))
  print(paste("ETA:", param_grid$eta[iteration]))
  print(paste("GAMMA:", param_grid$gamma[iteration]))
  
  temp_model <- xgboost(data = xgboost_train,
                        max.depth = param_grid$max_depth[iteration],
                        nrounds = param_grid$nrounds[iteration],
                        eta = param_grid$eta[iteration],
                        gamma = param_grid$gamma[iteration],
                        params = list(objective = "binary:logistic"),
                        eval_metric = 'auc')

  pred <- predict(temp_model, xgboost_test)

  pred_factor = factor(ifelse(pred >= threshold, 1, 0))

  pred <- testing(cv_split) %>%
    bind_cols(pred = pred_factor)

  conf_mat <- confusionMatrix(as.factor(as.numeric(test_y)), pred_factor,
                              positive = "1")
  
  # evaluation metrics
  # calculate error rate, specificity and sensitivity
  
  return(data.frame(max_depth = param_grid$max_depth[iteration],
                    nrounds = param_grid$nrounds[iteration],
                    eta = param_grid$eta[iteration],
                    gamma = param_grid$gamma[iteration],
                    accuracy = conf_mat[["overall"]][["Accuracy"]],
                    sensitivity = conf_mat[["byClass"]][["Sensitivity"]],
                    specificity = conf_mat[["byClass"]][["Specificity"]],
                    positive_predictive_value = conf_mat[["byClass"]][["Pos Pred Value"]],
                    negative_predictive_value = conf_mat[["byClass"]][["Neg Pred Value"]]))
}

cv_results <- pmap_dfr(.l = list(1:nrow(param_grid)),
                       .f = model_cv_helper,
                       param_grid = param_grid)

write_csv(cv_results, "misc/participation_model_cv_results.csv")

# now using the optimal hyperparameter values found above, find the optimal 
# decision threshold

# We already do some threshold cutoffs to get a better idea above, but use an ROC
# curve with the optimal parameters above to get a better

cv_results <- read_csv("misc/participation_model_cv_results.csv")

# PARTICIPATION MODEL PARAMETER VALUES:
# - max depth: 6
# - n rounds: 100
# - eta: 0.1
# - gamma: 1