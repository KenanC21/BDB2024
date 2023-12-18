source("code/util/create_and_standardize_week_data.R")
source("code/modeling/train_participation_model.R")
source("code/modeling/train_tackle_model.R")
source("code/modeling/create_position_radius.R")
source("code/modeling/predict_at_hypothetical_point.R")
library(future)
library(furrr)

path <- "C:/Users/Michael Egle/BDB2024"

setwd(path)

plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")


test <- readRDS("misc/participation_model_pred.RDS")



tictoc::tic()
test_position_radius <- create_position_radius(test)
tictoc::toc()

print(nrow(test_position_radius))