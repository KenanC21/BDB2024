library(svDialogs)
library(future)
library(furrr)
# path <- dlg_input(message = "Input your working directory:",
#                   default = "C:/DIRECTORY/BDB2024")$res

path <- "/Users/michaelegle/BDB2024"

setwd(path)
source("code/util/create_and_standardize_week_data.R")
source("code/modeling/train_participation_model.R")
source("code/modeling/train_tackle_model.R")
source("code/modeling/create_position_radius.R")
source("code/modeling/predict_at_hypothetical_point.R")

# load in supplementary data
plays <- read_csv("data/plays.csv")
tackles <- read_csv("data/tackles.csv")

tackle_model <- readRDS("models/tackle_model.RDS")

pred_week <- 9

all_position_circles_with_all_predictor_variables <- readRDS("misc/all_position_circles_with_all_predictor_variables.RDS") %>%
  filter(week == pred_week)

print(paste("Calculating Probabilities for Week", pred_week))

print("Loaded in data")
tictoc::tic()
tackle_probs_by_position <- predict_at_hypothetical_point(all_position_circles_with_all_predictor_variables, tackle_model, pred_week)
tictoc::toc()
print("Tackle probabilities added")

saveRDS(tackle_probs_by_position, paste0("misc/pred_tackle_probs_at_all_points_week_", pred_week, ".RDS"))

print("DONE")

