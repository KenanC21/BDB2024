library(tidyverse)
library(dplyr)
library(nflfastR)
library(gt)
library(gtExtras)
windowsFonts("Roboto" = windowsFont("Roboto"))

source("code/final_scripts/final_results.R")

#Top 15 Players by Misalignment by Pass and Run plays respectively
Top15_pass = season_results_pass %>%
  group_by(position) %>% 
  mutate(position = recode(position,"CB" = "Cornerback",
                         "LB" = "Linebacker",
                         "S" = "Safety",
                         "DT" = "Defensive Tackle",
                         "DE" = "Defensive End")) %>%
  filter(misalignment_rank < 16)

Top15_run = season_results_run %>%
  group_by(position) %>% 
  mutate(position = recode(position,"CB" = "Cornerback",
                           "LB" = "Linebacker",
                           "S" = "Safety",
                           "DT" = "Defensive Tackle",
                           "DE" = "Defensive End")) %>%
  filter(misalignment_rank < 16)


#load_roster from nflreadr to get 2022 headshots
roster = nflreadr::load_rosters(2022)
names(roster)[7] <- "displayName"
heads = roster[ , c('displayName', 'headshot_url')]

#merge headshot url's with Top15_pass and Top15_run ordered by misalignment rank
pass_headshots = merge(Top15_pass, heads, by = 'displayName') %>%
  arrange(misalignment_rank, .by_group = TRUE) 
run_headshots = merge(Top15_run, heads, by = 'displayName') %>%
  arrange(misalignment_rank, .by_group = TRUE) 



#gt theme from TheMockUp
gt_theme_pff <- function(data, ...) {
  data %>%
    # Add head shot w/ web_image
    text_transform(
      locations = cells_body(
        vars(headshot_url)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 35
        )
      }
    ) %>%
    # Relabel columns
    cols_label(
      misalignment_rank = 'Rank',
      position = "Position",
      headshot_url = " ",
      snaps = "Snaps",
      avg_misalignment = 'AVG Misalignment',
      avg_tackle_prob_lost = 'AVG Tackle Prob Lost',
      displayName = "Player"
    ) %>%
    # if missing, replace NA w/ ---
    fmt_missing(
      columns = everything(),
      missing_text = "---"
    ) %>%
    # Change font color and weight for numeric col
    tab_style(
      style = list(
        cell_text(color = "black", weight = "bold")
      ),
      locations = cells_body(
        columns = 7
      )
    ) %>%
    # Make column labels and spanners all caps
    opt_all_caps() %>%
    # add row striping
    opt_row_striping() %>%
    # change overall table styling for borders and striping
    tab_options(
      column_labels.background.color = "black",
      table_body.hlines.color = "transparent",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "transparent",
      row.striping.background_color = "#f9f9fb",
      data_row.padding = px(3)
    ) %>%
    cols_width(
      misalignment_rank ~ px(70),
      displayName ~ px(150),
      headshot_url ~ px(60),
      avg_misalignment ~ px(150),
      avg_tackle_prob_lost ~ px(150),
      position ~ px(70),
      everything() ~ px(60)
    ) %>% 
    # change color of border separating the text from the sourcenote
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "black", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        rows = nrow(data$`_data`)
      )
    ) %>%
    # change font to Lato throughout (note no need to have Lato locally!)
    opt_table_font(
      font = "Roboto"
    ) %>%
    cols_align("center") %>%
    opt_align_table_header("center") %>%
    tab_style(style = cell_text(whitespace = "nowrap",
                                align = "center"),
              locations = cells_row_groups()) %>%
    tab_style(style = cell_text(v_align = "middle"),
              locations = cells_body())
}

# Top 15 Pass
pass_table <- pass_headshots %>%
  select(misalignment_rank, snaps, displayName, headshot_url, avg_misalignment, avg_tackle_prob_lost, position) %>%
  mutate(avg_misalignment = round(avg_misalignment, 4),
         avg_tackle_prob_lost = round(avg_tackle_prob_lost, 4),
         Position = case_when(position == "Cornerback" ~ "CB",
                                      position == "Linebacker" ~ "LB",
                                      position == "Safety" ~ "S",
                                      position == "Defensive Tackle" ~ "DT",
                                      position == "Defensive End" ~ "DE")) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 15 Players in Pass Play Misalignment by Position")

pass_table

# Top 15 Run
run_table <- run_headshots %>%
  select(misalignment_rank, snaps, displayName, headshot_url, avg_misalignment, avg_tackle_prob_lost, position) %>%
  mutate(avg_misalignment = round(avg_misalignment, 4),
         avg_tackle_prob_lost = round(avg_tackle_prob_lost, 4),
         Position = case_when(position == "Cornerback" ~ "CB",
                              position == "Linebacker" ~ "LB",
                              position == "Safety" ~ "S",
                              position == "Defensive Tackle" ~ "DT",
                              position == "Defensive End" ~ "DE")) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 15 Players in Run Play Misalignment by Position")

run_table
