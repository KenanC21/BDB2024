library(tidyverse)
library(dplyr)
library(viridis)
library(nflfastR)
library(gt)
library(gtExtras)
library(webshot2)
library(scales)
windowsFonts("Roboto" = windowsFont("Roboto"))

source("code/final_scripts/final_results.R")

# code from this super handy stack overflow thread for side by side GT tables
# https://stackoverflow.com/questions/65835639/arrange-gt-tables-side-by-side-or-in-a-grid-or-table-of-tables
misalignment_rp_table <- function(x){
  gt(x) %>% 
    tab_options(column_labels.hidden = TRUE) %>% 
    as_raw_html()
}

#Top 15 Players by Misalignment by Pass and Run plays respectively
Top15_pass = season_results_pass %>%
  group_by(position) %>% 
  mutate(position = recode(position,"CB" = "Cornerback",
                         "LB" = "Linebacker",
                         "S" = "Safety",
                         "DT" = "Defensive Tackle",
                         "DE" = "Defensive End")) %>%
  filter(misalignment_rank < 11)

Top15_run = season_results_run %>%
  group_by(position) %>% 
  mutate(position = recode(position,"CB" = "Cornerback",
                           "LB" = "Linebacker",
                           "S" = "Safety",
                           "DT" = "Defensive Tackle",
                           "DE" = "Defensive End")) %>%
  filter(misalignment_rank < 11)


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
      headshot_url = " ",
      snaps = "Snaps",
      avg_misalignment = 'AVG Misalignment',
      displayName = "Player Name"
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
        columns = 6
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

# # Top 15 Pass
# pass_table <- pass_headshots %>%
#   select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, avg_tackle_prob_lost, snaps) %>%
#   mutate(avg_misalignment = round(avg_misalignment, 4),
#          avg_tackle_prob_lost = round(avg_tackle_prob_lost, 4)) %>%
#   gt(groupname_col = 'position') %>%
#   gt_theme_pff() %>%
#   tab_header(title = "Top 15 Players in Pass Play Misalignment by Position")
# 
# pass_table
# 
# # Top 15 Run
# run_table <- run_headshots %>%
#   select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, avg_tackle_prob_lost, snaps) %>%
#   mutate(avg_misalignment = round(avg_misalignment, 4),
#          avg_tackle_prob_lost = round(avg_tackle_prob_lost, 4)) %>%
#   gt(groupname_col = 'position') %>%
#   gt_theme_pff() %>%
#   tab_header(title = "Top 15 Players in Run Play Misalignment by Position")
# 
# run_table


# Defensive Tackle --------------------------------------------------------
dt_pass_table <- pass_headshots %>%
  filter(position == "Defensive Tackle") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Pass Play Misalignment by Position")

dt_pass_table

dt_run_table <- run_headshots %>%
  filter(position == "Defensive Tackle") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Run Play Misalignment by Position")

dt_run_table

dt_table <- gt_two_column_layout(tables = list(dt_run_table, dt_pass_table),
                                 vwidth = 1000,
                                 output = "save",
                                 filename = "viz/dt_misalignment_table.png")

dt_table

# Safety ------------------------------------------------------------------
saf_pass_table <- pass_headshots %>%
  filter(position == "Safety") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Pass Play Misalignment by Position")

saf_pass_table

saf_run_table <- run_headshots %>%
  filter(position == "Safety") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Run Play Misalignment by Position")

saf_run_table

saf_table <- gt_two_column_layout(tables = list(saf_run_table, saf_pass_table),
                                 vwidth = 1000,
                                 output = "save",
                                 filename = "viz/dsaf_misalignment_table.png")

saf_table

# Defensive End -----------------------------------------------------------
de_pass_table <- pass_headshots %>%
  filter(position == "Defensive End") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Pass Play Misalignment by Position")

de_pass_table

de_run_table <- run_headshots %>%
  filter(position == "Defensive End") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Run Play Misalignment by Position")

de_run_table

de_table <- gt_two_column_layout(tables = list(de_run_table, de_pass_table),
                                 vwidth = 1000,
                                 output = "save",
                                 filename = "viz/de_misalignment_table.png")

de_table

# Linebacker --------------------------------------------------------------
lb_pass_table <- pass_headshots %>%
  filter(position == "Linebacker") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Pass Play Misalignment by Position")

lb_pass_table

lb_run_table <- run_headshots %>%
  filter(position == "Linebacker") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Run Play Misalignment by Position")

lb_run_table

lb_table <- gt_two_column_layout(tables = list(lb_run_table, lb_pass_table),
                                 vwidth = 1000,
                                 output = "save",
                                 filename = "viz/lb_misalignment_table.png")

lb_table

# Cornerback --------------------------------------------------------------
cb_pass_table <- pass_headshots %>%
  filter(position == "Cornerback") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Pass Play Misalignment by Position")

cb_pass_table

cb_run_table <- run_headshots %>%
  filter(position == "Cornerback") %>% 
  select(position, misalignment_rank, displayName, headshot_url, avg_misalignment, snaps) %>%
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>%
  gt(groupname_col = 'position') %>%
  gt_theme_pff() %>%
  tab_header(title = "Top 10 Players in Run Play Misalignment by Position")

cb_run_table

cb_table <- gt_two_column_layout(tables = list(cb_run_table, cb_pass_table),
                                 vwidth = 1000,
                                 output = "save",
                                 filename = "viz/cb_misalignment_table.png")

cb_table




season_results_rp %>% 
  filter(position == "DT") %>% 
  arrange(misalignment_rank) %>% 
  group_by(relevel(factor(run_pass), "run")) %>% 
  slice_head(n = 10) %>% 
  select(misalignment_rank, displayName, avg_misalignment, snaps) %>% 
  mutate(avg_misalignment = round(avg_misalignment, 3)) %>% 
  group_map(~ misalignment_rp_table(.x)) %>% 
  data.frame(.) %>% 
  setNames(., c("Run", "Pass")) %>% 
  gt() %>% 
  fmt_markdown(columns = TRUE)


