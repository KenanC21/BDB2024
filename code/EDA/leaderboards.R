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

#round avg_misalignment to 4 decimals
season_results_rp_wider = season_results_rp_wider %>%
  mutate(avg_misalignment_run = round(avg_misalignment_run, 4),
         avg_misalignment_pass = round(avg_misalignment_pass, 4))

#Top 10 Players by Misalignment by Pass and Run plays respectively
Top10_pass = season_results_rp_wider %>%
  group_by(position) %>% 
  mutate(position = recode(position,"CB" = "Cornerback",
                         "LB" = "Linebacker",
                         "S" = "Safety",
                         "DT" = "Defensive Tackle",
                         "DE" = "Defensive End")) %>%
  filter(misalignment_rank < 11)

Top10_run = season_results_rp_wider %>%
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
heads_pass = roster[ , c('displayName', 'headshot_url')] %>% 
  rename("displayName_pass" = "displayName")
heads_run = roster[ , c('displayName', 'headshot_url')] %>% 
  rename("displayName_run" = "displayName")


#merge headshot url's with Top10_pass and Top10_run ordered by misalignment rank
pass_headshots = merge(Top10_pass, heads_pass, by = 'displayName_pass') %>%
  group_by(position) %>%
  arrange(misalignment_rank, .by_group=TRUE) %>% 
  ungroup()

run_headshots = merge(Top10_run, heads_run, by = 'displayName_run') %>%
  group_by(position) %>%
  arrange(misalignment_rank, .by_group=TRUE) %>% 
  relocate('headshot_url', .after = 'snaps_run') %>% 
  ungroup()

#Head shots for pass and run respectively added to df
leaders_headshots <- run_headshots %>%
  add_column('pass_headshot' = pass_headshots$headshot_url) %>% 
  rename("run_headshot" = "headshot_url")

#gt theme from TheMockUp
gt_theme_pff <- function(data, ...) {
  data %>%
    # Add head shot w/ web_image
    text_transform(
      locations = cells_body(
        vars(pass_headshot, run_headshot)
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
      misalignment_rank_run = "Rank",
      run_headshot = " ",
      displayName_run = "Player Name",
      avg_misalignment_run = "Avg Misalignment",
      snaps_run = "Snaps",
      misalignment_rank_pass = "Rank",
      pass_headshot = " ",
      displayName_pass = "Player Name",
      avg_misalignment_pass = "Avg Misalignment",
      snaps_pass = "Snaps"
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
      misalignment_rank_run ~ px(70),
      misalignment_rank_pass ~ px(70),
      displayName_run ~ px(150),
      displayName_pass ~ px(150),
      pass_headshot ~ px(60),
      run_headshot ~ px(60),
      avg_misalignment_run ~ px(150),
      avg_misalignment_pass ~ px(150),
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


# Defensive Tackle --------------------------------------------------------
dt_table <- leaders_headshots %>% 
  filter(position == "Defensive Tackle") %>% 
  select(misalignment_rank_run = misalignment_rank, starts_with("run_"), ends_with("_run"), starts_with("pass_"), ends_with("_pass"), -starts_with("nflId")) %>% 
  mutate(misalignment_rank_pass = misalignment_rank_run) %>% 
  relocate(misalignment_rank_pass, .after = snaps_run) %>% 
  gt() %>% 
  gt_theme_pff() %>% 
  tab_header(title = "Top 10 Defensive Tackles in Tackle Misalignment") %>% 
  gt_add_divider(columns = c(starts_with("snaps_")),
                 sides = "right",
                 color = "black") %>% 
  gt_add_divider(columns = c(misalignment_rank_run),
                 sides = "left",
                 color = "black") %>% 
  tab_spanner(label = "Run",
              columns = c(misalignment_rank_run:snaps_run)) %>% 
  tab_spanner(label = "Pass",
              columns = c(misalignment_rank_pass:snaps_pass))

dt_table

gtsave(dt_table, filename = "viz/dt_misalignment_table.png")

# Safety ------------------------------------------------------------------
saf_table <- leaders_headshots %>% 
  filter(position == "Safety") %>% 
  select(misalignment_rank_run = misalignment_rank, starts_with("run_"), ends_with("_run"), starts_with("pass_"), ends_with("_pass"), -starts_with("nflId")) %>% 
  mutate(misalignment_rank_pass = misalignment_rank_run) %>% 
  relocate(misalignment_rank_pass, .after = snaps_run) %>% 
  gt() %>% 
  gt_theme_pff() %>% 
  tab_header(title = "Top 10 Safeties in Tackle Misalignment") %>% 
  gt_add_divider(columns = c(starts_with("snaps_")),
                 sides = "right",
                 color = "black") %>% 
  gt_add_divider(columns = c(misalignment_rank_run),
                 sides = "left",
                 color = "black") %>% 
  tab_spanner(label = "Run",
              columns = c(misalignment_rank_run:snaps_run)) %>% 
  tab_spanner(label = "Pass",
              columns = c(misalignment_rank_pass:snaps_pass))

saf_table

gtsave(saf_table, filename = "viz/saf_misalignment_table.png")

# Defensive End -----------------------------------------------------------
de_table <- leaders_headshots %>% 
  filter(position == "Defensive End") %>% 
  select(misalignment_rank_run = misalignment_rank, starts_with("run_"), ends_with("_run"), starts_with("pass_"), ends_with("_pass"), -starts_with("nflId")) %>% 
  mutate(misalignment_rank_pass = misalignment_rank_run) %>% 
  relocate(misalignment_rank_pass, .after = snaps_run) %>% 
  gt() %>% 
  gt_theme_pff() %>% 
  tab_header(title = "Top 10 Defensive Ends in Tackle Misalignment") %>% 
  gt_add_divider(columns = c(starts_with("snaps_")),
                 sides = "right",
                 color = "black") %>% 
  gt_add_divider(columns = c(misalignment_rank_run),
                 sides = "left",
                 color = "black") %>% 
  tab_spanner(label = "Run",
              columns = c(misalignment_rank_run:snaps_run)) %>% 
  tab_spanner(label = "Pass",
              columns = c(misalignment_rank_pass:snaps_pass))

de_table

gtsave(de_table, filename = "viz/de_misalignment_table.png")

# Linebacker --------------------------------------------------------------
lb_table <- leaders_headshots %>% 
  filter(position == "Linebacker") %>% 
  select(misalignment_rank_run = misalignment_rank, starts_with("run_"), ends_with("_run"), starts_with("pass_"), ends_with("_pass"), -starts_with("nflId")) %>% 
  mutate(misalignment_rank_pass = misalignment_rank_run) %>% 
  relocate(misalignment_rank_pass, .after = snaps_run) %>% 
  gt() %>% 
  gt_theme_pff() %>% 
  tab_header(title = "Top 10 Linebackers in Tackle Misalignment") %>% 
  gt_add_divider(columns = c(starts_with("snaps_")),
                 sides = "right",
                 color = "black") %>% 
  gt_add_divider(columns = c(misalignment_rank_run),
                 sides = "left",
                 color = "black") %>% 
  tab_spanner(label = "Run",
              columns = c(misalignment_rank_run:snaps_run)) %>% 
  tab_spanner(label = "Pass",
              columns = c(misalignment_rank_pass:snaps_pass))

lb_table

gtsave(lb_table, filename = "viz/lb_misalignment_table.png")

# Cornerback --------------------------------------------------------------
cb_table <- leaders_headshots %>% 
  filter(position == "Cornerback") %>% 
  select(misalignment_rank_run = misalignment_rank, starts_with("run_"), ends_with("_run"), starts_with("pass_"), ends_with("_pass"), -starts_with("nflId")) %>% 
  mutate(misalignment_rank_pass = misalignment_rank_run) %>% 
  relocate(misalignment_rank_pass, .after = snaps_run) %>% 
  gt() %>% 
  gt_theme_pff() %>% 
  tab_header(title = "Top 10 Cornerbacks in Tackle Misalignment") %>% 
  gt_add_divider(columns = c(starts_with("snaps_")),
                 sides = "right",
                 color = "black") %>% 
  gt_add_divider(columns = c(misalignment_rank_run),
                 sides = "left",
                 color = "black") %>% 
  tab_spanner(label = "Run",
              columns = c(misalignment_rank_run:snaps_run)) %>% 
  tab_spanner(label = "Pass",
              columns = c(misalignment_rank_pass:snaps_pass))

cb_table

gtsave(cb_table, filename = "viz/cb_misalignment_table.png")
