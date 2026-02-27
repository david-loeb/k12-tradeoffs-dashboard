library(dplyr, warn.conflicts = F)

# Raw trends data
all_states <- readRDS(here::here(
  '../results/treat_ctrl_data/treat-groups_all-data.rds'
)) |>
  filter(!(year %in% c(2001, 2003)))
treat_means <- readRDS(here::here(
  '../results/treat_ctrl_data/treat_means.rds'
)) |>
  filter(!(year %in% c(2001, 2003)))
did_ctrl_means <- readRDS(here::here(
  '../results/treat_ctrl_data/did_ctrl_means.rds'
)) |>
  filter(!(year %in% c(2001, 2003))) |>
  select(-n_treated)
synth_ctrl_means <- readRDS(here::here(
  '../results/treat_ctrl_data/synth_ctrl_means.rds'
)) |>
  filter(
    !grepl('aggregate group', treat_group),
    !(year %in% c(2001, 2003))
  ) |>
  mutate(
    treat_group = stringr::str_remove(treat_group, ' st'),
    treat_group = ifelse(
      grepl('AK', drop_st),
      paste0(treat_group, ' no ak'),
      treat_group
    )
  ) |>
  select(-drop_st, -n_treated)
did_ctrl_states <- readRDS(here::here(
  '../results/treat_ctrl_data/did_ctrl_states.rds'
)) |>
  select(-n_treated)
synth_ctrl_states <- readRDS(here::here(
  '../results/treat_ctrl_data/synth_ctrl_states.rds'
)) |>
  mutate(
    treat_group = ifelse(
      grepl('AK', drop_st),
      paste0(treat_group, ' no ak'),
      treat_group
    )
  ) |>
  select(-drop_st)

# Model results data
synth_res <- readRDS(here::here('../results/synth.rds')) |>
  filter(
    !(!grepl('aggregate', g) & is.na(year) | is.na(year) & is.na(e)),
    !(grepl('aggregate', g) & is.na(e) & year < 1990)
  ) |>
  mutate(g = ifelse(grepl('AK', drop_st), paste0(g, ' no ak'), g)) |>
  select(-drop_st, -se, -se_type)
did_res <- bind_rows(
  readRDS(here::here('../results/did.rds')),
  readRDS(here::here('../results/did_st.rds'))
) |>
  filter(!(is.na(year) & is.na(e))) |>
  mutate(g = ifelse(grepl('AK', drop_st), paste0(g, ' no ak'), g)) |>
  select(-drop_st, -se, -crit)

# Save
arrow::write_ipc_stream(
  synth_res, here::here('dashboard/data/synth_res.arrow')
)
arrow::write_ipc_stream(
  did_res, here::here('dashboard/data/did_res.arrow')
)
arrow::write_ipc_stream(
  synth_ctrl_means, here::here('dashboard/data/synth_ctrl_means.arrow')
)
arrow::write_ipc_stream(
  synth_ctrl_states, here::here('dashboard/data/synth_ctrl_states.arrow')
)
arrow::write_ipc_stream(
  did_ctrl_means, here::here('dashboard/data/did_ctrl_means.arrow')
)
arrow::write_ipc_stream(
  did_ctrl_states, here::here('dashboard/data/did_ctrl_states.arrow')
)
arrow::write_ipc_stream(
  treat_means, here::here('dashboard/data/treat_means.arrow')
)
arrow::write_ipc_stream(
  all_states, here::here('dashboard/data/treat-groups_all-data.arrow')
)
