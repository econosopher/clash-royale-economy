library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Default visualization helpers (538-inspired styling without extra deps)
theme_538ish <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "panel",
      plot.title = element_text(hjust = 0, face = "bold", color = "#0f172a"),
      plot.subtitle = element_text(hjust = 0, color = "#475569"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#e2e8f0", linewidth = 0.2),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      axis.title = element_text(face = "bold", color = "#0f172a"),
      axis.text = element_text(color = "#334155"),
      legend.title = element_blank(),
      legend.key = element_blank()
    )
}

plot_trophies <- function(df) {
  # arena bands by contiguous spans
  spans <- df %>% mutate(arena = arena_end) %>%
    mutate(group = cumsum(c(1, diff(as.integer(factor(arena))) != 0))) %>%
    group_by(group, arena) %>% summarise(xmin = min(day)-0.5, xmax = max(day)+0.5, .groups = 'drop')
  
  # Add arena name annotations
  max_t <- max(df$trophies_end, na.rm = TRUE)
  min_t <- min(df$trophies_end, na.rm = TRUE)
  span <- max(1, max_t - min_t)
  label_level <- max_t - max(50, 0.05 * span)
  arena_labels <- spans %>%
    mutate(label_x = xmin + (xmax - xmin) / 2,
           label_y = label_level)

  ggplot(df, aes(day, trophies_end)) +
    {if(nrow(spans)>0) geom_rect(data=spans, inherit.aes = FALSE, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf), fill="#e2e8f0", alpha=0.16)} +
    geom_line(color = "#2563eb", linewidth = 1) +
    geom_hline(yintercept = seq(0, max(df$trophies_end, na.rm = TRUE) + 400, by = 400),
               linetype = "dotted", color = "#cbd5f5", linewidth = 0.25) +
    geom_text(data = arena_labels, aes(x = label_x, y = label_y, label = arena),
              size = 3, color = "#475569", fontface = "bold", check_overlap = TRUE) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
    labs(title = "Trophies by Day", x = "Day", y = "Trophies", subtitle = "End-of-day trophy totals") +
    theme_538ish(12)
}

plot_gold_flow <- function(df) {
  dd <- df %>% select(day, matches=gold_from_matches, pass=gold_from_pass) %>%
    pivot_longer(cols = c(matches, pass), names_to = 'src', values_to = 'gold')
  ggplot(dd, aes(day, gold, fill = src)) +
    geom_col(position = 'fill') +
    scale_fill_manual(values = c(matches = "#2563eb", pass = "#7c3aed"), labels = c(matches = "Match Rewards", pass = "Pass Rewards")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Daily Gold Mix", subtitle = "Share of income by source", x = "Day", y = "Share of Gold") +
    guides(fill = guide_legend(title = NULL)) +
    theme_538ish(12)
}

plot_wallet_balance <- function(df) {
  dd <- df %>% mutate(wallet = cumsum(gold_total - gold_spent_upgrades))
  ggplot(dd, aes(day, wallet)) +
    geom_line(color = "#0f172a", linewidth = 1) +
    geom_point(color = "#0f172a", size = 1.5) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Wallet Balance", subtitle = "Cumulative gold after spend", x = "Day", y = "Gold") +
    theme_538ish(12)
}

plot_winrate <- function(df) {
  ggplot(df, aes(day, winrate*100)) +
    geom_line(color = "#10b981", linewidth = 1) +
    geom_hline(yintercept = 50, linetype = "dotted", color = "#cbd5f5") +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1), limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Win Rate Over Time", subtitle = "Daily mean win percentage", x = "Day", y = "Win Rate") +
    theme_538ish(12)
}

plot_box_rarity_mix <- function(df) {
  cols <- c('boxes_common','boxes_rare','boxes_epic','boxes_legendary')
  if (!all(cols %in% names(df))) return(NULL)
  base <- df %>% select(day, common = boxes_common, rare = boxes_rare, epic = boxes_epic, legendary = boxes_legendary)
  if (nrow(base) == 0) return(NULL)
  rr <- base |>
    mutate(total = pmax(1, common + rare + epic + legendary)) |>
    mutate(across(c(common, rare, epic, legendary), ~ .x/total*100)) |>
    select(day, common, rare, epic, legendary) |>
    pivot_longer(-day, names_to = "rarity", values_to = "share")
  ggplot(rr, aes(day, share, fill = rarity)) +
    geom_area(position = "stack") +
    scale_fill_manual(values = c(common="#94a3b8", rare="#2563eb", epic="#7c3aed", legendary="#f59e0b")) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1, scale = 1)) +
    labs(title = "Mystery Box Rarity Mix", subtitle = "Share of boxes earned per rarity", x = "Day", y = "Share of Daily Boxes") +
    theme_538ish(12)
}

# Research plots
upgrade_tables <- function() {
  base <- getOption("cr_econ_tables_base", default = normalizePath(file.path("assets","research","gemini"), mustWork = FALSE))
  list(
    Common = file.path(base, "tables", "card_upgrade_costs_common.csv"),
    Rare = file.path(base, "tables", "card_upgrade_costs_rare.csv"),
    Epic = file.path(base, "tables", "card_upgrade_costs_epic.csv"),
    Legendary = file.path(base, "tables", "card_upgrade_costs_legendary.csv"),
    Champion = file.path(base, "tables", "card_upgrade_costs_champion.csv")
  )
}

.to_int <- function(x) as.integer(readr::parse_number(as.character(x)))
.to_level <- function(s) {
  s <- as.character(s); if (grepl("->", s)) as.integer(sub(".*->", "", s)) else if (grepl("-\\>", s)) as.integer(sub(".*-\\>", "", s)) else NA_integer_
}

plot_research_upgrades_cum <- function() {
  tabs <- upgrade_tables()
  all <- purrr::imap_dfr(tabs, function(path, nm){
    if (!file.exists(path)) return(NULL)
    df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
    df %>% mutate(gold_cost = .to_int(`Gold Cost`), to_level = .to_level(Level), rarity = nm) %>%
      mutate(cum_gold_derived = cumsum(replace_na(gold_cost, 0)))
  })
  ggplot(all, aes(to_level, cum_gold_derived, color = rarity)) +
    geom_line() + geom_point(size = 1) +
    scale_color_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b", Champion="#10b981")) +
    labs(title = "Card Upgrade Cumulative Gold (Derived)", x = "Target Level", y = "Cumulative Gold", color = "Rarity") +
    theme_538ish(12)
}

plot_research_lucky_final <- function() {
  p <- file.path("assets","research","gemini","tables","table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv")
  if (!file.exists(p)) return(NULL)
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE)) |>
    pivot_longer(cols = ends_with("Chance"), names_to = "star", values_to = "pct") |>
    mutate(pct = readr::parse_number(pct))
  ggplot(df, aes(star, pct, fill = `Reward Type`)) +
    geom_col(position = "stack") +
    labs(title = "Lucky Star Final Reward Distribution by Star Tier (Arena 16+)", x = "Final Rarity (Star Tier)", y = "%") +
    theme_minimal(base_size = 12)
}

# Effective post-filter reward shares (sim-used)
plot_research_lucky_final_effective <- function() {
  p <- file.path("assets","research","gemini","tables","table_2_2_lucky_drop_final_reward_probabilities_by_final_rarity_arena_16.csv")
  if (!file.exists(p)) return(NULL)
  allowed <- c("Gold","Random Cards","Banners","Emotes")
  df <- suppressWarnings(readr::read_csv(p, show_col_types = FALSE)) |>
    pivot_longer(cols = ends_with("Chance"), names_to = "star", values_to = "pct") |>
    mutate(pct = readr::parse_number(pct)) |>
    filter(`Reward Type` %in% allowed) |>
    group_by(star) |>
    mutate(pct = pct/sum(pct)) |>
    ungroup()
  ggplot(df, aes(star, pct*100, fill = `Reward Type`)) +
    geom_col(position = "stack") +
    labs(title = "Lucky Star Final Reward Distribution (Effective, Post-filter)", x = "Final Rarity (Star Tier)", y = "%") +
    theme_minimal(base_size = 12)
}

plot_research_lucky_star <- function() {
  cfg <- default_config()
  stars <- simulate_lucky_star_stars(cfg, trials = 10000)
  df <- bind_rows(lapply(names(stars), function(w){
    tibble::tibble(win = paste0("Win ", w), star = paste0(1:5, "-Star"), pct = stars[[w]])
  }))
  ggplot(df, aes(star, pct, fill = win)) +
    geom_col(position = "dodge") +
    labs(title = "Lucky Star: Final Star Distribution by Daily Win (Simulated)", x = "Final Star Tier", y = "%") +
    theme_minimal(base_size = 12)
}

plot_pass_progress <- function(df) {
  max_c <- max(df$pass_crowns_total, na.rm = TRUE)
  ticks <- tibble::tibble(y = pretty(c(0, max_c), n = 6))
  ggplot(df, aes(day, pass_crowns_total)) +
    geom_line(color = "#0ea5e9") +
    geom_hline(data = ticks, aes(yintercept = y), linetype = "dashed", color = "#e5e7eb", linewidth = 0.3) +
    labs(title = "Pass Progress (Crowns)", x = "Day", y = "Crowns (cumulative)") +
    theme_538ish(12)
}

plot_trophies_drift <- function(df) {
  dd <- df %>% mutate(delta = c(0, diff(trophies_end)))
  ma <- stats::filter(dd$delta, rep(1/3, 3), sides = 1)
  dd$ma3 <- as.numeric(ma)
  ggplot(dd, aes(day, delta)) +
    geom_col(fill = "#f59e0b") +
    geom_line(aes(y = ma3), color = "#ef4444", linewidth = 1, na.rm = TRUE) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "#94a3b8") +
    labs(title = "Trophies Drift (per day)", x = "Day", y = "Δ Trophies") +
    theme_538ish(12)
}

plot_deck_levels <- function(df) {
  if (!all(c('avg_level_common','avg_level_rare','avg_level_epic','avg_level_legendary') %in% names(df))) return(NULL)
  dd <- df %>% select(day, common = avg_level_common, rare = avg_level_rare, epic = avg_level_epic, legendary = avg_level_legendary, overall = avg_level)
  long <- dd %>% pivot_longer(cols = c(common, rare, epic, legendary), names_to = 'rarity', values_to = 'avg_level_r')
  ggplot() +
    geom_line(data = long, aes(day, avg_level_r, color = rarity), linewidth = 0.9) +
    geom_line(data = dd, aes(day, overall, color = 'overall'), linewidth = 1.1, linetype = 'dashed') +
    scale_color_manual(values = c(common="#94a3b8", rare="#2563eb", epic="#7c3aed", legendary="#f59e0b", overall="#111827"), name = "Rarity") +
    labs(title = "Average Card Level by Rarity (with Overall)", x = "Day", y = "Avg Level") +
    theme_538ish(12)
}

plot_upgrade_efficiency_daily <- function(df) {
  dd <- df %>% mutate(eff = ifelse(gold_spent_upgrades > 0, power_gained / (gold_spent_upgrades/1000), NA_real_))
  valid <- dd %>% filter(!is.na(eff))
  if (nrow(valid) == 0) return(NULL)
  ggplot(valid, aes(day, eff)) +
    geom_line(color = "#16a34a") +
    geom_point(color = "#16a34a", size = 1.8) +
    labs(title = "Upgrade Efficiency (Daily Power per 1k Gold)", x = "Day", y = "Power / 1k Gold") +
    theme_538ish(12)
}

plot_upgrade_efficiency_cum <- function(df) {
  dd <- df %>% mutate(cum_power = cumsum(power_gained), cum_gold = cumsum(gold_spent_upgrades), eff = ifelse(cum_gold > 0, cum_power / (cum_gold/1000), NA_real_))
  valid <- dd %>% filter(!is.na(eff))
  if (nrow(valid) == 0) return(NULL)
  ggplot(valid, aes(day, eff)) +
    geom_line(color = "#065f46") +
    labs(title = "Upgrade Efficiency (Cumulative Power per 1k Gold)", x = "Day", y = "Power / 1k Gold") +
    theme_538ish(12)
}

plot_spend_by_rarity_range <- function(df) {
  cols <- c('gold_spent_common','gold_spent_rare','gold_spent_epic','gold_spent_legendary')
  if (!all(cols %in% names(df))) return(NULL)
  totals <- df %>% summarise(dplyr::across(all_of(cols), ~ sum(.x, na.rm = TRUE)))
  totals_vec <- as.numeric(totals[1, ])
  if (sum(totals_vec, na.rm = TRUE) == 0) return(NULL)
  rarities <- c('Common','Rare','Epic','Legendary')
  tt <- tibble::tibble(
    rarity = factor(rarities, levels = rarities),
    gold_spent = totals_vec
  )
  ggplot(tt, aes(rarity, gold_spent, fill = rarity)) +
    geom_col() +
    scale_fill_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b"), guide = 'none') +
    labs(title = "Gold Spent by Rarity (Selected Range)", x = "Rarity", y = "Gold") +
    theme_538ish(12)
}

plot_power_by_rarity_range <- function(df) {
  cols <- c('power_gained_common','power_gained_rare','power_gained_epic','power_gained_legendary')
  if (!all(cols %in% names(df))) return(NULL)
  totals <- df %>% summarise(dplyr::across(all_of(cols), ~ sum(.x, na.rm = TRUE)))
  totals_vec <- as.numeric(totals[1, ])
  if (sum(totals_vec, na.rm = TRUE) == 0) return(NULL)
  rarities <- c('Common','Rare','Epic','Legendary')
  tt <- tibble::tibble(
    rarity = factor(rarities, levels = rarities),
    power_gained = totals_vec
  )
  ggplot(tt, aes(rarity, power_gained, fill = rarity)) +
    geom_col() +
    scale_fill_manual(values = c(Common="#94a3b8", Rare="#2563eb", Epic="#7c3aed", Legendary="#f59e0b"), guide = 'none') +
    labs(title = "Power Gained by Rarity (Selected Range)", x = "Rarity", y = "Power") +
    theme_538ish(12)
}
