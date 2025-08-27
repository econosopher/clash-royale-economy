Clash Royale–style Economy Simulator

Overview

This is a lightweight, configurable R simulation for a mobile squad RPG economy inspired by Clash Royale.

The simulation models:
- Daily matches played per player
- Win/loss outcomes with bot rate and vertical progression modifiers
- Crowns gained per match (feeding Pass progression)
- Pass Royale levels (1 level per 20 crowns) with configurable rewards
- Daily win rewards: mystery boxes (up to 4/day) and gold per-win (no daily cap)
- Trophy rating changes: +30 on win, −29 on loss

Quick Start

1) Run the interactive Shiny dashboard:

   `Rscript app.R`

   Then open http://127.0.0.1:7789/ in your browser.

   The dashboard provides:
   - Visual configuration of all economy parameters
   - Real-time simulation with caching for performance
   - Multiple visualization tabs (daily metrics, charts, comparisons)
   - Data editor for Lucky Star rewards and upgrade costs
   - Config management (save/load custom configurations)
   - PNG export of all charts

2) Or run simulations from the command line:

   `Rscript R/run_sim.R --days 30 --matches_per_day 20 --bot_rate 0.15 --seed 42`

What’s Modeled (Defaults)

- Matches/day: fixed integer (default 15). Optional jitter can be enabled.
- Base win rate: 50%. Bot rate pushes win rate toward `bot_win_rate` (default 90%).
- Vertical progression: an additive win-rate bonus (default 0.0), e.g. 0.05 = +5%.
- Crowns per match:
  - On win: P(3 crowns)=0.30, P(2)=0.50, P(1)=0.20
  - On loss: P(2 crowns)=0.20, P(1)=0.40, P(0)=0.40
- Trophy changes: +30 on win, −29 on loss.
- Mystery boxes: 1 per win up to 4/day.
- Gold: per-win drop (default fixed 10). You can set a range via `--gold_min_amount` and `--gold_max_amount`.
- Pass Royale: 1 level per 20 crowns. Default reward per level: 50 gold.

Mystery Boxes & Lucky Star

- Up to 4 mystery boxes per day on wins.
- Each box roll picks a rarity by weight (defaults: C 70%, R 25%, E 4%, L 1%), then a card of that rarity, then quantity by rarity range.
- Lucky Star feature: Star-based reward system with upgrade probabilities (configurable via Data Editor in dashboard).
- Defaults are in `R/sim/loot.R` (rarity weights, card lists, quantity ranges). Customize to match your game.

Arenas

- Arena progression unlocks approximately every 400 trophies (configurable schedule in `R/sim/arena.R`).
- Each arena can add new cards to the drop pool and optionally override rarity weights.
- Loot rolls use the current arena at the time of the win.
- Per-day output includes the ending arena name.

Outputs

- Per-day summary: matches, wins, win rate, crowns, boxes, gold (match + pass), gold spent on upgrades, upgrades applied, power bonus to win rate, pass levels (cumulative), trophy rating, ending arena, and rarity counts of box drops.

Progression & Power

- Card copies from boxes accumulate per card. When a card reaches the required copies for its next level, it becomes eligible to upgrade (subject to gold cost).
- Requirements are per rarity and level and shared across all cards of that rarity. Defaults live in `R/sim/progression.R` (you can tune copies and gold costs).
- On upgrade, a card grants a rarity-specific power increment. Total power maps to an additive win-rate bonus via a configurable function (default linear with cap).
- Upgrades are applied greedily immediately after each match (using any gold from the match and any Pass rewards achieved that match). This impacts win rate for subsequent matches the same day.

Account Levels & Tower

- Each upgrade grants account points based on rarity and level step. Thresholds produce account level-ups.
- On account level-up:
  - Awards a Level-Up Chest (separate drop table; see `R/sim/config.R` → `levelup_chest_config`).
  - Increases King Tower level 1:1, which adds tower power to total power.
- Card drops from the Level-Up Chest can immediately trigger more upgrades (and more account points), allowing cascades within the same match.

Matchmaking Equilibrium

- Effective win rate blends base skill/bots, adds power bonus, then regresses toward a target win rate as the player climbs arenas.
- Target = `(1 - bot_rate) * 50% + bot_rate * bot_winrate`.
- Regression factor decays with arena index via `mm_convergence_per_arena` (default 0.15), causing win rate to settle back near target at higher arenas.

Match Tracing

- Print per-match evolution for a specific day using `--trace_day N` (e.g., `--trace_day 1`).
- Trace shows pre-match state (arena, trophies, gold, power bonus) and post-match results (win/loss, crowns, gold gained, pass gold gained, box drop, gold spent, upgrades, new power bonus).
- Season summary totals and averages.

Dashboard Features

The Shiny dashboard (`http://127.0.0.1:7789/`) includes:

- **Daily Table**: View per-day simulation metrics with upgrade efficiency summary
- **Visualizations**: 10+ interactive charts including:
  - Trophy progression and drift analysis
  - Gold flow (matches vs pass rewards)
  - Win rate evolution with power bonus
  - Box rarity distribution over time
  - Upgrade spending and power by rarity
  - Pass Royale progression
- **Compare**: Run multiple configurations side-by-side for A/B testing
- **Data Editor**: Modify Lucky Star tables and upgrade costs per rarity
- **Assumptions**: Configure all simulation parameters with validation
  - Save/load custom configurations
  - Real-time input validation warnings
  - Automatic caching of results

Extending the Model

- Replace crown distributions, gold values, or pass rewards in `R/sim/config.R`.
- Implement richer progression (e.g., deck power that increases with gold) and map it to `vertical_winrate_bonus` over time.
- Export CSV via `--csv_out results.csv` for deeper analysis.
- Use the Data Editor in the dashboard to customize Lucky Star rewards and upgrade costs.

Notes

- This is a single-player expectation simulator (one player’s run over the season). For population modeling, run many seeds and aggregate.
