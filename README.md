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
   - Data versioning: edit tables and save to named versions
   - Config management (save/load custom configurations)
   - JPEG export of all charts

2) Or run simulations from the command line:

   `Rscript R/run_sim.R --days 30 --matches_per_day 20 --bot_rate 0.15 --seed 42`

Testing

- Unit tests: `Rscript tests/run_tests.R`
- Full suite: `Rscript tests/run_all.R` (runs tests and, if `chromote` is installed, captures PNG page screenshots of the dashboard walkthrough under `runs/screenshots/`).

Setup

- Quick deps: `Rscript R/install_deps.R` installs required packages (shiny, bslib, ggplot2, dplyr, tidyr, purrr, readr, DT, stringr, jsonlite, digest). Optional: `chromote` and `processx` for the headless walkthrough.
- Reproducible env (recommended): `Rscript R/init_renv.R` to initialize renv and snapshot dependencies. On another machine, use `renv::restore()`.

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
- Defaults for Lucky Star loading live in `R/lucky_star.R`. The Data Editor reads/writes CSVs under `assets/research/.../tables` (see Data Versioning below).

Arenas

- Arena progression unlocks approximately every 400 trophies (default schedule in `R/sim_config.R`).
- Each arena can add new cards to the drop pool and optionally override rarity weights.
- Loot rolls use the current arena at the time of the win.
- Per-day output includes the ending arena name.

Outputs

- Per-day summary: matches, wins, win rate, crowns, boxes, gold (match + pass), gold spent on upgrades, upgrades applied, power bonus to win rate, pass levels (cumulative), trophy rating, ending arena, and rarity counts of box drops.

Progression & Power

- Card copies from boxes accumulate per card. When a card reaches the required copies for its next level, it becomes eligible to upgrade (subject to gold cost).
- Requirements are per rarity and level and shared across all cards of that rarity. Defaults live in `R/sim_config.R` (you can tune copies and gold costs).
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
- Regression factor decays with arena index via `mm_convergence_per_arena` (default 0.15), causing win rate to settle back near target at higher arenas. Configure in `R/sim_config.R`.

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
  - Save to a named Data Version. The app will read tables from that version for future simulations and caching.
- **Assumptions**: Configure all simulation parameters with validation
  - Save/load custom configurations
  - Real-time input validation warnings
  - Automatic caching of results

Extending the Model

- Replace crown distributions, gold values, or pass rewards in `R/sim_config.R`.
- Implement richer progression (e.g., deck power that increases with gold) and map it to `vertical_winrate_bonus` over time.
- Export CSV via `--csv_out results.csv` for deeper analysis.
- Use the Data Editor in the dashboard to customize Lucky Star rewards and upgrade costs.

Data Versioning

- Base tables live in `assets/research/gemini/tables`.
- Saving from the Data Editor creates or updates `assets/research/custom/<your-version>/tables/` and switches the active Data Version.
- The simulation cache key includes the selected Data Version to ensure correctness.

Data Versioning Walkthrough

Optionally include a short GIF of saving a custom version from the Data Editor at `assets/screenshots/data-versioning.gif`.

## Simulation Model Documentation

### Mathematical Model

The simulation uses a discrete-time Markov process to model player progression through a mobile game economy. Each day consists of multiple match outcomes that affect the player's state vector.

#### State Variables
- **S(t)** = {trophies, gold, power, cards, pass_level} at time t
- **T(t)** = Trophy count at day t
- **G(t)** = Gold balance at day t  
- **P(t)** = Power score at day t
- **C(t)** = Card inventory vector at day t
- **L(t)** = Pass level at day t

#### Win Probability Model

The win probability is calculated using a three-layer model:

1. **Base Layer**: Intrinsic win rate accounting for skill and bot matches
   ```
   W_base = (1 - bot_rate) × base_winrate + bot_rate × bot_winrate
   ```

2. **Power Layer**: Additive bonus from card upgrades
   ```
   W_power = W_base + min(power_slope × P(t), power_cap)
   ```

3. **Matchmaking Layer**: Arena-based convergence to equilibrium
   ```
   W_final = W_target + (W_power - W_target) × exp(-convergence × arena_index)
   ```

Where:
- `W_target = (1 - bot_rate) × 0.5 + bot_rate × bot_winrate`
- `arena_index = floor(T(t) / 400)`

#### Trophy Dynamics

Trophy changes follow an asymmetric random walk:
```
T(t+1) = T(t) + Σ[i=1 to M] { +30 if win_i, -29 if loss_i }
```

Expected daily trophy change:
```
E[ΔT] = M × (30 × W_final - 29 × (1 - W_final))
```

#### Economic Flow

Gold accumulation from matches and pass rewards:
```
G(t+1) = G(t) + G_matches(t) + G_pass(t) - G_upgrades(t)
```

Where:
- `G_matches(t) = Σ wins × uniform(gold_min, gold_max)`
- `G_pass(t) = floor(crowns(t) / 20) × gold_per_level`
- `G_upgrades(t)` = gold spent on card upgrades

#### Card Drop Distribution

Mystery box drops follow a hierarchical sampling process:

1. Sample rarity from trophy-dependent distribution
2. Sample specific card uniformly from rarity pool
3. Sample quantity from rarity-specific range

Rarity weights by trophy range:
- 0-1400: {C:70%, R:25%, E:4%, L:1%}
- 1400-2800: {C:60%, R:30%, E:8%, L:2%}
- 2800-4200: {C:50%, R:35%, E:12%, L:3%}
- 4200-5600: {C:40%, R:38%, E:17%, L:5%}
- 5600+: {C:30%, R:40%, E:22%, L:8%}

#### Upgrade System

Cards are upgraded using a greedy algorithm that maximizes power-per-gold ratio:

```
upgrade_value = power_gain / gold_cost
```

Upgrade priority ordering:
1. Highest value ratio
2. Rarity tier (legendary > epic > rare > common)
3. Alphabetical card name (for deterministic tie-breaking)

#### Power-to-Winrate Conversion

Power accumulation provides an additive win rate bonus:
```
winrate_bonus = min(power_slope × total_power, max_bonus)
```

Default parameters:
- `power_slope = 0.0005` (0.05% win rate per power point)
- `max_bonus = 0.20` (20% maximum win rate bonus)

### Performance Optimizations

The simulator includes several performance enhancements:

1. **Vectorized Operations**: Batch processing of matches and card drops
2. **File-based Caching**: Persistent cache using `cachem` package
3. **WebSocket Updates**: Real-time progress without blocking UI
4. **R6 Classes**: Efficient state management with reference semantics

### Architecture

The codebase follows a modular architecture:

```
R/
├── modules/           # Core simulation logic
│   ├── match_simulator.R
│   ├── progression_system.R
│   └── card_management.R
├── classes/          # R6 OOP classes
│   └── GameState.R
├── constants.R       # All magic numbers extracted
├── config_validator.R # Configuration validation
├── error_handler.R   # Centralized error handling
├── cache_manager.R   # File-based caching system
├── websocket_server.R # Real-time updates
└── vectorized_sim.R  # Performance optimizations
```

Notes

- This is a single-player expectation simulator (one player's run over the season). For population modeling, run many seeds and aggregate.
- All constants are defined in `R/constants.R` for easy tuning
- Configuration validation ensures parameter consistency
- Error handling provides informative debugging messages
