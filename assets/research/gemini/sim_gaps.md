Simulation Gaps Checklist (populated from Gemini research)

Purpose: Track economy mechanics described in the research that our current sim does not model, or simplifies materially. Use this to prioritize future sim extensions (non-goal items can be kept here for awareness).

Core Progression
- Level caps and costs: Our rules stop around level 10–11 equivalents and use much lower costs. Research indicates levels 1–14 with large gold steps (up to 100k per upgrade) and a level 15 Elite tier with no gold cost. Action: Decide whether to scale our arrays to match research or keep as simplified placeholders. File: `sim/progression.py`.
- Champion rarity: Research lists Champion as a fifth rarity with unique constraints (one per deck). Not modeled. Action: Add rarity in rules and upgrade tables if needed; deck constraint likely out-of-scope for our sim.
- Elite Level (15) with EWCs: Not modeled. Endgame upgrade (14→15) costs 50,000 EWCs; gold is not used. Action: Consider adding an EWC currency and an endgame conversion loop; or note as non-goal but document impact.
- EWC conversion: After a card reaches L14, surplus duplicates and magic items of that rarity convert to EWCs at asymmetric rates (Common 1, Rare 5, Epic 20, Legendary 1500, Champion 4000). Not modeled. Action: Add post-cap conversion buckets if we introduce EWCs.
- Tower Troops: Distinct ninth slot unit with upgrades mirroring cards and L15 via EWCs; level capped by King Level. Not modeled. Action: Decide to treat as an additional “card” track with power contribution and King Level cap, or defer as non-goal.

Daily Rewards & Loot
- Lucky Drops (post-2025): Chests replaced by daily Lucky Drops on first three wins with upgrade‑per‑spin mechanics and final rarity tables. Our sim uses generic per-arena boxes with daily cap. Action: Replace or add a Lucky Drop rewards path for wins 1–3; keep simple gold-only for later wins to match research.
- Bonus Rewards gold-only after certain wins: Research suggests wins 4–10 grant gold only; our sim grants fixed gold on every win and pass gold on level-ups. Action: Adjust per-win reward curve to align (e.g., tiers for wins 1–3 vs 4–10); keep tunable in config.
- Magic Items & Wild Cards: Lucky Drops include Wild Cards and Magic Items; our sim has none. Action: Track as gaps; model only if needed for progression realism.

Macro Systems
- Seasonal reset/floors: Partial trophy reset (e.g., into 10–12k range) each season, gating access to Ranked. Not modeled. Action: Add end-of-season trophy reset if we simulate multiple seasons; currently we simulate a single season.
- Ranked/Seasonal resource distribution: EWCs/Gold redistributed between Seasonal Arenas and Ranked; season targets (e.g., 15k trophies). Not modeled. Action: Keep as context unless multi‑season simulation is added.
- Season shop/event rewards: Not modeled. Action: Track as gaps.

Account/King Progression
- King Level (XP) gating Tower Troops: We have account points and tower power; we don’t gate a separate troop system by King Level. Action: If adding Tower Troops, gate by `account_level` analog.
- Donations/Requests XP: Not modeled. Action: Track as gap.

Pass Progress
- Pass track structure: We model crowns per level and flat gold reward; research suggests concentrated distribution of progression items via Lucky Drops and refined pass structure. Action: Keep current simplification; note as gap for non-gold rewards.

Matchmaking and Trophies
- League floors and seasonal transitions: Not modeled. Our MM regression is simplified by arena index. Action: Track as gap.

Economy Balancing Considerations
- Endgame incentive to max Common cards for EWC generation: Not modeled. Action: If EWCs are added, include conversion logic to reflect this incentive.

Summary of proposed minimal updates (optional, non-binding)
- Replace “mystery boxes” with Lucky Drops for first 3 daily wins; use provided upgrade/final rarity tables (CSV parsed) to parameterize.
- Make wins 4–10 gold-only (configurable), to match the stated tiering.
- Keep EWCs, magic items, and Tower Troops as documented gaps for now; optionally stub EWC currency and conversion after max level to prepare for future.

