# Assets Directory

This directory contains shared research data and configurations for the Clash Royale economy simulator.

## Structure

- `research/`: Research data and analysis
  - `gemini/tables/`: CSV files with game economy data (upgrade costs, drop rates, etc.)
  - `gemini/json/`: JSON versions of the research data
  
- `configs/`: Saved simulation configurations

## Research Data

The research CSVs under `research/gemini/tables/` contain extracted game economy data including:
- Card upgrade costs by rarity
- Tower troop upgrade costs
- Lucky Star drop probabilities and rewards
- Various economy parameters

## Notes

- Lucky Star system: First 3 wins/day produce Lucky Star rewards; later wins are gold-only
- Wild cards, books, and evolutions are excluded from the current implementation
- Cosmetic rewards (banners, emotes) are treated as no-ops in simulations