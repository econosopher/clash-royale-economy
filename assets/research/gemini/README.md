# Gemini Deep Research — Ingest + Review

## Research Data

This directory contains extracted game economy data from Clash Royale research.

## Structure

- `raw.txt` - Original research text blob
- `tables/` - CSV files with extracted economy data:
  - Card upgrade costs by rarity (common, rare, epic, legendary, champion)
  - Tower troop upgrade costs
  - Lucky Star drop probabilities and rewards
- `sim_gaps.md` - Analysis of mechanics not yet implemented in the simulator

## Data Files

The CSV tables contain critical game economy parameters used by the R simulator, including:
- Gold costs for card upgrades at each level
- Card copy requirements for upgrades
- Lucky Star spin probabilities and final reward distributions
- Tower troop progression costs

## Notes

- Wild cards, shop offers, and chest cycles are documented in `sim_gaps.md` but not yet implemented
- The data focuses on core progression mechanics (cards, gold, trophies) rather than cosmetic systems