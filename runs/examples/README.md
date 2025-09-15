Example outputs for reference

This folder holds tiny, static example artifacts that demonstrate the expected
structure of simulation outputs. They are not used by the app or tests.

- `sample_daily.json`: Minimal daily results for 3 days, matching the shape of
  `simulate_season(... )$daily`.

To generate fresh outputs locally, run:

```
Rscript tests/run_all.R
```

Charts are written to `runs/screenshots/`.

