# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Replica of the Sally Clark Bayesian Network from Fenton & Neil (2018), *Risk: Uncertainty and Decision-making* — specifically Figure 15.12 and Table 15.2 (Chapter 15).

## Dependencies

```r
install.packages(c("gRain", "ggplot2"))
```

## Running the analysis

```r
source("BN.R")
```

On Windows, if `Rscript` is not in PATH, use the full path:

```powershell
& "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" "BN.R"
```

This produces:
- Console output replicating **Table 15.2** (sequential evidence updates and their effect on `clark_guilty`)
- `figure_15_12_replication.pdf` and `figure_15_12_replication.png` — visual replica of Figure 15.12

## Architecture

All logic lives in `BN.R`, structured in three sequential parts:

**Part 1 — Bayesian Network (`gRain`)**  
Eight named CPTs are compiled into a `grain` object called `bn`. The key dependency structure is:
- `cause_A` → `cause_B` (sibling dependence: SIDS risk in B is conditioned on A's cause)
- `cause_A`, `cause_B` → `findings` (deterministic 3-state node: Neither/Either/Both murdered)
- `findings` → `clark_guilty`
- `cause_A` → `bruising_A`, `disease_A` (and symmetric for B, sharing `prob_bruising`/`prob_disease`)

`prior` holds the unconditional marginals queried from `bn` before any evidence is set.

**Part 2 — Table 15.2**  
Sequential `setEvidence()` calls on `bn` accumulate trial observations (`bn_1` → `bn_2` → `bn_3` → `bn_4`), querying `clark_guilty["Yes"]` after each step.

**Part 3 — Figure 15.12**  
Pure `ggplot2` drawing. `node_info` defines node positions on a 0–26.5 × 1.2–17.2 canvas (`cx`/`cy` = centre, `W`/`H` = half-dimensions). `build_bars()` converts prior probabilities into bar coordinates. `border_point()` computes arrow endpoints at node edges. All probability values come from `prior` — nothing is hardcoded.

## Reference values (Fenton & Neil 2018, Table 15.2)

| Evidence added | Expected P(guilty=Yes) |
|---|---|
| None | 7.89% |
| Child A bruising True | 28.87% |
| Child A signs of disease False | 30.93% |
| Child B bruising True | 69.13% |
| Child B signs of disease False | 70.19% |
