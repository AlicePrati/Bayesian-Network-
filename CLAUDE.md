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
- `figure_15_12_replica.pdf` and `figure_15_12_replica.png` — visual replica of Figure 15.12

## Architecture

All logic lives in `BN.R`, structured in three sequential parts:

**Part 1 — Bayesian Network (`gRain`)**  
Eight CPTs are defined and compiled into a `grain` object. The key dependency structure is:
- `cause_A` → `cause_B` (sibling dependence: SIDS risk in B is conditioned on A's cause)
- `cause_A`, `cause_B` → `findings` (3-state aggregation node: Neither/Either/Both murdered)
- `findings` → `clark_guilty`
- `cause_A` → `bruising_A`, `disease_A` (and symmetric for B)

**Part 2 — Table 15.2**  
Sequential calls to `setEvidence()` accumulate observations one at a time (bruising_A → disease_A → bruising_B → disease_B), querying `clark_guilty["Yes"]` after each step.

**Part 3 — Figure 15.12**  
Pure `ggplot2` drawing. Node positions, widths, and heights are defined in the `node_info` data frame (coordinates in a 0–26.5 × 1.2–17.2 canvas). `build_bars()` computes bar geometry from prior probabilities queried live from the network — no hardcoded values.

## Code comments (thesis context)

Comments in `BN.R` should explain the **why**, not the what. Key spots that warrant a comment:
- Above `p_bruising`/`p_disease`: source is Fenton & Neil Table 15.1; values are symmetric across children by design
- Above `cptable(~cause_B | cause_A, ...)`: sibling dependence — family history of SIDS raises prior for second child
- Above `cptable(~findings | ...)`: deterministic node, fully determined by cause combination
- Above `prior <- querygrain(...)`: unconditional (prior) query, no evidence set
- Above the `setEvidence` chain: cumulative evidence updates replicating Table 15.2
- Above `node_info`: canvas coordinate system (x: 0–26.5, y: 1.2–17.2); cx/cy are centres, W/H are half-dimensions
- Above `border_pt`: geometric helper so arrows connect at node edges, not centres

## Reference values (Fenton & Neil 2018, Table 15.2)

| Evidence added | Expected P(guilty=Yes) |
|---|---|
| None | 7.89% |
| Child A bruising True | 28.87% |
| Child A signs of disease False | 30.93% |
| Child B bruising True | 69.13% |
| Child B signs of disease False | 70.19% |
