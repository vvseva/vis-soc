# Third Places & Urban Segregation

> **Agent-Based Modeling, Parameter Sweeps, and Public Exploranation**  
> *Vsevolod Suschevskiy & Benjamin Jarvis* — Linköping University  
> Presented at **CS2Nordics 2026**

[![NetLogo](https://img.shields.io/badge/NetLogo-7.0.4-orange.svg)](https://ccl.northwestern.edu/netlogo/)
[![R](https://img.shields.io/badge/R-4.4+-blue.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-Document-447099.svg)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

---

## Overview

This repository investigates how **everyday amenities and social infrastructure ("third places")** interact with residential choice to either alleviate or reinforce urban segregation. While classic Schelling models treat agents on an isotropic checkerboard considering only immediate residential neighbors, this project extends spatial agent-based models (ABMs) into a **layered spatial-network framework** that accounts for:

1. **Street Networks & Physical Barriers**: Connected road corridors and geographic barriers (e.g., rivers) that delineate interaction opportunities and line-of-sight.
2. **Third Places as Social Anchors**: Cafés, libraries, community hubs, and parks endowed with defined catchment radiuses, ethnic affiliations, and exclusivity policies.
3. **Multi-Attribute Utility**: Household satisfaction modeled as a weighted composite ($\beta_1 \dots \beta_6$) balancing residential neighborhood demographics, venue co-attendee homophily, and commuting dynamics.
4. **Dynamic & Emergent Interventions**: In-flight policy shocks including machine-learning-driven emergent venue placement (Voronoi relaxation / Lloyd's medoid clustering) and phased civic rollouts.
5. **Multi-Dimensional Segregation Measurement**: Tracking all major dimensions from Massey & Denton (1988)—*Evenness (Dissimilarity)*, *Exposure (Isolation)*, *Concentration (Delta)*, and *Clustering*.

---

## Repository Structure

```text
vis-soc/
├── README.md                                  <- Root project documentation
├── netlogo/
│   ├── roads/
│   │   ├── Segregation Experiments.nlogox     <- Experiment-optimized model (fully parameterized, high-speed)
│   │   ├── Segregation Interventions.nlogox   <- Interactive interventions model with UI visualizations
│   │   ├── Segregation Utility.nlogox         <- Baseline multi-attribute model
│   │   ├── Segregation roads.nlogox           <- Road network extension
│   │   └── *.shp, *.dbf                       <- Spatial shapefiles (Norrköping geographic data)
│   ├── segregation_experiments.qmd            <- Dedicated Quarto pipeline for Segregation Experiments.nlogox
│   ├── cs2nordic2.qmd                         <- Primary R analysis & BehaviorSpace experiment pipeline
│   ├── slidesCS2Nordic.qmd                    <- CS2Nordics 2026 Reveal.js slide deck
│   ├── slidesCS2Nordic.html                   <- Rendered conference presentation
│   ├── experiments.qmd                        <- Parameter sweeps and empirical benchmarks
│   ├── *.png, *.gif                           <- Publication plots and simulation animations
│   ├── results_experiments_2f.RData          <- Output data from Segregation Experiments factorial run
│   └── results_2f_experiment.RData            <- Output data from earlier factorial simulation runs
├── shiny/
│   └── logolink_shiny/
│       ├── app.R                              <- Interactive Shiny web interface for logolink
│       ├── create_experiment_template.R       <- Automated experiment generator
│       └── www/                               <- Web styling assets
├── peter/
│   └── stacked.qmd                            <- Secondary exploratory analysis
└── vis-soc.Rproj                              <- RStudio project root
```

---

## Simulation Models

### 1. `Segregation Experiments.nlogox` *(Recommended for Batch Sweeps & R)*

Located at [`netlogo/roads/Segregation Experiments.nlogox`](netlogo/roads/Segregation%20Experiments.nlogox).  
This model is specifically refactored for automated batch execution via `logolink` and BehaviorSpace:

- **First-Class Parameterization**: Fixed thresholds and intervention timings are exposed as clean model controls:
  - `pre-beta1-neighbors` (default `0.50`) & `pre-beta2-venue-group` (default `0.00`): Baseline residential homophily phase.
  - `post-beta1-neighbors` (default `1.00`) & `post-beta2-venue-group` (default `0.00`): Post-intervention target utility weights.
  - `intervention-tick` (default `400`): Exact step when policy shocks or emergent amenities activate.
  - `emergent-venue-capacity` (default `50`): Population mass per emergent venue ($K = \lceil N / \text{cap} \rceil$).
  - `relocation-fraction` (default `0.10`): Proportion of least-satisfied households relocating per tick ($10\%$).
  - `utility-noise-sd` (default `0.10`): Standard deviation of Gaussian utility perturbation.
  - `clustering-sample-size` (default `150`): Monte Carlo sample size for exponential spatial clustering.
  - `timeline-stage-interval` (default `100`): Spacing between staged timeline rollouts.
- **High-Performance Implementations**:
  - Replaced $O(N^2)$ distance loops in clustering metrics with an efficient Monte Carlo sample ($n = 150$), reducing evaluations by $>99\%$ with zero loss of statistical validity.
  - Replaced pairwise distance searches in Lloyd's medoid calculation with an $O(N)$ centroid center-of-mass medoid lookup.
  - Pre-cached static census tract centers (`tract-centers`) and barrier presence (`has-rivers?`) during setup.
- **Bug Fix**: Added `tracker-id` to `families-own` to prevent Voronoi tracker IDs from overwriting the patch `grid-id` macro district coordinates.
- **Zero-Dependency Headless Footprint**: Stripped unused NetLogo extensions (`gis`, `nw`, `dbscan`), removed unused link breeds, and decoupled interactive GUI mouse loops to guarantee rock-solid headless batch execution.

### 2. `Segregation Interventions.nlogox` *(Interactive GUI Model)*

Located at [`netlogo/roads/Segregation Interventions.nlogox`](netlogo/roads/Segregation%20Interventions.nlogox).  
Includes interactive GUI widgets, manual venue/road drawing tools, and spring-physics network visualization routines (`visualize-venue-network`, `visualize-road-network`).

---

## Model Mechanics

### 1. Conceptual Framework (Coleman's Boat)

```text
Macro Level:     [ Urban Amenity Policy & Infrastructure ] ───────> [ Macro Segregation Indices ]
                             │                                              ▲
                             ▼                                              │
Micro Level:     [ Local Spatial Opportunity Structure ] ───────> [ Household Relocation Choice ]
```

### 2. Multi-Attribute Utility Formulation

Every household $j$ computes utility as a normalized weighted score across 6 spatial dimensions, perturbed by stochastic noise $\epsilon \sim \mathcal{N}(0, \sigma^2)$:

$$\text{Utility}_j = \frac{\sum_{i=1}^{6} \beta_i \cdot \text{Similarity}_{i,j}}{\sum_{i=1}^{6} \beta_i} + \epsilon$$

| Weight | Parameter Name | Social / Spatial Dimension |
| :--- | :--- | :--- |
| $\beta_1$ | `beta1-neighbors` | Proportion of ingroup families within `neighborhood-distance` (with line-of-sight checks). |
| $\beta_2$ | `beta2-venue-group` | Ingroup homophily among co-attendees of visited venues. |
| $\beta_3$ | `beta3-venue-loc` | Demographic composition of residential areas surrounding visited venues. |
| $\beta_4$ | `beta4-home-venues` | Ingroup proportion of patrons visiting venues in the agent's home neighborhood. |
| $\beta_5$ | `beta5-park-group` | Demographic similarity among co-patrons of local green spaces. |
| $\beta_6$ | `beta6-roads-group` | Demographic similarity among commuters sharing traversed transit paths. |

### 3. Migration & Relocation

At each simulation tick:
- Households are sorted by total utility; the **lowest fraction** (`relocation-fraction`, default $10\%$) are marked unhappy.
- Unhappy agents evaluate vacant habitable patches within their reachable road network (via breadth-first search accounting for road-speed multipliers).
- When an agent moves, attendee rosters for venues, parks, and commuter links are updated incrementally ($O(1)$) to preserve computational efficiency.

### 4. Intervention Regimes

- **Emergent Venues (`emergent-venues`)**:
  - *Ticks 0–400*: Simulation establishes baseline residential clustering under `pre-beta1-neighbors` and `pre-beta2-venue-group`.
  - *Shock Tick*: Venues spawn directly inside emergent demographic clusters using **Lloyd's Voronoi Relaxation** (finding spatial medoids for each ethnic cluster of capacity 50). Simultaneously, utility switches to `post-beta1-neighbors` and `post-beta2-venue-group`.
- **Timeline Staging (`timeline-experiment`)**:
  - *Stage 1*: Introduces a single amenity at the city center.
  - *Stage 2*: Spawns an adjacent venue of the contrasting group (local paired integration).
  - *Stage 3*: Deploys 16 venues symmetrically across all 8 peripheral districts.
- **Dynamic Infrastructure (`dynamic-tick-150`)**:
  - Restructures transit grids (`build-grid` or `erase-random`) at tick 150, displacing families on newly paved corridors.

---

## Analysis & Quarto Pipelines

### 1. `netlogo/segregation_experiments.qmd` *(Primary Pipeline for New Model)*

Designed specifically for [`Segregation Experiments.nlogox`](netlogo/roads/Segregation%20Experiments.nlogox):
- **Experiment 1 (Beta Weight Transitions)**: Sweeps 5 post-intervention beta mixes under emergent venues, generating median trajectory plots with IQR ribbons (`experiments_beta_transition.png`) and terminal distribution boxplots at Tick 800.
- **Experiment 2 (Full Factorial Sweep)**: Crosses 5 utility weight mixes with 4 levels of venue exclusivity (`Strict 1.0`, `High 0.75`, `Low 0.25`, `Universal 0.0`), caches data to `results_experiments_2f.RData`, and renders multi-panel faceted trajectories (`experiments_exclusivity_relaxation.png`).

### 2. `netlogo/cs2nordic2.qmd` *(Original Analysis Pipeline)*

Runs parameter sweeps against [`Segregation Interventions.nlogox`](netlogo/roads/Segregation%20Interventions.nlogox):
- Investigates post-shock dissimilarity trajectories and generates publication figures `venues_location_relaxation.png` and `venues_exclusivity_relaxation.png`.

### Key Substantive Insights
1. **The Anchor Effect**: Shifting utility towards venue co-attendance ($\beta_2 \to 1.0$) causes residential dissimilarity to decline significantly, because shared amenities anchor households in mixed residential neighborhoods even when agents maintain homophilic preferences within the venues.
2. **Exclusivity Governs Spatial Patterns**: Highly exclusive third places lock agents into hyper-segregated enclaves surrounding mono-ethnic hubs. Relaxing exclusivity to universal/porous admission allows mixed residential settlement to emerge organically.

---

## Getting Started

### Prerequisites

- **NetLogo**: Version 7.0.4 or later.
- **R Environment**: R 4.4+ with the following packages:
  ```r
  install.packages(c("tidyverse", "patchwork", "janitor", "colorspace", "ggnewscale", "shiny", "bslib"))
  # Install logolink for NetLogo BehaviorSpace interfacing:
  remotes::install_github("bastistician/logolink")
  ```

### Running Simulations

1. **Headless Experiments in R (Recommended)**:
   - Open [`netlogo/segregation_experiments.qmd`](netlogo/segregation_experiments.qmd) in RStudio or VS Code.
   - Run chunks interactively or render the entire document to execute BehaviorSpace sweeps and regenerate all figures.
2. **Interactive GUI Exploration**:
   - Open NetLogo 7.0.4.
   - Load [`netlogo/roads/Segregation Experiments.nlogox`](netlogo/roads/Segregation%20Experiments.nlogox) or [`Segregation Interventions.nlogox`](netlogo/roads/Segregation%20Interventions.nlogox).
   - Adjust sliders, select an `intervention-scenario`, and click `setup` and `go`.
3. **Interactive Shiny Dashboard**:
   - Run the dedicated GUI application:
     ```r
     shiny::runApp("shiny/logolink_shiny")
     ```

---

## References & Citations

- **Massey, D. S., & Denton, N. A. (1988).** The dimensions of residential segregation. *Social Forces*, 67(2), 281-315.
- **Schelling, T. C. (1971).** Dynamic models of segregation. *Journal of Mathematical Sociology*, 1(2), 143-186.
- **Silver, D., & Adler, P. (2015).** The power of third places. *The Routledge Handbook of Planning for Health and Well-Being*.
- **Roberto, E. (2016).** The spatial proximity and connectivity method for measuring segregation. *Sociological Methodology*, 46(1), 182-224.
- **Coleman, J. S. (1990).** *Foundations of Social Theory*. Harvard University Press.

---

## Authors & Contact

- **Vsevolod Suschevskiy** — Linköping University ([vsevolod.suschevskiy@liu.se](mailto:vsevolod.suschevskiy@liu.se))
- **Benjamin Jarvis** — Linköping University
