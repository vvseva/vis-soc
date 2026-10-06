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
│   │   ├── Segregation Interventions.nlogox   <- Flagship NetLogo model (latest)
│   │   ├── Segregation Utility.nlogox         <- Baseline multi-attribute model
│   │   ├── Segregation roads.nlogox           <- Road network extension
│   │   └── *.shp, *.dbf                       <- Spatial shapefiles (Norrköping geographic data)
│   ├── cs2nordic2.qmd                         <- Primary R analysis & BehaviorSpace experiment pipeline
│   ├── slidesCS2Nordic.qmd                    <- CS2Nordics 2026 Reveal.js slide deck
│   ├── slidesCS2Nordic.html                   <- Rendered conference presentation
│   ├── experiments.qmd                        <- Parameter sweeps and empirical benchmarks
│   ├── *.png, *.gif                           <- Publication plots and simulation animations
│   └── results_2f_experiment.RData            <- Cached factorial simulation outputs
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

## Core Model: `Segregation Interventions.nlogox`

The latest simulation model is located at [`netlogo/roads/Segregation Interventions.nlogox`](netlogo/roads/Segregation%20Interventions.nlogox).

### 1. Conceptual Framework (Coleman's Boat)

```text
Macro Level:     [ Urban Amenity Policy & Infrastructure ] ───────> [ Macro Segregation Indices ]
                             │                                              ▲
                             ▼                                              │
Micro Level:     [ Local Spatial Opportunity Structure ] ───────> [ Household Relocation Choice ]
```

### 2. Multi-Attribute Utility Formulation

Every household $j$ computes utility as a normalized weighted score across 6 spatial dimensions, perturbed by stochastic noise $\epsilon \sim \mathcal{N}(0, 0.1^2)$:

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
- Households are sorted by total utility; the **bottom 10% least satisfied** are marked unhappy.
- Unhappy agents evaluate vacant habitable patches within their reachable road network (via breadth-first search accounting for road-speed multipliers).
- When an agent moves, attendee rosters for venues, parks, and commuter links are updated incrementally ($O(1)$) to preserve computational efficiency.

### 4. Intervention Regimes

- **Emergent Venues (`emergent-venues`)**:
  - *Ticks 0–400*: Simulation establishes baseline residential clustering ($\beta_1 = 0.5, \beta_2 = 0$).
  - *Tick 400 Shock*: Venues are spawned directly inside emergent demographic clusters using **Lloyd's Voronoi Relaxation** (5 iterations finding spatial medoids for each ethnic cluster of capacity 50). Simultaneously, utility switches to the target experimental weights (`target-beta1`, `target-beta2`).
- **Timeline Staging (`timeline-experiment`)**:
  - *Tick 400*: Introduces a single amenity at the city center.
  - *Tick 500*: Spawns an adjacent venue of the contrasting group (local paired integration).
  - *Tick 600*: Deploys 16 venues symmetrically across all 8 peripheral districts.
- **Dynamic Infrastructure (`dynamic-tick-150`)**:
  - Restructures transit grids (`build-grid` or `erase-random`) at tick 150, displacing families on newly paved corridors.

---

## Analysis & Experiments: `cs2nordic2.qmd`

The Quarto document [`netlogo/cs2nordic2.qmd`](netlogo/cs2nordic2.qmd) runs headless experiments using the R package [`logolink`](https://github.com/) to communicate directly with NetLogo BehaviorSpace.

### Experiment 1: Beta Weight Transition
Evaluates what happens when agents transition from purely residential sorting to venue affiliation at Tick 400:
- **Condition 1**: Only Neighbors ($\beta_1 = 1.0, \beta_2 = 0.0$)
- **Condition 2**: High Neighbors ($\beta_1 = 0.75, \beta_2 = 0.25$)
- **Condition 3**: Balanced ($\beta_1 = 0.50, \beta_2 = 0.50$)
- **Condition 4**: High Venues ($\beta_1 = 0.25, \beta_2 = 0.75$)
- **Condition 5**: Only Venues ($\beta_1 = 0.00, \beta_2 = 1.00$)
- **Outputs**: Trajectory plots with median & IQR ribbons (`venues_location_relaxation.png`) and terminal distribution boxplots at Tick 800.

### Experiment 2: Exclusivity $\times$ Utility Sweep ($5 \times 4$ Factorial)
Crosses the 5 beta weight pairs with 4 levels of venue exclusivity:
- **Strict (1.0)**: Strict in-group entry only.
- **High (0.75)** / **Low (0.25)**: Probabilistic admittance governed by household adventurousness.
- **Universal (0.0)**: Open access to all groups.
- **Outputs**: Comprehensive multi-panel trajectory grid across Dissimilarity, Clustering, and Exposure (`venues_exclusivity_relaxation.png`).

### Key Substantive Insights
1. **The Anchor Effect**: Shifting utility towards venue co-attendance ($\beta_2 \to 1.0$) causes residential dissimilarity to decline significantly, because shared amenities anchor households in mixed residential neighborhoods even when agents maintain homophilic preferences within the venues.
2. **Exclusivity Governs Spatial Patterns**: Highly exclusive third places lock agents into hyper-segregated enclaves surrounding mono-ethnic hubs. Relaxing exclusivity to universal/porous admission allows mixed residential settlement to emerge organically.

---

## Getting Started

### Prerequisites

- **NetLogo**: Version 7.0.4 or later (ensure NetLogo extensions `gis`, `nw`, and `dbscan` are installed or present in your NetLogo extensions directory).
- **R Environment**: R 4.4+ with the following packages:
  ```r
  install.packages(c("tidyverse", "patchwork", "janitor", "colorspace", "ggnewscale", "shiny", "bslib"))
  # Install logolink for NetLogo BehaviorSpace interfacing:
  remotes::install_github("bastistician/logolink")
  ```

### Running the Model

1. **Interactive GUI Exploration**:
   - Open NetLogo 7.0.4.
   - Load [`netlogo/roads/Segregation Interventions.nlogox`](netlogo/roads/Segregation%20Interventions.nlogox).
   - Select an `intervention-scenario` (e.g. `emergent-venues` or `timeline-experiment`).
   - Click `setup` and `go`.
2. **Headless Experiments in R**:
   - Open [`netlogo/cs2nordic2.qmd`](netlogo/cs2nordic2.qmd) in RStudio or VS Code.
   - Adjust `model_path` to point to your local `.nlogox` file.
   - Render the Quarto document or execute chunks sequentially to trigger BehaviorSpace runs and generate updated figures.
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
