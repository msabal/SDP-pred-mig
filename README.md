# Risk and reward of shoreline habitats shape optimal salmon migration with implications for habitat restoration

This repository accompanies the paper: ADD CITATION

## Abstract
Risk-reward dynamics are critical drivers of animal migrations. However, it is often unknown how risk-reward dynamics affect fine-scale migration behavior or how that can be integrated into conservation and management. Juvenile salmon migrate along highly altered river shorelines where risk-reward dynamics may explain variation in pauses during this vulnerable life stage and inform the billion-dollar industry of habitat restoration. We used an optimality model simulating juvenile salmon fine-scale migration behavior to examine how risk-reward mechanisms may influence the proportion of pauses between natural and altered river shorelines. Next, we evaluated whether habitat restoration can increase salmon fitness even if natural habitats attract more predators and how the quantity of natural habitat affects the diversity of movement choices, growth, survival, and fitness.	We found that habitat-dependent risk dynamics (predator abundance and salmon escape ability) exhibited a stronger effect increasing the proportion of pauses in natural habitats compared to reward dynamics (foraging gain and energy refugia). However, it can still be optimal for salmon to pause at natural shorelines, despite 30% more predators, if sufficient  benefits among escape ability, foraging gain, and/or energy refugia are present (restoration quality). As the quantity of natural habitats increased, it was optimal for salmon to pause more during outmigration to take advantage of risk-reward benefits, which increased river growth, and survival rates, and the probability of returning as an adult (fitness). Synthesis and applications: Our model shows how risk- and reward mechanisms may contribute to a previously unexplained fine-scale pattern of salmon pausing more atmovement in natural vs. altered shorelines. Risk-reward tradeoffs were especially relevant for management implications.  Our results suggest that habitat restoration should not be avoided due to fear of increasing predator abundances but should focus on supporting diverse beneficial ecological mechanisms. The benefits of natural shorelines ultimately increased fitness despite lower cumulative migration survival to the ocean, which is a metric many consider to be maximized for salmon success.

## Usage Notes

Models, analyses, and figures were generated using R software - R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>

Some parameter symbols differ between the R script and the current manuscript draft.

| R Script Symbol | Manuscript Symbol | Description                      |
|-----------------|-------------------|----------------------------------|
| `A[]`; `A_max`  | `L`; `L_h`        | Location                         |
| `U`             | `B`               | Movement behavior                |
| `Bu`            | `delta_B`         | Mortality from movement behavior |
| `Bw`            | `rho`             | Mortality coef for salmon weight |
| `beta`          | `β`               | Stock-recruit density term       |


### 1. Manuscript_V1_All_Code.R

The [Manuscript_V1_All_Code.R](https://github.com/msabal/SDP-pred-mig/blob/main/scripts/New%20Organization%20May%202022/Manuscript_V1_All_Code.R) script contains all of the code to run stochastic dynamic programming (SDP) models and generate figures in the manuscript. This script will run SDP models, but output tracks from the main model scenarios can be found in the [Manuscript V1](https://github.com/msabal/SDP-pred-mig/tree/main/results/Manuscript%20V1) folder.
