# Habitat restoration shapes risk-reward tradeoffs, movement behavior, and fitness in migrating juvenile salmon 

This repository accompanies the paper: **ADD CITATION**

## Abstract
1)	Habitat restoration can often fail to benefit target species. This failure can result from a lack of understanding about how animals use restored sites with novel risk-reward tradeoffs and connections between behavior and fitness.

2)	We developed a dynamic state variable model, which predicts juvenile salmon outmigration behavior as a function of habitat-dependent ecological factors, physiology, and fitness. We used the model to explore how risk-reward tradeoffs between natural and altered shorelines shape optimal salmon outmigration. Specifically, we asked: which ecological factors most influence the frequency of pauses, what happens if predators are also attracted to restored sites, and how do fitness benefits relate to the quantity of restored habitat?

3)	We found that risk-related factors (predator abundance, salmon vulnerability) had stronger independent influences on salmon pausing at restored sites compared to reward factors (foraging gain, energy refugia). However, it often remained optimal for salmon to pause at natural shorelines due to these other ecological benefits despite added predation risk (15% higher predator abundance). As the quantity of natural habitats increased, salmon shifted from a “go fast” to “go slow” outmigration strategy to maximize fitness.

4)	*Synthesis and applications*: A behavioral perspective can inform habitat restoration design for single species management. Our model suggests that restoration efforts can provide fitness benefits that outweigh increased risk if predators are also attracted to restored habitats. Our model suggests that more natural habitats benefit salmon fitness and may increase diversity in outmigration behaviors.


## Usage Notes

Models, analyses, and figures were generated using R statistical programming language - R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>

### 1. Some parameter symbols differ between the R script and the current manuscript draft.

| R Script Symbol | Manuscript Symbol | Description                      |
|-----------------|-------------------|----------------------------------|
| `A[]`; `A_max`  | L; L<sub>h</sub>     | Location                         |
| `U`             | B                    | Movement behavior                |
| `Bu`            | &delta;<sub>B</sub>  | Mortality contribution from movement behavior |
| `Bw`            | &rho;                | Mortality coefficient for salmon weight |
| `N`          | P<sub>n</sub>           | Proportion of natural habitat       |
| `a`          | &alpha;<sub>1</sub>     | Metabolic coefficient       |
| `b`          | &alpha;<sub>2</sub>     | Metabolic coefficient      |
| `c`          | i                       | Time of peak ocean growth potential
| `dn0`          | d<sub>1</sub>         | Metabolic scaling parameter       | 
| `d`          | d<sub>2</sub>           | Metabolic scaling parameter      | 


### 2. Manuscript_All_Code.R

The [Manuscript_All_Code.R](https://github.com/msabal/SDP-pred-mig/blob/main/scripts/Sabal%20et%20al.%20Manuscript%20Submission/Manuscript_All_Code.R) script contains code to run stochastic dynamic programming (SDP) models and generate figures in the manuscript. This script sources functions defined in **Manuscript_All_Functions.R** and reads parameter values for each scenario from **Parameters_Scenarios.csv**. This script will run SDP models, but output tracks from the main model scenarios can be found in the [Manuscript V1](https://github.com/msabal/SDP-pred-mig/tree/main/results/Manuscript%20V1) folder.

### 3. Manuscript_All_Functions.R

The [Manuscript_All_Functions.R](https://github.com/msabal/SDP-pred-mig/blob/main/scripts/Sabal%20et%20al.%20Manuscript%20Submission/Manuscript_All_Functions.R) script contains support functions that are called by **Manuscript_All_Code.R**.

### 4. Parameters_Scenarios.csv

The [Parameters_Scenarios.csv](https://github.com/msabal/SDP-pred-mig/blob/main/scripts/Sabal%20et%20al.%20Manuscript%20Submission/Parameters_Scenarios.csv) file contains the parameter values used for each scenario in **Manuscript_All_Code.R**.
