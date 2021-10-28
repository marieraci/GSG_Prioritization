# GSG_Prioritization
Repository for prioritization process for the greater sage-grouse in the Rock Springs Field Office. Contained are scripts to show how the prioritization was structured, but some data was privately shared and not included. Where possible, the sources to data is included in the Load file.

Running the Initialize file will cause all following numbered R files in the directory to run sequentially.

The prioritization is split into 5 scripts. Several R packages are integral to this process including prioritizr version 7.1.1 (Hanson et al., 2021 https://prioritizr.net/index.html) and a license with Gurobi Optimization (a free academic license is available; https://www.gurobi.com/). Multiple data pieces are required and were pre-processed in ArcMap to ensure consistencies in format, resolution, extent, and projection.

Prioritizations followed objectives: 
    1. identify priority areas, 
          1.a) identify priority areas outside of PACs, 
          1.b) identify priority areas on public lands, 
          1.c) identify priority areas on private lands (conservation easements), and 
    2. identify areas suitable for restoration. 

Solutions are analyzed using metrics irreplaceability, contiguity, and return on investment (ROI). 

Finally plots were created to display solutions (tmaps) and solution metrics (ggboxplots). Porfolios are also generated and plotted.
