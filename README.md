# GSG_Prioritization
Repository for prioritization process for the greater sage-grouse in the Rock Springs Field Office. 

Running the Initialize file will cause all following R files in the directory to run sequentially.

The prioritization is split into 5 scripts. Several R packages are integral to this process including prioritizr, raster, and landscapemetrics. Multiple data pieces are also required and are sourced in Load. Data pieces were pre-processed in ArcMap to ensure consistencies in format, resolution, extent, and projection.

Prioritizations followed objectives 1. identify priority areas, 1.a) identify priority areas outside of PACs, 1.b) identify priority areas on public lands, 1.c) identify priority areas on private lands (conservation easements), and 2. identify areas suitable for restoration. 

Solutions are analyzed using metrics irreplaceability, contiguity, and return on investment (ROI). 

Plots were created to display solutions (tmaps) and solution metrics (ggboxplots).
