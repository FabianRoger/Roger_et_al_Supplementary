
Roger et al. - Appendix_2

This folder contains all the code necessary to reproduce the analysis and figures that are
presented in the manuscript and the supplementary material. It also contains the necessary 
raw data , except for the sequencing data which are not provided (due to file size) and 
have not yet been deposited. 

The raw sequencing data will be deposited as soon as possible and can be provided before
that upon request. 

The file 'Processing of Sequencing Data.Rmd' contains the full (but not executable) code 
for the analysis of the sequencing data. 


1) to run the code please open the Roger_et_al.Rproj file 

2) before you run the code please run the install_checkpointed_packages.R script

# this script will install all the packages needed for the analysis in the same version that
# has been used to produce this analysis. (with the exception of phyloseq which has to be 
# installed manually)

# the checkpoint() function has to be run each time the R-session is restarted so that 
# the library path points to library installed by checkpoint and not the default library

3) run the code in this order:

###### the full executable code for the main analysis is in the .Rmd  files. 
###### the corresponding reports can be found in the .html files with the same name
###### and can be generated from the .Rmd files (in RStudio, click Knit HTML symbol)

1 - 4 (in whichever order):

 - Analysis of OTU Data.Rmd
 - Bacterial_Cell_Counts.Rmd
 - Biolog plates.Rmd
 - Nutrient_data.Rmd
 
 5 (run last)
 
 - DIV-EF.Rmd
 
 6 to reproduce the literature graph (Figure 4)

 - Literature_Graph.R
 
 7 to reproduce the temperature graph (Figure S2)
 
 - Temperature.R

 8 the code used for the reanalysis of the literature data can be found in 
 
 - reanalyzed_data_lit_rev.R

### This code depends on MASS and plyr. Both packages interfere with dplyr so you may have
### to restart your R-session if you loaded those packages to make the other scripts run
### smoothly. 



##### Avoid spaces or special characters in the library path to the folder containing the
##### R project. This is likely the reason if knitr fails to compile the .Rmd files correctly



