
install.packages("checkpoint")
library(checkpoint)

checkpoint("2015-10-01",
           use.knitr = TRUE)

# checkpoint will ask if it can create a directory in the home directory under ~/.checkpoint
 
# to intstall the library --> confirm with 'y'

### this installes a new (hidden) library folder in your current working directory called '/.checkpoint'

######### the phyloseq package is not on CRAN and can't (unfortunately) be
######### version controled in the same way. It has to be installed from
######### bioconducter

source("https://bioconductor.org/biocLite.R")
biocLite("phyloseq") # do not opt to update all packages. (type 'n')


