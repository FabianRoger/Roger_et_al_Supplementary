# borrowed from multifunc package. 
# I don't want to load it because it loads MASS and plyr which both mess with dplyr.

# The functions are copied from the GitHub page as is.


# https://github.com/jebyrnes/multifunc/blob/master/R/whichVars.R (2015-12-09)

whichVars<-function(a.df, vars=NA, thresh=2/3){
  if(is.na(vars[1])) stop("No column names supplied.")
  tot<-nrow(a.df)
  
  #figure out which vars we can actually use
  usevar<-sapply(vars, function(x) sum(is.na(a.df[[x]]))/tot>thresh)
  vars<-vars[!usevar]
  
  vars
}


# https://github.com/jebyrnes/multifunc/blob/master/R/getFuncMaxed.R (2015-12-09)
# changed one line (65 at GitHub, 38 here) using plyr::colwise to use apply() instead

getFuncMaxed<-function(adf, vars=NA, thresh=0.7, proportion=F, prepend="Diversity", maxN=1){
  if(is.na(vars)[1]) stop("You need to specify some response variable names")
  
  #which are the relevant variables
  vars<-whichVars(adf, vars)
  
  #scan across all functions, see which are >= a threshold
  #funcMaxed<-rowSums(colwise(function(x) x >= (thresh*max(x, na.rm=T)))(adf[,which(names(adf)%in%vars)]))
  getMaxValue<-function(x){
    l<-length(x)    
    mean( sort(x, na.last=F)[l:(l-maxN+1)], na.rm=T)
  }
  
  #original with plyr
  #funcMaxed<-rowSums(colwise(function(x) x >= thresh*getMaxValue(x))(adf[,which(names(adf)%in%vars)]))
  
  #alternative with apply
  funcMaxed <- rowSums(apply( adf[,which(names(adf)%in%vars)], 2, function(x) x >= thresh*getMaxValue(x)))
  
  
  if(proportion) funcMaxed<-funcMaxed/length(vars)
  
  #bind together the prepend columns and the functions at or above a threshold
  ret<-data.frame(cbind(adf[,which(names(adf) %in% prepend)], funcMaxed))
  names(ret) <- c(names(adf)[which(names(adf) %in% prepend)], "funcMaxed")
  
  #how many functions were considered
  ret$nFunc<-length(vars)
  
  ret
}


# function to plot scientific axis labels
scientific_10 <- function(x) {
  parse(text = gsub("e", " %*% 10^", scientific_format()(x)))
}
