
# test for correlations in published data #

# write a function to generate data with the same summery stsatistics as extracted from 
# the published data

find.data.points <- function( x, var, n ) {
  
  require(plyr)
  require(MASS)
  require(reshape2)
  
  # function to find a set of raw datapoints that satisfy the constraints 
  # given by the aggregated data 
  
  #input variables
  
  # x is a dataframe with thre columns: 
  # x[ , 1 ] is a string or factor column which contains the variable name
  # x[ , 2 ] is a numeric column giving the mean
  # x[ , 3 ] is a numeric column giving either the variance, the standard deviation
  # or the standard error of the mean
  
  # var should be a string and either of "sd" (standard deviation),
  # "sem" (standard error of mean) or "var" (variance)
  
  # n gives the sample size 
  
  # outout
  
  # a dataframe with as many rows as the input dataframe and as many columns
  # as the sample size (+1)
  
  
  # convert sd or sem into variance
  if (var == "sd") { x[ , 3]  <-  x[ , 3] ^ 2 }
  if (var == "sem") { x[ , 3]  <-  ( x[ , 3] * sqrt( n)) ^ 2 }
  
  #calculate set of points
  Points <- mapply("mvrnorm", n, x[,2], x[,3], empirical = T)
  
  # rearrange, join to DF, rearrange and sort
  tPoints <- t(Points)
  DPoints <- data.frame( x[ ,1], tPoints )
  DPoints <- melt( DPoints, id.var = 1 )
  DPoints <- DPoints[with(DPoints, order( DPoints[ ,1])), ]

  colnames( DPoints)[ 1 ]  <- colnames(x)[ 1]
 
  return(DPoints)
  }


######## simulation some data to show that the outcome of the statistics only 
######## depends on mean and variance (not on actual datapoints)


# specify data to simulate
Levels <- 3
SampleSize <- 3
sd  <- 2
Means <- sample( c(1:10), Levels)

# simulate Data
simDAT <- data.frame(LEVELS = rep( LETTERS[ 1 : Levels], each = SampleSize), 
                     DATA = unlist(lapply(Means, function(x) rnorm(SampleSize, x, 1))))

# calculate mean and sd from data
smryDAT <- ddply( simDAT, .(LEVELS), summarize, MEAN = mean(DATA), sd = sd(DATA))

# find new set of points with same mean and variance
newDAT  <- find.data.points( smryDAT, var = "sd", n = SampleSize)

# plot simulated data (black) and new data (red)
ggplot(simDAT, aes( x = LEVELS, y = DATA))+
  geom_point(col = "black")+
  geom_point(data = newDAT, aes( x = LEVELS, y = value), col = "red")


# anova of simulated data
summary(aov(DATA ~ LEVELS, data = simDAT))

# anova of new data
summary(aov(value ~ LEVELS, data = newDAT))

# TukeyHSD test of simulated data
TukeyHSD(aov(DATA ~ LEVELS, data = simDAT))

# TukeyHSD test of new data
TukeyHSD(aov(value ~ LEVELS, data = newDAT))

# linear regression of simulated data
summary(lm(DATA ~ as.numeric(LEVELS), data = simDAT))

# linear of new data
summary(lm(value ~ as.numeric(LEVELS), data = newDAT))






################################################################################

# test data from published articles

################################################################################

#######################
# (Matos et al. 2005) #
#######################

# report data for two treatments (with and without invasion) for two functions
# (cell abundance and CLPP) but don't test an effect of diversity

# number of replicates is always 3
n <- 3

Data <- data.frame(Inv = rep(c("w/o inv", "with inv"), each = 4),
                   DIV = c("High", "Medium", "Low", "Gnotobiotic"),
                   Exp = rep( c ("TotCounts", "CLPP"), each = 8),
                   Value = c(10.80, 10.74, 10.51, 10.96,
                             10.81, 10.76, 10.32, 10.77,
                             92, 85, 55, 76,
                             92, 88, 70, 81),
                   sd = c(0.15, 0.07, 0.09, 0.04,
                          0.12, 0.09, 0.05, 0.51,
                          0.58, 1.15, 2.12, 3.21,
                          0.58, 1.53, 5.13, 1.53))

# test one way anovas, seperately for each Treatment * Function comb (4)

##### Abundance, no invasion #####

#subset data
DFsub_A_nI  <- Data[ Data$Inv == "w/o inv" & Data$Exp == "TotCounts", ][ , c( "DIV", "Value", "sd")]

# simulate new data
DFnew_A_nI <- find.data.points( DFsub_A_nI, var = "sd", n = n)

# add diversity order variable
DFnew_A_nI$DIV_level <- rep( c(2,4,1,3), each = 3)

# plot
ggplot(DFnew_A_nI, aes( x = DIV_level, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ DIV_level, data = DFnew_A_nI)) # p = 0.192



##### Abundance, with invasion #####

#subset data
DFsub_A_wI  <- Data[ Data$Inv == "with inv" & Data$Exp == "TotCounts", ][ , c( "DIV", "Value", "sd")]

# simulate new data
DFnew_A_wI <- find.data.points( DFsub_A_wI, var = "sd", n = n)

# add diversity order variable
DFnew_A_wI$DIV_level <- rep( c(2,4,1,3), each = 3)


# plot
ggplot(DFnew_A_wI, aes( x = DIV_level, y = value ) )+
  geom_point() +
  stat_smooth(method="lm") +
  theme_bw(base_size = 15)

# test effect of diversity

summary( lm( value ~ DIV_level, data = DFnew_A_wI)) # p = 0.06294


##### CLLP, without invasion #####

#subset data
DFsub_CLPP_nI  <- Data[ Data$Inv == "w/o inv" & Data$Exp == "CLPP", ][ , c( "DIV", "Value", "sd")]

# simulate new data
DFnew_CLPP_nI <- find.data.points( DFsub_CLPP_nI, var = "sd", n = n)

# add diversity order variable
DFnew_CLPP_nI$DIV_level <- rep( c(2,4,1,3), each = 3)

# plot
ggplot(DFnew_CLPP_nI, aes( x = DIV_level, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ DIV_level, data = DFnew_CLPP_nI)) # p = 9.908e-07, r^2 = 0.909

##### CLLP, with invasion #####

#subset data
DFsub_CLPP_wI  <- Data[ Data$Inv == "with inv" & Data$Exp == "CLPP", ][ , c( "DIV", "Value", "sd")]

# simulate new data
DFnew_CLPP_wI <- find.data.points( DFsub_CLPP_wI, var = "sd", n = n)

# add diversity order variable
DFnew_CLPP_wI$DIV_level <- rep( c(2,4,1,3), each = 3)

# plot
ggplot(DFnew_CLPP_wI, aes( x = DIV_level, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

# test effect of diversity

summary( lm( value ~ DIV_level, data = DFnew_CLPP_wI)) # p = 4.307e-06, r^2 = 0.909 




#######################
# (Szabó et al. 2007) #
#######################

# I digitized Figure 2 with Plot digitizer 2.2.6 to check effect of dilution on biomass
# three replicates

n <- 3

DATA_BM <- read.table("LiteratureDocs/Szabo_2007_BM.csv", sep = ",", header = T)

#calculate sd as mean from confidence interval range
DATA_BM$sd <- ( (DATA_BM$mean - DATA_BM$sdmin) + (DATA_BM$sdmax - DATA_BM$mean)) / 2

##### control treatment #####

#subset data
DATA_C <- DATA_BM[ DATA_BM$Treatment == "controle", ][, c("Dilution", "mean", "sd")]

# simulate new data
DATA_new_C <- find.data.points( DATA_C, var = "sd", n = n)

# plot
ggplot(DATA_new_C[DATA_new_C$Dilution < 8,], aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_C[DATA_new_C$Dilution < 8,])) # p = 0.0222, r^2 = 0.18


##### Phenole treatment #####

#subset data
DATA_P <- DATA_BM[ DATA_BM$Treatment == "Phenol", ][, c("Dilution", "mean", "sd")]

# simulate new data
DATA_new_P <- find.data.points( DATA_P, var = "sd", n = n)

# plot
ggplot(DATA_new_P, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_P)) # p = 0.000556, r^2 = 0.5065 



##### Humic treatment #####

#subset data
DATA_H <- DATA_BM[ DATA_BM$Treatment == "Humic", ][, c("Dilution", "mean", "sd")]

# simulate new data
DATA_new_H <- find.data.points( DATA_H, var = "sd", n = n)

# plot
ggplot(DATA_new_H, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_H)) # p = 0.0008635, r^2 = 0.4506



####### anova with all values for comparison with published anova


DATA_all <- DATA_BM[, c("Treatment","Dilution", "mean", "sd")]

# make single variable out of it becasue function right now doesn't handle two factors
DATA_all$TreatDil <- paste(DATA_all$Treatment, DATA_all$Dilution )

# simulate new data
DATA_new_all <- find.data.points( DATA_all[, c("TreatDil","mean","sd")], var = "sd", n = n)

DATA_new_all <- join(DATA_new_all , DATA_all)

# test effect of diversity and treatment
TukeyHSD(aov( value ~ as.factor(Dilution) + Treatment, data = DATA_new_all))






##########################
# (Franklin et al. 2001) #
##########################

n  <- 3

DATA_F <- data.frame(Dilution = c(0,1,2,3,4,5,6),
                 Cells = c(22, 13, 24, 30, 39, 73, 110),
                 sd = c(10, 8.6, 14, 15, 2.8, 19, 9))

# simulate new data
DATA_new_F <- find.data.points( DATA_F, var = "sd", n = n)

# plot
ggplot(DATA_new_F, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_F)) # p = 1.416e-06, r^2 = 0.6994


##########################
# (Franklin & Mills 2006) #
##########################

n  <- 2

DATA_F_K <- data.frame(Dilution = c(0,2,3,4,5,6),
                     Kmax = c(135,74,24,47,35,36),
                     sd = c(47,23,5,17,3,14))

# simulate new data
DATA_new_F_K <- find.data.points( DATA_F_K, var = "sd", n = n)

# plot
ggplot(DATA_new_F_K, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_F_K)) # p =  0.00381 , r^2 = 0.5423 


# anova test effect of diversity
summary( aov( value ~ as.factor(Dilution), data = DATA_new_F_K)) # p =  0.0227 


###########################
# (Griffiths et al. 2001) #
###########################

##### Biomass #####

DATA_G_BM <- data.frame(Dilution = c(0,2,4,6),
                        BM = c(9.77, 11.21, 10.10, 28.04),
                        sem = c(0.54, 2.95, 2.43, 3.15))

n <- 3

# simulate new data
DATA_new_G_BM <- find.data.points( DATA_G_BM, var = "sem", n = n)

# plot
ggplot(DATA_new_G_BM, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_BM)) # p =  0.00953 , r^2 = 0.4561 

##### Thymidine intake #####

DATA_G_T <- data.frame(Dilution = c(0,2,4,6),
                        Thymidine = c(19.88, 7.62, 13.62, 40.81),
                        sem = c(2.55, 5.07, 5.62, 4.93))

n <- 3

# simulate new data
DATA_new_G_T <- find.data.points( DATA_G_T, var = "sem", n = n)

# plot
ggplot(DATA_new_G_T, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_T)) # p =  0.06806 , r^2 = 0.2244 



##### Leucine intake #####

DATA_G_L <- data.frame(Dilution = c(0,2,4,6),
                       Leucine = c(585, 221.6, 418.2, 645.8),
                       sem = c(59.8, 200, 222.9, 50.3))

n <- 3

# simulate new data
DATA_new_G_L <- find.data.points( DATA_G_L, var = "sem", n = n)

# plot
ggplot(DATA_new_G_L, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_L)) # p-value: 0.6312 



##### Potential Nitrification Rate #####

DATA_G_PNR <- data.frame(Dilution = c(0,2,4,6),
                       PNR = c(1.44, 1.11, 1.48, 0.77),
                       sem = c(0.09, 0.29, 0.34, 0.27))

n <- 3

# simulate new data
DATA_new_G_PNR <- find.data.points( DATA_G_PNR, var = "sem", n = n)

# plot
ggplot(DATA_new_G_PNR, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_PNR)) #  p-value: 0.2119

# anova test effect of diversity
summary( aov( value ~ as.factor(Dilution), data = DATA_new_G_PNR)) # p =  0.272
TukeyHSD( aov( value ~ as.factor(Dilution), data = DATA_new_G_PNR)) 


##### Resistance against copper disturbance ######

DATA_G_C1 <- data.frame(Dilution = c(0,2,4,6),
                         mean = c(-24.196,-15.086,-27.733,-9.032),
                         sem = c(-29.052,-24.364,-29.074,-12.224))

DATA_G_C1$sem <- abs(DATA_G_C1$sem - DATA_G_C1$mean)

n <- 3

# simulate new data
DATA_new_G_C1 <- find.data.points( DATA_G_C1, var = "sem", n = n)

# plot
ggplot(DATA_new_G_C1, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_C1)) #  p-value: 0.27659

##### Resilience against copper disturbance ######

DATA_G_C28 <- data.frame(Dilution = c(0,2,4,6),
                        mean = c(-23.081, 3.579, -21.485, -8.344),
                        sem = c(-26.068, -3.942, -25.556, -9.746))

DATA_G_C28$sem <- abs(DATA_G_C28$sem - DATA_G_C28$mean)

n <- 3

# simulate new data
DATA_new_G_C28 <- find.data.points( DATA_G_C28, var = "sem", n = n)

# plot
ggplot(DATA_new_G_C28, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_C28)) #  p-value: 0.598

##### Resistance against heat disturbance ######

DATA_G_H1 <- data.frame(Dilution = c(0,2,4,6),
                        mean = c(-16.236, -22.387, -45.674, -11.774),
                        sem = c(-24.332, -24.19, -53.932, -15.84))

DATA_G_H1$sem <- abs(DATA_G_H1$sem - DATA_G_H1$mean)

n <- 3

# simulate new data
DATA_new_G_H1 <- find.data.points( DATA_G_H1, var = "sem", n = n)

# plot
ggplot(DATA_new_G_H1, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_H1)) #  p-value: 0.8279

##### Resilience against heat disturbance ######

DATA_G_H28 <- data.frame(Dilution = c(0,2,4,6),
                         mean = c(-24.639, -7.006, -20.285, -24.293),
                         sem = c(-30.262, -12.56, -26.878, -28.769))

DATA_G_H28$sem <- abs(DATA_G_H28$sem - DATA_G_H28$mean)

n <- 3

# simulate new data
DATA_new_G_H28 <- find.data.points( DATA_G_H28, var = "sem", n = n)

# plot
ggplot(DATA_new_G_H28, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G_H28)) #  p-value: 0.6918

###########################
# Griffits et al 2004 #
##########################

##### Resistance against copper disturbance ######

DATA_G4_C1 <- data.frame(Dilution = c(2,4,6,8),
                        mean = c(-24.595, -33.85, -41.669, -55.096),
                        sem = c(-26.745, -35.963, -46.068, -57.998))

DATA_G4_C1$sem <- abs(DATA_G4_C1$sem - DATA_G4_C1$mean)

n <- 6

# simulate new data
DATA_new_G4_C1 <- find.data.points( DATA_G4_C1, var = "sem", n = n)

# plot
ggplot(DATA_new_G4_C1, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G4_C1)) #  R-squared:  0.708 p-value: 1.572e-07

##### Resilience against copper disturbance ######

DATA_G4_C28 <- data.frame(Dilution = c(2,4,6,8),
                         mean = c(-29.32, -36.781, -42.313, -60.034),
                         sem = c(-31.798, -39.136, -45.954, -62.451))

DATA_G4_C28$sem <- abs(DATA_G4_C28$sem - DATA_G4_C28$mean) 

n <- 6

# simulate new data
DATA_new_G4_C28 <- find.data.points( DATA_G4_C28, var = "sem", n = n)

# plot
ggplot(DATA_new_G4_C28, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G4_C28)) #  R-squared:  0.702  p-value: 1.978e-07

##### Resistance against heat disturbance ######

DATA_G4_H1 <- data.frame(Dilution = c(2,4,6,8),
                        mean = c(-86.268, -83.879, -78.718, -85.569),
                        sem = c(-87.326, -84.333, -82.353, -87.32))

DATA_G4_H1$sem <- abs(DATA_G4_H1$sem - DATA_G4_H1$mean)

n <- 6

# simulate new data
DATA_new_G4_H1 <- find.data.points( DATA_G4_H1, var = "sem", n = n)

# plot
ggplot(DATA_new_G4_H1, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G4_H1)) #  p-value: 0.4949

##### Resilience against heat disturbance ######

DATA_G4_H28 <- data.frame(Dilution = c(2,4,6,8),
                         mean = c(-7.821, -3.338, -14.871, -19.366),
                         sem = c(-10.779, -7.884, -18.752, -21.498))

DATA_G4_H28$sem <- (DATA_G4_H28$sem - DATA_G4_H28$mean) * -1

n <- 6

# simulate new data
DATA_new_G4_H28 <- find.data.points( DATA_G4_H28, var = "sem", n = n)

# plot
ggplot(DATA_new_G4_H28, aes( x = Dilution, y = value ) )+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw(base_size = 15)

# regression test effect of diversity
summary( lm( value ~ Dilution, data = DATA_new_G4_H28)) #  R-squared:  0.2342,  p-value: 0.00965



###########################
# (Tardy et al. 2014) #
###########################

####### Temperature Resistance #########

DATA_T_T0 <- data.frame(Dilution = c(0,3,5),
                    mean = c(-7.371, -19.385, -3.323),
                    sd = c(2.352, 5.196, 6.7335))

DATA_T_T0_new <- find.data.points(DATA_T_T0, var = "sd", n = 3)

# plot
ggplot(DATA_T_T0_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_T_T0_new)) #  p-value: 0.7772


####### Temperature Resilience #########

DATA_T_T80 <- data.frame(Dilution = c(0,3,5),
                        mean = c(-3.624, -2.676, -0.762),
                        sd = c(1.131, 5.3465, 5.8005))

DATA_T_T80_new <- find.data.points(DATA_T_T80, var = "sd", n = 3)

# plot
ggplot(DATA_T_T80_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_T_T80_new)) #   p-value: 0.4525


####### Mercury Resistance #########

DATA_M_T0 <- data.frame(Dilution = c(0,3,5),
                         mean = c(-24.838, -26.918, -20.931),
                         sd = c(2.612, 5.6715, 4.622))

DATA_M_T0_new <- find.data.points(DATA_M_T0, var = "sd", n = 3)

# plot
ggplot(DATA_M_T0_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_M_T0_new)) #   p-value: 0.4187


####### Mercury Resilience #########

DATA_M_T80 <- data.frame(Dilution = c(0,3,5),
                        mean = c(-4.151, -4.872, -6.542),
                        sd = c(0.6865, 3.632, 3.8075))

DATA_M_T80_new <- find.data.points(DATA_M_T80, var = "sd", n = 3)

# plot
ggplot(DATA_M_T80_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_M_T80_new)) #    p-value: 0.3553



###########################
# (Wertz et al 2007) #
###########################

n <- 5

# calculating data on total bacterial abundance (denitrifiers + nitrite oxidizer)

DATA_BAC_DN <- data.frame(DIL = c( 1,3,4,5,6,8),
                          mean = c( 7.93, 9.4, 6.17, 6.66, 5.79, 3.5),
                          sem = c( 1.64, 2.52, 2.32, 1.82, 2.88, 3.5))

DATA_BAC_NO <- data.frame(DIL = c( 1,3,4,5),
                          mean2 = c( 2.21, 9.94, 5.45, 3.16),
                          sem2 = c( 1.12, 1.6, 1.51, 1.39))

# convert sem to variance

DATA_BAC_DN$s <- (DATA_BAC_DN$sem * sqrt( n)) ^ 2

DATA_BAC_NO$s2 <- (DATA_BAC_NO$sem2 * sqrt( n)) ^ 2

# sum abundance and take average variance (as same number of replicates)

DATA_BAC <- join(DATA_BAC_DN, DATA_BAC_NO, by = "DIL")

DATA_BAC[is.na(DATA_BAC)] <- 0

DATA_BAC$sum.mean <- DATA_BAC$mean + DATA_BAC$mean2

DATA_BAC$sum.s <- DATA_BAC$s

DATA_BAC[1:4,]$sum.s <- (DATA_BAC[1:4,]$s + DATA_BAC[1:4,]$s2)/2

DATA_BAC <- DATA_BAC[, c("DIL", "sum.mean", "sum.s")]

# simulate new data
DATA_BAC_new <- find.data.points( DATA_BAC, var = "var", n = n)

# plot
ggplot(DATA_BAC_new, aes( x = DIL, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)


summary( lm (value ~ DIL, data = DATA_BAC_new)) # p-value: 0.006453

#################################
# (Hernandez-Raquet et al 2013) #
#################################

# relationship Dilution ~ AWCD on ecolog plates

DATA_HR_AWCD <- data.frame(Dilution = c(1,3,5,8),
                         mean = c(1.058,0.936,0.588,0.135),
                         sd = c(0.2415,0.159,0.102,0.081))

DATA_HR_AWCD_new <- find.data.points(DATA_HR_AWCD, var = "sd", n = 3)

# plot
ggplot(DATA_HR_AWCD_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_HR_AWCD_new)) #    R-squared:  0.8488
                                                         #    p-value: 1.282e-05


# relationship Dilution ~ NC on ecolog plates

DATA_HR_NC <- data.frame(Dilution = c(1,3,5,8),
                           mean = c(30.134, 29.535, 25.183, 8.373),
                           sd = c(1.658, 1.4085, 1.316, 5.5675))

DATA_HR_NC_new <- find.data.points(DATA_HR_NC, var = "sd", n = 3)

# plot
ggplot(DATA_HR_NC_new, aes( x = Dilution, y = value ) )+
  geom_point()+
  stat_smooth(method = "lm")+
  theme_bw(base_size = 15)

summary( lm (value ~ Dilution, data = DATA_HR_NC_new)) #    R-squared:  0.7729
                                                       #    p-value: 0.0001015

