##PROFIT PRICE PREDICTION
# SECTIONS
# 1. DATA OBSERVATION
# 2. DATA CLEANSE
# 3. CORRELATIONS
# 4. TRANSFORMATIONS
# 5. LINEAR REGRESSION
# 6. LASSO REGRESSION
# 7. XBGOOST REGRESSION
# 8. AVERAGE PRICES
# 9. MAPS

#Install the packages required

install.packages(c("yaml","knitr","ggplot2","plyr","dplyr","corrplot","caret","gridExtra","scales","Rmisc","ggrepel",
                   "randomForest","psych","xgboost","pbkrtest","Hmisc", "psych","car","rgl", "psych", "e1071", "caret",
                   "Hmisc", "rgl","viridis","Rtools", "corrgram","pastecs","ggpubr","tidyverse", "energy","Rgooglemaps", 
                   "png", "units", "spatstat", "rjson", "maptools", "sp", "rgdal", "raster", "calibrate","xtable", dependencies = all))

#Load the packages required
require(pastecs)
require(ggpubr)
require(tidyverse)
require(Hmisc)
require(corrplot)
library(psych)
library(energy)
library(car)
library(corrgram)
library(testthat)
library(ggplot2)
library(cowplot) 
library(outliers)
library(scales)
library(rgl)
library(e1071)
library(outliers)
library(viridis)
library(arm)
library(caret)
library(ggpubr)
library(pbkrtest)
library(yaml)
library(knitr)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(reshape2)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)
library(RColorBrewer)
library(tidyr)
library(dplyr)        
library(readr)
library(sf)
library(maptools)
library(googleway)
library(ggmap)
library(ggthemes)
library(ggrepel)
library(spatstat)
library(units)
library(RgoogleMaps)
library(png)
library(jpeg)
library(rjson)
library(tidyr)
library(tibble)
library(maptools)
library(sp)
library(rgdal)
library(raster)
library(calibrate)
library(xtable)

##### Set up the environment
# Define your working directory - use forward / slash
setwd("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES")

##### SECTION 1. DATA OBSERVATION

##### Read in the Grand Table csv and explore the data
grand_table <- read.csv("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE_TAX.csv", sep = ",", na.strings = "?")
head(grand_table)
dim(grand_table) #12947 rows, 55 columns

##### Replace all zeros in DV's with NA

grand_table$AV_PR_M[grand_table$AV_PR_M == 0] <- NA
grand_table$AV_PR_T[grand_table$AV_PR_T == 0] <- NA

##### Subset the data by the property purpose type

profit <- subset(grand_table, TYPE_N=='1')
residential <- subset (grand_table, TYPE_N=='2')
head(profit)
head(residential)
nrow(profit) # 10254 rows
nrow(residential) # 2693 rows

##### Continue only with profit-yielding

summary(profit)

nrow(profit[is.na(profit)])
nrow(profit[is.na(profit$AV_PR_M) |is.na(profit$TAX_SQM),])
nrow(profit[is.na(profit$AV_PR_M),])
nrow(profit[is.na(profit$TAX_SQM),])

profit[is.na(profit$AV_PR_M) | is.na(profit$AV_PR_M),]

##### Remove CAD, NUM_BUI, TYPE_N, TAX_SQM, TAX_1, TOTAL, AV_PR_T columns

profit$CAD <- NULL
profit$TYPE_N <- NULL
profit$TAX_SQM <- NULL
profit$TAX_1 <- NULL
profit$TOTAL <- NULL
profit$AV_PR_T <- NULL
profit$N_BUI <- NULL #can use as categorical

dim(profit)# Down to 49 columns

##### CONTINUE ONLY WITH: : Profit-yielding purpose, sales only

profit_sales <- profit[!(is.na(profit$AV_PR_M)),]
profit_sales
summary(profit_sales) #Check there are no N/A's listed ie. missing values
dim(profit_sales) # Left with 913 rows and 49 columns

# Let us look at the variables to consider their relationship
dev.off()
boxplot(profit_sales, main = "boxplot.matrix(...., main = ...)",
        notch = TRUE, col = 1:4)

#this is not good as the scales are so different

##### Print out summaries of each column
summary(profit_sales [2:49], digits = 2)

summary_all_vars <- xtable(summary(profit_sales [2:49]))

sink("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/summary_all_vars_1.txt")
print(summary(profit_sales [2:25]))
sink()

sink("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/summary_all_vars_2.txt")
print(summary(profit_sales [26:49]))
sink()

##### Let's normalise to the same scale to compare their distribution shapes in a mass boxplot

rescaled.profit_sales <- data.frame(lapply(profit_sales, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/100)))  ###need to do without y value????
head(rescaled.profit_sales)
summary(rescaled.profit_sales)

png(height=800, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_BOXPLOT_VARS_SCALED.png")
boxplot(rescaled.profit_sales[2:49], main = "Box plots of all the variables (normalised and scaled)",
        notch = FALSE, col=brewer.pal(n = 3, name = "RdBu"), par(las = 2), ylab = "Scaled value (0-100)")
dev.off()

# The variables have differing scales and many are not normally distributed.

##### Let's create a correlation matrix spreadsheet with method 'spearman' with p and r values of the variables' correlations. PRINTED
# 
# flat_cor_mat <- function(cor_r, cor_p){
#   cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
#   cor_r <- gather(cor_r, column, cor, -1)
#   cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
#   cor_p <- gather(cor_p, column, p, -1)
#   cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
#   cor_p_matrix
# }
# cor_3 <- rcorr(as.matrix(all), type=c("spearman"),2)
# 
# my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
# head(my_cor_matrix)
# write.csv(my_cor_matrix, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_MATRIX_p&r_spearman.csv")

##### Let's do the same but with Pearson, to compare with the variables once they have been normalised.  PRINTED
# 
# flat_cor_mat <- function(cor_r, cor_p){
# cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
# cor_r <- gather(cor_r, column, cor, -1)
# cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
# cor_p <- gather(cor_p, column, p, -1)
# cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
# cor_p_matrix
# }
# cor_3 <- rcorr(as.matrix(all), type=c("pearson"),2)
# 
# my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
# head(my_cor_matrix)
# write.csv(my_cor_matrix, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_MATRIX_p&r_pearson.csv")


##### Let's divide the data into groups of independent variable type and plot histograms. Use the unscaled data

# head(profit_sales)
# str(profit_sales)
# dim(profit_sales)
# 
# #1.First set of distance variables
# profit.distances <- (profit_sales [4:33])
# profit.distances.pr <- profit_sales[,c(4:33, 49)]
# head(profit.distances.pr)
# 
# 
# 
# png(height=1000, width=800, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_profit.distances_a_HISTOGRAMS.png")
# ggplot(gather(profit.distances), aes(value))+geom_histogram(fill="purple",color="black",bins = 10) + xlab("Distances (m)") +
#   ylab("Number of cadastres") +
#   ggtitle("Nearest distance from Hiiu cadastres to each variable") + facet_wrap(~key, scales = 'free_x') + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, size = 10, hjust=1), axis.text.y = element_text(size = 10, hjust=1)) 
# dev.off()
# 
# 
# #2. Percentage variables
# profit.percentages <- (profit_sales [34:43])
# head(profit.percentages)
# 
# png(height=1000, width=800, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_profit.percentages_HISTOGRAMS.png")
# ggplot(gather(profit.percentages), aes(value))+geom_histogram(fill="darkgreen",color="black",bins = 10) + xlab("Average percentage") +  ylab("Number of cadastres") +
#   ggtitle("Percentage of each variable in Hiiu cadastres") + facet_wrap(~key, scales = 'free_x') + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, size = 10, hjust=1), axis.text.y = element_text(size = 10, hjust=1)) 
# dev.off()
# 
# #3. Soil averages variables
# profit.averages <- (profit_sales [44:48])
# head(profit.averages)
# 
# png(height=1000, width=800, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_profit.averages_HISTOGRAMS.png")
# ggplot(gather(profit.averages), aes(value))+geom_histogram(fill="brown",color="black",bins = 10) + xlab("Average percentage") +
#   ylab("Count") +
#   ggtitle("Soil averages variables in Hiiu cadastres") + facet_wrap(~key, scales = 'free_x') + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, size = 10, hjust=1), axis.text.y = element_text(size = 10, hjust=1)) 
# dev.off()

##### Now make the training sample

train <- profit_sales
summary(train)
dim(train) #913  49
#Left with 913 rows to make the traing model 

##### Continue only with Profit purpose, NO sales only
##### Now make the testing sample

nrow(profit)
head(profit)
profit_no_sales <- profit[(is.na(profit$AV_PR_M)),]
profit_no_sales
summary(profit_no_sales) #CHECK THERE ARE NO N/A's listed ie. missing values
dim(profit_no_sales) 

test <- profit_no_sales
summary(test)
dim(test)
#Left with 9341 rows to make the prediction model (roughly 10x the size of the training model. Not ideal, perhaps)  

dim(train)
str(train[,c(1:10, 49)]) #display first 10 variables and the response variable

#Getting rid of the IDs but keeping the test IDs in a vector. These are needed to compose the submission file
test_labels <- test$FID_1 #THIS IS ONLY THE TEST COLUMN
str(test_labels)
test$FID_1 <- NULL
train$FID_1 <- NULL

head(test)
head(train)
test$AV_PR_M <- NA
head(test)

all <- rbind(train, test)
summary(all)
dim(all)
## [1] 10254   48
#Without the Id's, the dataframe consists of 48 columns including the dependent variable AV_PR_M

#4. Exploring some of the most important variables
#4.1 The response variable; AV_PR_M

summary(all$AV_PR_M)
#summary(all$AV_PR_T)
#PRINT



##### It is clear that the dependent variable is highly skewed. The range is from 0.005 to 6.967, but the median is 0.206. 

# 4.2 The most important numeric independent variables
##### Using the correlation matrix, let's explore the most correlated independent variables.

#4.2.1 Correlations with AV_PR_M

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')      

(numericVars)
(numericVarNames)
#There are 48 numeric variables (including DV)

all_numVar <- all[, numericVars]
##### Create a new variable of the correlations between the variables, using Spearman's method. 
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs", method = "spearman") #correlations of all numeric variables

##### Sort the correlations with the dependent variable by decending order, from strong to weak
cor_sorted <- as.matrix(sort(cor_numVar[,'AV_PR_M'], decreasing = TRUE ))
##### Select only the significantly correlated independent variables with the dependent variable (> +/- 0.1)
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
dev.off()

png(height=1000, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_corrplot_untransformed.png")
corrplot.mixed(cor_numVar, upper = "ellipse", tl.col = "black", tl.pos = "lt", tl.cex =1, lower.col = "black", number.cex = 1)
dev.off()

#It is clear that there are no highly correlated variables with AV_PR_M. There are however 13 variables that have some significant correlation (>= +/- 0.1 with the dependent variable). This includes AV_PR_T, which I will not analyse any further.


##### The most highly correlated variable: D_ARA
summary(all$D_ARA)
#The variable with the highest correlation. with AV_PR_M is D_ARA. The correlation (with Spearman) is not high however, at 0.27.
#Potential reason for correlaiton: land purchased that is designated as profit yielding might be bought for potential agricultural use, or perhaps to extend the neighbouring fields. 

##### The second most correlated variable: P_FIE. 
summary(all$P_FIE)
#this is highly correlated with P_ARA. This one will be dropped for having the lower p_value

##### The third most correlated variable: P_WOO. 
summary(all$P_WOO) # There are many zeros in the variable. 

##### The fourth most correlated variable: S_FER 
summary(all$S_FER) # There are many zeros in the variable. 




gg1<-ggplot(data=all[!is.na(all$AV_PR_M),], aes(x=D_ARA, y=AV_PR_M))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(labels = comma)  +
  ggtitle("Scatterplot of Distance to Arable Land
              vs Average Sold Price (Euros/m2)
                 and the top 3 outliers") +
  geom_text_repel(aes(label = ifelse(all$D_ARA[!is.na(all$AV_PR_M)]>3000, rownames(all), '')))

gg2<-ggplot(data=all[!is.na(all$AV_PR_M),], aes(x=P_FIE, y=AV_PR_M))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(labels = comma)  +
  ggtitle("Scatterplot of Percentage of Fields
            vs Average Sold Price (Euros/m2)
                 and the top 3 outliers") 
 # + geom_text_repel(aes(label = ifelse(all$P_FIE[!is.na(all$AV_PR_M)], rownames(all), '')))

gg3 <-ggplot(data=all[!is.na(all$AV_PR_M),], aes(x=P_WOO, y=AV_PR_M))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(labels = comma)  +
  ggtitle("Scatterplot of Percentage of Woodland Vegetation
                vs Average Sold Price (Euros/m2)
                    and the top 3 outliers") 
 # + geom_text_repel(aes(label = ifelse(all$P_WOO[!is.na(all$AV_PR_M)], rownames(all), '')))

gg4<-ggplot(data=all[!is.na(all$AV_PR_M),], aes(x=S_FER, y=AV_PR_M))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(labels = comma)  +
  ggtitle("Scatterplot of Average Soil Fertility
  Percentage vs Average Sold Price (Euros/m2)
          and the top 3 outliers") +
  geom_text_repel(aes(label = ifelse(all$S_FER[!is.na(all$AV_PR_M)]<2, rownames(all), '')))
dev.off()

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

png(height=1200, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_HIST_4_MOST_CORRELATED_IV.png")
multiplot(gg1, gg2, gg3, gg4, layout=layout)
dev.off()

#############################

##### SECTION 2. DATA CLEANSE

#############################

#5 Checking for missing data and label encoding
#5.1 Completeness of the data
#First, see which variables contain missing values.

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')
#There is 1 column with missing values
#As already thought, only the DV's have missing data. I don't need to imputate for missing data. 

##### Checking for categorical variables

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 'categoric variables')

#There are 49 numeric variables, and 0 categoric variables

##### Finding variable importance with a quick Random Forest to give rough indication of the most important IV's

dim(all)

set.seed(2018)
quick_RF <- randomForest(x=all[1:913,-(48)], y=all$AV_PR_M[1:913], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]


png(height=800, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_QUICK_RANDOM_FOREST_TOP_20.png")
ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none") + ggtitle("The 20 most important variables Mean Squared Error (MSE) according to Random Forest")
dev.off()

#Of the IV's, the most important variables according to Random Forest are S_CLA, S_SIL and D_LDF. However, the top 2 variables have the same score and there is likely multicollinearity. 

##### It is notable that S_FER which was the only soil variable to show significant correlation (spearman) with the DV is missing from the random forest top 20. 
#it is notable that the soil proportion variables all have significant correlations, in both the random forest and the above linear model, except for S_ROC and S_FER, of which S_ROC is absent in both.
##### It was assumed that S_FER would have been a combination of the other soil variables and thus highly correlated with them, however:
cor(all$S_FER, (all$S_CLA +all$S_SAN + all$S_ROC + all$S_SIL))
#0.08637016
cor(all$S_CLA, (all$S_SAN), method = "spearman")
#-0.9002148 clear multicollinearity
# S_SAN will be dropped, for having high multicollinearity, and a lower correletion with AV_PR_M, as well as a  p-value >0.05
cor(all$S_CLA, all$S_SIL, method = "spearman")
# S_SIL will also be dropped, for having high multicollinearity, and a lower correletion with AV_PR_M

##### Prepare data for modelling
##### Dropping highly correlated variables (from correlation spreadsheet) with correlations over 0.7. I am dropping the variable with the lowest correlation with the dependent variable.

##### Dropping high correlated variables (from correlation matrix)
#Looking at the full correlation matrix table and the random forest, remove highly intercorrelated independent variables and then the percentage variables hold preference over the distance variables

#P_FIE dropped over D_ARA (lower R2 value) * also means not removing P_WOO, which is highly correlated with P_FIE
#S_SAN dropped over S_CLA (lower R2 and not significant p-value)
#S_SIL dropped over S_CLA (lower R2)
#P_HOR dropped over P_ARA (they're exactly the same)
#D_AIR dropped over D_LAN (Closely located. Both undesirable)
#D_SHO dropped over D_COA (lower R2)
#P_PRI dropped over P_BUI (lower R2 (both have non significant high p-values)) 
#D_BUI dropped over D_LCB (lower R2 (both have non significant p-values)) 
#D_WLL dropped over D_WET (lower R2 (both have non significant high p-values)) 
#D_GRA dropped over P_GRA (lower R2 and non significant high p-value)) 

drop_high_vars <- c('P_FIE',  'S_SAN','S_SIL', 'P_HOR', 'D_AIR',  'D_SHO', 'P_PRI', 'D_BUI', 'D_WLL', 'D_GRA')

summary(drop_high_vars)
all <- all[,!(names(all) %in% drop_high_vars)]

summary(all)
dim(all) #10254    38


##### REMOVING OUTLIERS

#############################

##### SECTION 3. CORRELATIONS

#############################

##### Correlation matrix of the now reduced dataset to check for multicollinearity

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')      

(numericVars)
(numericVarNames)
#There are 38 numeric variables (including DV)

all_numVar <- all[, numericVars]
##### Create a new variable of the correlations between the variables, using Spearman's method. 
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs", method = "spearman") #correlations of all numeric variables

##### Sort the correlations with the dependent variable by decending order, from strong to weak
cor_sorted <- as.matrix(sort(cor_numVar[,'AV_PR_M'], decreasing = TRUE ))
##### Select only the significantly correlated independent variables with the dependent variable (> +/- 0.1)
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

png(height=1000, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_corrplot_untransformed_2.png")
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex =1, lower.col = "black", number.cex = 1)
dev.off()


##### Pairs panel of the now reduced dataset to check for multicollinearity

most_corr <- data.frame(all$AV_PR_M, all$P_WOO, all$D_LDF, all$D_DRA, all$D_RIV, all$D_PRO, all$D_HAR, all$D_COA, all$AREA, all$S_FER)
most_corr <- c('AV_PR_M', 'D_ARA', 'P_WOO', 'D_LDF', 'D_DRA', 'D_RIV', 'D_PRO', 'D_HAR', 'D_COA', 'AREA', 'S_FER')
head(most_corr)
high_cor <- all[,(names(all) %in% most_corr)]
head(high_cor)

png(height=1500, width=1500, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_PAIRS_PANEL_NO_HCORR.png")
pairs.panels(high_cor, gaps = 0, lm = TRUE, method="spearman", cex.cor =1, cex.main = 2, main = "Correlation and distribution visualisations of the most correlated variables")
dev.off()


################################

##### SECTION 4. TRANSFORMATIONS

################################

##### Skewness and normalizing of the numeric independent variables

##### This stage is the centering and scaling of the 'true numeric'IVs not therefore including the labelled DV! Below, the dataframe is split into one with all (true) numeric variables, and another dataframe holding the (ordinal) factors. In this example, all of the variables are numeric.

numericVarNames <- numericVarNames[!(numericVarNames %in% c('AV_PR_M', 'AV_PR_T' ))]
#numericVarNames was created before having done anything

###### Pre-processing of indepedent variables before modelling

#####Seperating the IV's from the DV for the purpose of transformation and normalisation

DFnumeric <- all[, names(all) %in% numericVarNames]
summary(DFnumeric)
dim(DFnumeric)

#####  In order to fix the skewness, I am taking the log for all numeric predictors with an absolute skew greater than 0.8 (actually: log+1, to avoid division by zero issues). As a rule of thumb, skewness should be between -1 and 1.
skew(DFnumeric)

for(i in 1:ncol(DFnumeric)){if (abs(skew(DFnumeric[,i]))>0.8){DFnumeric[,i] <- log(DFnumeric[,i] +1)}}

skew(DFnumeric)
summary(DFnumeric)

# [1] -0.19804796  0.20858000 -2.89457070  0.74106481  1.22554881
# [6] -0.95979768 -0.64960104 -3.07043596  3.27322223  0.55140991
# [11]  0.28702815  0.61154747 -0.74081178  0.32427421 -0.86265492
# [16] -2.26393441  0.68302126  0.39979726 -1.17797187 -0.52387210
# [21]  0.22799100  0.64749785 -1.83878038 -2.69748203 -1.78508897
# [26] -2.23054937 -1.31776219 20.77248506  3.95283668  1.02260835
# [31] 16.42779968  8.89636728  5.48318325 -0.58843084  1.48702515
# [36] -0.04218856 -4.50967071

# Better, but not perfect. ideally should be between -1 and 1

##### Normalizing the variables

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)
DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)
#10254    37
skew(DFnorm)
head(DFnorm)

png(height=800, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_BOXPLOT_VARS_SCALED_log.png")
boxplot(DFnorm, main = "Box plots of all the variables (normalised and scaled)",
        notch = FALSE, col=brewer.pal(n = 3, name = "RdBu"), par(las = 2), ylab = "Scaled value (0-100)")
dev.off()
#hist(DFnorm)

# #method 2 - centre and scale. Makes no differnce, except for visualisation purposes
# Scaled.Num <- data.frame(lapply(DFnumeric, function(x) scale(x, center = TRUE, scale = max(x, na.rm = TRUE)/100)))  ###need to do without y value????
# head(Scaled.Num)
# summary(Scaled.Num)
# skew(Scaled.Num)
# 
# boxplot(Scaled.Num, main = "Box plots of all the variables (normalised and scaled)",
#         notch = FALSE, col=brewer.pal(n = 3, name = "RdBu"), par(las = 2), ylab = "Scaled value (0-100)")
# png(height=800, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_BOXPLOT_VARS_SCALED_log.png")
# boxplot(Scaled.Num, main = "Box plots of all the variables (normalised and scaled)",
#         notch = FALSE, col=brewer.pal(n = 3, name = "RdBu"), par(las = 2), ylab = "Scaled value (0-100)")
# dev.off()
# 
# hist(Scaled.Num)

##### If categorical variables added or modified, process them here and then combine them back in below. Add DFdummies into cbind if use factor variables

##### Combining the different variable types

head(DFnorm)
combined <- cbind(DFnorm) #combining all (now numeric) predictors into one dataframe and the point where other data types are added back in
head(combined)

##### Normalising the skewed distribution of the dependent variable

skew(all$AV_PR_M)
# 9.063706

qqnorm(all$AV_PR_M)
qqline(all$AV_PR_M)

# The skew of 9.06 indicates a right skew that is far too high, and the QQ-plot shows this; that the average sold prices are not normally distributed.

price_log <-(log(all$AV_PR_M)) #make copy for visualisation purposes. GGplot needs a data.frame
summary(price_log)
glimpse(price_log)

##### Print comparison qqplots and histograms

png(height=800, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_TRANSFORMED_DV.png")
par(mfrow=c(2,2))
qqnorm(all$AV_PR_M, main ="Untransformed AV_PR_M")
qqline(all$AV_PR_M)
hist(all$AV_PR_M, main ="Untransformed AV_PR_M", col = "steelblue")
qqnorm(price_log, main = "Log transformed AV_PR_M")
qqline(price_log)
hist(price_log, main ="Transformed AV_PR_M", col = "steelblue")
dev.off()

#####Now continue with transformed DV for model

all$AV_PR_M <- log(all$AV_PR_M) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$AV_PR_M)

## 0.1946893 # The skew is much better
hist(all$AV_PR_M)

##### Composing train and test sets
#The model will be built using the train_data sample of the data
#The model will be validated using the test_data sample of the data

train_data <- combined[!is.na(all$AV_PR_M),]
test_data <- combined[is.na(all$AV_PR_M),]
head(train_data) # (SHOULD BE) all (processed) independent variables PROFIT SALES (READY FOR CROSS VALIDATION)
summary(train_data)
head(test_data) # (SHOULD BE) all (processed) independent variables PROFIT NO SALES (READY FOR PRICE PREDICTION)
summary(train_data)
dim(train_data) #913  37

##### Check whether the transformations have changed the crude correlations between the variable. RESULT = NO CHANGE

# AV_PR_M <- all$AV_PR_M
# summary(AV_PR_M)
# 
# all_normalised <- cbind(combined, AV_PR_M)
# summary(all_normalised)
# dim(all_normalised) #10254    38


##### Correlation matrix of the now TRANSFORMED AND CENTRE-SCALED dataset to check for changes in correlations
# 
##### Let's create a correlation matrix spreadsheet with method 'spearman' with p and r values of the variables' correlations
# # 
# # flat_cor_mat <- function(cor_r, cor_p){
# #   cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
# #   cor_r <- gather(cor_r, column, cor, -1)
# #   cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
# #   cor_p <- gather(cor_p, column, p, -1)
# #   cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
# #   cor_p_matrix
# # }
# # cor_3 <- rcorr(as.matrix(all_normalised), type=c("spearman"),2)
# # 
# # my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
# # head(my_cor_matrix)
# # write.csv(my_cor_matrix, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_TRANS_NORMAL_MATRIX_p&r_spearman.csv")
# 
##### Let's try Pearson, now that the data has been normalised
# 
# # flat_cor_mat <- function(cor_r, cor_p){
# #   cor_r <- rownames_to_column(as.data.frame(cor_r), var = "row")
# #   cor_r <- gather(cor_r, column, cor, -1)
# #   cor_p <- rownames_to_column(as.data.frame(cor_p), var = "row")
# #   cor_p <- gather(cor_p, column, p, -1)
# #   cor_p_matrix <- left_join(cor_r, cor_p, by = c("row", "column"))
# #   cor_p_matrix
# # }
# # cor_3 <- rcorr(as.matrix(all_normalised), type=c("pearson"),2)
# # 
# # my_cor_matrix <- flat_cor_mat(cor_3$r, cor_3$P)
# # head(my_cor_matrix)
# # write.csv(my_cor_matrix, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_TRANS_NORMAL_MATRIX_p&r_pearson.csv")
#dev.off()
# corrplot.mixed(all_normalised, tl.col="black", tl.pos = "lt", tl.cex =1, lower.col = "black", number.cex = 1)


#####SECTION 5: LINEAR REGRESSION

head(train_data)
summary(train_data)

##### Stepwise Linear Regression Model

# Multiple regression model utilises a simple formula:
# AV_PR_M = INT + B1 x IV1 + B2 x IV2 + B3 x IVn
  
# Using a stepwise selection of variables by backwards elimination
# Considering all candidate variables and eliminate one at the time

#Because of the small training set compared to testing set, cross validation will be used.

##### While developing the model, iteratively analyse variables for
#     - Normality of distribution
#     - Extreme values 
#     - Multiple collinearity
#     - Homoscedasticity (even distribution of residuals)
#     - p-value of coefficients and R2/F-statistic of the model (p-values removed one-at-a-time in order of highest value until all are significant (<0.05))

##### Add the price data back into the training set
##### remove all NA's
AV_PR_M <- all$AV_PR_M
summary(AV_PR_M)
AV_PR_M <- AV_PR_M[!is.na(AV_PR_M)]
summary(AV_PR_M)

lm_train <- cbind(train_data, AV_PR_M)# A dataset with AV_PR_M included for the split linear model
summary(lm_train)
boxplot(lm_train) 

#SPLIT train_data

# Split data into training and validation samples
# We will use (train.size) 80% for training and (valid.size) 20% for validation.

#The best formula will be used to predict the prices for the unsold cadastres: (test_data).

set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(lm_train$AV_PR_M), round(length(lm_train$AV_PR_M) * train.size))
train.sample <- lm_train[train.index,]
valid.sample <- lm_train[-train.index,]
head(train.sample)

##### Fit the model (1)
fit <- lm(AV_PR_M ~ ., data=train.sample)
summary(fit) # R2=0.1463, F=4.377

dev.off
plot(fit)

# Check for non-linearity properly (from "car"), if good go further
# This can only be done after the model was created
crPlots(fit)

# Eliminate extreme values
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)# identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample) %in% c("12047", "6463", "12248")),]     

##### Refit the model (2)
fit <- lm(AV_PR_M ~ ., data=train.sample)

summary(fit) # R2=0.1459 , F=4.353
crPlots(fit)

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)%in% c("2638", "11518", "4085")),]     

##### Refit the model (3)
fit <- lm(AV_PR_M ~ ., data=train.sample)
summary(fit) # R2=15.55, F=4.594

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("7035", "105", "4086")),]   

##### Refit the model (4)
fit <- lm(AV_PR_M ~ ., data=train.sample)
summary(fit) # R2=0.1665, F=4.887

#Now the Residuals are much more balanced
#   Min       1Q   Median       3Q      Max 
# -2.67350 -0.34912 -0.00007  0.33612  2.56118 

# Check and eliminate further extremes if any
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)                        # We should continue checking Cook!

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
vif(fit)

### Refit the model (5) - drop D_QKM due to multiple collinearity
fit <- lm(AV_PR_M ~. -D_QKM, data=train.sample)
summary(fit) # R2= 0.1628, but tough it was inflated, F=4.89
vif(fit) # All the variables are less than 4. Most are within 1 to 2. Removing D_QKM reduced some others

### Refit the model (6) - drop D_SCH due to p-value (0.9)
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH, data=train.sample)
summary(fit) # R2=0.1641 , F=5.037
vif(fit)

### Refit the model (7) - drop D_FOR due to p-value (0.89)
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR, data=train.sample)
summary(fit) # R2=0.1653  , F=5.192
vif(fit)

### Refit the model (8) - drop D_FEN due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN, data=train.sample)
summary(fit) # R2=0.1663  , F=5.353
vif(fit)

### Refit the model (9) - drop D_WET due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET, data=train.sample)
summary(fit) # R2=0.1669  , F=5.506
vif(fit)

### Refit the model (10) - drop D_RIV due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV, data=train.sample)
summary(fit) # R2=0.168  , F=5.689
vif(fit)

### Refit the model (11) - drop P_BUI due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI, data=train.sample)
summary(fit) # R2=0.169   , F=5.879
vif(fit)

### Refit the model (12) - drop D_WAT due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT, data=train.sample)
summary(fit) # R2= 0.1699    , F=6.08
vif(fit)

### Refit the model (12) - drop D_WAT due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN, data=train.sample)
summary(fit) # R2=  0.1706     , F=6.288
vif(fit)

### Refit the model (13) - drop P_ARA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA, data=train.sample)
summary(fit) # R2=  0.1713     , F=6.514
vif(fit)

### Refit the model (14) - drop P_WOO due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO, data=train.sample)
summary(fit) # R2=  0.1721      , F=6.756
vif(fit)

### Refit the model (15) - drop D_DRA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA, data=train.sample)
summary(fit) # R2=  0.1725      , F=7.002
vif(fit)

### Refit the model (16) - drop D_ELE due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE, data=train.sample)
summary(fit) # R2=  0.1726     , F=7.26
vif(fit)

### Refit the model (17) - drop P_PRO due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO, data=train.sample)
summary(fit) # R2=  0.173     , F=7.547    #####THIS IS THE PEAK
vif(fit)

### Refit the model (18) - drop D_COA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA, data=train.sample)
summary(fit) # R2=  0.1725     , F=7.822
vif(fit)

### Refit the model (19) - drop D_LCB due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB, data=train.sample)
summary(fit) # R2=  0.1721     , F=8.127  #####THE GAP BETWEEN THE MULTIPLE AND ADJUSTED R2 IS SHRINKING
vif(fit)

### Refit the model (20) - drop D_RDS due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS, data=train.sample)
summary(fit) # R2=  0.1717     , F=8.463  #####THE GAP BETWEEN THE MULTIPLE AND ADJUSTED R2 IS SHRINKING
vif(fit)

### Refit the model (21) - drop P_WET due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET, data=train.sample)
summary(fit) # R2=  0.1717     , F=8.858 
vif(fit)

### Refit the model (22) - drop S_CLA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA, data=train.sample)
summary(fit) # R2=  0.1718     , F=9.297 
vif(fit)

### Refit the model (23) - drop S_ROC due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC, data=train.sample)
summary(fit) # R2=  0.1715     , F=9.768 
vif(fit)

### Refit the model (24) - drop D_WDV due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV, data=train.sample)
summary(fit) # R2=  0.1715     , F=10.32 
vif(fit)

### Refit the model (25) - drop D_ENV due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV, data=train.sample)
summary(fit) # R2=  0.1704     , F=10.86 
vif(fit)

### Refit the model (26) - drop D_LDF due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF, data=train.sample)
summary(fit) # R2=  0.1691     , F=11.47
vif(fit)

### Refit the model (27) - drop D_QUA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA, data=train.sample)
summary(fit) # R2=  0.1681     , F=12.19
vif(fit)

### Refit the model (28) - drop D_PAT due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA -D_PAT, data=train.sample)
summary(fit) # R2=  0.1668      , F=13.01
vif(fit)


##### All variables now have p-values of less than 0.1. Let's remove all over 0.05

### Refit the model (29) - drop D_GVE due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA -D_PAT -D_GVE, data=train.sample)
summary(fit) # R2=  0.1637      , F=13.82
vif(fit)

### Refit the model (30) - drop D_SPO due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA -D_PAT -D_GVE -D_SPO, data=train.sample)
summary(fit) # R2=  0.1622      , F=14.94
vif(fit)

### Refit the model (31) - drop P_GRA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA -D_PAT -D_GVE -D_SPO -P_GRA, data=train.sample)
summary(fit) # R2=  0.1604      , F=16.28
vif(fit)

### Refit the model (32) - drop P_GRA due to p-value 
fit <- lm(AV_PR_M ~. -D_QKM -D_SCH -D_FOR -D_FEN -D_WET -D_RIV -P_BUI  -D_WAT -D_SAN -P_ARA -P_WOO -D_DRA -D_ELE -P_PRO -D_COA -D_LCB -D_RDS -P_WET -S_CLA -S_ROC -D_WDV -D_ENV -D_LDF -D_QUA -D_PAT -D_GVE -D_SPO -P_GRA -D_PRO, data=train.sample)
summary(fit) # R2= 0.1587        , F=17.98   #PRINT
vif(fit)

#THIS (MODEL 32) IS THE OPTIMUM STEPWISE MODEL. ALL VARIABLES ARE SIGNIFICANT IN TERMS OF P-VALUES AND ALL THE VIF SCORES ARE NO HIGHER THAN 1.2. 

##### More variables have been removed than kept, so let's write it the other way round

# drop_mod_var <- c("D_QKM","D_SCH","D_FOR","D_FEN","D_WET","D_RIV","P_BUI","D_WAT","D_SAN","P_ARA","P_WOO","D_DRA","D_ELE","P_PRO","D_COA","D_LCB","D_RDS","P_WET","S_CLA","S_ROC","D_WDV","D_ENV","D_LDF","D_QUA","D_PAT","D_GVE","D_SPO","P_GRA","D_PRO")
# newdata <- train.sample[,!(names(train.sample) %in% drop_mod_var)]
# summary(newdata)
# summary(train.sample)
# dim(newdata)

keep_mod_var <- c("AREA", "D_ARA", "D_BOG", "D_HAR", "D_ORD", "D_SPR", "P_WAT", "S_FER", "AV_PR_M")
newdata <- train.sample[c(names(train.sample) %in% keep_mod_var)]
summary(newdata)
# summary(train.sample)
# dim(newdata)

dev.off()
forwardlm <- lm(AV_PR_M ~., data = newdata)
sink("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_lm_optiumum_summary_forward.txt")
print(summary(forwardlm))
sink() 


##### Repeat model but with variables written out for printing purposes

final_lm <- lm(AV_PR_M~ AREA+ D_ARA+ D_BOG+ D_HAR+ D_ORD+ D_SPR+ P_WAT+ S_FER, data=train.sample)
RSS <- c(crossprod(final_lm$residuals))
RSS
#RESIDUAL SUM OF SQUARES = 276
MSE <- RSS/length(final_lm$residuals)
MSE
#MEAN SQUARE ERROR = 0.382
RMSE <- sqrt(MSE)
RMSE 
#ROOT MEAN SQUARED ERROR =0.618
sig2 <- RSS / final_lm$df.residual
sig2
#Pearson estimated residual variance=0.387
summary (final_lm)


# Call:
#   lm(formula = AV_PR_M ~ AREA + D_ARA + D_BOG + D_HAR + D_ORD + 
#        D_SPR + P_WAT + S_FER, data = train.sample)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.6639 -0.3708  0.0014  0.3725  2.6348 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.5156     0.0253  -59.81  < 2e-16 ***
#   AREA         -0.1322     0.0270   -4.90  1.2e-06 ***
#   D_ARA         0.1775     0.0253    7.01  5.5e-12 ***
#   D_BOG         0.0578     0.0270    2.14  0.03282 *  
#   D_HAR        -0.0922     0.0269   -3.43  0.00063 ***
#   D_ORD        -0.0559     0.0259   -2.16  0.03107 *  
#   D_SPR        -0.0681     0.0243   -2.81  0.00513 ** 
#   P_WAT        -0.1597     0.0603   -2.65  0.00823 ** 
#   S_FER        -0.1385     0.0290   -4.77  2.2e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.622 on 712 degrees of freedom
# Multiple R-squared:  0.168,	Adjusted R-squared:  0.159 
# F-statistic:   18 on 8 and 712 DF,  p-value: <2e-16

sink("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_lm_optiumum_variables_displayed.txt")
print(summary(final_lm))
sink()

##### R2 = 0.159 F =17.98
##### A new model that used a subset of the same variables that were in the optimum reverse linear model produces exactly the same results!

##### Lets visualise the linearity again
#PRINT

png(height=1000, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_final_lm_RESIDUAL_PLOTS_STEPWISE_LM.png")
par(mfrow=c(2,2))
plot(final_lm, main = "Residual Plots from Stepwise Linear Regression Model")
dev.off()

png(height=1000, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_final_lm_CRPLOTS_STEPWISE_LM.png")
crPlots(final_lm)
dev.off()
#PRINT

sink("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_final_lm_summary.txt")
print(summary(final_lm))
sink() 


###################################################

##### Use the trained lm model to predict the price (Euro/sqm) in the training dataset
lm_train_sample_Pred <- predict(final_lm, train.sample)
predictions_train_lm <- exp(lm_train_sample_Pred) #need to reverse the log to the real values
head(predictions_train_lm)

##### Use the trained lm model to predict the price (Euro/sqm) in the valid dataset
lm_valid_sample_Pred <- predict(final_lm, valid.sample)
predictions_valid_lm <- exp(lm_valid_sample_Pred) #need to reverse the log to the real values
head(predictions_valid_lm)

##### Check how good is the model on the training set's real SOLD PRICES- correlation^2, RME and MAE
train.corr <- round(cor(lm_train_sample_Pred, train.sample$AV_PR_M), 2)
train.RMSE <- round(sqrt(mean((10 ^ lm_train_sample_Pred - 10 ^ train.sample$AV_PR_M)^2)))
train.MAE <- round(mean(abs(10 ^ lm_train_sample_Pred - 10 ^ train.sample$AV_PR_M)))
c(train.corr^2, train.RMSE, train.MAE)
# With all prep is: 0.1681 3.0000 0.0000 



# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(lm_valid_sample_Pred, valid.sample$AV_PR_M), 2)
valid.RMSE <- round(sqrt(mean((10 ^ lm_valid_sample_Pred - 10 ^ valid.sample$AV_PR_M)^2)))
valid.MAE <- round(mean(abs(10 ^ lm_valid_sample_Pred - 10 ^ valid.sample$AV_PR_M)))
c(valid.corr^2, valid.RMSE, valid.MAE)
# With all prep is: 0.1024 1.0000 0.0000

##### On the unseen dataset, the model accuracy is considerably lower. In this instance, due to the small data set cross-validation should be used, but vars selection needs to be auto.

# lmpreds <- round(predict(lm_model,newdata = testnum), 2)
# ## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
# ## fit may be misleading
head(train_data)
final_lm_predict <- predict(final_lm, test_data)
predictions_final_lm <- exp(final_lm_predict) #need to reverse the log to the real values
head(predictions_final_lm)

final_lm_price_predictions <- data.frame(Id = test_labels, AV_PR_M = (predictions_final_lm))
write.csv(final_lm_price_predictions, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_predictions_final_lm.csv", row.names = F)

# ##### LINEAR REGRESSION WITH CROSS VALIDATION
# 
# summary(lm_train) #A reminder that this dataset has the transformed variables with AV_PR_M amd no missimg values. This is what is needed for cross validation. 
# ##### Set parameters for REPEAT cross-validation
# ##### prepare training scheme
# 
# ##### CROSS VALIDATION
# 
# my_control <-trainControl(method="cv", number=5)
# head(my_control)
# 
# ##### REPEAT CROSS VALIDATION
# repeat_control <- trainControl(method="repeatedcv", number=10, repeats=5)
# head(repeat_control)
# 
# 
# ##### LINEAR MODEL, K-CROSS VALIDATION, FINAL (BEST) PREVIOUS VARIABLES ONLY, 100% SOLD PRICES
# final_lm_cv <- train(AV_PR_M~ AREA+ D_ARA+ D_BOG+ D_HAR+ D_ORD+ D_SPR+ P_WAT+ S_FER, data=lm_train, method="lm",metric="RMSE", maximize=FALSE,trControl= my_control)
# summary (final_lm_cv)
# 
# # Call:
# #   lm(formula = .outcome ~ ., data = dat)
# # 
# # Residuals:
# #   Min     1Q Median     3Q    Max 
# # -3.681 -0.357  0.020  0.393  2.667 
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)  -1.5173     0.0235  -64.61  < 2e-16 ***
# #   AREA         -0.1597     0.0262   -6.10  1.6e-09 ***
# #   D_ARA         0.1860     0.0242    7.68  4.2e-14 ***
# #   D_BOG         0.0199     0.0254    0.78   0.4328    
# # D_HAR        -0.1101     0.0255   -4.33  1.7e-05 ***
# #   D_ORD        -0.0563     0.0244   -2.31   0.0214 *  
# #   D_SPR        -0.0699     0.0240   -2.92   0.0036 ** 
# #   P_WAT        -0.0312     0.0389   -0.80   0.4228    
# # S_FER        -0.0748     0.0238   -3.14   0.0018 ** 
# #   ---
# #   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# # 
# # Residual standard error: 0.668 on 904 degrees of freedom
# # Multiple R-squared:  0.139,	Adjusted R-squared:  0.131 
# # F-statistic: 18.3 on 8 and 904 DF,  p-value: <2e-16
# 
# ##### Using cross validation on all of the sold prices, as opposed to building a training set out of 80% of them, the model provides slightly different results. The R2 value is lower (but much closer to the Multiple R2). Some of the previously statistically significant variables are now non-significant. 
# #Let's check for multicollinearity
# 
# final_lm_cv$bestTune
# final_lm_cv$results
# # intercept  RMSE Rsquared   MAE RMSESD RsquaredSD  MAESD
# # 1      TRUE 0.675    0.118 0.494 0.0463     0.0576 0.0249
# 
# #find out how many of the variables are not used in the model (and hence have coefficient 0).
# 
# 
# lmVarImp <- varImp(final_lm_cv,scale=F)
# lmImportance <- lmVarImp$importance
# 
# varsSelected <- length(which(lmImportance$Overall!=0))
# varsNotSelected <- length(which(lmImportance$Overall==0))
# 
# cat('lm used', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
# ## lm used 8 variables in its model, and did not select 0 variables. 
# 
# ##### Look at the importance of the variables used
# lmImportance
# 
# # Overall
# # AREA    6.096
# # D_ARA   7.676
# # D_BOG   0.785
# # D_HAR   4.327
# # D_ORD   2.305
# # D_SPR   2.915
# # P_WAT   0.802
# # S_FER   3.139
# 
# 
# ##### LINEAR MODEL, K-CROSS VALIDATION, ALL VARIABLES, 100% SOLD PRICES
# 
# ##### train the model
# fit_lm_cv <- train(AV_PR_M~. -D_COA -D_BOG -D_FEN -D_FOR -D_WET -D_WDV -D_RIV -P_BUI -P_WOO -S_ROC, data=lm_train, method="lm", trControl=my_control, tuneLength=5)
# 
# summary(fit_lm_cv) #R2 = 0.1415,	f = 5.064 
# fit_lm_cv$results 
# # intercept     RMSE  Rsquared       MAE     RMSESD RsquaredSD      MAESD
# # 1      TRUE 0.694877 0.1051309 0.5060674 0.05871435 0.04086707 0.02809232
# 
# dev.off()
# 
# ########
# #install.packages("DAAG")   # if not already installed 
# #library(DAAG)
# cv.lm(data = lm_train, form.lm = formula(AV_PR_M ~ .), m=5, dots = 
#         FALSE, seed=29, plotit=TRUE, printit=TRUE)
# summary(cv.lm)
# 
# 
# printOut <- capture.output(CV.list=cv.lm(data = lm_train, form.lm = formula(AV_PR_M ~ .), m=10, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE))
# # function to parse the output 
# # to be adapted if necessary for your needs
# GetValues <- function(itemName,printOut){
#   line <- printOut[grep(itemName,printOut)]
#   items <- unlist(strsplit(line,"[=]|  +"))
#   itemsMat <- matrix(items,ncol=2,byrow=TRUE)
#   vectVals <- as.numeric(itemsMat[grep(itemName,itemsMat[,1]),2])
#   return(vectVals)
# }
# 
# # get the Mean square values as a vector
# MS <- GetValues("Mean square",printOut)
# sum((MS)/10) #3.16
# 


##### SECTION 6: Lasso Regression Model

##### Use caret cross validation to find the best value for lambda, which is the only hyperparameter that needs to be tuned for the lasso model.

set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
head(my_control)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train_data, y=all$AV_PR_M[!is.na(all$AV_PR_M)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

# alpha lambda
#44     1 0.0225

##### Obtain the best tuned root mean squared error (RMSE)
min(lasso_mod$results$RMSE)
# 0.675
##### Obtain the best tuned Rsquared (R2)
max(lasso_mod$results$Rsquared[is.finite(lasso_mod$results$Rsquared)])
#0.126

#find out how many of the variables are not used in the model (and hence have coefficient 0).

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso used', varsSelected, 'variables in its model, and did not select', varsNotSelected, 'variables.')
## Lasso used 17 variables in its model, and did not select 20 variables. 

##### Look at the importance of the variables used
lassoImportance
# Overall
# AREA  1.18e-01
# D_ARA 1.45e-01
# D_BOG 0.00e+00
# D_COA 0.00e+00
# D_DRA 8.61e-03
# D_ELE 0.00e+00
# D_ENV 7.71e-03
# D_FEN 0.00e+00
# D_FOR 0.00e+00
# D_GVE 0.00e+00
# D_HAR 7.68e-02
# D_LDF 2.25e-03
# D_LCB 0.00e+00
# D_ORD 2.02e-02
# D_PAT 1.76e-03
# D_PRO 1.28e-02
# D_QUA 0.00e+00
# D_QKM 4.01e-02
# D_RIV 0.00e+00
# D_RDS 0.00e+00
# D_SAN 0.00e+00
# D_SCH 1.80e-05
# D_SPO 1.49e-02
# D_SPR 4.22e-02
# D_WAT 0.00e+00
# D_WET 0.00e+00
# D_WDV 0.00e+00
# P_ARA 0.00e+00
# P_BUI 0.00e+00
# P_GRA 5.03e-02
# P_PRO 0.00e+00
# P_WAT 3.40e-05
# P_WET 3.77e-05
# P_WOO 0.00e+00
# S_CLA 0.00e+00
# S_ROC 0.00e+00
# S_FER 6.16e-02


varImp(lasso_mod)
# Overall
# D_ARA 100.0000
# AREA   81.2478
# D_HAR  53.0111
# S_FER  42.5022
# P_GRA  34.7517
# D_SPR  29.1157
# D_QKM  27.7105
# D_ORD  13.9145
# D_SPO  10.3079
# D_PRO   8.8150
# D_DRA   5.9399
# D_ENV   5.3211
# D_LDF   1.5503
# D_PAT   1.2121
# P_WET   0.0260
# P_WAT   0.0234
# D_SCH   0.0124
# D_RIV   0.0000
# D_ELE   0.0000
# P_WOO   0.0000

##### Use the trained lasso model to predict the price (Euro/sqm) in the test_data dataset
LassoPred <- predict(lasso_mod, test_data)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)


##### SECTION 7: XGBoost model

##### Set up a grid that tunes the key parameters of max_depth and min_child_weight, as well as the eta (learning rate), in order to avoid overfitting

xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

##### Use the carat package to perform the hyperparameter tuning (let the package find the best hyperparameter values (using 5-fold cross-validation))

# 
# # This is a slow process
#xgb_caret <- train(x=train_data, y=all$AV_PR_M[!is.na(all$AV_PR_M)], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid)
xgb_caret$bestTune

#       nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 13    1000         4 0.01     0                1                3         1
##### Now work directly with the XGB package

##### Prepare the data in the correct format
label_train <- all$AV_PR_M[!is.na(all$AV_PR_M)]

# Put the test_data and train_data data into two seperate Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label= label_train)
dtrain #visualise
dtest <- xgb.DMatrix(data = as.matrix(test_data))
dtest #visualise

##### Bring across the best tuned values from the caret cross validation
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.01, #default = 0.3
  gamma=0,
  max_depth=4, #default=6
  min_child_weight=3, #default=1
  subsample=1,
  colsample_bytree=1)

##### Perform cross validation to determine the best number of rounds (for the given set of parameters).

xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 1000, nfold = 5, showsd = T, stratified = T, print_every_n = 40, early_stopping_rounds = 10, maximize = F)

#Stopping. Best iteration:
# [365]	train-rmse:0.454109+0.022387	test-rmse:0.656157+0.052883

#train the model using the best iteration found by cross validation
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 365 )#nrounds is taken from best iteration

XGBpred <- predict(xgb_mod, dtest)
predictions_XGB <- exp(XGBpred) #need to reverse the log to the real values
head(predictions_XGB)

##### Look at the importance of the variables used

cat('XGBoost used', xgb_mod$nfeatures, 'variables in its model')
## XGBoost used 37 variables in its model #It used them all

library(Ckmeans.1d.dp) #required for ggplot clustering
mat <- xgb.importance (feature_names = colnames(train_data),model = xgb_mod)

png(height=1000, width=1000, pointsize=15, file="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/PS_XGBoost_FEATURE_IMPORTANCE.png")
xgb.ggplot.importance(importance_matrix = mat[1:32], rel_to_first = TRUE)
dev.off()
#PRINT

# mat
# Feature         Gain        Cover   Frequency
# 1:    AREA 0.1814180641 0.1119181586 0.160034980
# 2:   D_COA 0.1245945510 0.0828567236 0.059903804
# 3:   S_FER 0.0729192246 0.0608838715 0.052033231
# 4:   D_RIV 0.0637225662 0.0368859107 0.039790118
# 5:   P_WOO 0.0586486369 0.0601634271 0.064713599
# 6:   D_FEN 0.0554146399 0.0509774099 0.061215566
# 7:   S_CLA 0.0541894175 0.0585624396 0.040227372
# 8:   D_LDF 0.0422418229 0.0299335520 0.021862702
# 9:   D_QKM 0.0374438111 0.0792657365 0.054219502
# 10:   D_ENV 0.0339186331 0.0215999905 0.027109751
# 11:   D_BOG 0.0307255394 0.0108740760 0.033668561
# 12:   D_SPO 0.0264342787 0.0572823517 0.041539134
# 13:   D_HAR 0.0255405065 0.0481869167 0.036292086
# 14:   D_QUA 0.0251466558 0.0403224164 0.038041102
# 15:   D_SCH 0.0246157962 0.0422407927 0.031482291
# 16:   P_GRA 0.0229935065 0.0280095582 0.027984259
# 17:   D_PRO 0.0177273727 0.0372531548 0.032356799
# 18:   P_PRI 0.0141873746 0.0064376552 0.013992129
# 19:   D_DRA 0.0139009813 0.0154853415 0.020550940
# 20:   D_SPR 0.0125462092 0.0235050252 0.018801924
# 21:   D_WDV 0.0106770229 0.0120874561 0.014866638
# 22:   D_PAT 0.0100953767 0.0139440399 0.025797989
# 23:   D_GVE 0.0097448563 0.0179422956 0.019239178
# 24:   D_ORD 0.0080777236 0.0214567442 0.019676432
# 25:   D_WAT 0.0072425270 0.0014731894 0.010056843
# 26:   D_BUI 0.0035411926 0.0008145375 0.008307827
# 27:   D_WET 0.0030993065 0.0132657267 0.009182335
# 28:   D_RDS 0.0030760511 0.0044413361 0.005247049
# 29:   D_ELE 0.0029985115 0.0069544652 0.005684303
# 30:   P_WAT 0.0012776781 0.0042650870 0.003060778
# 31:   S_ROC 0.0010260972 0.0005020641 0.001749016
# 32:   D_SAN 0.0008140684 0.0002085497 0.001311762
# Feature         Gain        Cover   Frequency


##### SECTION 8: Averaging predictions

#RMSE lm =0.618
#RMSE LASSO =0.675
#RMSE XGBOOST = 0.65
# Since all the models lm, lasso and XGBoost algorithms are very different, averaging predictions likely improves the scores. As the lm model does better regarding the cross validated RMSE score than XGBOOST and even more so than LASSO, I'm weighting lm*3, XGBOOST*2 and LASSO*1

sub_avg <- data.frame(Id = test_labels, AV_PR_M = (3*predictions_final_lm+2*predictions_XGB+predictions_lasso)/6)
head(sub_avg)
summary(sub_avg)

write.csv(sub_avg, file ="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/PROFIT_SALES/OUTPUT/ps_predictions_AVERAGES.csv", row.names = F)


##### SECTION 9: MAPPING
#png("example.png", res=300, unit="in", height=4, width = 6)

#ggplot()+
#  geom_sf(data = county, aes(fill=MNIMI), colour="black")

#dev.off()


HIIU <- st_read("HIIU_MAAKOND_20190101.shp")
st_crs(HIIU) <- 3301
HIIU <- HIIU %>% mutate(code = gsub(" maakond", "", MNIMI)) # 6

plot(HIIU["MNIMI"])

ggplot()+
  geom_sf(data = HIIU, aes(fill=MNIMI), colour="black")


GRAND_MAP <- st_read("GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE_TAX.shp")

GRAND_MAP_PROFIT <- subset(GRAND_MAP, TYPE_N = 1)

st_crs(GRAND_MAP_PROFIT) <- 3301
glimpse(GRAND_MAP_PROFIT)

GRAND_MAP_PROFIT <- GRAND_MAP_PROFIT %>% mutate(code = gsub(" FID_1", "", AV_PR_M)) # 6

ggplot()+
  geom_sf(data=GRAND_MAP_PROFIT, aes(fill=AV_PR_M))

GMP_PREDICTION <- cbind (GRAND_MAP_PROFIT, sub_avg)

ggplot()+
  geom_sf(data=GRAND_MAP_PROFIT, aes(fill=AV_PR_M))+
  scale_fill_gradientn(colours = c("yellow", "red1", "red4", "navyblue"))+
  labs(fill = "Population \ndensity")

################################


#Read shapefile with:
  
chn = readOGR("D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/RESIDENTIAL_SALES/OUTPUT","GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE_TAX") # first arg is path, second is shapefile name w/o .shp


my.shapefile <- readOGR(dsn="D:/THESIS/GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE/R/SCRIPTS/RESIDENTIAL_SALES/OUTPUT", layer="GRAND_TABLE_PROFIT_RES_ALLK_JOINS_FULL_PRICE_TAX")
plot(my.shapefile) # does it look ok?

glimpse(sub_avg)
?merge
joined <- merge(GRAND_MAP_PROFIT, sub_avg, by.x="FID_1", by.y="Id")

glimpse(joined)

summary(joined)
summary(GRAND_MAP_PROFIT)

ggplot()+
  geom_sf(data=joined, aes(fill=AV_PR_M.y))

ggplot()+
  geom_sf(data=joined, aes(fill=AV_PR_M.y))+
  scale_fill_gradientn(colours = c("yellow", "red1", "red4", "navyblue"))+
  labs(fill = "Population \ndensity")



library(rgdal)  
foo <- readOGR(dsn="myfolder",layer="myshapefileWithoutEnding")
foo@data$MY_VALUE <- myDataFrameWithOneColumn[,1]
writeOGR(foo, ".", myfilename, driver="ESRI Shapefile",overwrite_layer=TRUE) 


library(foreign)
dbfdata <- read.dbf("file.dbf", as.is = TRUE)
## add new attribute data (just the numbers 1 to the number of objects)
dbfdata$new.att <- 1:nrow(dbfdata)

## overwrite the file with this new copy
write.dbf(dbfdata, "file.dbf")

# Or read the geometry and attribute data with the rgdal package (so you could modify the relationships as well and create a completely new shapefile):
  
library(rgdal)
## read "/path/to/files/filename.shp"
shp <- readOGR("/path/to/files/", "filename")  

## add new attribute data (just the numbers 1 to the number of objects)
shp$new.att <- 1:nrow(shp)

## write out to a new shapefile
writeOGR(shp, "/path/to/files/", "filename2")  










est_map <- get_map("estonia", color="bw", zoom = 7)
?register_google

install.packages("tmap")
library(tmap)
data("NLD_prov")

tmap_mode('view')

tm_shape(HIIU) + tm_polygons('maakond') + tm_layout(basemaps = c('OpenStreetMap'))


list.files(pattern = ".shp")
file_county <- list.files(pattern = ".shp")
GRAND_MAP <- st_read(file_county)

GRAND_MAP_PROFIT <- subset(GRAND_MAP, TYPE_N = 1)

st_crs(GRAND_MAP_PROFIT) <- 3301
glimpse(GRAND_MAP_PROFIT)

GRAND_MAP_PROFIT <- GRAND_MAP_PROFIT %>% mutate(code = gsub(" FID_1", "", AV_PR_M)) # 6

plot(GRAND_MAP_PROFIT["AV_PR_M"])

data("GRAND_MAP_PROFIT")

tmap_mode('view')

tm_shape(GRAND_MAP_PROFIT) + tm_polygons('D_AIR') + tm_layout(basemaps = c('OpenStreetMap'))



cn <- gCentroid(NLD_prov)

pt <- spTransform(GRAND_MAP_PROFIT, CRS('+proj=longlat +ellps=3301 +datum=3301 +no_defs '))

base_map <- get_map(c(GRAND_MAP_PROFIT), maptype = "basemap", source = "osm", zoom = 7)

?get_map
NLD_prov_latlong <- spTransform(NLD_prov, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs '))

NLD_prov_latlong$id <- rownames(NLD_prov_latlong@data)

NLD <- fortify(NLD_prov_latlong, region = 'id')

NLD <- join(NLD,NLD_prov_latlong@data, by = 'id')

ggmap(base_map) + geom_polygon(data = GRAND_MAP_PROFIT,aes(x = long,y = lat, group = group, fill = population),alpha=0.5)









ggplot()+
  geom_sf(data = HIIU, colour="grey")+
  geom_sf(data = GRAND_MAP_PROFIT, aes(fill=AV_PR_M), colour="black")

ggplot()+
  geom_sf(data = HIIU, colour="grey")+
  geom_sf(data = GRAND_MAP_PROFIT, aes(colour = AV_PR_M), size=1) + scale_fill_gradientn(colours=c("khaki", "yellow", "red", "black"), labels=c(0, 1, 2, 3, 7), breaks=c(0, 1, 2, 3, 4))
u  