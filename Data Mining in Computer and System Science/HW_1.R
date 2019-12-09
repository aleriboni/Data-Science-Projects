# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# ------------------------------------------------------------------------------------------------------ #
# use read.table(), type ?read.table for help
adult_db <- read.table(file = "http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = FALSE,
                       sep = ",", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = "?")


# Assign attribute names (column names) to the data we just imported
# Attribute names are in separate file "adult.names", scroll down to the bottom of this file
# Attribute names such as ("age", "workclass", "fnlwgt",...)
# Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
# this is the class attribute, so we just name it as "class"

colnames(adult_db) = c("age",
                       "workclass",
                       "fnlwgt",
                       "education",
                       "education_num",
                       "marital_status",
                       "occupation",
                       "relationship",
                       "race",
                       "sex",
                       "capital_gain",
                       "capital_loss",
                       "hours_per_week",
                       "native_country",
                       "class")


# 2. Check for missing values
# Write code to plot missingness and count missing values each attribute has
# Inorder to plot missingness, you need to first install "Amelia" package
# Hint: use "apply" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# --------------------------------------------------------------------------------------------------------------- #

library(Amelia)
head(adult_db)
# plot missing values in data

# count number of missing values in all attributes

#count.missval(adult_db)
apply(adult_db, MARGIN = 2, FUN = function(x) sum(is.na(x)))
missmap(adult_db, y.cex = 0.5, x.cex = 0.8, rank.order = FALSE, legend = FALSE, margins = c(7,2))


# Delete records (rows) with any missing value
adult_db_nomiss <- na.omit(adult_db)# ****** YOUR CODE HERE *******
apply(adult_db_nomiss, MARGIN = 2, FUN = function(x) sum(is.na(x)))



# 3. We will take only small chunk of the data for our purpose.
# So, randomly select 1000 records from among 30 thousand records in the dataset.
# ------------------------------------------------------------------------------- #
set.seed(145)
idx = sample(1:nrow(adult_db_nomiss),1500)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL



# 3a. Examine attributes of the dataset
# Plot histogram for numeric attribute "age", with 100 (for <=50K) and 50(for >50K) breaks, 
# show main title and attribute name on the plot.
# --------------------------------------------------------------------------------------------------------

hist(adult_db_lim$age[which(adult_db_lim$class == "<=50K")], breaks = 100, 
     xlab = "age", ylab = "frequency", col = rgb(0,1,0,0.5), main = "Age of Adults")

hist(adult_db_lim$age[which(adult_db_lim$class == ">50K")], breaks = 50, 
     xlab = "age", ylab = "frequency", col = rgb(1,0,0,0.5), add=T)

legend(x = 60, y = 40, legend = c("<=50K", ">50K"),
       col=c(rgb(0,1,0,0.5), rgb(1,0,0,0.5)), pch = 20, cex = 0.75)


# ******************************************************* #

# 3b. Plot barchart for categorical attribute "relationship", 
# show legend, attribute name and main title for the plot.
color_barplot <- c("red", "green", "blue", "brown","pink","black")
names_relationship <- levels(as.factor(adult_db_lim$relationship))


barplot(table(adult_db_lim$relationship), col=color_barplot,
        main = "Relationship of adults",
        names.arg = names_relationship,
        cex.names = 0.8, horiz = FALSE, mar=c(6,6,2,2), las=2)

legend(x = 5, y = 600, legend = names_relationship,
       col=color_barplot, pch = 20, cex = 0.75)

# ************************************************* #

# 3c. Plot a boxplot for attribute "Age" for groups with earning "<=50K", ">50K"
# ------------------------------------------------------------------------------

boxplot(age ~ class, data = adult_db_lim, pch=20, col="red", main = "Age of adults",
        names =c("<=50k",">50k"), las = 1)



# 4 Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_cat <- adult_db_lim[,c("class")]



# Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes
adult_db_num_std <- apply(adult_db_numeric, 2, scale)
  

# we can check the mean and standard deviation of the standardized data
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)

# 5a. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# ----------------------------------------------------------------------------------

pr.out <- prcomp(adult_db_num_std, scale = TRUE, center = TRUE)
adult_db_pca <- pr.out$x

class_cat[which(class_cat == "<=50K")] <- 2
class_cat[which(class_cat != 2)] <- 3
plot(adult_db_pca[,1:2], col = class_cat, pch = 20, main = "First two PC")

legend(x = 0, y = 8, legend = c("<=50K", ">50K"),
       col=c(2,3), pch = 20, cex = 0.75)

# 5b. Plot percentage of the variance explained by principal components
# ---------------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"
# proportion of variance explained by principal components

pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)

# plot variance explained
# two plots side by side (1 row 2 columns), divide plot region into two

par(mfrow = c(1,2), oma = c(1,1,2,1), las = 1)
plot(pve, xlab = "PC", ylab = "Variance", type = "b", ylim = c(0,1), col = "red")
plot(cumsum(pve), xlab = "PC", ylab = "Cumulative", type = "b", ylim = c(0,1), col = "red")
mtext("Proportion of Variance explained by PC", outer = TRUE)
par(mfrow = c(1,1))

# 5c. write answer for this as a comment using #
# ------------------------------------------------------------------------------
# How many principal components are needed to explain 50% and 90% of variance respectively
# Answer:
# I need 3 principal components to explain 50% and all the 6 principal components to explain 90% of variance.
# It possible to make this consideration by looking the graphs of the previous point and, in particular, 
# using the command cumsum(pve).

