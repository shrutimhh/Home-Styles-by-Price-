# Home-Styles-by-Price-
# Bar Chart Comparison of Home Styles by Price 

labs(title = "Bar Chart Comparison of Home Styles by Price",
y = "Probability",
x = "Home Styles",
fill = "Price") +
scale_fill_manual(values = c("black" , "grey")) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
library("readxl")
library("ggplot2")
Generation_data = read_excel(file.choose())
Generation_data
crosstab = table(Generation_data$Generation, Generation_data$`Leave`)
crosstab
# Calculate probabilities
prob_data <- prop.table(crosstab, margin = 1) # calculates the probabilities of each re
prob_data = data.frame(prob_data)
prob_data
# Rename columns
colnames(prob_data) = c("Generation", "Leave", "Probability")
prob_data
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey")) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rlang::last_error()
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey")) +
#theme_bw() +
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()
)
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey"))
View(prob_data)
# Compute the chi-square test
chi_test = chisq.test(crosstab)
chi_test #output the result
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "count", position = "stack") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey"))
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey"))
prob_data
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation))
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge")
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge")
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey" , "red"))
install.packages("tidyr")
library(readxl)
library(tidyr)
ethics_data <- read_excel(file.choose())
ethics_data
# Reshape the data from wide to long format
eth_data <- eth_data %>%
pivot_longer(cols = c(MManagers, MReaserchers, Advertising),
names_to = "Individual", values_to = "Ethics_Score")
# Reshape the data from wide to long format
eth_data <- ethics_data %>%
pivot_longer(cols = c(MManagers, MReaserchers, Advertising),
names_to = "Individual", values_to = "Ethics_Score")
# Reshape the data from wide to long format
eth_data <- ethics_data %>%
pivot_longer(cols = c(MManagers, MResearchers, Advertising),
names_to = "Individual", values_to = "Ethics_Score")
# Step 2: Exploratory data analysis
# Descriptive statistics
summary(ethics_data) #individual column summary
summary(eth_data) #total data summary
View(eth_data)
View(eth_data)
View(ethics_data)
# Data Visualization
boxplot(Ethics_Score ~ Individual, data = pl_data,
main = "Individuals Ethics Score", xlab = "Jobs",
ylab = "Ethics Score")
# Data Visualization
boxplot(Ethics_Score ~ Individual, data = eth_data,
main = "Individuals Ethics Score", xlab = "Jobs",
ylab = "Ethics Score")
# Step 3: Compute anova test statistic
anova <- aov(Ethics_Score ~ Individual, data = eth_data)
summary(anova)
View(anova)
# Step 4: Pairwise comparison
# H0: u1 = u3
# Ha: u1 =/ u3
pairwise.t.test(eth_data$Ethics_Score, eth_data$Individual, p.adj = "bonferroni")
library("readxl")
time_df <- read_excel(file.choose())
time_df
summary(time_df)
# Clean the data to remove missing datapoints
Internet_df <- subset(time_df, select = Internet)
Internet_df <- na.omit(Internet_df)
#Create a vector
Talking <- time_df$Talking
Talking
Internet <- Internet_df$Internet
Internet
# Calculate the sample mean for the two times
mean_talking <- mean(Talking)
mean_talking
mean_internet <- mean(Internet)
mean_internet
# Calculate the same variance for the two times
var_talking <- var(Talking)
var_talking
var_internet <- var(Internet)
var_internet
sqrt(var_talking)
sqrt(var_internet)
# Step 2 - compute the f-test. the function "var.test" helps us to
# perform the F-test
ftest <- var.test(Talking, Internet, alternative = "greater")
ftest
#install.packages("ggplot2")
library("readxl")
library("ggplot2")
FLhomes_data = read_excel(file.choose())
FLhomes_data = read_excel(file.choose())
FLhomes_data
#install.packages("ggplot2")
library("readxl")
library("ggplot2")
FLhomes_data = read_excel(file.choose())
FLhomes_data
View(FLhomes_data)
# Compute the cross-tabulation of the data
crosstab = table(FLhomes_data$Price, FLhomes_data$Style)
crosstab
# Calculate probabilities
prob_data <- prop.table(crosstab, margin = 1) # calculates the probabilities of each re
prob_data = data.frame(prob_data)
prob_data
# Rename columns
colnames(prob_data) = c("Price", "Style", "Probability")
prob_data
# Create bar chart
ggplot(prob_data, aes(x = Style, y = Probability, fill = Price)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Home Styles by Price",
y = "Probability",
x = "Home Styles",
fill = "Price") +
scale_fill_manual(values = c("black" , "grey")) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Compute the chi-square test
chi_test = chisq.test(crosstab)
chi_test #output the result
library("readxl")
# Load the data
home_data <- read_excel(file.choose())
# Load the data
home_data <- read_excel(file.choose())
home_data
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
xtab
# Compute the chi-sqaure test
chi_test <- chisq.test(xtab)
chi_test
# Compute the pairwise comparisons
n <- apply(xtab, 1, sum) #help us compute sample size for each group
pairwise_comp <- pairwise.prop.test(xtab, n = n, p.adjust.method = "bonferroni")
pairwise_comp
library("readxl")
library("ggplot2")
Generation_data = read_excel(file.choose())
Generation_data
crosstab = table(Generation_data$Generation, Generation_data$`Leave`)
crosstab
# Calculate probabilities
prob_data <- prop.table(crosstab, margin = 1) # calculates the probabilities of each re
prob_data = data.frame(prob_data)
prob_data
# Rename columns
colnames(prob_data) = c("Generation", "Leave", "Probability")
prob_data
# Create bar chart
ggplot(prob_data, aes(x = Leave, y = Probability, fill = Generation)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Generation that would leave job",
y = "Probability",
x = "Leave",
fill = "Generation") +
scale_fill_manual(values = c("black" , "grey" , "red"))
# Compute the chi-square test
chi_test = chisq.test(crosstab)
chi_test #output the result
library("readxl")
# Import data
travel_df <- read_excel(file.choose())
travel_df
summary(travel_df)
# Count the number of samples
samp_size <- nrow(travel_df)
samp_size
# Create the vector
cost <- travel_df$Cost
cost
# Calculate the sample variance using the function "var"
var_cost <- var(cost)
var_cost
library("readxl")
# Import data
travel_df <- read_excel(file.choose())
travel_df
summary(travel_df)
# Count the number of samples
samp_size <- nrow(travel_df)
samp_size
# Create the vector
cost <- travel_df$Cost
View(travel_df)
library("readxl")
# Import data
travel_df <- read_excel(file.choose())
travel_df
summary(travel_df)
# Count the number of samples
samp_size <- nrow(travel_df)
samp_size
# Create the vector
cost <- travel_df$Cost
cost
# Calculate the sample variance using the function "var"
var_cost <- var(cost)
var_cost
sqrt(var_cost)
# Calculate the sample standard deviation
sd_cost <- sd(cost)
sd_cost
# Compute the chi-squared statistics
test_chi <- ((samp_size-1)*var_cost)/321 # 321 is o^2
test_chi
# Compute the p-value using the function "pchisq"
p_value <- 2 * pchisq(test_chi, samp_size-1, lower.tail = FALSE) #since it upper
list(test_chi, p_value)
a <- 260.20
s <- 70.69
n <- 20
error <- qnorm(0.975) * s/sqrt(n)
a - error
a + error
library(readxl)
library(tidyr)
ethics_data <- read_excel(file.choose())
ethics_data
# Reshape the data from wide to long format
eth_data <- ethics_data %>%
pivot_longer(cols = c(MManagers, MResearchers, Advertising),
names_to = "Individual", values_to = "Ethics_Score")
# Step 2: Exploratory data analysis
# Descriptive statistics
summary(ethics_data) #individual column summary
summary(eth_data) #total data summary
# Data Visualization
boxplot(Ethics_Score ~ Individual, data = eth_data,
main = "Individuals Ethics Score", xlab = "Jobs",
ylab = "Ethics Score")
# Step 3: Compute anova test statistic
anova <- aov(Ethics_Score ~ Individual, data = eth_data)
summary(anova)
# Step 4: Pairwise comparison
# H0: u1 = u3
# Ha: u1 =/ u3
pairwise.t.test(eth_data$Ethics_Score, eth_data$Individual, p.adj = "bonferroni")
# Load the data
home_data <- read_excel(file.choose())
home_data
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
library("readxl")
# Load the data
home_data <- read_excel(file.choose())
home_data
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
View(home_data)
# Load the data
home_data <- read_excel(file.choose())
library("readxl")
# Load the data
home_data <- read_excel(file.choose())
home_data
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
xtab
# Compute the chi-sqaure test
chi_test <- chisq.test(xtab)
chi_test
# Compute the pairwise comparisons
n <- apply(xtab, 1, sum) #help us compute sample size for each group
pairwise_comp <- pairwise.prop.test(xtab, n = n, p.adjust.method = "bonferroni")
pairwise_comp
# Load the data
home_data <- read_excel(file.choose())
home_data
library("readxl")
# Load the data
home_data <- read_excel(file.choose())
home_data
# Compute the cross-tabulation of the data
xtab <- table(home_data$`Home Type`, home_data$`Likely Repurchase`)
xtab
# Compute the chi-sqaure test
chi_test <- chisq.test(xtab)
chi_test
# Compute the pairwise comparisons
n <- apply(xtab, 1, sum) #help us compute sample size for each group
pairwise_comp <- pairwise.prop.test(xtab, n = n, p.adjust.method = "bonferroni")
pairwise_comp
library("readxl")
# Import data
travel_df <- read_excel(file.choose())
travel_df
summary(travel_df)
# Count the number of samples
samp_size <- nrow(travel_df)
samp_size
# Calculate the sample variance using the function "var"
var_cost <- var(cost)
# Create the vector
cost <- travel_df$Cost
cost
# Calculate the sample variance using the function "var"
var_cost <- var(cost)
var_cost
sqrt(var_cost)
# Calculate the sample standard deviation
sd_cost <- sd(cost)
sd_cost
# Compute the chi-squared statistics
test_chi <- ((samp_size-1)*var_cost)/321 # 321 is o^2
test_chi
# Compute the p-value using the function "pchisq"
p_value <- 2 * pchisq(test_chi, samp_size-1, lower.tail = FALSE) #since it upper
list(test_chi, p_value)
library(readxl)
library(tidyr)
ethics_data <- read_excel(file.choose())
ethics_data
# Reshape the data from wide to long format
eth_data <- ethics_data %>%
pivot_longer(cols = c(MManagers, MResearchers, Advertising),
names_to = "Individual", values_to = "Ethics_Score")
View(eth_data)
# Step 2: Exploratory data analysis
# Descriptive statistics
summary(ethics_data) #individual column summary
summary(eth_data) #total data summary
# Data Visualization
boxplot(Ethics_Score ~ Individual, data = eth_data,
main = "Individuals Ethics Score", xlab = "Jobs",
ylab = "Ethics Score")
# Step 3: Compute anova test statistic
anova <- aov(Ethics_Score ~ Individual, data = eth_data)
summary(anova)
# Step 4: Pairwise comparison
# H0: u1 = u3
# Ha: u1 =/ u3
pairwise.t.test(eth_data$Ethics_Score, eth_data$Individual, p.adj = "bonferroni")
a <- 260.20
s <- 70.69
n <- 20
error <- qnorm(0.975) * s/sqrt(n)
a - error
a + error
# Load the packages into R
library(ggpubr) #helps us create results that are publication ready
library(tidyverse) # for data manupulation and visualization
library(broom) #tudy model output
library("readxl")
library(gclus) # Allows us to have our scatter plot with correlation embedded
theme_set(theme_pubr())
# Step 1: Import data into Rstudio
Sal_df <- read_excel(file.choose())
head(Sal_df)
glimpse(Sal_df)
# Step 2: Descriptive statistics and data visualization
summary(Sal_df)
# Scatter plot, correlation coefficient and visualization
pairs(~Experience + Testscore + Salary, data = Sal_df) #simple correlation plot
corr <- abs(cor(Sal_df)) #correlation in absolute value
corr #outputs the correlation results, r = sample correlation coefficient
#Step 3: Building the model
# Regression equation: Salary = b0 + b1*Experience + b2*Testscore
model <- lm(Salary ~ Experience + Testscore, data = Sal_df)
model
# Step 4: Model Summary
summary(model) #focus on p-value to determine if there's any significance
# let's look at only the coefficients
summary(model)$coefficient
# ANOVA Table
anova(model)
#install.packages("ggplot2")
library("readxl")
library("ggplot2")
FLhomes_data = read_excel(file.choose())
#install.packages("ggplot2")
library("readxl")
library("ggplot2")
FLhomes_data = read_excel(file.choose())
FLhomes_data
# Compute the cross-tabulation of the data
crosstab = table(FLhomes_data$Price, FLhomes_data$Style)
crosstab
# Calculate probabilities
prob_data <- prop.table(crosstab, margin = 1) # calculates the probabilities of each re
prob_data = data.frame(prob_data)
prob_data
# Rename columns
colnames(prob_data) = c("Price", "Style", "Probability")
prob_data
# Create bar chart
ggplot(prob_data, aes(x = Style, y = Probability, fill = Price)) +
geom_bar(stat = "identity", position = "dodge") +
labs(title = "Bar Chart Comparison of Home Styles by Price",
y = "Probability",
x = "Home Styles",
fill = "Price") +
scale_fill_manual(values = c("black" , "grey")) +
theme_bw() +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# Compute the chi-square test
chi_test = chisq.test(crosstab)
chi_test #output the result
library(ggpubr)
library(tidyverse)
library("readxl")
theme_set(theme_pubr())
Brok_df <- read_excel(file.choose())
head(Brok_df)
summary(Brok_df)
plot(Brok_df$Speed, Brok_df$Satisfaction, main= "Scatter diagram",
xlab= "Speed", ylab= "Satisfaction", pch=19)
cor(Brok_df$Speed, Brok_df$Satisfaction)
model <- lm(Satisfaction ~ Speed, data = Brok_df)
model
ggplot(Brok_df, aes(Speed, Satisfaction)) +
geom_point() +
stat_smooth(method = lm, se = FALSE)
summary(model)
library("readxl")
library("caret") # helps us to split our data into trainign and testing sets
library(pscl) # gets the pseudo r-square for logistic regression
library(Hmisc) #used to find the correlation and its p-values
# Step : import and summarize the daata
hospital_df <- read_excel(file.choose())
library(ggpubr)
library(tidyverse)
library("readxl")
theme_set(theme_pubr())
Brok_df <- read_excel(file.choose())
head(Brok_df)
summary(Brok_df)
plot(Brok_df$Speed, Brok_df$Satisfaction, main= "Scatter diagram",
xlab= "Speed", ylab= "Satisfaction", pch=19)
cor(Brok_df$Speed, Brok_df$Satisfaction)
model <- lm(Satisfaction ~ Speed, data = Brok_df)
model
ggplot(Brok_df, aes(Speed, Satisfaction)) +
geom_point() +
stat_smooth(method = lm, se = FALSE)
summary(model)
confint(model)
load("~/.RData")
load("~/.RData")
load("~/.RData")
