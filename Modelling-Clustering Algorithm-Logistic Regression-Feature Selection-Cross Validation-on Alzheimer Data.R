# Load libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(cluster)
#install.packages(factoextra)
library(factoextra)
library(gridExtra)
library(caret)
#install.packages("xtable")
library(xtable)

# ===========================================================================
# Load data
Alzheimer <- read.csv('project data.csv')
head(Alzheimer)
str(Alzheimer)


# Convert M/F into numeric values
Alzheimer$M.F <- ifelse(Alzheimer$M.F == 'M', 1, 
                        ifelse(Alzheimer$M.F == 'F', 0, NA))

# Confirm the conversion
head(Alzheimer)


# Remove rows with Group = 'Converted'
Alzheimer <- Alzheimer %>%
  filter(Group != 'Converted')

# Remove missing values
Alzheimer <- na.omit(Alzheimer)

# Generate summary of Alzheimer
summary(Alzheimer)


# ========================================================================
# Select all numeric variables
attach(Alzheimer)
numeric_vars <- c('Age', 'EDUC', 'SES', 'MMSE', 'CDR', 'eTIV', 'nWBV', 'ASF')
numeric_vars

# Find standard deviation of variables
sds <- apply(Alzheimer[, numeric_vars], 2, sd)
print(sds)

# Create appropriate plots
ggplot(Alzheimer,
       aes(x = Group, y = Age, fill = as.factor(M.F))) +
  geom_boxplot() +
  labs(x = 'Group', y = 'Age', title = paste('Boxplot of Demented and Nondemented based on Age and Gender'), fill = 'Gender (M.F)') +
  scale_fill_manual(
    values = c("0" = "tomato1", "1" = "lightseagreen" ),
    labels = c("0" = "Female", "1" = "Male"))



# Convert M.F to factor 
Alzheimer$M.F <- as.factor(ifelse(M.F == 1, 'Male', 'Female'))

gender_G <- ggplot(Alzheimer,
                   aes(x = M.F,
                       fill = Group)) +
  geom_bar(position = 'dodge', color = 'black') +
  geom_text(aes(label = ..count..), stat = 'count', vjust = 0.5, colour = 'black') +
  labs(x = 'Gender', y = 'Count',
       title = paste('Barchart of Gender by Demented vs Nondemented')) 

ggplotly(gender_G)


# ========================================================================

# To get number of demented and non-demented of male and females
Alzheimer %>%
  group_by(M.F) %>%
  count(Group)
# M.F Group           n
# <dbl> <chr>       <int>
# 0 Demented       51
# 0 Nondemented   129
# 1 Demented       76
# 1 Nondemented    61


# ========================================================================
# To get number of demented and non-demented of male and females
Alzheimer %>%
  group_by(M.F) %>%
  count(M.F)
# M.F     n
#   <dbl> <int>
# 0   180
# 1   137

# ========================================================================
#To get the proportion of demented and non-demented in male and females
# proportion of demented male
md <- 76/137
print(md)
# 0.5547445

# proportion of non-demented male
m_nd <- 61/137
print(m_nd)
# 0.4452555

# proportion of demented female
fd <- 51/180
print(fd)
# 0.2833333

# proportion of non-demented female
f_nd <- 129/180
print(f_nd)
# 0.7166667

# ========================================================================
# Convert M/F into numeric values
Alzheimer$M.F <- ifelse(Alzheimer$M.F == 'Male', 1, 
                        ifelse(Alzheimer$M.F == 'Female', 0, NA))

# Convert Group variable to numeric values
Alzheimer$Group <- ifelse(Alzheimer$Group == 'Demented', 1,
                          ifelse(Alzheimer$Group == 'Nondemented', 0, NA))
head(Alzheimer)
mv <- any(is.na(Alzheimer))
print(mv)
# [1] FALSE

# Similarity measure
# Visualise the distance matrix using the get_dist and fviz_dist from the R
# package factoextra
distance.Euclidean <- get_dist(Alzheimer)
fviz_dist(distance.Euclidean, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

distance.corr <- get_dist(Alzheimer, stand = TRUE, method = "pearson")
fviz_dist(distance.corr, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# ========================================================================
# Standardize features
scaled_A_vars <- scale(Alzheimer)

# Determining the optimal number of clusters
fviz_nbclust(scaled_A_vars, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)


# K-Means Clustering
set.seed(123)
kmeans2 <- kmeans(scaled_A_vars, centers = 2, nstart = 20)
kmeans3 <- kmeans(scaled_A_vars, centers = 3, nstart = 20)
kmeans4 <- kmeans(scaled_A_vars, centers = 4, nstart = 20)
kmeans3

# To visualise the results the fviz_cluster function can be used:
fviz_cluster(kmeans2, data = scaled_A_vars, stand = FALSE)
fviz_cluster(kmeans3, data = scaled_A_vars, stand = FALSE)
fviz_cluster(kmeans4, data = scaled_A_vars, stand = FALSE)


f1 <- fviz_cluster(kmeans2, geom = "point", data = scaled_A_vars) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = scaled_A_vars) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = scaled_A_vars) + ggtitle("k = 4")


grid.arrange(f1, f2, f3, nrow = 2)

cluster_means <- xtable(kmeans3$centers)

print(cluster_means)

# ========================================================================
# Implement feature selection on the data set 
attach(Alzheimer)
y_Group <- as.numeric(Alzheimer[,1])
X <- Alzheimer[,2:10]

model1 <- glm(y_Group~.,data=X)
summary(model1)

step1 <- step(model1,method="backward")
summary(step1)
# y_Group ~ M.F + Age + EDUC + CDR + nWBV + ASF (Features selected)

model2 <- lm(y_Group~1,data=X)
step2 <- step(model2, scope=~ M.F + Age + EDUC + SES + MMSE + CDR+ eTIV + nWBV + ASF,
            method="forward")
summary(step2)
# y_Group ~ CDR + M.F + eTIV + EDUC (Features selected)

b_model <- lm(y_Group ~ M.F + Age + EDUC + CDR + nWBV + ASF)
summary(b_model)

f_model <- lm(y_Group ~ CDR + M.F + eTIV + EDUC)
summary(f_model)

anova(b_model, f_model)

# ========================================================================
# Convert Group variable to factor
Alzheimer$Group <- as.factor(Alzheimer$Group)

# Cross Validation (CV)
# For 5-fold CV
trControl <- trainControl(method = "cv", number = 5)

#lda
lda.fit <- train(Group ~ CDR + M.F + eTIV + EDUC,
                 method = "lda",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = Alzheimer)

lda.pred <- predict(lda.fit,Alzheimer)
t1 <- table(lda.pred, Alzheimer$Group)
confusionMatrix(lda.fit)
confusionMatrix(t1)

# ========================================================================
#glm
glm.fit <- train(Group ~ CDR + M.F + eTIV + EDUC,
                 method = "glm",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = Alzheimer)

glm.pred <- predict(glm.fit,Alzheimer)
t2 <- table(glm.pred, Alzheimer$Group)
confusionMatrix(glm.fit)
confusionMatrix(t2)


# ========================================================================
#qda
qda.fit <- train(Group ~ CDR + M.F + eTIV + EDUC,
                 method = "qda",
                 trControl = trControl,
                 metric = "Accuracy",
                 data = Alzheimer)

qda.pred <- predict(qda.fit,Alzheimer)
t3 <- table(qda.pred, Alzheimer$Group)
confusionMatrix(qda.fit)
confusionMatrix(t3)

# ========================================================================
#knn
knn.fit <- train(Group ~ CDR + M.F + eTIV + EDUC,
                 method = "knn",
                 tuneGrid = expand.grid(k = 1:10),
                 trControl = trControl,
                 metric = "Accuracy",
                 data = Alzheimer)

knn.pred <- predict(knn.fit,Alzheimer)
t4 <- table(knn.pred, Alzheimer$Group)
confusionMatrix(knn.fit)
confusionMatrix(t4)
# =============================================================================
