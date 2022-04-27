library(tree)
library(ggplot2)
library(InformationValue)
install.packages("recipes", dependencies = TRUE)
library(caret)
scores
mean(scores$h_score)
sd(scores$h_score)
min(scores$h_score)
max(scores$h_score)
median(scores$h_score) 
length(scores$h_score) 

cor(scores)
scores$victory <- ifelse(scores$h_score >=4, "Yes", "No")
View(scores)
scores$victory <- as.factor(scores$victory)
scores <- scores[,-1]
View(scores)
set.seed(2)
train <- sample(1:nrow(scores), nrow(scores)*0.6)
validation <- -train
training_data <- scores[train,]
validation_data <- scores[validation,]
test_victory = scores$victory[validation]
tree_model = tree(victory ~., training_data )
plot(tree_model, type="uniform")
text(tree_model, pretty=0)
tree_pred <- predict(tree_model, validation_data, type="class")
mean(tree_pred != test_victory)
cm <- confusionMatrix(data=tree_pred, reference = test_victory)
cm

plot(pruned, type="uniform")
text(pruned, pretty=0)
