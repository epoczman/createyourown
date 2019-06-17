# Load the libraries required for this project
# Can change to load if not loaded
library(tidyverse)
library(caret)
library(rpart)

# Import the data and store in the object "mushrooms"
url <- "https://raw.githubusercontent.com/epoczman/createyourown/master/mushrooms.csv"
download.file(url, "mushrooms.csv")
mushrooms <- read.csv("mushrooms.csv")

# Examine the dataset
str(mushrooms)
head(mushrooms)
sapply(mushrooms, function(m) length(unique(m)))
levels(mushrooms$veil.type)

# Verify if there are any missing values
sum(is.na(mushrooms))
sapply(mushrooms, function(m) unique(m))
unique(mushrooms$stalk.root)
round(sum(mushrooms$stalk.root == "?")/nrow(mushrooms), digits = 2)*100

# Remove variable with no predictive value
# Relevel class variable to have level p (poisonous) as the first level
mushrooms_new <- mushrooms%>% select(-"veil.type", -"odor", -"spore.print.color", -"stalk.root") %>% mutate(class = relevel(class, "p"))

#Establish the prevalence of outcome variable: proportion of poisonous mushrooms in the total sample.
mushrooms_new %>% summarize(poisonous = round(mean(class == "p"), digits = 2), edible = round(mean(class == "e"), digits = 2))
round(mean(mushrooms_new$class=="p"), digits = 2)*100
mushrooms_new %>% ggplot(aes(x = class)) +  geom_bar(aes(y=..prop.., group = 1))

# Define the outcome variable
y <- mushrooms_new$class

# Split the dataset to create training and test set
test_index <- createDataPartition(y, times = 1, p = 0.1, list = FALSE)
train_set <- mushrooms_new[-test_index,]
test_set <- mushrooms_new[test_index,]
nrow(train_set)
nrow(test_set)

# Model 1
train_rpart <- train(class ~ ., method = "rpart", data = train_set) 
ggplot(train_rpart, highlight = TRUE)

train_rpart

# Visualise the decision tree using rpart.plot package
library(rpart.plot)
rpart.plot(train_rpart$finalModel)

# Create a plot to visualise how using the two variables selected help identify poisonous mushrooms
train_set %>% mutate(stalk = ifelse(stalk.surface.above.ring=="k", "silky stalk surface", "other stalk surface")) %>% ggplot(aes(stalk, gill.size, color = class)) + geom_point(position = "jitter")

# Calculate model accuracy using test set
confusionMatrix(predict(train_rpart, test_set), test_set$class)$overall["Accuracy"]

# Verify model sensitivity as it is more important to detect poisonous mushroom than edible one
confusionMatrix(predict(train_rpart, test_set), test_set$class)

# Model 2
library(randomForest)
train_rf <- train(class ~ ., method = "rf", data = train_set) 

# Visualise the outcome
ggplot(train_rf, highlight = TRUE)

# Plot variables with the highest importance
plot(varImp(train_rf), top=20)

# Print accuracy of the selected model
confusionMatrix(predict(train_rf, test_set), test_set$class)$overall["Accuracy"]

# Confirm sensitivity of the model is 1 (expected as accuracy = 1)
confusionMatrix(predict(train_rf, test_set), test_set$class)




