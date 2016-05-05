library(readr)
library(plyr)
library(dplyr)
library(stringr)
library(data.table)
library(caret)
library(DMwR)
library(randomForest)




####################################################################################
# Training a model on the geocoded NFIRS data combined with the ACS data
# Model predicts whether a fire resulted in a death or no death given ACS features
# Use the weights from the learned model to predict on all 74,000+ census tracts
####################################################################################

rm(list = ls())
gc()



########################################
# Predicting injury vs. other
########################################

modeling_injury_dataset_imp = read_csv("/users/nickbecker/Documents/R Workspace/modeling_injury_dataset_imp.csv")

modeling_injury_dataset_imp = modeling_injury_dataset_imp %>%
  mutate(target = as.factor(target)) %>%
  as.data.frame()

# # Removing low variance variables for speed optimization
# nzv = nearZeroVar(modeling_injury_dataset_imp)
# nzv
# colnames(modeling_injury_dataset_imp)[nzv]
# 
# modeling_injury_dataset_imp_simple = modeling_injury_dataset_imp[, -nzv]


# Logistic regression
logit_injury <- glm(target ~ ., data = modeling_injury_dataset_imp[, -c(1)], family = "binomial")

summary(logit_injury)
logit_injury$coefficients

preds <- data.frame(predictions = predict(logit_injury, modeling_injury_dataset_imp[, -c(1,2)], type = "response"))
preds = preds %>%
  mutate(pred_label = ifelse(predictions > 0.5, "other", "injury"))

head(preds)

confusionMatrix(preds$pred_label, modeling_injury_dataset_imp[, 2])




### Bring in the relevant ACS features for prediction
acs_tract_data = read_csv("/users/nickbecker/Downloads/acs_tract_data.csv")
glimpse(acs_tract_data)
acs_tract_data = acs_tract_data %>% as.data.frame()

system.time(
  acs_tract_data <- centralImputation(acs_tract_data)
)

acs_injury_preds <- data.frame(prediction = predict(logit_injury, acs_tract_data, type = "response"))
acs_injury_preds = acs_injury_preds %>%
  mutate(pred_label = ifelse(prediction > 0.5, "other", "injury"))

head(acs_injury_preds)

acs_injury_preds = cbind(acs_injury_preds, tractid = acs_tract_data$tractid)



write.csv(acs_injury_preds, "/users/nickbecker/Documents/R Workspace/acs_tract_injury_predictions.csv",
          row.names = FALSE)













########################################
# Predicting death vs. other
########################################

modeling_death_dataset_imp = read_csv("/users/nickbecker/Documents/R Workspace/modeling_death_dataset_imp.csv")

modeling_death_dataset_imp = modeling_death_dataset_imp %>%
  mutate(target = as.factor(target))



# 
# # Removing low variance variables for speed optimization
# nzv = nearZeroVar(modeling_injury_dataset_imp)
# nzv
# colnames(modeling_injury_dataset_imp)[nzv]
# 
# modeling_injury_dataset_imp_simple = modeling_injury_dataset_imp[, -nzv]
# 


# Logistic regression
logit_death <- glm(target ~ ., data = modeling_death_dataset_imp[, -c(1)], family = "binomial")

summary(logit_death)
logit_death$coefficients

preds <- data.frame(predictions = predict(logit_death, modeling_death_dataset_imp[, -c(1,2)], type = "response"))
preds = preds %>%
  mutate(pred_label = ifelse(predictions > 0.5, "other", "death"))
head(preds)

confusionMatrix(preds$pred_label, modeling_death_dataset_imp[[2]])


### Bring in the relevant ACS features for prediction
acs_tract_data = read_csv("/users/nickbecker/Downloads/acs_tract_data.csv")
glimpse(acs_tract_data)
acs_tract_data = acs_tract_data %>% as.data.frame()

system.time(
  acs_tract_data <- centralImputation(acs_tract_data)
)

acs_death_preds <- data.frame(prediction = predict(logit_death, acs_tract_data, type = "response"))
acs_death_preds = acs_death_preds %>%
  mutate(pred_label = ifelse(prediction > 0.5, "other", "death"))

head(acs_death_preds)

acs_death_preds = cbind(acs_death_preds, tractid = acs_tract_data$tractid)

write.csv(acs_death_preds, "/users/nickbecker/Documents/R Workspace/acs_tract_death_predictions.csv",
          row.names = FALSE)




####################################################
# Combining for an overall Injury/Death Risk score
####################################################


combined_predictions = data.frame(tractid = acs_tract_data$tractid,
                      death_prob = acs_death_preds$prediction,
                      injury_prob = acs_injury_preds$prediction)

# Created the final risk score from by averaging and taking 1 - the score
# Doing this because we predicted on "other" so higher probability -> other, not death/injury
# This makes the higher risk tracts the ones with higher risk probabilities, not lower

combined_predictions = combined_predictions %>%
  mutate(death_injury_risk_prob = 1 - ((death_prob + injury_prob) / 2)) %>%
  select(tractid, death_injury_risk_prob)

quantile(combined_predictions$death_injury_risk_prob, probs = seq(0, 1, .1))
head(combined_predictions)

write.csv(combined_predictions, "/users/nickbecker/Documents/R Workspace/acs_tract_death_injury_risk_scores.csv",
          row.names = FALSE)












####################################################
#         Random Forest Predictions
####################################################
set.seed(12)

# Random Forest Injury
rf_injury <- randomForest(target ~ ., data = modeling_injury_dataset_imp[, -c(1)],
                          ntree = 250)

# Variable Importance
varImpPlot(rf_injury)

variable_importance_df = varImp(rf_injury)
variable_importance_df = variable_importance_df %>%
  mutate(variable = row.names(variable_importance_df))  %>%
  arrange(desc(Overall)) %>%
  filter(row_number() <= 50) %>%
  arrange(Overall)

variable_importance_df$variable = factor(variable_importance_df$variable,
                                         levels = variable_importance_df[[2]])

rf_injury_imp_plot = ggplot(variable_importance_df,
       aes(x = variable, y = Overall)) +
  geom_bar(stat = "identity") +
  ggtitle("Random Forest Injury Importance Plot") +
  theme(text = element_text(size=20)) +
  coord_flip()

png("/users/nickbecker/Documents/R Workspace/model_3a_rf_injury_imp_plot.png", width = 1200, height = 900)
print(rf_injury_imp_plot)
dev.off()


# Predictions
acs_injury_preds <- data.frame(prediction_labels = predict(rf_injury, acs_tract_data, type = "response"),
                               prediction_prob = predict(rf_injury, acs_tract_data, type = "prob"))

acs_injury_preds = cbind(acs_injury_preds, tractid = acs_tract_data$tractid)

head(acs_injury_preds)

write.csv(acs_injury_preds, "/users/nickbecker/Documents/R Workspace/acs_tract_injury_rf_predictions.csv",
          row.names = FALSE)






# Random Forest Death
rf_death <- randomForest(target ~ ., data = modeling_death_dataset_imp[, -c(1)],
                         ntree = 250)


# Variable Importance
varImpPlot(rf_death)

variable_importance_df = varImp(rf_death)
variable_importance_df = variable_importance_df %>%
  mutate(variable = row.names(variable_importance_df))  %>%
  arrange(desc(Overall)) %>%
  filter(row_number() <= 50) %>%
  arrange(Overall)

variable_importance_df$variable = factor(variable_importance_df$variable,
                                         levels = variable_importance_df[[2]])

rf_death_imp_plot = ggplot(variable_importance_df,
                            aes(x = variable, y = Overall)) +
  geom_bar(stat = "identity") +
  theme(text = element_text(size=20)) +
  ggtitle("Random Forest Death Importance Plot") +
  coord_flip()

png("/users/nickbecker/Documents/R Workspace/model_3a_rf_death_imp_plot.png", width = 1200, height = 900)
print(rf_death_imp_plot)
dev.off()


# Predictions
acs_death_preds <- data.frame(prediction_labels = predict(rf_death, acs_tract_data, type = "response"),
                              prediction_prob = predict(rf_death, acs_tract_data, type = "prob"))

acs_death_preds = cbind(acs_death_preds, tractid = acs_tract_data$tractid)

head(acs_death_preds)

write.csv(acs_death_preds, "/users/nickbecker/Documents/R Workspace/acs_tract_death_rf_predictions.csv",
          row.names = FALSE)



####################################################
# Combining for an overall Injury/Death Risk score
####################################################


combined_predictions_rf = data.frame(tractid = acs_tract_data$tractid,
                                  death_prob = acs_death_preds$prediction_prob.death,
                                  injury_prob = acs_injury_preds$prediction_prob.injury)

# Created the final risk score from by averaging two RF models

combined_predictions_rf = combined_predictions_rf %>%
  mutate(death_injury_rf_risk_prob = ((death_prob + injury_prob) / 2)) %>%
  select(tractid, death_injury_rf_risk_prob)

quantile(combined_predictions_rf$death_injury_rf_risk_prob, probs = seq(0, 1, .1))
head(combined_predictions_rf)

write.csv(combined_predictions_rf, "/users/nickbecker/Documents/R Workspace/acs_tract_death_injury_rf_risk_scores.csv",
          row.names = FALSE)






