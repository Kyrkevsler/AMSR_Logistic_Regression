setwd('C:/Users/admin/Documents/Krysler Files/R files/Logistic Regression')

cancer_data = read.csv('cancer.csv')[, -1]
cancer_data


library(datasets)
library(ggplot2)
library(tidyverse)
library(gmodels)
library(car)


# Fitting the model
lg_model <- glm(cancer ~ ., family = "binomial", data = cancer_data)
summary(lg_model)

# The predictor variable age, gender, and fruit are significant since they are lower than p-value = 0.05.

lg_model$coefficients

# INTERPRETATIONS:

# A one-unit increase in the age is associated with an increase in log odds of cancer by 0.1175121 units assuming that gender and fruit are fixed.

# If a person is either male(1) or female(0), it is associated with an increase in log odds of cancer by 2.5736980 units assuming that age and fruit are fixed.

# A one-unit increase in fruit intake is associated with a decrease in log odds of cancer by -2.0468690 units assuming that age and gender are fixed.


######################## Prediction 

# Predict the probability of any given (age, gender, bmi, smoking, exercise, fruit, veg):

newdata <- data.frame(age= c(58, 42), gender= c(0, 1), bmi= c(19.29561, 25.256798),
                      smoking= c(0, 0), exercise= c(0, 0), fruit= c(2, 1), veg= c(4, 6))

(prediction <- predict(lg_model, newdata, type = "response"))


# Predict the classification of cancer using the model with the p > 0.5 as Cancerous.

prob <- predict(lg_model, type="response")
prob

Predicted_data <- ifelse(prob < 0.5, "Not Cancerous", "Cancerous")
Predicted_data

Actual_data <- factor(cancer_data$cancer[-66], labels= c("Not Cancerous", "Cancerous"))
Actual_data

CrossTable(Actual_data, Predicted_data, prop.r= TRUE, prop.c= TRUE,
           prop.t= FALSE, prop.chisq= FALSE)


# Graph of the Model

c_data <- cancer_data$cancer[-66]
df <- data.frame(c_data, prob)

ggplot(df, aes(prob, c_data)) +
  geom_point(alpha= 0.5, color = "red") +
  stat_smooth(method= "glm", color= "steelblue", se= T, method.args = list(family=binomial))+
  ggtitle(label = "Logistic Model Plot", subtitle = waiver())+
  xlab("Predicted Value")+
  ylab("Probability of Cancer")


##################### Linearity Assumption

# 1. Remove qualitative variables from the original data frame and bind the logit values to the data if there are any.

# Select only numeric predictors

cancer_data[, c(2, 8)] <- list(NULL) 
cancer_data %>% select_if(is.numeric)
predictors <- colnames(cancer_data)


# Bind the logit and tidying the data for plot

cancer_data <- cancer_data[-66, ] %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


# 2. Create the scatter plots:

ggplot(cancer_data, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5, col = "red") +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_wrap(~predictors, scales = "free_y")

########################## Influential Values

plot(lg_model, which = 4, id.n = 5, col = "steelblue")

# Di (Cook’s distance) value of more than 1 indicates an influential value

# Observed Value Points 6, 21, 26, 40, and 47 are maybe considered as influential values.


########################## Multicollinearity

vif(lg_model)

# In our data, no predictor exceeded the value of 5 or 10, therefore, there is no multicollinearity.

