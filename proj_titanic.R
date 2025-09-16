# Titanic
library(tidyverse)
library(titanic)

summary(titanic_train)
count(titanic_train)

# check NA using sapply()
sapply(titanic_train, function(x) sum(is.na(x)))

sapply(titanic_train, function(x) sum(x == ""))

# copy original dataset to my object
titanic_df <- titanic_train

# Feature Selection:: Drop Not use Columns
titanic_df$PassengerId <- NULL
titanic_df$Name <- NULL
titanic_df$Ticket <- NULL
titanic_df$Cabin <- NULL
titanic_df$Embarked <- NULL

# Feature Selection:: Drop Age.na
titanic_df <- na.omit(titanic_df)

summary(titanic_df)
count(titanic_df) #714 rows

# titanic <- data.frame(titanic)
# head(titanic)
# typeof(titanic$PassengerId)

# Data Transformation
titanic_df <- titanic_df %>%
  select(pclass = Pclass, sex = Sex, age = Age, sibsp = SibSp, parch = Parch, fare = Fare, survived = Survived) %>%
  mutate(sex = if_else(sex=='male', 0, 1),
         surv = if_else(survived==0, 'Died', 'Survived'))
head(titanic_df)

# boxplot:: Age
titanic_df %>% 
  ggplot(aes(age)) +
  geom_boxplot()

# boxplot:: Age+surv
titanic_df %>% 
  ggplot(aes(x=age, y=surv)) +
  geom_boxplot()

# boxplot:: Fare
titanic_df %>% 
  ggplot(aes(fare)) +
  geom_boxplot()

# boxplot:: Fare+surv
titanic_df %>% 
  ggplot(aes(x=fare, y=surv)) +
  geom_boxplot()

# bar:: pclass
titanic_df %>% 
  ggplot(mapping = aes(x=pclass)) +
  geom_bar(fill = 'salmon', alpha=0.6)

# bar:: pclass+surv
titanic_df %>% 
  ggplot(mapping = aes(x=pclass)) +
  geom_bar(fill = 'salmon', alpha=0.6) +
  facet_wrap(~surv, ncol=2)

# bar:: sex
titanic_df %>% 
  ggplot(mapping = aes(x=sex)) +
  geom_bar(fill = 'salmon', alpha=0.6)

# bar:: sex+surv
titanic_df %>% 
  ggplot(mapping = aes(x=sex)) +
  geom_bar(fill = 'salmon', alpha=0.6) +
  facet_wrap(~surv, ncol=2)

# bar:: sibsp
titanic_df %>% 
  ggplot(mapping = aes(x=sibsp)) +
  geom_bar(fill = 'salmon', alpha=0.6)

# bar:: sibsp+surv
titanic_df %>% 
  ggplot(mapping = aes(x=sibsp)) +
  geom_bar(fill = 'salmon', alpha=0.6) +
  facet_wrap(~surv, ncol=2)

# bar:: parch
titanic_df %>% 
  ggplot(mapping = aes(x=parch)) +
  geom_bar(fill = 'salmon', alpha=0.6)

# bar:: parch+surv
titanic_df %>% 
  ggplot(mapping = aes(x=parch)) +
  geom_bar(fill = 'salmon', alpha=0.6) +
  facet_wrap(~surv, ncol=2)

# bar:: survived
titanic_df %>% 
  ggplot(mapping = aes(x=survived)) +
  geom_bar(fill = 'salmon', alpha=0.6)

# Drop Label Variable titanic_df$surv
titanic_df$surv <- NULL

# Test Multivalidate
model_all <- glm(survived ~ ., data = titanic_df, family = 'binomial')
summary(model_all)


# Feature Selection:: Drop Not use Cols
titanic_df$parch <- NULL
titanic_df$fare <- NULL


# Split data 80:20
set.seed(137)
n <- nrow(titanic_df)
id <- sample(1:n, size = n*0.8)
train_df <- titanic_df[id, ]
test_df <- titanic_df[-id, ]

# train model
logit_model <- glm(survived ~ pclass + sex + age + sibsp, data = train_df, family = "binomial")
p_train <- predict(logit_model, type = "response") # probability %

train_df$p_train <- p_train
train_df$pred <- if_else(p_train >= 0.5, 1, 0)
train_df$pred_name <- if_else(p_train >= 0.5, "Survived", "Died")
tail(train_df, 10)

## confusion matrix table() cross
conf <- table(train_df$pred, train_df$survived, 
              dnn=c('predicted', 'actual'))
conf

## evaluation
accu <- (conf[1,1] + conf[2,2]) / sum(conf)
precision <- conf[2,2] /  (conf[2,1] + conf[2,2])
recall <- conf[2,2] / (conf[1,2] + conf[2,2])
f1score <- 2 *((precision * recall) / (precision + recall))

# res_tr <- mean(titanic_df$survived == titanic_df$pred)
# res_tr

cat('Accuracy : ', accu,
    '\nPrecision : ', precision,
    '\nRecall : ', recall,
    '\nF1 Score : ', f1score)

# -------------------------------------------------------------------------------

# another df for test
# titanic_test
# titanic_gender_model

# test_df <- titanic_test 
# test_df <- na.omit(test_df)

# join df + drop na
# test_df <- titanic_test %>%
#   inner_join(titanic_gender_model, by="PassengerId") %>%
#   na.omit()

# Data Transformation
# test_df <- test_df %>%
#   select(PassengerId, pclass = Pclass, sex = Sex, age = Age, sibsp = SibSp, parch = Parch, fare = Fare) %>%
#   mutate(sex = if_else(sex=='male', 0, 1))
# head(test_df)

# test model
p_test <- predict(logit_model, newdata = test_df, type = "response") # probability %

test_df$p_test <- p_test
test_df$pred <- if_else(p_test >= 0.5, 1, 0)
test_df$pred_name <- if_else(p_test >= 0.5, "Survived", "Died")
tail(test_df, 10)

# res_te <- mean(test_df$survived == test_df$pred)
# res_te

# cat("Mean Accuracy Trian:", res_tr,
#     "\nMean Accuracy Test:", res_te)

# join df + drop na
# test_df <- test_df %>%
#   inner_join(titanic_gender_model, by="PassengerId")
# tail(test_df, 10)

## confusion matrix table() cross
conf <- table(test_df$pred, test_df$survived, 
              dnn=c('predicted', 'actual'))
conf

## evaluation
accu <- (conf[1,1] + conf[2,2]) / sum(conf)
precision <- conf[2,2] /  (conf[2,1] + conf[2,2])
recall <- conf[2,2] / (conf[1,2] + conf[2,2])
f1score <- 2 *((precision * recall) / (precision + recall))

cat('Accuracy : ', accu,
    '\nPrecision : ', precision,
    '\nRecall : ', recall,
    '\nF1 Score : ', f1score)


## ---------------------------
# summary(logit_model)
## logit(p) = 3.07026 - 1.29951 * pclass + 2.44141 * sex - 0.04553 * age - 0.37714 * sibsp

coefs <- coef(logit_model)
coefs

pclass_ <- 2
sex_ <- 0
age_ <- 47
sibsp_ <- 1

# Calc Realtime
survived_pred <- coefs[[1]] + coefs[[2]]*pclass_ + coefs[[3]]*sex_ + coefs[[4]]*age_ + coefs[[4]]*sibsp_ 
survived_pred

# Calc Realtime -> Prop
survived_prop <- exp(survived_pred) / (1 + exp(survived_pred))
survived_prop

# Calc Realtime -> Prop -> Survived?
is_survived <- if_else(survived_prop >= 0.5, 1, 0)
is_survived

# Calc Realtime -> Prop -> SurvivedName?
is_survived_name <- if_else(survived_prop >= 0.5, "Survived", "Died")
is_survived_name

# -----------------------------------------
conf
conf_df <- as.data.frame.matrix(conf)


rownames(conf_df) <- c("predicted_died", "predicted_survived")
colnames(conf_df) <- c("actual_died", "actual_survived")
conf_df

accu
value <- accu
remaining <- 1 - value

# create DataFrame
data <- data.frame(
  category = c("Value", "Remaining"),
  value = c(value, remaining)
)

# create stacked bar chart
ggplot(data, aes(x = "", y = value, fill = category)) +
  geom_bar(stat = "identity", width = 1) +
  theme_void() +
  scale_fill_manual(values = c("lightgray", "skyblue")) +
  theme(legend.position = "none")