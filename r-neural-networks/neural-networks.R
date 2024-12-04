library(neuralnet)
library(caret)

uu <- "https://raw.githubusercontent.com/vmoprojs/DataLectures/master/concrete.csv"
concrete <- read.csv(url(uu))
# set.seed(12345)
# concrete <- concrete[order(runif(nrow(concrete))), ]
concrete <- concrete[-1]
str(concrete)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)

concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

concrete_model <- neuralnet(strength ~ cement + slag +
                              ash + water + superplastic +
                              coarseagg + fineagg + age,
                            data = concrete_train)

plot(concrete_model)

nn_test = function(data)
{
  concrete_model <- neuralnet(strength ~ cement + slag +
                                ash + water + superplastic +
                                coarseagg + fineagg + age,
                              data = data)
}

predicted_strength <- predict(concrete_model, concrete_test[1:8])
cor(predicted_strength, concrete_test$strength)

concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

plot(concrete_model2)

predicted_strength2 <- predict(concrete_model2, concrete_test[1:8])
cor(predicted_strength2, concrete_test$strength)

denormalize <- function(x_norm,x_orig)
{
  mmin <- min(x_orig);mmax <- max(x_orig)
  denorm_val <- x_norm*(mmax-mmin)+mmin
  return(denorm_val)
}

R2(denormalize(predicted_strength2,concrete$strength),
   denormalize(concrete_test$strength,concrete$strength))

concrete_model3 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic +
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = c(5,2,3),act.fct = "tanh")

plot(concrete_model3)
predicted_strength3 <- predict(concrete_model3, concrete_test[1:8])


R2(denormalize(predicted_strength3,concrete$strength),
   denormalize(concrete_test$strength,concrete$strength))

tune.grid.neuralnet <- expand.grid(
  layer1 = 2:3,
  layer2 = 2:3,
  layer3 = 2:3
)

control <- trainControl(method="repeatedcv", number=5)

nnet_caret <- caret::train(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, 
                           data = concrete_train,
                           method = 'neuralnet', 
                           linear.output = TRUE, 
                           tuneGrid = tune.grid.neuralnet,
                           metric = "RMSE",
                           trControl = control)


nnet_caret$finalModel$tuneValue # valores de las capas

predicted_strength4 <- predict(nnet_caret,concrete_test[1:8])

c(m1 =R2(denormalize(predicted_strength,concrete$strength),
         denormalize(concrete_test$strength,concrete$strength))
  ,
  m2 = R2(denormalize(predicted_strength2,concrete$strength),
          denormalize(concrete_test$strength,concrete$strength))
  ,
  m3 = R2(denormalize(predicted_strength3,concrete$strength),
          denormalize(concrete_test$strength,concrete$strength))
  ,
  m4 = R2(denormalize(predicted_strength4,concrete$strength),
          denormalize(concrete_test$strength,concrete$strength)))

