---
title: "R Notebook"
output: html_notebook
---

```{r}
# ML
library(rpart)
library(nutshell)
library(caret)
library(rattle)
library(rpart.plot)
```

```{r}
# evaluation for the models
caculate_rms_error <- function(mdl, train, test, yval) {
  train.yhat <- predict(object = mdl, newdata = train)
  test.yhat <- predict(object = mdl, newdata = test)
  train.y <- with(train, get(yval))
  test.y <- with(test, get(yval))
  train.err <- sqrt(mean((train.yhat - train.y)^2)) # RMSE
  test.err <- sqrt(mean((test.yhat - test.y)^2)) # RMSE
  c(train.err = train.err, test.err = test.err)
}
```

```{r}
# split up data set
auto_pv.train.indices <- sample(1:nrow(auto_pv),1393841)
auto_pv.test.indices <- setdiff(rownames(auto_pv), auto_pv.train.indices)
training <- auto_pv[auto_pv.train.indices,] 
testing <- auto_pv[-auto_pv.train.indices,]

# model
auto_pv_model <- rpart(
  value ~ nb_pageviews + abandon_state, data = training
)
```
```{r}
plot(auto_pv_model, uniform = TRUE, compress = TRUE, lty = 3, branch = .7)
text(auto_pv_model, all = TRUE, digits = 7, use.n = TRUE, cex = .8, xpd = TRUE)
rsq.rpart(auto_pv_model)
# train/test comparsion
caculate_rms_error(auto_pv_model, training, testing, "value")
```

```{r}
auto_pv_model_train <- train(
  value ~ nb_pageviews + Date + abandon_state, method = "rpart", data = training
)
```
```{r}
fancyRpartPlot(auto_pv_model_train$finalModel)
```

```{r}
# normalization
preprocess.training <- preProcess(training, method = c("center", "scale"))
scaled.training <- predict(preprocess.training, training)
scaled.testing <- predict(preprocess.training, testing)
```




