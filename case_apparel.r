---
title: "Data Challenge| Chris.C"
output:
  html_notebook: default
  html_document: default
---

```{r, warning=FALSE}
library(readr)
library(dplyr)
library(reshape2)
library(qdapRegex)
library(ggplot2)
library(lubridate)
library(class)
```

```{r, warning=FALSE}
# load files
puma_clo <- read_csv("C:/Users/User/Pictures/R_2017/Retviews/DataScientistChallenge/DataScientistChallenge/clothes_puma.csv")

puma_evo <- read_delim(
  "C:/Users/User/Pictures/R_2017/Retviews/DataScientistChallenge/DataScientistChallenge/evolutions_puma.csv",
  ";",
  escape_double = FALSE,
  col_types = cols(size_name = col_character(), 
                   timestamp = col_character(),
                   value = col_number()), 
  trim_ws = TRUE)
```

```{r, warning=FALSE}
# filter
puma_clo_select <- puma_clo %>% select(product_id, main_title, category)
# merge table
puma_evo_clo <- left_join(puma_evo, puma_clo_select, by = "product_id")

# extract cateory
puma_evo_clo$category <- rm_between(puma_evo_clo$category, '[', ']', extract=TRUE)

puma_evo_clo$category.a <- cities <- gsub("^(.*?),.*", "\\1", puma_evo_clo$category)
puma_evo_clo$category.a <- gsub("\"","", puma_evo_clo$category.a, fixed = TRUE)

puma_evo_clo$category.b <- rm_between(puma_evo_clo$category, ',', ',', extract=TRUE)
puma_evo_clo$category.b <- gsub("c","", puma_evo_clo$category.b, fixed = TRUE)
puma_evo_clo$category.b <- gsub("(","", puma_evo_clo$category.b, fixed = TRUE)
puma_evo_clo$category.b <- gsub(")","", puma_evo_clo$category.b, fixed = TRUE)
puma_evo_clo$category.b <- gsub("\"","", puma_evo_clo$category.b, fixed = TRUE)
puma_evo_clo$category.b <- gsub("\\","", puma_evo_clo$category.b, fixed = TRUE)

puma_evo_clo$category.c <- sub('.*,\\s*', '', puma_evo_clo$category)
puma_evo_clo$category.c <- gsub("\"","", puma_evo_clo$category.c, fixed = TRUE)

# ymd for date
puma_evo_clo$ymd <- as.POSIXct(puma_evo_clo$timestamp)
```

```{r, warning=FALSE}
# selct price and stock_decrease as main analysis types, to analyse the sale history and prediction
# explore price distribution
puma_evo_clo_price <- puma_evo_clo %>%
  filter(type == "price")
ggplot(puma_evo_clo_price) +
  geom_bar(aes(x = category.a, y = value), position="dodge", stat= "identity") +
  coord_flip()
```

```{r, warning=FALSE}
# explore stock_decrease distribution
puma_evo_clo_decrease <- puma_evo_clo %>%
  filter(type == "stock_decrease")
ggplot(puma_evo_clo_decrease) +
  geom_bar(aes(x = category.a, y = value), position="dodge", stat= "identity") +
  coord_flip()
```

```{r, warning=FALSE}
# compare the relationship between price and stock_decrease
puma_evo_clo_decrease <- puma_evo_clo_decrease %>%
  select(type, value)
names(puma_evo_clo_decrease) <- c("type_decrease","value_decrease")
  # merge price and stock_decrease as a pair
puma_evo_clo_price <- cbind(puma_evo_clo_price, puma_evo_clo_decrease)
```
```{r, warning=FALSE}
  # price x decrease
ggplot(puma_evo_clo_price) +
  geom_point(aes(x = value, y = value_decrease, color = category.a)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  ylim(0, 25)
  # ymd x decrease
ggplot(puma_evo_clo_price) +
  geom_point(aes(x = ymd, y = value_decrease, color = category.a)) +
  theme(legend.position = "top", legend.title = element_blank()) +
  ylim(0, 25)

# plot shows most stock decrease clustering in price 10-40 and a smaller group in 70-110 
# shoes are the most hot product type, skirt is the lowest
```

```{r, warning=FALSE}
# Weekdays analysis, comparing latest week with previous week
  # create date type
puma_evo_clo_price$ymd <- ymd(puma_evo_clo_price$ymd)
puma_evo_clo_price$ww <-  format(puma_evo_clo_price$ymd, format = "%a")
puma_evo_clo_price$dd <-  format(puma_evo_clo_price$ymd, format = "%d")
  # previous week
puma_evo_clo_price_agg <- filter(puma_evo_clo_price, ymd >= "2017-05-21" & ymd <= "2017-05-27")
puma_evo_clo_price_agg <- aggregate(cbind(value_decrease) ~ dd+ww, data = puma_evo_clo_price_agg, FUN = sum)
puma_evo_clo_price_agg <- aggregate(cbind(value_decrease) ~ ww, data = puma_evo_clo_price_agg, FUN= mean)
puma_evo_clo_price_agg$sd <- sd(puma_evo_clo_price_agg$value_decrease)
  # current week
puma_evo_clo_price_mm <- filter(puma_evo_clo_price, ymd >= "2017-05-28" & ymd <= "2017-06-03")
puma_evo_clo_price_mm <- aggregate(cbind(value_decrease) ~ dd+ww, data = puma_evo_clo_price_mm, FUN = sum)
puma_evo_clo_price_mm <- aggregate(cbind(value_decrease) ~ ww, puma_evo_clo_price_mm, FUN = mean)
  # merge
puma_evo_clo_price_agg <- left_join(x = puma_evo_clo_price_agg, y= puma_evo_clo_price_mm, by= "ww")
  # plot
puma_weekdays <- ggplot(puma_evo_clo_price_agg, aes(x = ww, y = value_decrease.x)) +
  geom_crossbar(aes(ymin = value_decrease.x - sd, ymax = value_decrease.x + sd), width = .2,
                position = position_dodge(width = .5),
                size = 1, color = "#cccccc", fill = "#cccccc") +
  geom_point(aes(x = ww, y = value_decrease.y), shape = 20, size = 7, color = "#1abc9c") +
  labs(x = "", y = "", title = "Weekday Comparsion - stock_decrease") +
  theme(plot.title = element_text(hjust = 0)) +  
  theme(legend.position ="none",
        axis.text.x = element_text(face = "bold", size = 10, color = "#666666"),
        axis.text.y = element_text(face = "bold", size = 7, color = "#666666"),
        axis.line = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank()) +
  scale_x_discrete(limits = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), position = "top") +
  scale_y_continuous(labels = comma, position = "right") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())    
puma_weekdays <- ggdraw(add_sub(puma_weekdays, " Dots = this week  Bars = previous week +/- sd",
                        size = 8, fontface = "italic"))
puma_weekdays
```

```{r, warning=FALSE}
# heat calendar - example in May
puma_calender <- puma_evo_clo_price %>%
  filter(ymd >= "2017-05-01" & ymd <= "2017-05-31") %>%
  mutate(weekday = format(ymd, '%a'),
         weeknum = format(ymd, '%W'),
         dd = format(ymd, '%d')) %>%
  group_by(weekday, weeknum,dd) %>%
  arrange(weekday, weeknum, dd) %>%
  summarise(stock_decrease = sum(value_decrease)) %>%
  # plot
  ggplot(aes(x = weekday, y = as.numeric(weeknum))) +
  geom_tile(aes(fill = stock_decrease), color = "#ffffff") +
  geom_text(aes(label = dd), color = "#030303", size= 3.5, face= "bold") +
  labs(x= "", y= "", title= "Hot stock_decrease Day") +
  theme(axis.text.x = element_text(face = "bold", size= 10, color = "#666666"),
        axis.text.y = element_blank()) +
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.title=element_blank()) +
  theme(legend.position="right", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 9, face= "bold")) +
  theme(plot.title = element_text(hjust = 0)) +
  scale_x_discrete(limits = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), position = "bottom") +
  scale_fill_gradient(low = "#ffffff", high = "#1abc9c") +
  scale_y_continuous(trans = "reverse") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),
        panel.grid.major.x = element_blank())
puma_calender
```

```{r, warning=FALSE}
# ML 
# create a sale variable for prediction 
puma_evo_clo_price$sale <- ifelse(puma_evo_clo_price$value_decrease > 0, "TRUE", "FALSE")
puma_evo_clo_price <- puma_evo_clo_price %>% select(value, value_decrease, sale)
  # formatting
puma_evo_clo_price[, c(1:2)] <- sapply(puma_evo_clo_price[, c(1:2)], as.numeric)
puma_evo_clo_price$sale <- as.logical(puma_evo_clo_price$sale)
puma_evo_clo_price$value[is.na(puma_evo_clo_price$value)] <- 0
puma_evo_clo_price$value[is.nan(puma_evo_clo_price$value)] <- 0
puma_evo_clo_price$value[which(!is.finite(puma_evo_clo_price$value))] <- 0
puma_evo_clo_price$value_decrease[is.na(puma_evo_clo_price$value_decrease)] <- 0
puma_evo_clo_price$value_decrease[is.nan(puma_evo_clo_price$value_decrease)] <- 0
puma_evo_clo_price$value_decrease[which(!is.finite(puma_evo_clo_price$value_decrease))] <- 0

# split up training set and testing set
puma_evo_clo_price.train <- sample(1:nrow(puma_evo_clo_price),3628)
puma_evo_clo_price.test <- setdiff(rownames(puma_evo_clo_price), puma_evo_clo_price.train)
puma_training <- puma_evo_clo_price[puma_evo_clo_price.train,] 
puma_testing <- puma_evo_clo_price[-puma_evo_clo_price.train,]
```

```{r, warning=FALSE}
# apply knn model
puma_prediction_knn <-  knn(train = puma_training, test = puma_testing, cl = puma_training$sale, k = 1)

table(puma_prediction_knn, puma_testing$sale)
mean(puma_prediction_knn == puma_testing$sale)
```

```{r, warning=FALSE}
# KNN plot
knn_ggplot <- function(train, test, k) {
    KNN <- knn(train = train, test = test, cl = puma_training$sale, k)
    test$predict <- KNN
    # change factor to numeric
    test$z <- c(0, 1)[sapply(test$predict, as.numeric)]
    
    title = paste('k=', as.character(k), sep = '')
    gg <- ggplot(data = test, aes(value, value_decrease)) +
            geom_point(aes(colour = predict), size = 0.5) +
            geom_contour(aes(z = z), colour ='black', size = 0.5) +
            ylim(0, 25)
    # training points
    gg <- gg + geom_point(data = train, aes(x = value, y = value_decrease,
                                          colour = as.factor(sale), shape = 'x'))
    return(gg)
}

knn_ggplot(puma_training, puma_testing, 10)
```


```{r, warning=FALSE}
# consider different number of neighbors in roder to measure prediction %
puma_prediction_knn_1_10 <- rep(0, 10)
k <- 1:10
for(x in k){
  puma_prediction_knn <- knn(puma_training, puma_testing, puma_training$sale, k = x)
  puma_prediction_knn_1_10[x] <- mean(puma_prediction_knn == puma_testing$sale)
}
plot(k, puma_prediction_knn_1_10, type = "b")

# k = 1 give the best prediction %
```

```{r, warning=FALSE}
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
```

```{r, warning=FALSE}
# regression_tree
puma_tree <- rpart(value_decrease ~ value, x = TRUE, data = puma_training)

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

caculate_rms_error(puma_tree, puma_training, puma_testing, "sale")
  #plot(puma_tree, uniform = TRUE, compress = TRUE, lty = 3, branch = .7)
  #text(puma_tree, all = TRUE, digits = 7, use.n = TRUE, cex = .8, xpd = TRUE)
rsq.rpart(puma_tree)

# fancy plot
puma_tree.b <- train(value_decrease ~ value, method = "rpart", data = puma_training)
fancyRpartPlot(puma_tree.b$finalModel)
```

```{r, warning=FALSE}
# Summary
  ## ML-KNN, the results give a proper accurate rate. However, the decision boundary is not clear so change the dimensions or test other models will be better.
  ## ML-RT, we can apply more dimensions to get more competed predictive and complicated results.
  ## promoted/new arrival features can also be created as the dimensons to measure the predictions
  ## if inlcudes user session, demographic and transaction data, we can calssify user attributions by different feaatures, linking to the product ategories. 
```

