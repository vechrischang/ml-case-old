---
title: "case_innogames"
author: "chrisc"
date: "November 11, 2017"
output: html_document
---

```{r setup, include=FALSE}
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
library(class)
library(rpart)
library(caret)
library(e1071)
library(rpart.plot)
library(rattle)
library(scales)
```

```{r}
# fliter for <= 30 sessions
id_1st_session30 <- df.ig %>%
  group_by(unique_id) %>%
  mutate(cumsum_sessions = cumsum(sessions_total)) %>%
  filter(cumsum_sessions <= 35)
```
```{r}
id_1st_session30_summary <- id_1st_session30 %>%
    group_by(unique_id,registration_platform, marketing_source, active_four_weeks) %>%
    summarise(uni_banchees = unique(lifetime_banchees_spent),
              uni_logindays = unique(lifetime_logindays),      
              sum_sessions = sum(sessions_total),
              sum_durations = sum(session_duration),
              sum_transactions = sum(transactions),
              sum_points = sum(max_points),
              sum_events = sum(eventcount_total),
              sum_quests  = sum(questa_closed),
              sum_messages = sum(eventcount_message),
              sum_fights = sum(eventcount_fight),
              sum_trades = sum(eventcount_trade),
              sum_bulids = sum(eventcount_build),
              sum_recuits = sum(eventcount_recruit),
              sum_visits = sum(eventcount_visit),
              sum_researches = sum(eventcount_research))

id_1st_session30_summary$chum <- ifelse(id_1st_session30_summary$active_four_weeks == "false", 1, 0)
```

```{r}
# user from marketing_source
user_marketing_source <- id_1st_session30 %>%
  group_by(marketing_source) %>%
  summarise(uni_users = n_distinct(unique_id)) %>%
  ggplot(aes(x = marketing_source, y = uni_users, fill = marketing_source)) +
  geom_bar(position = "stack", stat= "identity", width= 1) + 
  geom_text(aes(label = percent(uni_users/sum(uni_users))),
            position =  position_stack(vjust = 1), size = 6,
            fontface= "bold", color = "black") +
  coord_polar(theta = "y") +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(axis.text.x= element_blank(), axis.text.y= element_blank(),
        axis.ticks=element_blank(), axis.line=element_blank()) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "User by mrketing source") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  scale_fill_manual(values=c("#ffcc66","#cccc00","#1abc9c"))
user_marketing_source

# user from platform
user_registration_platform <- id_1st_session30 %>%
  group_by(registration_platform) %>%
  summarise(uni_users = n_distinct(unique_id)) %>%
  ggplot(aes(x = registration_platform, y = uni_users, fill = registration_platform)) +
  geom_bar(position = "stack", stat= "identity", width= 1) + 
  geom_text(aes(label = percent(uni_users/sum(uni_users))),
            position =  position_stack(vjust = 1), size = 6,
            fontface= "bold", color = "black") +
  coord_polar(theta = "y") +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(axis.text.x= element_blank(), axis.text.y= element_blank(),
        axis.ticks=element_blank(), axis.line=element_blank()) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "User by platform") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  scale_fill_manual(values=c("#ffcc66","#cccc00","#1abc9c"))
user_registration_platform

# user from active_four_weeks
user_active_four_weeks <- id_1st_session30 %>%
  group_by(active_four_weeks) %>%
  summarise(uni_users = n_distinct(unique_id)) %>%
  ggplot(aes(x = active_four_weeks, y = uni_users, fill = active_four_weeks)) +
  geom_bar(position = "stack", stat= "identity", width= 1) + 
  geom_text(aes(label = percent(uni_users/sum(uni_users))),
            position =  position_stack(vjust = 1), size = 6,
            fontface= "bold", color = "black") +
  coord_polar(theta = "y") +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(axis.text.x= element_blank(), axis.text.y= element_blank(),
        axis.ticks=element_blank(), axis.line=element_blank()) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "active four weeks") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  scale_fill_manual(values=c("#ffcc66","#cccc00","#1abc9c"))
user_active_four_weeks
```

```{r}
## density x registration_platform
# logindays
login_density <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(uni_logindays, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,100) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "logindays") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_density
# sessions_total
login_sessions_total <- id_1st_session30_summary %>%
  ggplot() + 
  geom_density(aes(sum_sessions, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,30) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +  
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "sessions_total") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sessions_total
# duration
login_duration <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_durations, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,2000) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "duration") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_duration


# maxpoints
login_maxpoints <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_points, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,2000) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "maxpoints") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_maxpoints
# sum_events
login_sum_events <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_events, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,400) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "events") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_events
# sum_quests
login_sum_quests <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_quests, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,30) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "quests") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_quests
# sum_messages
login_sum_messages <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_messages, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,5) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "messages") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_messages
# sum_fights
login_sum_fights <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_fights, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,20) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "fights") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_fights
# sum_trades
login_sum_trades <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_trades, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,5) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "trades") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_trades
# sum_bulids
login_sum_bulids <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_bulids, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,30) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "bulids") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_bulids
# sum_recuits
login_sum_recuits <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_recuits, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,20) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "recuits") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_recuits
# sum_visits
login_sum_visits <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_visits, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,5) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "visits") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_visits
# sum_researches
login_sum_researches <- id_1st_session30_summary %>%
  ggplot() +
  geom_density(aes(sum_researches, group = registration_platform, fill = registration_platform),alpha = 0.1, position = "stack") +
  facet_grid(facets = registration_platform ~. , scales = "free") +
  xlim(0,20) +
  theme(axis.text.x= element_text(face= "bold", size= 12),
        axis.text.y= element_text(face= "bold", size= 12)) +
  theme(legend.position="right", legend.title= element_blank(),
        legend.text = element_text(colour= "black", size= 14, face= "bold")) +
  theme(plot.title = element_text(hjust = 0, size = 20, face = "bold")) +  
  labs(x= "", y= "", title= "researches") +
  theme(panel.background = element_rect(fill = "grey96", color = "grey96"),
        plot.background = element_rect(fill = "grey96", color = "grey96"),        
        panel.grid.major = element_blank()) +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
login_sum_researches
```

```{r}
# two_dim
two_dim_login_duration <- id_1st_session30_summary %>%
  ggplot(aes(x = sum_sessions, y = sum_durations)) +
  geom_point(aes(group = registration_platform, color = registration_platform), size = .8) +
  #geom_area(aes(fill = registration_platform, group = registration_platform), stat="identity", position= 'stack') +  
  facet_grid(facets = registration_platform ~. , scales = "free") +
  theme(strip.background = element_blank(), strip.text.y = element_blank())
two_dim_login_duration
```

```{r}
# comparsion t and f behaviour

# correlation test
library(PerformanceAnalytics)
library(zoo)
library(xts)
corr_test <- subset(id_1st_session30_summary, select = c(uni_logindays, sum_sessions, sum_durations, sum_points, sum_events))
#corr_test <- subset(id_1st_session30_summary, select = c(sum_points, sum_events, sum_quests, sum_fights, sum_bulids, sum_researches))
corr_test.plot <- chart.Correlation(corr_test,
                                    method = "pearson",
                                    histogram = TRUE,
                                    pch=16)
```

```{r}
# prediction_logistics.regression
model_train <- sample(1:nrow(id_1st_session30_summary), 262204)
model_test <- setdiff(rownames(id_1st_session30_summary), model_train)
model_train_sets <- id_1st_session30_summary[model_train,]
model_test_sets <- id_1st_session30_summary[-model_train,]
```
```{r}
  # model
logistic <- glm(chum ~ uni_logindays+sum_sessions+sum_durations, data = model_train_sets, family = binomial())
logistic_b <- glm(chum ~ sum_points+sum_events+sum_fights+sum_bulids+sum_recuits+sum_researches,
                  data = model_train_sets, family = binomial())
  # predict
logistic_prediction <- mean(predict.glm(logistic, model_test_sets,type = "response"))
logistic_prediction
logistic_prediction_b <- mean(predict.glm(logistic, model_test_sets,type = "response"))
logistic_prediction_b
  # plot
model_test_sets2 <- model_test_sets
model_test_sets2$vs <- predict.glm(logistic, model_test_sets,type = "response")
plot(chum ~ sum_durations, data = model_train_sets, col = "red4")
lines(chum ~ sum_durations, model_test_sets2, col="green4", lwd = 2)
```

```{r}
# prediction_regression.tree
caculate_rms_error <- 
function(mdl, train, test, yval){
	train.yhat <- predict(object = mdl, newdata = train)
	test.yhat <- predict(object = mdl, newdata = test)
	train.y <- with(train, get(yval))
	test.y <- with(test, get(yval))
	train.err <- sqrt(mean((train.yhat - train.y)^2)) # RMSE
	test.err <- sqrt(mean((test.yhat - test.y)^2)) # RMSE
	c(train.err = train.err, test.err = test.err)
	}

regression_tree <- rpart(active_four_weeks ~ uni_banchees+sum_sessions+sum_durations+sum_transactions+sum_events,
                         control = rpart.control(cp = 0.005), data = model_train_sets)
rsq.rpart(regression_tree)
fancyRpartPlot(regression_tree)

regression_tree_predict <- predict(regression_tree, model_test_sets, type = "class")
table(model_test_sets$active_four_weeks, regression_tree_predict)
accurate_rate <- 1-((6499+8611)/(87101+10163+8611+6499))
accurate_rate
#regression_tree_plot <- train(active_four_weeks ~ uni_banchees+sum_sessions+sum_durations+sum_transactions+sum_events,
#                              method = "rpart", data = model_train_sets)

```

```{r}
regression_tree_b <- rpart(active_four_weeks ~ uni_banchees+sum_sessions+sum_durations+sum_transactions+sum_events+
                                               sum_points+sum_events+sum_quests+sum_fights+sum_bulids+sum_researches,
                           control = rpart.control(cp = 0.005), data = model_train_sets)
rsq.rpart(regression_tree_b)
fancyRpartPlot(regression_tree_b)

regression_tree_predict_b <- predict(regression_tree_b, model_test_sets, type = "class")
table(model_test_sets$active_four_weeks, regression_tree_predict_b)
accurate_rate_b <- 1-((5911+7974)/(87689+10000+5911+7974))
accurate_rate_b
```


uni_banchees+uni_logindays+sum_sessions+sum_durations+sum_transactions
sum_points+sum_events+sum_quests+sum_fights+sum_bulids+sum_researches
sum_recuits+sum_visits+sum_messages+sum_trades



