---
title: "UBER Data Test | Chris.C"
output:
  html_document: default
  pdf_document: default
---

```{r, warning=FALSE, message=FALSE}
library(readr)
library(dplyr)
library(reshape2)
library(qdapRegex)
library(ggplot2)
library(ggthemes)
library(ggalt)
library(scales)
library(lubridate)
library(reshape2)
library(lubridate)
library(class)
library(cowplot)
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
```

```{r, warning=FALSE, message=FALSE}
# load data
u_raw <- read_csv("C:/Users/User/Pictures/R_2017/Uber/Uber_data.csv")
# tidy and clean
u_raw_tidy <- u_raw %>%
  arrange(signup_date)
u_raw_tidy$signup_date <- mdy(u_raw_tidy$signup_date)
u_raw_tidy$bgc_date <- mdy(u_raw_tidy$bgc_date)
u_raw_tidy$vehicle_added_date <- mdy(u_raw_tidy$vehicle_added_date)
u_raw_tidy$first_completed_date <- mdy(u_raw_tidy$first_completed_date)
  # duration
u_raw_tidy$sig_com <- u_raw_tidy$first_completed_date - u_raw_tidy$signup_date
u_raw_tidy$bcg_com <- u_raw_tidy$first_completed_date - u_raw_tidy$bgc_date
u_raw_tidy$veh_com <- u_raw_tidy$first_completed_date - u_raw_tidy$vehicle_added_date
  # apply mutple columns on data type
u_raw_tidy[, c(12:14)] <- sapply(u_raw_tidy[, c(12:14)], as.numeric)
u_raw_tidy$completed <- ifelse(u_raw_tidy$sig_com >= 1, "TRUE", "FALSE")
u_raw_tidy$completed[is.na(u_raw_tidy$completed)] <- "FALSE"

u_raw_tidy$sig_com[is.na(u_raw_tidy$sig_com)] <- 0
u_raw_tidy$bcg_com[is.na(u_raw_tidy$bcg_com)] <- 0
u_raw_tidy$veh_com[is.na(u_raw_tidy$veh_com)] <- 0
```

```{r, warning=FALSE, message=FALSE}
# explore timestamp
u_date <- u_raw_tidy %>%
  mutate(dd = format(signup_date, '%d')) %>%
  group_by(dd) %>%
  summarise(total_completed = length(sig_com[sig_com > 0]),
            total_incompleted = length(sig_com[sig_com == 0]),
            average_sin_to_com_day = mean(sig_com[sig_com > 0]))
u_date$completed_rate = u_date$total_completed/(u_date$total_completed + u_date$total_incompleted)
u_date$completed_rate <- as.numeric(u_date$completed_rate)
  # ompleted rate
ggplot(u_date) +
  geom_line(aes(x = dd, y = completed_rate), group = 1, color = "#1abc9c", size = 1) +
  theme(legend.position="top", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  labs(x= "", y= "", title= "Daily Completed Rate from Signup to completed") +
  ylim(0, 0.3) +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())
# the line plot above indicates daily completed rate showing a waved trend in Jan, most decreases happen on the weekend.
```
```{r, warning=FALSE, message=FALSE}
  # average_sin_to_com_day
ggplot(u_date) +  
  geom_bar(aes(x = dd, y = average_sin_to_com_day, fill = average_sin_to_com_day),
           position="dodge", stat= "identity") +
  theme(legend.position="top", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  labs(x= "", y= "", title= "Average Days From Sign Up to Complete") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())
```

```{r, warning=FALSE, message=FALSE}
# explore vehicle year
u_veh_year <- u_raw_tidy %>%
  group_by(vehicle_year) %>%
  summarise(total_completed = length(sig_com[sig_com > 0]),
            total_incompleted = length(sig_com[sig_com == 0]),
            average_sin_to_com_day = mean(sig_com[sig_com > 0]))
u_veh_year$completed_rate = u_veh_year$total_completed/(u_veh_year$total_completed + u_veh_year$total_incompleted)
u_veh_year$completed_rate <- as.numeric(u_veh_year$completed_rate)
ggplot(filter(u_veh_year, vehicle_year >= 2001 & vehicle_year <= 2016)) +
  geom_line(aes(x = vehicle_year, y = completed_rate), group = 1, color = "#6e8196", size = 1) +
  geom_smooth(aes(x = vehicle_year, y = completed_rate), method = "loess") +
  theme(legend.position="none", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 7, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 7, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  labs(x= "", y= "", title= "Completed Rate from Signup to Completed by Vehicle Year") +
  ylim(0.2, 0.6) +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())

# the plot shows above, indicating the growth of completed rate is followed by the vehicle year.  
```

```{r, warning=FALSE, message=FALSE}
# explore vehicle brand type
u_veh <- u_raw_tidy %>%
  group_by(vehicle_make) %>%
  summarise(total_completed = length(sig_com[sig_com > 0]),
            total_incompleted = length(sig_com[sig_com == 0]),
            average_sin_to_com_day = mean(sig_com[sig_com > 0]))
u_veh$completed_rate = u_veh$total_completed/(u_veh$total_completed + u_veh$total_incompleted)
u_veh <- u_veh %>%
  arrange(desc(completed_rate))

ggplot(filter(u_veh, vehicle_make != "NA" & average_sin_to_com_day != 0)) +
  geom_bar(aes(x = vehicle_make, y = average_sin_to_com_day, fill = average_sin_to_com_day),
           position ="dodge", stat= "identity") +
  geom_line(aes(x = vehicle_make, y = completed_rate), group = 1, color = "#1abc9c", size = 1) +
  coord_flip() +
  theme(legend.position="right", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 7, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  labs(x= "", y= "", title= "Car Brand - Average Days Sign up to Complete / Completed Rate") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())

# above bar chart shows both higher completed days on average in both Oldsmobile and Landrover
```

```{r, warning=FALSE, message=FALSE}
# apply Random Forests
# only select the metrics for the model need
u_rf <- u_raw_tidy %>%
  select(signup_channel, signup_date, sig_com, bcg_com, veh_com) %>%
  filter(sig_com > 0)

# split up training set and testing set
# train / test set account for 70%/30%
u_rf.train <- sample(1:nrow(u_rf), 4286)
u_rf.test <- setdiff(rownames(u_rf), u_rf.train)
u_rf_training <- u_rf[u_rf.train,] 
u_rf_testing <- u_rf[-u_rf.train,]

# run the model
u_rf_tree <- rpart(sig_com ~ ., x = TRUE, data = u_rf_training)

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

# error comparsion for train/test set
caculate_rms_error(u_rf_tree, u_rf_training, u_rf_testing, "sig_com")
rsq.rpart(u_rf_tree)

# plot tree
u_rf_tree_plot <- train(sig_com ~ ., method = "rpart", data = u_rf_training)
fancyRpartPlot(u_rf_tree_plot$finalModel)

# Based on the error test, it shoows train.err and test.err are close. Also, when the number of splits reach to 6, the error reduce to reasonable level at 0.1
# R-square gives a very stong above 0.8 when the split number is 5, meaning that the model shows a good fit based on the dimensions.
# the tree indicates when background check time is below than 12, the completed time will be lowwer compared with above 12.
```


```{r}
# Suggestions
# provide some weekend bonus to motivate the drivers to sign up.
# narrow the vehicle year gap, select the newer vehicle of the drivers would intcrease the completed rete.
# Based on the results from the prediction radnom forests, improve the backgound check time would increase the completed time, boosting the percentage of trip completed.
```














