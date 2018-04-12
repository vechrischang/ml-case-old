```{r, echo=FALSE, warning=FALSE, message=FALSE, include=FALSE, results='hide'}
# Plot
library(ggplot2) 
library(ggthemes) 
library(ggalt) 
library(scales) 
library(lubridate)
# Forecasting  
library(forecast) 
library(xts)
library(tseries)
# Data tidy
library(dplyr) 
library(reshape2) 
library(gridExtra)
```

```{r}
# Read train data set
read.csv(paste0("N:\\user\\chris\\raw_data\\train.csv"))
# Explore data set
str(gug_train)
# Check unique observations
sapply(gug_train[, c(1:7)], n_distinct)
```

```{r}
# Device x match_type
gug_pivot_dm <- gug_train %>%
  mutate(ymd = format(Date, '%Y/%m/%d')) %>%
  group_by(ymd,
           Device_ID,
           Match_type_ID) %>%
  summarise(nb_click = sum(Clicks),
            RPC = sum(Revenue)/sum(Clicks))
```

```{r}
# Based on the plots, different combinations in device and match_ype, indicating the levels of RPC.
# In order to get better the forecasting outcome, building up each combination of the forecasting model could maximise the forecasting results.
# In this case, choose a combination as an example In the practical method, we can run a loop to execute different combinations.
# Plotting
gug_pivot_dm_p1 <- ggplot(gug_pivot_dm) +
  geom_line(data = gug_pivot_dm, aes(x = as.Date(ymd), y = RPC, group = as.factor(Match_type_ID),
                              color = as.factor(Match_type_ID)),linetype= 1, size= .7) +
  facet_grid(facets = .~ as.factor(Device_ID), scales = "free") +
  theme(legend.position="top", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank()) +  
  labs(x= "", y= "", title= "device x match_type")
gug_pivot_dm_p1

```

```{r}
# Trend line plotting
gug_pivot_dm_c1 <- subset(gug_pivot_dm, Device_ID == "298643508640" & Match_type_ID == "95725474456")
gug_pivot_dm_p2 <- ggplot(gug_pivot_dm_c1) +
  geom_line(aes(x = as.Date(ymd), y = RPC, group = 1), linetype= 1, size= .7) +
  geom_smooth(aes(x = as.Date(ymd), y = RPC)) +
  theme(legend.position="top", legend.title=element_blank(),
        legend.text = element_text(colour= "black", size= 8, face= "bold")) +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank()) +  
  labs(x= "", y= "", title= "a combination")
gug_pivot_dm_p2

```

```{r}
# There are many forecasting models such as naive, ets, st1, neural, bats and so on.
# In this case, I use the ARIMA model to process data.

# Test for stationarity
# If p value = 0.01 then is stationary. if not, do differencing to get a stationary series.
# The result shows p = 0,01 so it is stationary.
print(adf.test(gug_pivot_dm_c1$RPC))
```

```{r, fig.width=8.6, fig.height=3}
# ACF and PACF for testing model parameters in ARIMA
# We can decide the AR order and MA order based on the test results
par(mfrow = c(1, 2), bg = "grey96", mar = c(2.5,3,2,2),
    cex.axis = .7, font.axis = 2, col.axis = "#666666")
gug_acf = acf(gug_pivot_dm_c1$RPC, main = 'ACF Plot', lag.max = 20)
gug_pacf = pacf(gug_pivot_dm_c1$RPC, main = 'PACF Plot', lag.max = 20)
```

```{r}
# Create time series objects
gug_pivot_dm_c1$ymd <- as.Date(gug_pivot_dm_c1$ymd, format = '%Y/%m/%d')
    # Create xts time series object
gug_pivot_dm_xts <- xts(gug_pivot_dm_c1$RPC, order.by = gug_pivot_dm_c1$ymd)
    # Create ts time series object 
gug_pivot_dm_ts <- ts(gug_pivot_dm_xts, start = c(2014, yday("2014-12-14")),
                 end = c(2015, yday("2015-04-07")), frequency = 365) 
```

```{r}
# Identify AR / MA
# For the parameters for AR/MA, use PACF to decide p in AR model and ACF to decide q in MA model.
# In this case, I use auto.arima to automatically select:
  # a. number of differences by unit root tests.
  # b. get minimum AICc to deicde the p,q  

# Auto.arima
gug_pivot_dm_fit <- auto.arima(gug_pivot_dm_ts, xreg = fourier(gug_pivot_dm_ts, K = 1), trace = TRUE)
summary(gug_pivot_dm_fit)
# Check residuals
acf(gug_pivot_dm_fit$residuals)
```


```{r}
# When decide the (p,d,q) after above processes, we can start to run the forecasting model.
# Forecasting
gug_pivot_dm_forecast <- forecast(gug_pivot_dm_fit, h = 7, xreg = fourier(gug_pivot_dm_ts, K = 1, h = 7))
summary(gug_pivot_dm_forecast)
plot(gug_pivot_dm_forecast)
```