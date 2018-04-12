```{r, echo=FALSE, warning=FALSE, message=FALSE, include=FALSE, results='hide'}
# click forecasting
p12 <- unique_client %>%
  filter(Date >= "2016-04-01" & Date <= "2017-03-31") %>%
  filter(type %in% c("VeContact","VePrompt","VePanel")) %>%
  mutate(ds = strftime(Date, '%Y/%m/%d')) %>%
  group_by(ds) %>%
  arrange(ds) %>%
  summarise(y = sum(clicked),
            convs = sum(convs),
            sales = sum(sales))
p12 <- prophet(p12, growth = "linear")
p12a <- make_future_dataframe(p12, periods = 180)
p12b <- predict(p12, p12a)
p12d <- prophet_plot_components(p12, p12b)
p12c <- ggplot() +
  # history
  geom_smooth(data = filter(p12b, ds >="2016-02-01" & ds <= "2017-03-04"),
              aes(x = ds, y = yhat), alpha = 0,
              color = "#394049", linetype = 1, size = 1, method = "auto") +
  # forecasting
  geom_smooth(data = filter(p12b, ds >="2017-02-28"),
              aes(x = ds, y = yhat), alpha = 0,
              color = "#1abc9c", linetype = 1, size = 1, method = "auto") +  
  # forecasting - range
  geom_ribbon(data = filter(p12b, ds >="2017-03-01"),
                 aes(x = ds, ymin = yhat - exp(4), ymax = yhat + exp(6)),
                 fill = "#1abc9c", linetype = 1, size = .7, alpha = .25) +
  # formatting
  labs(x= "", y= "", title= "Click Forecasting") +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  scale_y_continuous(labels = scales::comma, position = "right") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())

# convs forecasting
p10 <- unique_client %>%
  filter(Date >= "2016-02-01" & Date <= "2017-02-28") %>%
  filter(type %in% c("VeContact","VePrompt","VePanel")) %>%
  mutate(ds = strftime(Date, '%Y/%m/%d')) %>%
  group_by(ds) %>%
  arrange(ds) %>%
  summarise(clicks = sum(clicked),
            y = sum(convs),
            sales = sum(sales))
p10 <- prophet(p10, growth = "linear")
p10a <- make_future_dataframe(p10, periods = 180)
p10a$ds <- as.Date(p10a$ds)
p10b <- predict(p10, p10a)
p10c <- ggplot() +
  # history
  geom_smooth(data = filter(p10b, ds >="2016-02-01" & ds <= "2017-03-04"),
              aes(x = ds, y = yhat), alpha = 0,
              color = "#394049", linetype = 1, size = 1) +
  # forecasting
  geom_smooth(data = filter(p10b, ds >="2017-02-28"),
              aes(x = ds, y = yhat), alpha = 0,
              color = "#BCCF00", linetype = 1, size = 1) +  
  # forecasting - range
  geom_ribbon(data = filter(p10b, ds >="2017-03-01"),
                 aes(x = ds, ymin = yhat - exp(2), ymax = yhat + exp(4)),
                 fill = "#BCCF00", linetype = 1, size = .7, alpha = .25) +
    #geom_linerange(data = filter(p10b, ds >="2017-03-01"),
    #               aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
    #               color = "#BCCF00", linetype = 1, size = .7, alpha = .2) +
  labs(x= "", y= "", title= "Conversion Forecasting") +
  theme(axis.text.x= element_text(face= "bold", size= 10, color = "#666666"),
        axis.text.y= element_text(face= "bold", size= 10, color = "#666666")) +
  theme(axis.line=element_blank(), axis.ticks.y=element_blank(), axis.title=element_blank()) +
  theme(plot.title = element_text(hjust = 0)) +  
  scale_y_continuous(labels = scales::comma, position = "right") +
  theme(panel.background = element_rect(fill = "grey96"),
        plot.background = element_rect(fill = "grey96"),        
        panel.grid.major = element_line(colour = "gray65"),
        panel.grid.major.x = element_blank())

ggdraw(xlim = c(0,1.05), ylim = c(0,1.4)) +
  draw_plot(p12c, x=0, y=.035, width=.5, height=1.35) +
  draw_plot(p10c, x=.55, y=.035, width=.5, height=1.35)
```