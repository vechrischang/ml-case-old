else if(n_distinct(grep(pattern = 'outlier', x = p3$interpolated.open_rate)) >= 60)

 # for a time seires object
    p3b.1 <- xts(p3$open_rate, order.by = p3$Date)    
    p3b.2 <- xts(p3$click_rate, order.by = p3$Date)
    p3b.3 <- xts(p3$conv_rate, order.by = p3$Date)
    p3b.1a <- ts(p3b.1, start = c(2016, yday("2016-04-01")),
                 end = c(2016, yday("2016-12-31")), frequency = 365)
    p3b.2a <- ts(p3b.2, start = c(2016, yday("2016-04-01")),
                 end = c(2016, yday("2016-12-31")), frequency = 365)
    p3b.3a <- ts(p3b.3, start = c(2016, yday("2016-04-01")),
                 end = c(2016, yday("2016-12-31")), frequency = 365)
        # ARIMA model
    p3b.1b <- forecast(
      auto.arima(p3b.1a, xreg = fourier(p3b.1a, K = 1)), 
      h = 90, xreg = fourier(p3b.1a, K = 1, h = 90))
    p3b.2b <- forecast(
      auto.arima(p3b.2a, xreg = fourier(p3b.2a, K = 1)),
      h = 90, xreg = fourier(p3b.2a, K = 1, h = 90))
    p3b.3b <- forecast(
      auto.arima(p3b.3a, xreg = fourier(p3b.3a, K = 1)),
      h = 90, xreg = fourier(p3b.3a, K = 1, h = 90))
        # new data frame
    p3b <- data.frame(p3b.1b, p3b.2b, p3b.3b)
    p3b <- add_rownames(p3b, "Date")
    p3b$Date <- as.numeric(p3b$Date)
    p3b$Date <- format(date_decimal(p3b$Date), "%Y/%m/%d")
    p3b$Date <- as.Date(p3b$Date, format = "%Y/%m/%d")

# IQR filter
   Q1 - c*IQD / Q3 + c*IQD
p3$open.lower <- quantile(p3$open_rate, probs = .25) -
  1.5 * IQR(p3$open_rate, na.rm = FALSE, type = 6)
p3$click.lower <- quantile(p3$click_rate, probs = .25) -
  1.5 * IQR(p3$click_rate, na.rm = FALSE, type = 6)
p3$conv.lower <- quantile(p3$conv_rate, probs = .25) -
  1.5 * IQR(p3$conv_rate, na.rm = FALSE, type = 6)
p3$open.upper <- quantile(p3$open_rate, probs = .75) +
  .0 * IQR(p3$open_rate, na.rm = FALSE, type = 6)
p3$click.upper <- quantile(p3$click_rate, probs = .75) +
  .0 * IQR(p3$click_rate, na.rm = FALSE, type = 6)
p3$conv.upper <- quantile(p3$conv_rate, probs = .75) +
  .0 * IQR(p3$conv_rate, na.rm = FALSE, type = 6)