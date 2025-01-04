tick <- c('SE', 'CVNA', 'NVDA', 'RKWBF', 'SQ','BABA','TPX','AAPL','PLTR','PFE', 'RIO', 'NET', 'META', 'AMKBY', 'GOOG', 'AZN', 'CRWD', 'MU', 'SALRY')

price_data <- tq_get(tick,
                     from = '2011-01-01',
                     to = '2024-12-20',
                     get = 'stock.prices')



log_ret_tidy <- price_data %>% dplyr::group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

head(log_ret_tidy)%>%kable()


log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()


log_ret_xts=log_ret_xts%>%as.data.frame()%>%drop_na()

summary(log_ret_xts)%>%kable()



head(log_ret_xts)%>%kable()


mean_ret <- colMeans(log_ret_xts,na.rm = T)
print(round(mean_ret, 5))%>%kable()


cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat,4))%>%kable()


wts <- runif(n = length(tick))
wts <- wts/sum(wts)
print(wts)%>%kable()


port_returns <- (sum(wts * mean_ret) + 1)^252 - 1


port_risk <- sqrt(t(wts) %*%(cov_mat %*% wts) )
print(port_risk)%>%kable()


sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)%>%kable()


num_port <- 10000000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)



for (i in seq_along(port_returns)) {

  wts <- runif(length(tick))
  wts <- wts/sum(wts)

  # Storing weight in the matrix
  all_wts[i,] <- wts

  # Portfolio returns

  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1

  # Storing Portfolio Returns values
  port_returns[i] <- port_ret


  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd

  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate

  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr

}


portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)



colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))




head(portfolio_values)%>%kable()





min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]



p <- max_sr %>%
  gather(SALRY:SE:CVNA:BABA:SQ:TPX:AAPL:AZN, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

p

