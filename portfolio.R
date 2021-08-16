library(mosaic)
library(quantmod)
library(foreach)

# Extreme Aggressive ETF
growth = c('ARKK', 'SOXL', 'TQQQ', 'IWP')
getSymbols(growth)

ARKKa = adjustOHLC(ARKK)
SOXLa = adjustOHLC(SOXL)
TQQQa = adjustOHLC(TQQQ)
IWPa = adjustOHLC(IWP)

growth_return = cbind(ClCl(ARKKa), ClCl(SOXLa), ClCl(TQQQa), ClCl(IWPa))
growth_return = as.matrix(na.omit(growth_return))


initial = 100000
growth_sim = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial
  weights = c(0.25, 0.25, 0.25, 0.25)
  holdings = total_wealth * weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for (today in 1:n_days) {
    growth.today = resample(growth_return, 1,orig.ids=FALSE)
    holdings = holdings + holdings * (1+growth.today)
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}


mean(growth_sim[,n_days])
quantile(growth_sim[,n_days] - initial, prob =0.05)

# Dividend ETF - conservative
dividend = c('USRT', 'HYG', 'SPDV', 'SDY')
getSymbols(dividend)

USRTa = adjustOHLC(USRT)
HYGa = adjustOHLC(HYG)
SPDVa = adjustOHLC(SPDV)
SDYa = adjustOHLC(SDY)

dividend_return = cbind(ClCl(USRTa), ClCl(HYGa), ClCl(SPDVa), ClCl(SDYa))
dividend_return = as.matrix(na.omit(dividend_return))

initial = 100000
dividend_sim = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial
  weights = c(0.25, 0.25, 0.25, 0.25)
  holdings = total_wealth * weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for (today in 1:n_days) {
    dividend.today = resample(dividend_return, 1,orig.ids=FALSE)
    holdings = holdings + holdings * (1+dividend.today)
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}


mean(dividend_sim[,n_days])
quantile(dividend_sim[,n_days] - initial, prob =0.05)


#Blended
blend_return = cbind(ClCl(USRTa), ClCl(HYGa), ClCl(SPDVa), ClCl(SDYa),ClCl(ARKKa), ClCl(SOXLa), ClCl(TQQQa), ClCl(IWPa))
blend_return = as.matrix(na.omit(blend_return))
initial = 100000
blend_sim = foreach(i=1:5000, .combine='rbind') %do% {
  total_wealth = initial
  weights = c(0.1, 0.1, 0.1, 0.1, 0.15, 0.15, 0.15, 0.15)
  holdings = total_wealth * weights
  n_days = 20
  wealthtracker = rep(0, n_days)
  for (today in 1:n_days) {
    blend.today = resample(blend_return, 1,orig.ids=FALSE)
    holdings = holdings + holdings * (1+blend.today)
    total_wealth = sum(holdings)
    wealthtracker[today] = total_wealth
  }
  wealthtracker
}


mean(blend_sim[,n_days])
quantile(blend_sim[,n_days] - initial, prob =0.05)
