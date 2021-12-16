n = 45
ps = c(.301, .176, .125, .097, .079, .067, .058, .051, .046)
logps = log(ps)
paste(round(logps, 3), collapse = " & ")
fs = cumsum(ps)
sum(1:9*ps)


os = rep(n/9, 9)
es = n * ps

qchisq(.95, 7:10)

phihathat = sum((os-es)^2 / es)
phihathat

sum(((1:9)-ps)^2 / ps)

sum((os-n * fs)^2 / (n * fs))

dhathat = max(fs - (1:9)/9)
dhathat
sqrt(n) * dhathat

2 * log(1/9^45 / prod(ps^5))

loglik_unif = 9 * 5 * log(1/9)
aic_unif  = -2 * loglik_unif
loglik_benford = 5 * sum(logps)
aic_benford  = -2 * loglik_benford

1 / (1 + exp((aic_unif - aic_benford) / 2))


pacman::p_load(quantmod, lubridate, ggplot2, data.table)

#let's look at SPY (the S&P 500 which is a proxy for the total American market) 
#vs QQQ (a fund that proxies for American tech / biotech) only
num_years = 1

#get the publicly available financial data:
getSymbols("SPY", from = Sys.Date() %m-% years(num_years), to = Sys.Date(), warnings = FALSE, auto.assign = TRUE)
SPY = data.table(SPY)
SPY[, prop_change := SPY.Close / shift(SPY.Close) - 1]
SPY[, prop_change_centered := prop_change - mean(prop_change, na.rm = TRUE)]
SPY = SPY[2 : .N]

n = nrow(SPY)
B = 1e6
excess_kurtoses = array(NA, B)
for (b in 1 : B){
  bootstrap_samp = sample(SPY$prop_change_centered, replace = TRUE)
  sigsqhat = var(bootstrap_samp) * (n - 1) / n
  xbar = mean(bootstrap_samp)
  excess_kurtoses[b] = mean((bootstrap_samp - xbar)^4) / sigsqhat^2
}
excess_kurtoses = excess_kurtoses - 3

mean((SPY$prop_change_centered - mean(SPY$prop_change_centered))^4) / var(SPY$prop_change_centered)^2  - 3

ggplot(data.frame(excess_kurtoses = excess_kurtoses)) + 
  geom_histogram(aes(x = excess_kurtoses), bins = 1000)

quantile(excess_kurtoses, c(.025,.975))




set.seed(1984)
n = 10
xs = round(rnorm(n, 0, 5), 2)
paste(xs, collapse = ", ")
sum(xs^2)
() * sqrt(2)/n


