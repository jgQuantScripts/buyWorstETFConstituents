source("DogsOfThe_FUN.R")

# assign ETF
ETF = "DIA"
# get Constituents for ETF
CONST = getConstituents(ETF)
# extract tickers
TICKERS = as.character(c(ETF,CONST$symbol))
# get data for tickers
e <- new.env()
getSymbols(TICKERS, from="2020-11-30",env = e)
XTS  <-  do.call(merge,eapply(e,Ad))
colnames(XTS) <- gsub(".Adjusted","",names(XTS))

# Strategy function
# buys the Dogs of the ETF
# XTS    : xts of Price Data
# N      : Number of stocks to buy 
# Period : Rebalance Period -> 'D' = Daily | 'W' = Weekly | 'M' = Monthly
D = buyWorst(XTS=XTS, N=4, Period = "D", ETF=ETF)
W = buyWorst(XTS=XTS, N=4, Period = "W", ETF=ETF)
M = buyWorst(XTS=XTS, N=4, Period = "M", ETF=ETF)
# ********************************************************************************************************
# convert to xts object & plot
d = xts(cbind(as.numeric(D$rets),as.numeric(D$etf)), order.by = as.Date(D$Date, format="%Y-%m-%d"))
colnames(d) <- c("rets","etf")
charts.PerformanceSummary(d, geometric = FALSE)

w = xts(cbind(as.numeric(W$rets),as.numeric(W$etf)), order.by = as.Date(W$Date, format="%Y-%m-%d"))
colnames(w) <- c("rets","etf")
charts.PerformanceSummary(w, geometric = FALSE)

m = xts(cbind(as.numeric(M$rets),as.numeric(M$etf)), order.by = as.Date(M$Date, format="%Y-%m-%d"))
colnames(m) <- c("rets","etf")
charts.PerformanceSummary(m, geometric = FALSE)
# ********************************************************************************************************

