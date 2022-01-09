require("httr");require("dplyr");require("purrr");require("data.table");require("rvest");require("quantmod")
require("pbapply");require("RobinHood");require("rvest");require("IBrokers");require("RQuantLib")
# require("reticulate")
### function to get constituents from barChart
getConstituents = function(ticker){
  # page url
  pg <- html_session(paste0("https://www.barchart.com/etfs-funds/quotes/",ticker,"/constituents"))
  # save page cookies
  cookies <- pg$response$cookies
  # Use a named character vector for unquote splicing with !!!
  token <- URLdecode(dplyr::recode("XSRF-TOKEN", !!!setNames(cookies$value, 
                                                             cookies$name)))
  # get data by passing in url and cookies
  pg <- 
    pg %>% rvest:::request_GET(
      paste0("https://www.barchart.com/proxies/core-api/v1/EtfConstituents?",
             "composite=",ticker,"&fields=symbol%2CsymbolName%2Cpercent%2CsharesHeld%2C",
             "symbolCode%2CsymbolType%2ClastPrice%2CdailyLastPrice&orderBy=percent",
             "&orderDir=desc&meta=field.shortName%2Cfield.type%2Cfield.description&",
             "page=1&limit=10000&raw=1"),
      config = httr::add_headers(`x-xsrf-token` = token)
    )
  
  # raw data
  data_raw <- httr::content(pg$response)
  # convert into a data table
  data <- rbindlist(lapply(data_raw$data,"[[",6), fill = TRUE, use.names = TRUE) %>% suppressWarnings()
  # subset stocks only 
  data = subset(data,data$symbolType == 1)
  # trim data frame
  data = data[,1:3]
  # format percentages
  data$percent <- as.numeric(data$percent)/100
  # sort by weight
  data = data[order(data$percent, decreasing = TRUE),]
  # return data frame
  data
}
# Strategy function
# buys the Dogs of the ETF
# XTS    : xts of Price Data
# N      : Number of stocks to buy 
# Period : Rebalance Period -> 'D' = Daily | 'W' = Weekly | 'M' = Monthly
buyWorst = function(XTS, N, Period,ETF){
  # for daily rebalancing:
  if(Period == 'D'){
    # calculate return
    tmp <- na.omit(ROC(x = XTS,type = "discrete"))
    etf = tmp[,ETF]
    tmp <- tmp[,-base::which(names(tmp) == ETF)]
    # create a vector of worst performers each period
    RETS = lapply(as.list(1:(nrow(tmp)-1)), function(ii){
      SORT = as.data.frame(t(tmp[ii,]))
      SORT = SORT[order(SORT,decreasing = FALSE),,drop=FALSE]
      symbols2Buy = rownames(SORT)[1:N]
      # get next periods returns
      SUBSET = tmp[ii+1,symbols2Buy]
      BM     = etf[ii+1,]
      IDX = index(SUBSET)
      RETS = round(rowMeans(SUBSET),5)
      OUT = as.data.frame(cbind(paste(IDX),t(symbols2Buy),RETS,round(coredata(BM),4)))
      colnames(OUT) = c("Date",paste0("Asset",1:N), "rets","etf")
      OUT
    })
    RETS = rbindlist(RETS,use.names = TRUE,fill = TRUE)
  }
  # for weekly rebalancing:
  if(Period == 'W'){
    # calculate return
    tmp <- do.call(merge,lapply(as.list(1:ncol(XTS)), function(ii){
      df<- weeklyReturn(x=XTS[,ii])
      colnames(df) = names(XTS[,ii])
      df
    }))
    etf = tmp[,ETF]
    tmp <- tmp[,-base::which(names(tmp) == ETF)]
    # create a vector of worst performers each period
    RETS = lapply(as.list(1:(nrow(tmp)-1)), function(ii){
      SORT = as.data.frame(t(tmp[ii,]))
      SORT = SORT[order(SORT,decreasing = FALSE),,drop=FALSE]
      symbols2Buy = rownames(SORT)[1:N]
      # get next periods returns
      SUBSET = tmp[ii+1,symbols2Buy]
      BM     = etf[ii+1,]
      IDX = index(SUBSET)
      RETS = round(rowMeans(SUBSET),5)
      OUT = as.data.frame(cbind(paste(IDX),t(symbols2Buy),RETS,round(coredata(BM),4)))
      colnames(OUT) = c("Date",paste0("Asset",1:N), "rets","etf")
      OUT
    })
    RETS = rbindlist(RETS,use.names = TRUE,fill = TRUE)
  }
  # for monthly rebalancing:
  if(Period == 'M'){
    # calculate return
    tmp <- do.call(merge,lapply(as.list(1:ncol(XTS)), function(ii){
      df<- monthlyReturn(x=XTS[,ii])
      colnames(df) = names(XTS[,ii])
      df
    }))
    tmp <- tmp["202012::"]
    etf = tmp[,ETF]
    tmp <- tmp[,-base::which(names(tmp) == ETF)]
    # create a vector of worst performers each period
    RETS = lapply(as.list(1:(nrow(tmp)-1)), function(ii){
      SORT = as.data.frame(t(tmp[ii,]))
      SORT = SORT[order(SORT,decreasing = FALSE),,drop=FALSE]
      symbols2Buy = rownames(SORT)[1:N]
      # get next periods returns
      SUBSET = tmp[ii+1,symbols2Buy]
      BM     = etf[ii+1,]
      IDX = index(SUBSET)
      RETS = round(rowMeans(SUBSET),5)
      OUT = as.data.frame(cbind(paste(IDX),t(symbols2Buy),RETS,round(coredata(BM),4)))
      colnames(OUT) = c("Date",paste0("Asset",1:N), "rets","etf")
      OUT
    })
    RETS = rbindlist(RETS,use.names = TRUE,fill = TRUE)
  }
  # return data frame
  RETS
}
# *********************************************************************
# *********************************************************************
