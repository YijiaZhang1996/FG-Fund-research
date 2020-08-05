#This script is used to build model to evaluate product's stock-choosing ability
#To run this script, you need to install and library package "WindR" and "stringr" first

#Define fundemental variables
fund_code<-"002692.OF"
bench_index<-"000998.SH"
start_date_series<-c("2016-06-16","2017-01-01","2018-01-01","2019-01-01","2020-01-01")
end_date_series<-c("2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-06-30")
r_f<-0.015
#for weekly data
r_f_adjusted<-(1+r_f)^(1/52)-1
#for monthly data
#r_f_adjusted<-(1+r_f)^(1/12)-1
#for daily data
#r_f_adjusted<-(1+r_f)^(1/250)-1


stock_choosing_ability_list<-data.frame(matrix(NA, ncol=5, nrow = 0))
for (i in 1:length(start_date_series)) {
  #T-M model
  
  #get serial of return rate
  fund_net_value<-w.wsd(fund_code,"NAV_adj",start_date_series[i],end_date_series[i],"Period=W")
  r_p<-fund_net_value$Data$NAV_ADJ[2:length(fund_net_value$Data$NAV_ADJ)]/fund_net_value$Data$NAV_ADJ[1:(length(fund_net_value$Data$NAV_ADJ)-1)]-1
  index_value<-w.wsd(bench_index,"close",start_date_series[i],end_date_series[i],"Period=W")
  r_m<-index_value$Data$CLOSE[2:length(index_value$Data$CLOSE)]/index_value$Data$CLOSE[1:(length(index_value$Data$CLOSE)-1)]-1
  
  #get excess return
  excess_fund_return<-r_p-r_f_adjusted
  excess_index_return<-r_m-r_f_adjusted
  excess_index_return_square<-excess_index_return^2
  data_tm<-data.frame(excess_fund_return,excess_index_return,excess_index_return_square)
  
  #regression model
  tm_model<-lm(formula = excess_fund_return~excess_index_return+excess_index_return_square,data = data_tm)
  result<-summary(tm_model)
  stock_choosing_ability_list[i,1]<-fund_code
  stock_choosing_ability_list[i,2]<-start_date_series[i]
  stock_choosing_ability_list[i,3]<-end_date_series[i]
  stock_choosing_ability_list[i,4]<-result$coefficients[1,1]
  stock_choosing_ability_list[i,5]<-result$coefficients[1,4]
}

names(stock_choosing_ability_list)<-c("product code","start date","end date","alpha","sig level")
write.csv(stock_choosing_ability_list, "stock_choosing_ability_list_002692_W.csv")
