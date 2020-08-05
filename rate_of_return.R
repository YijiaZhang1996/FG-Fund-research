# This script is used to evaluate past performance of a fund, including return rate and rank in different interval
# To run this script, you need to install and library package "WindR" first

# Define fundemental variables
fund_code<-'000940.OF'
bench_index_industry<-"000998.SH"
bench_index_all<-"000300.SH"

start_date<-"2015-01-23"
end_date<-"2020-06-30"
start_date_series<-c("2015-01-23","2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01")
end_date_series<-c("2015-12-31","2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-06-30")

# # 1. Fetch cumulative rate of return and plot
# w_wsd_data<-w.wsd(fund_code,"SI_nav_adj_return",start_date,end_date)
# Cumulative_rate_of_return_fund<-w_wsd_data$Data$SI_NAV_ADJ_RETURN
# 
# 
# TMT_industry_index<-w.wsd(bench_index_industry,"close",start_date,end_date)
# hs_300_index<-w.wsd(bench_index_all,"close",start_date,end_date)
# 
# function_index_return<-function(index_value){
#   return(100*(index_value/index_value[1]-1))
# }
# 
# Cumulative_rate_of_return_industry_index<-function_index_return(TMT_industry_index$Data$CLOSE)
# Cumulative_rate_of_return_hs300_index<-function_index_return(hs_300_index$Data$CLOSE)
# Date<-w_wsd_data$Data$DATETIME
# 
# plot(Date,Cumulative_rate_of_return_fund,type='l',main = '成立以来累计收益率',xlab='时间',ylab='累计收益率(%)',ylim=c(-30,100))
# lines(Date,Cumulative_rate_of_return_industry_index,type='l',col="red" )
# lines(Date,Cumulative_rate_of_return_hs300_index,type='l',col="blue" )

# 2. Calculate interval return
# get interval return rate

function_interval_return_rate_fund<-function(fund_code,start_date_series,end_date_series){
  interval_return_rate_fund<-w.wss(fund_code,'return','annualized=1',paste0('startDate=',start_date_series),
                                   paste0('endDate=',end_date_series))$Data$RETURN/100
  return(interval_return_rate_fund)
}

function_interval_return_rate_index<-function(bench_index,start_date_series,end_date_series){
  interval_return_rate_index<-w.wsd(bench_index,"risk_returnyearly_index",start_date_series,end_date_series,
                                    "returnType=1;Period=Y")$Data$RISK_RETURNYEARLY_INDEX/100
  return(interval_return_rate_index)
}

interval_return_rate_list<-data.frame(matrix(NA, ncol=6, nrow = 0))
for (i in 1:length(start_date_series)) {
  interval_return_rate_fund<-function_interval_return_rate_fund(fund_code,start_date_series[i],end_date_series[i])
  interval_return_rate_bench_industry<-function_interval_return_rate_index(bench_index_industry,start_date_series[i],end_date_series[i])
  interval_return_rate_bench_all<-function_interval_return_rate_index(bench_index_all,start_date_series[i],end_date_series[i])
  
  interval_return_rate_list[i,1]<-fund_code
  interval_return_rate_list[i,2]<-start_date_series[i]
  interval_return_rate_list[i,3]<-end_date_series[i]
  interval_return_rate_list[i,4]<-interval_return_rate_fund
  interval_return_rate_list[i,5]<-interval_return_rate_bench_industry
  interval_return_rate_list[i,6]<-interval_return_rate_bench_all
}
names(interval_return_rate_list)<-c("fund code","start date","end date","interval return-fund","interval return-fund-000998.SH","interval return-000300.SH")

write.csv(interval_return_rate_list, "interval_return_rate_list.csv")
plot()  

# 3. Calculate rank in TMT fund pool

