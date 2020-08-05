#Define fundemental variables
fund_code<-'000940.OF'
start_date<-"2015-01-23"
end_date<-"2020-06-30"

#Fetch cumulative rate of return of fund and plot it
w_wsd_data<-w.wsd(fund_code,"SI_nav_adj_return",start_date,end_date)
Cumulative_rate_of_return<-w_wsd_data$Data$SI_NAV_ADJ_RETURN
Date<-w_wsd_data$Data$DATETIME
plot(Date,Cumulative_rate_of_return,type='l')

#Fetch cumulative rate of return of index
w_wsd_data_index<-w.wsd("000998.SH","close","2015-01-23","2020-06-30")
function_index_return<-function(index_value){
  return(index_value/index_value[1]-1)
}
