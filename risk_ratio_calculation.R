#This script is used to calculate risk index: Maxdownside, Info ratio, Treynor ratio and Jensen ratio
#To run this script, you need to install and library package "WindR" first

#Define fundemental variables
fund_code<-c("000940.OF","002692.OF","001071.OF")
bench_index<-"000998.SH"
r_f<-0.015
start_date_series<-c("2017-01-01","2018-01-01","2019-01-01","2020-01-01")
end_date_series<-c("2017-12-31","2018-12-31","2019-12-31","2020-06-30")
result_list<-data.frame(matrix(NA, ncol=7, nrow = 0))

#get interval return rate
function_interval_return_rate_fund<-function(fund_code,start_date_series,end_date_series){
  interval_return_rate_fund<-w.wss(fund_code,'return','annualized=1',paste0('startDate=',start_date_series),
                                   paste0('endDate=',end_date_series))$Data$RETURN/100
  return(interval_return_rate_fund)
}

function_interval_return_rate_bench<-function(bench_index,start_date_series,end_date_series){
  interval_return_rate_bench<-w.wsd(bench_index,"risk_returnyearly_index",start_date_series,end_date_series,
                                    "returnType=1;Period=Y")$Data$RISK_RETURNYEARLY_INDEX/100
  return(interval_return_rate_bench)
}

#get daily return rate
function_daily_return_rate_fund<-function(fund_code,start_date_series,end_date_series){
  daily_return_rate_fund<-w.wsd(fund_code,"NAV_adj_return1",start_date_series,end_date_series,"Period=D")$Data$NAV_ADJ_RETURN1/100
  return(daily_return_rate_fund)
  }

function_daily_return_rate_bench<-function(bench_index,start_date_series,end_date_series){
  daily_return_rate_bench<-w.wsd(bench_index,"pct_chg",start_date_series,end_date_series)$Data$PCT_CHG/100
  return(daily_return_rate_bench)
}

#calculate beta
function_calculate_beta<-function(daily_return_rate_fund,daily_return_rate_bench){
  beta_data<-data.frame(daily_return_rate_fund,daily_return_rate_bench)
  beta_regression<-lm(formula = daily_return_rate_fund~daily_return_rate_bench,data = beta_data)
  beta<-beta_regression[["coefficients"]][["daily_return_rate_bench"]]
  return(beta)
}

# calculate info ratio
function_info_ratio<-function(daily_return_rate_fund,daily_return_rate_bench){
  daily_excess_return<-daily_return_rate_fund-daily_return_rate_bench
  info_ratio<-(250)^(1/2)*mean(daily_excess_return)/sd(daily_excess_return)
  return(info_ratio)
}

#calculate Treynor ratio
function_Treynor_ratio<-function(interval_return_rate_fund,r_f,beta){
  Treynor_ratio<-(interval_return_rate_fund-r_f)/beta
  return(Treynor_ratio)
}

#calculate Jensen ratio
function_Jensen_ratio<-function(interval_return_rate_fund,interval_return_rate_bench,r_f,beta){
  Jensen_ratio<-((interval_return_rate_fund-r_f)-beta*(interval_return_rate_bench-r_f))
  return(Jensen_ratio)
}

# calculate maxdownside
function_maxdownside<-function(fund_code,start_date_series,end_date_series){
  maxdownside<-w.wss(fund_code,'risk_maxdownside',paste0('startDate=',start_date_series),
                     paste0('endDate=',end_date_series))$Data$RISK_MAXDOWNSIDE/100
  return(maxdownside)
  }

for (j in 1:length(fund_code)){
  result_list_single<-data.frame(matrix(NA, ncol=7, nrow = 0))
  for (i in 1:length(end_date_series)) {
    interval_return_rate_fund<-function_interval_return_rate_fund(fund_code[j],start_date_series[i],end_date_series[i])
    interval_return_rate_bench<-function_interval_return_rate_bench(bench_index,start_date_series[i],end_date_series[i])
    daily_return_rate_fund<-function_daily_return_rate_fund(fund_code[j],start_date_series[i],end_date_series[i])
    daily_return_rate_bench<-function_daily_return_rate_bench(bench_index,start_date_series[i],end_date_series[i])
    
    beta<-function_calculate_beta(daily_return_rate_fund,daily_return_rate_bench)
    
    info_ratio<-function_info_ratio(daily_return_rate_fund,daily_return_rate_bench)
    
    Treynor_ratio<-function_Treynor_ratio(interval_return_rate_fund,r_f,beta)

    Jensen_ratio<-function_Jensen_ratio(interval_return_rate_fund,interval_return_rate_bench,r_f,beta)
    
    maxdownside<-function_maxdownside(fund_code[j],start_date_series[i],end_date_series[i])
    
    result_list_single[i,1]<-fund_code[j]
    result_list_single[i,2]<-start_date_series[i]
    result_list_single[i,3]<-end_date_series[i]
    result_list_single[i,4]<-info_ratio
    result_list_single[i,5]<-Treynor_ratio
    result_list_single[i,6]<-Jensen_ratio
    result_list_single[i,7]<-maxdownside
  }
  result_list<-rbind(result_list,result_list_single)
}
names(result_list)<-c("product code","start date","end date","Info ratio","Treynor","Jensen","Maxdownside")
write.csv(result_list, "risk_index_result_list.csv")




