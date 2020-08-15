#This script is used to calculate risk index: Maxdownside, Info ratio, Treynor ratio and Jensen ratio
#To run this script, you need to install and library package "WindR" first

#Define fundemental variables
t1<-proc.time()

fund_code<-c("000940.OF","002692.OF","006751.OF","257070.OF","001513.OF","320007.OF","007490.OF","001071.OF",
             "519674.OF","000697.OF","519772.OF","000404.OF","110013.OF","002939.OF","610002.OF")

bench_index<-"000998.SH"
r_f<-0.015
result_list<-data.frame(matrix(NA, ncol=8, nrow = 0))

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
function_info_ratio<-function(interval_return_rate_fund,interval_return_rate_bench,daily_return_rate_fund,daily_return_rate_bench){
  daily_excess_return<-daily_return_rate_fund-daily_return_rate_bench
  info_ratio<-(interval_return_rate_fund-interval_return_rate_bench)/((250)^(1/2)*sd(daily_excess_return))
  # info_ratio<-(250)^(1/2)*mean(daily_excess_return)/sd(daily_excess_return)

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
  
  result_list_single<-data.frame(matrix(NA, ncol=8, nrow = 0)) ### ncol is number of index in result list

  # 如果成立日=基金经理接管日：起始日用赎回开始日
  # 如果成立日≠基金经理接管日：起始日用基金经理接管日
  
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-w_wss_data<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  if(fund_setup_date==manager_start_date){
    start_date_series<-c(fund_redmstart_date,w.tdays(fund_redmstart_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1)
    end_date_series<-w.tdays(fund_redmstart_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
  }
  else{
    start_date_series<-c(manager_start_date,w.tdays(manager_start_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1)
    end_date_series<-w.tdays(manager_start_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
  }
  

  for (i in 1:length(end_date_series)) {
    
    interval_return_rate_fund<-function_interval_return_rate_fund(fund_code[j],start_date_series[i],end_date_series[i])
    interval_return_rate_bench<-function_interval_return_rate_bench(bench_index,start_date_series[i],end_date_series[i])
    daily_return_rate_fund<-function_daily_return_rate_fund(fund_code[j],start_date_series[i],end_date_series[i])
    daily_return_rate_bench<-function_daily_return_rate_bench(bench_index,start_date_series[i],end_date_series[i])
    
    beta<-function_calculate_beta(daily_return_rate_fund,daily_return_rate_bench)
    
    info_ratio<-function_info_ratio(interval_return_rate_fund,interval_return_rate_bench,daily_return_rate_fund,daily_return_rate_bench)
    
    Treynor_ratio<-function_Treynor_ratio(interval_return_rate_fund,r_f,beta)

    Jensen_ratio<-function_Jensen_ratio(interval_return_rate_fund,interval_return_rate_bench,r_f,beta)
    
    maxdownside<-function_maxdownside(fund_code[j],start_date_series[i],end_date_series[i])
    
    result_list_single[i,1]<-fund_code[j]
    result_list_single[i,2]<-(fund_setup_date==manager_start_date)
    result_list_single[i,3]<-as.character(start_date_series[i])
    result_list_single[i,4]<-as.character(end_date_series[i])
    result_list_single[i,5]<-info_ratio
    result_list_single[i,6]<-Treynor_ratio
    result_list_single[i,7]<-Jensen_ratio
    result_list_single[i,8]<-maxdownside
  }
  result_list<-rbind(result_list,result_list_single)
}
names(result_list)<-c("product code","setupdate equals to manager startdate","start date",
                      "end date","Info ratio","Treynor","Jensen","Maxdownside")
write.csv(result_list, "risk_index_result_list.csv")

t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))

