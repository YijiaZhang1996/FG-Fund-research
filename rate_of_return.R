# This script is used to evaluate past performance of a fund, including return rate and rank in different interval
# To run this script, you need to install and library package "WindR" first

# Define fundemental variables
t1<-proc.time()

fund_code<-c("257070.OF","001513.OF","320007.OF","007490.OF","001071.OF","519674.OF","000697.OF","519772.OF",
             "000404.OF","110013.OF","002939.OF","610002.OF")
bench_index_industry<-"000998.SH"
bench_index_all<-"000300.SH"
interval_return_rate_list<-data.frame(matrix(NA, ncol=6, nrow = 0)) 

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

function_daily_return<-function(daily_value){
  return(100*(daily_value/daily_value[1]-1))
}

for (j in 1:length(fund_code)) {
  
  interval_return_rate_list_single<-data.frame(matrix(NA, ncol=6, nrow = 0)) 
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  if(fund_setup_date==manager_start_date){
    start_date<-fund_redmstart_date
    end_date<-"20200630"
    start_date_series<-c(fund_redmstart_date,w.tdays(fund_redmstart_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1)
    end_date_series<-w.tdays(fund_redmstart_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
  }else{
    start_date<-manager_start_date
    end_date<-"20200630"
    start_date_series<-c(manager_start_date,w.tdays(manager_start_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1)
    end_date_series<-w.tdays(manager_start_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
  }
  
  #####1.自成立日起复权单位净值增长率，可能不是成立日开始
  fund_nav<-w.wsd(fund_code[j],"NAV_adj",start_date,end_date)
  TMT_industry_index<-w.wsd(bench_index_industry,"close",start_date,end_date)
  hs_300_index<-w.wsd(bench_index_all,"close",start_date,end_date)
  
  Cumulative_rate_of_return_fund<-function_daily_return(fund_nav$Data$NAV_ADJ)
  Cumulative_rate_of_return_industry_index<-function_daily_return(TMT_industry_index$Data$CLOSE)
  Cumulative_rate_of_return_hs300_index<-function_daily_return(hs_300_index$Data$CLOSE)
  Date<-fund_nav$Data$DATETIME
  
  ######2.加图例   ylim范围太小
  plot(Date,Cumulative_rate_of_return_fund,type='l',main = paste0('接管以来累计收益率:',fund_code[j],sep=""),xlab='时间',ylab='累计收益率(%)',
       ylim=c(min(Cumulative_rate_of_return_fund)-30,max(Cumulative_rate_of_return_fund)+30))
  lines(Date,Cumulative_rate_of_return_industry_index,type='l',col="red" )
  lines(Date,Cumulative_rate_of_return_hs300_index,type='l',col="blue" )
  
  legend("top",legend=c("Fund","TMT Industry Index","HS300 Index"),cex=0.5,ncol=1,bty="n",col=c("black","red","blue"),lwd=1)  

  
  # 2. Calculate interval return
  # get interval return rate
    
  for (i in 1:length(start_date_series)) {
    interval_return_rate_fund<-function_interval_return_rate_fund(fund_code[j],start_date_series[i],end_date_series[i])
    interval_return_rate_bench_industry<-function_interval_return_rate_index(bench_index_industry,start_date_series[i],end_date_series[i])
    interval_return_rate_bench_all<-function_interval_return_rate_index(bench_index_all,start_date_series[i],end_date_series[i])
    
    interval_return_rate_list_single[i,1]<-fund_code[j]
    interval_return_rate_list_single[i,2]<-as.character(start_date_series[i])
    interval_return_rate_list_single[i,3]<-as.character(end_date_series[i])
    interval_return_rate_list_single[i,4]<-interval_return_rate_fund
    interval_return_rate_list_single[i,5]<-interval_return_rate_bench_industry
    interval_return_rate_list_single[i,6]<-interval_return_rate_bench_all
  }
  interval_return_rate_list<-rbind(interval_return_rate_list,interval_return_rate_list_single)
}


names(interval_return_rate_list)<-c("fund code","start date","end date","interval return-fund","interval return-fund-000998.SH","interval return-000300.SH")

write.csv(interval_return_rate_list, "interval_return_rate_list_outer.csv")   

# 3. Calculate rank in TMT fund pool          ####4.读取可比名单，加rank排名  参考OverseaFund[, RANK.YTD := frank(-YTDReturn, ties.method = "min"), by="FundType2"]

t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))

