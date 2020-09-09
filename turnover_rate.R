# This script is used to calculate turnover rate of funds
# To run this script, you need to install and library package "WindR" and "stringr" first
# 由于买入成本和卖出收入的指标只在年报和半年报披露，且2020H1还未披露，所以只计算到2019年末，半年报披露后可修改接口中的日期参数更新至2020H1
# 2020H1的数据求出后要年化（*2）

t1<-proc.time()

# Define fundemental variables
fund_code<-c("000940.OF","002692.OF","006751.OF","257070.OF","001513.OF","320007.OF","007490.OF","001071.OF","519674.OF","000697.OF","519772.OF",
             "000404.OF","110013.OF","002939.OF","610002.OF")
turnover_rate_result<-data.frame(matrix(NA, ncol=6, nrow = 0))

for(j in 1:length(fund_code)){
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  
  # 开始管理日期在下半年，舍弃该年数据，从下年开始算
  if(as.numeric(substr(manager_start_date,6,7))>6){
    quarter_date_series<-w.tdays(manager_start_date,"2019-12-31","Days=Alldays;Period=Q")$Data$DATETIME
    year_date_series<-w.tdays(manager_start_date,"2019-12-31","Days=Alldays;Period=Y")$Data$DATETIME[-1]
    
    cost_and_income_list<-data.frame(matrix(NA, ncol=4, nrow = 0))
    # 求分子（平均收入和支出）
    for(i in 1:length(year_date_series)){
      cost_and_income<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',year_date_series[i]))
      cost_and_income_list[i,1]<-fund_code[j]
      cost_and_income_list[i,2]<-as.character(year_date_series[i])
      cost_and_income_list[i,3]<-(cost_and_income$Data$PRT_BUYSTOCKCOST+cost_and_income$Data$PRT_SELLSTOCKINCOME)/2
      cost_and_income_list[i,4]<-substr(year_date_series[i],1,4)
    }
    colnames(cost_and_income_list)<-c("fund_code","date","average_cost_and_income","year")
    
    # 求分母（季度平均资产净值）
    stock_value_quarter<-data.frame(matrix(NA, ncol=3, nrow = 0))
    for(k in 1:length(quarter_date_series)){
      stock_value<-w.wss(fund_code[j],'prt_fundnetasset_total','unit=1',
                         paste0('rptDate=',quarter_date_series[k]))$Data$PRT_FUNDNETASSET_TOTAL
      # 排除某些产品成立后当期没有披露季报、进而没有资产净值数据的情况
      if(stock_value!='NaN'){
        date<-as.character(quarter_date_series[k])
        year<-substr(quarter_date_series[k],1,4)
        stock_value_temp<-data.frame(matrix(c(date,stock_value,year),ncol=3))
        stock_value_quarter<-rbind(stock_value_quarter,stock_value_temp)
      }
    }
    colnames(stock_value_quarter)<-c("date","stock_value","year")
    
    # 季度资产净值数据，按年份求平均值,得到分母
    stock_value_year<-aggregate(as.numeric(stock_value_quarter$stock_value),list(stock_value_quarter$year),mean)
    colnames(stock_value_year)<-c("year","stock_value")
    
    # 把分子分母的数值按年份merge，求出换手率
    merge_result<-merge(cost_and_income_list,stock_value_year,by=intersect(names(cost_and_income_list), names(stock_value_year)))
    merge_result$turnover_rate<-merge_result$average_cost_and_income/merge_result$stock_value
  }
  
  # 开始管理日期在上半年，需使用该年所覆盖的数据
  else{
    quarter_date_series<-w.tdays(manager_start_date,"2019-12-31","Days=Alldays;Period=Q")$Data$DATETIME
    year_date_series<-w.tdays(manager_start_date,"2019-12-31","Days=Alldays;Period=Y")$Data$DATETIME
    cost_and_income_list<-data.frame(matrix(NA, ncol=4, nrow = 0))
    stock_value_quarter<-data.frame(matrix(NA, ncol=3, nrow = 0))
    fund_setup_date<-w_wss_data<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
    
    # 如果成立日期等于接管日期，收入和成本需要使用年报数据的年化值（按年末到赎回开放日总天数年化）
    if(fund_setup_date==manager_start_date){
      
      # 获取赎回开放日
      redm_start_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
      interval_days<-as.numeric(difftime(year_date_series[1],redm_start_date))
      
      cost_and_income<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',year_date_series[1]))
      cost_and_income_list[1,1]<-fund_code[j]
      cost_and_income_list[1,2]<-as.character(year_date_series[1])
      # 将年报数据按天数年化，得到年化的收入成本估计值
      cost_and_income_list[1,3]<-(cost_and_income$Data$PRT_BUYSTOCKCOST+cost_and_income$Data$PRT_SELLSTOCKINCOME)/2/(interval_days/365)
      cost_and_income_list[1,4]<-substr(year_date_series[1],1,4)
      
      if(length(year_date_series)>=2){
        for(i in 2:length(year_date_series)){
          cost_and_income<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',year_date_series[i]))
          cost_and_income_list[i,1]<-fund_code[j]
          cost_and_income_list[i,2]<-as.character(year_date_series[i])
          cost_and_income_list[i,3]<-(cost_and_income$Data$PRT_BUYSTOCKCOST+cost_and_income$Data$PRT_SELLSTOCKINCOME)/2
          cost_and_income_list[i,4]<-as.numeric(substr(year_date_series[i],1,4))
        }
      }
      colnames(cost_and_income_list)<-c("fund_code","date","average_cost_and_income","year")
      
      for(k in 1:length(quarter_date_series)){
        stock_value<-w.wss(fund_code[j],'prt_fundnetasset_total','unit=1',paste0('rptDate=',quarter_date_series[k]))$Data$PRT_FUNDNETASSET_TOTAL
        if(stock_value!='NaN'){
          date<-as.character(quarter_date_series[k])
          year<-as.numeric(substr(quarter_date_series[k],1,4))
          stock_value_temp<-data.frame(matrix(c(date,stock_value,year),ncol=3))
          stock_value_quarter<-rbind(stock_value_quarter,stock_value_temp)
        }
      }
      colnames(stock_value_quarter)<-c("date","stock_value","year")
      
      # 按年份求平均值,得到分母
      stock_value_year<-aggregate(as.numeric(stock_value_quarter$stock_value),list(stock_value_quarter$year),FUN=mean)
      colnames(stock_value_year)<-c("year","stock_value")
      
      # 把分子分母的数值按年份merge
      merge_result<-merge(cost_and_income_list,stock_value_year,by=intersect(names(cost_and_income_list), names(stock_value_year)))
      merge_result$turnover_rate<-merge_result$average_cost_and_income/merge_result$stock_value
    }
    
    # 如果成立日不等于接管日，即基金经理是后面接手的产品，第一年的收入和成本数据要使用（年报数据-半年报数据）*2来估算
    else{
      # 提取接管日后的第一个半年报日期，和收入成本数据
      half_year_date<-w.tdays(manager_start_date,"","Days=Alldays;Period=S")$Data$DATETIME[1]
      cost_and_income_half_year<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',half_year_date))

      cost_and_income<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',year_date_series[1]))
      cost_and_income_list[1,1]<-fund_code[j]
      cost_and_income_list[1,2]<-as.character(year_date_series[1])
      # 计算收入成本时用2*（年报-半年报）得到年化的收入成本估计值
      cost_and_income_list[1,3]<-(cost_and_income$Data$PRT_BUYSTOCKCOST-cost_and_income_half_year$Data$PRT_BUYSTOCKCOST)
      +(cost_and_income$Data$PRT_SELLSTOCKINCOME-cost_and_income_half_year$Data$PRT_SELLSTOCKINCOME)
      cost_and_income_list[1,4]<-substr(year_date_series[1],1,4)
      
      # 排除某些产品只有一个年度报告期的情况
      if(length(year_date_series)>=2){
        for(i in 2:length(year_date_series)){
          
          cost_and_income<-w.wss(fund_code[j],'prt_buystockcost,prt_sellstockincome','unit=1',paste0('rptDate=',year_date_series[i]))
          cost_and_income_list[i,1]<-fund_code[j]
          cost_and_income_list[i,2]<-as.character(year_date_series[i])
          cost_and_income_list[i,3]<-(cost_and_income$Data$PRT_BUYSTOCKCOST+cost_and_income$Data$PRT_SELLSTOCKINCOME)/2
          cost_and_income_list[i,4]<-substr(year_date_series[i],1,4)
        }
      }
      colnames(cost_and_income_list)<-c("fund_code","date","average_cost_and_income","year")
    }
    
    # 求分母（季度平均资产净值）
    stock_value_quarter<-data.frame(matrix(NA, ncol=3, nrow = 0))
    for(k in 1:length(quarter_date_series)){
      stock_value<-w.wss(fund_code[j],'prt_fundnetasset_total','unit=1',
                         paste0('rptDate=',quarter_date_series[k]))$Data$PRT_FUNDNETASSET_TOTAL
      # 排除某些产品成立后当期没有披露季报、进而没有资产净值数据的情况
      if(stock_value!='NaN'){
        date<-as.character(quarter_date_series[k])
        year<-substr(quarter_date_series[k],1,4)
        stock_value_temp<-data.frame(matrix(c(date,stock_value,year),ncol=3))
        stock_value_quarter<-rbind(stock_value_quarter,stock_value_temp)
      }
    }
    colnames(stock_value_quarter)<-c("date","stock_value","year")
    
    # 季度资产净值数据，按年份求平均值,得到分母
    stock_value_year<-aggregate(as.numeric(stock_value_quarter$stock_value),list(stock_value_quarter$year),mean)
    colnames(stock_value_year)<-c("year","stock_value")
    
    # 把分子分母的数值按年份merge，求出换手率
    merge_result<-merge(cost_and_income_list,stock_value_year,by=intersect(names(cost_and_income_list), names(stock_value_year)))
    merge_result$turnover_rate<-merge_result$average_cost_and_income/merge_result$stock_value
    
  }
  turnover_rate_result<-rbind(turnover_rate_result,merge_result)
}

write.csv(turnover_rate_result, "turnover_rate_result.csv")

t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))


