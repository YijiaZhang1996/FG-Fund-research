# This code is used to evaluate overweighted and underweighted return, relative to a specific index
# To run this script, you need to install and library package "WindR" and "stringr" first

# Define fundemental variables
t1<-proc.time()

fund_code<-c('000940.OF','002692.OF','006751.OF')
index_code<-"000998.SH"
over_under_weighted_result<-data.frame(matrix(NA, ncol=6, nrow = 0)) 

for(j in 1:length(fund_code)){
  
  over_under_weighted_result_single<-data.frame(matrix(NA, ncol=6, nrow = 0)) 
  
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-w_wss_data<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  if(fund_setup_date==manager_start_date){
    report_date<-w.tdays(fund_redmstart_date,"2020-06-30","Days=Alldays;Period=S")$Data$DATETIME
  }else{
    report_date<-w.tdays(manager_start_date,"2020-06-30","Days=Alldays;Period=S")$Data$DATETIME
  }
  
  function_get_end_date<-function(return_start_date){
    return_end_date<-w.tdaysoffset(1,return_start_date,"Days=Alldays;Period=Q")$Data$DATETIME
    return(return_end_date)
  }
  
  return_start_date<-report_date+1
  return_end_date<-sapply(as.list(report_date), function_get_end_date)
  return_end_date<-as.Date(return_end_date,origin = "1970-01-01")
  return_start_date<-return_start_date[-length(return_start_date)]
  return_end_date<-return_end_date[-length(return_end_date)]
  #####1.wind function后退一个季度，不用手动输入日期
  
  for(i in 1:length(report_date)-1){
    
    # Fetch index constituent and fund heavy stock
    index_constituent<-w.wset('indexconstituent',paste(str_c('date=',report_date[i]),str_c('windcode=',index_code),sep=";"))$Data
    fund_heavy_stock<-w.wset('allfundhelddetail',paste(str_c('rptDate=',report_date[i]),str_c('windcode=',fund_code[j]),
                                                       'field=sec_name,rpt_date,stock_code,stock_name,marketvalueofstockholdings',sep=";"))$Data
    # 确保所选报告期有重仓数据 
    if (ncol(fund_heavy_stock)!=0) {
      total_marketvalueofstockholdings<-sum(fund_heavy_stock$marketvalueofstockholdings)
      # calculate NAV weight of heavy stock
      for(k in 1:length(fund_heavy_stock$stock_code)){
        fund_heavy_stock$weight[k]<-100*fund_heavy_stock$marketvalueofstockholdings[k]/total_marketvalueofstockholdings
      }
      colnames(fund_heavy_stock)[4]<-"wind_code"
      merge_data<-merge(index_constituent,fund_heavy_stock,by="wind_code",all.x = TRUE)
      merge_data$weight[is.na(merge_data$weight)]<-0
      
      # 持有指数成分占基金净值的比
      total_indexconstituent_held_in_portfolio<-sum(merge_data$weight)
      
      # calculate over and under weight
      merge_data$over_under_weight<-merge_data$weight-merge_data$i_weight  ########3.merge by stock code
      
      # calculate interval return of stock
      merge_data$pct_chg<-w.wss(merge_data$wind_code,'pct_chg_per',
                                paste0('startDate=',return_start_date[i]),paste0('endDate=',return_end_date[i]))$Data$PCT_CHG_PER
      
      # calculate overweighted and underweighted return
      merge_data$weight_diff_return<-merge_data$over_under_weight*merge_data$pct_chg
      
      # get sum of overweighted and underweighted return
      overweighted_return<-sum(merge_data[merge_data$over_under_weight>0,"weight_diff_return"])/100
      underweighted_return<-sum(merge_data[merge_data$over_under_weight<0,"weight_diff_return"])/100
      
      # save result
      over_under_weighted_result_single[i,1]<-fund_code[j]
      over_under_weighted_result_single[i,2]<-as.character(return_start_date[i])
      over_under_weighted_result_single[i,3]<-as.character(return_end_date[i])
      over_under_weighted_result_single[i,4]<-underweighted_return
      over_under_weighted_result_single[i,5]<-overweighted_return
      over_under_weighted_result_single[i,6]<-total_indexconstituent_held_in_portfolio
    }
  }
  over_under_weighted_result<-rbind(over_under_weighted_result,over_under_weighted_result_single)
}

# rename and save result to excel
names(over_under_weighted_result)<-c("fund code","start date","end date","underweighted_return",
                                     "overweighted_return","indexconstituent held in portfolio")
write.csv(over_under_weighted_result, "over_under_weighted_result_inner.csv")

t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))

