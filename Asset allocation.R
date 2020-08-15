# This script is to calculate industry purity of a fund (take TMT as example)
# To run this script, you need to install and library package "WindR" and "stringr" first

t1<-proc.time()

# Define fundemental variables
fund_code<-c("000940.OF","002692.OF","006751.OF")     ######1.add loop for multiple fund_code\start date\end date
industry_list<-c('电子','计算机','传媒','通信')
asset_allocation_result<-data.frame(matrix(NA, ncol=8, nrow = 0))

for(j in 1:length(fund_code)){
  
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  if(fund_setup_date==manager_start_date){
    start_date<-fund_redmstart_date
  }else{
    start_date<-manager_start_date
  }
  end_date<-"2020-06-30"
  report_date<-w.tdays(start_date,end_date,"Period=Q;Days=Alldays")$Data$DATETIME
  
  # stock proportion 
  stock_proportion<-w.wsd(fund_code[j],"prt_stocktonav",start_date,end_date,"Period=Q;Days=Alldays")
  plot(stock_proportion$Data$DATETIME,stock_proportion$Data$PRT_STOCKTONAV,type="l",
       main = paste0('报告期股票仓位:',fund_code[j],sep=""), xlab="日期",ylab='股票仓位（%）')
  
  # TMT industry purity and subindustry proportion
  asset_allocation_result_single<-data.frame(matrix(NA, ncol=8, nrow = 0))
  
  for (i in 1:length(report_date)){         ######3.检查是不是有没有行业的股票   按申万行业aggregate
    # 1. Fetch Heavy Stock                       
    
    heavy_stock<-w.wset('allfundhelddetail',paste(str_c('rptDate=',report_date[i]),str_c('windcode=',fund_code[j]),
                'field=sec_name,rpt_date,stock_code,stock_name,marketvalueofstockholdings',sep=";"))$Data
    
    if (ncol(heavy_stock)!=0){

      heavy_stock$industry<-w.wss(heavy_stock$stock_code,'industry_sw','industryType=1')$Data$INDUSTRY_SW
      industry_aggregate_result<-aggregate(heavy_stock$marketvalueofstockholdings,list(heavy_stock$industry),FUN = sum)
      colnames(industry_aggregate_result)<-c("industry","market_value")
      total_marketvalueofstockholdings<-sum(heavy_stock$marketvalueofstockholdings)
      
      electronic_proportion<-sum(industry_aggregate_result$market_value[industry_aggregate_result$industry=="电子"])/total_marketvalueofstockholdings 
      cs_proportion<-sum(industry_aggregate_result$market_value[industry_aggregate_result$industry=="计算机"])/total_marketvalueofstockholdings 
      media_proportion<-sum(industry_aggregate_result$market_value[industry_aggregate_result$industry=="传媒"])/total_marketvalueofstockholdings
      telecom_proportion<-sum(industry_aggregate_result$market_value[industry_aggregate_result$industry=="通信"])/total_marketvalueofstockholdings
      TMT_industry_purity<-electronic_proportion+cs_proportion+media_proportion+telecom_proportion
      
      # 4. Save results
      asset_allocation_result_single[i,1]<-fund_code[j]
      asset_allocation_result_single[i,2]<-as.character(heavy_stock$rpt_date[1])
      asset_allocation_result_single[i,3]<-electronic_proportion
      asset_allocation_result_single[i,4]<-cs_proportion
      asset_allocation_result_single[i,5]<-media_proportion
      asset_allocation_result_single[i,6]<-telecom_proportion
      asset_allocation_result_single[i,7]<-TMT_industry_purity
      asset_allocation_result_single[i,8]<-stock_proportion$Data$PRT_STOCKTONAV[i]
    }
  }
  asset_allocation_result<-rbind(asset_allocation_result,asset_allocation_result_single)
  
}

names(asset_allocation_result)<-c("product code","reporting date","电子","计算机","传媒","通信","TMT行业","股票仓位")
write.csv(asset_allocation_result, "asset_allocation_result_inner.csv")
  
t2<-proc.time()
t<-t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))
  
  