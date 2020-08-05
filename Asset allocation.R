# This script is to calculate industry purity of a fund (take TMT as example)
# To run this script, you need to install and library package "WindR" and "stringr" first

# Define fundemental variables
fund_code<-"000940.OF"
start_date<-"2015-01-23"
end_date<-"2020-06-30"
industry_list<-c('电子','计算机','传媒','通信')
report_date<-w.tdays(start_date,end_date,"Period=Q;Days=Alldays")$Data$DATETIME

# stock proportion 
stock_proportion<-w.wsd("000940.OF","prt_stocktonav",start_date,end_date,"Period=Q;Days=Alldays")
plot(stock_proportion$Data$DATETIME,stock_proportion$Data$PRT_STOCKTONAV,type="l",
     main = "报告期股票仓位", xlab="日期",ylab='股票仓位（%）')

# TMT industry purity and subindustry proportion
# Determine 1st_sw_industry of the heavy_stock
function_TMT_industry<-function(stock_code)
{
  industry_result<-w.wss(stock_code,'industry_sw','industryType=1')
  con1<-is.element(industry_result$Data$INDUSTRY_SW, industry_list)
}

function_cs_industry<-function(stock_code)
{
  industry_result<-w.wss(stock_code,'industry_sw','industryType=1')
  con2<-is.element(industry_result$Data$INDUSTRY_SW, industry_list[2])
}

function_electronic_industry<-function(stock_code)
{
  industry_result<-w.wss(stock_code,'industry_sw','industryType=1')
  con3<-is.element(industry_result$Data$INDUSTRY_SW, industry_list[1])
}

function_media_industry<-function(stock_code)
{
  industry_result<-w.wss(stock_code,'industry_sw','industryType=1')
  con4<-is.element(industry_result$Data$INDUSTRY_SW, industry_list[3])
}

function_telecom_industry<-function(stock_code)
{
  industry_result<-w.wss(stock_code,'industry_sw','industryType=1')
  con5<-is.element(industry_result$Data$INDUSTRY_SW, industry_list[4])
}

num_of_report_date<-0
industry_purity_result<-data.frame(matrix(NA, ncol=3, nrow = 0))
sub_industry_result<-data.frame(matrix(NA, ncol=6, nrow = 0))
for (i in 1:length(report_date)){
  # 1. Fetch Heavy Stock
  w_wset_data<-w.wset('allfundhelddetail',paste(str_c('rptDate=',report_date[i]),str_c('windcode=',fund_code),
                                                'field=sec_name,rpt_date,stock_code,stock_name,marketvalueofstockholdings',sep=";"))
  heavy_stock<-w_wset_data$Data
  print ('All heavy stock in this reporting date are:')
  print (heavy_stock)
  if (ncol(heavy_stock)!=0){
    num_of_report_date=num_of_report_date+1
    # 2. Filter Industry
    required_industry<-subset(heavy_stock, function_TMT_industry(heavy_stock$stock_code))
    cs_industry<-subset(heavy_stock, function_cs_industry(heavy_stock$stock_code))
    electronic_industry<-subset(heavy_stock, function_electronic_industry(heavy_stock$stock_code))
    media_industry<-subset(heavy_stock, function_media_industry(heavy_stock$stock_code))
    telecom_industry<-subset(heavy_stock, function_telecom_industry(heavy_stock$stock_code))
      
    print ('Heavy stock after industry filtering are:')
    print (required_industry)
    # 3. calculate proportion
    total_nav<-sum(heavy_stock$marketvalueofstockholdings)
    industry_purity <- sum(required_industry$marketvalueofstockholdings) / total_nav
    cs_proportion<-sum(cs_industry$marketvalueofstockholdings) / total_nav
    electronic_proportion<-sum(electronic_industry$marketvalueofstockholdings) / total_nav
    media_proportion<-sum(media_industry$marketvalueofstockholdings) / total_nav
    telecom_proportion<-sum(telecom_industry$marketvalueofstockholdings) / total_nav
      
    # 4. Save results
    industry_purity_result[i,1]<-fund_code
    industry_purity_result[i,2]<-heavy_stock$rpt_date[1]
    industry_purity_result[i,3]<-industry_purity
    
    sub_industry_result[i,1]<-fund_code
    sub_industry_result[i,2]<-heavy_stock$rpt_date[1]
    sub_industry_result[i,3]<-electronic_proportion
    sub_industry_result[i,4]<-cs_proportion
    sub_industry_result[i,5]<-media_proportion
    sub_industry_result[i,6]<-telecom_proportion
    }
  }
  names(industry_purity_result)<-c("product code","reporting date","industry purity")
  names(sub_industry_result)<-c("product code","reporting date","电子","计算机","传媒","通信")
  write.csv(industry_purity_result, "industry_purity_result.csv")
  write.csv(sub_industry_result, "sub_industry_result.csv")
  





