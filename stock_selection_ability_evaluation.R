#This script is used to build model to evaluate product's stock-choosing ability
#To run this script, you need to install and library package "WindR" and "stringr" first

#Define fundemental variables
fund_code<-fund_code<-c("257070.OF","001513.OF","320007.OF","007490.OF","001071.OF","519674.OF","000697.OF","519772.OF",
                        "000404.OF","110013.OF","002939.OF","610002.OF","000940.OF","002692.OF","006751.OF")    #####1.add loop to run multiple fund_code
bench_index<-"000998.SH"

r_f<-0.015
#for weekly data
r_f_adjusted<-(1+r_f)^(1/52)-1
#for monthly data
#r_f_adjusted<-(1+r_f)^(1/12)-1
#for daily data
#r_f_adjusted<-(1+r_f)^(1/250)-1

stock_selection_ability_list<-data.frame(matrix(NA, ncol=7, nrow = 0)) 

for(j in 1:length(fund_code)){
  
  # 如果成立日=基金经理接管日：起始日用基金赎回开始日
  # 如果成立日≠基金经理接管日：起始日用基金经理接管日
  
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-w_wss_data<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  if(fund_setup_date==manager_start_date){
    
    if(substr(fund_redmstart_date,6,7)<=11){
      start_date_series<-c(fund_redmstart_date,w.tdays(fund_redmstart_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1,fund_redmstart_date)
      end_date_series<-c(w.tdays(fund_redmstart_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME,"2020-06-30")
    }
    else{
      start_date_series<-c(w.tdays(fund_redmstart_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1,fund_redmstart_date)
      end_date_series<-w.tdays(fund_redmstart_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
      end_date_series<-c(end_date_series[2:length(end_date_series)],"2020-06-30")
    }
    
  }
  else{
    
    if(substr(manager_start_date,6,7)<=11){
      start_date_series<-c(manager_start_date,w.tdays(manager_start_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1,manager_start_date)
      end_date_series<-c(w.tdays(manager_start_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME,"2020-06-30")
    }
    else{
      start_date_series<-c(w.tdays(manager_start_date,"20191231","Period=Y;Days=Alldays")$Data$DATETIME+1,manager_start_date)
      end_date_series<-w.tdays(manager_start_date,"20200630","Period=Y;Days=Alldays")$Data$DATETIME
      end_date_series<-c(end_date_series[2:length(end_date_series)],"2020-06-30")
    }
    
  }
  
  stock_selection_ability_single<-data.frame(matrix(NA, ncol=7, nrow = 0)) 
  
  for (i in 1:length(start_date_series)) {
    #T-M model

    #get serial of return rate    ######3.NAV_adj_return check weekly return
    fund_net_value<-w.wsd(fund_code[j],"NAV_adj",start_date_series[i],end_date_series[i],"Period=W")
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
    stock_selection_ability_single[i,1]<-fund_code[j]
    stock_selection_ability_single[i,2]<-as.character(start_date_series[i])
    stock_selection_ability_single[i,3]<-as.character(end_date_series[i])
    stock_selection_ability_single[i,4]<-result$coefficients[1,1]
    stock_selection_ability_single[i,5]<-result$coefficients[1,4]   #####4.export  two coefficients and p value and sig level
    stock_selection_ability_single[i,6]<-result$coefficients[3,1]
    stock_selection_ability_single[i,7]<-result$coefficients[3,4]
  
  }
  stock_selection_ability_list<-rbind(stock_selection_ability_list,stock_selection_ability_single)
}

names(stock_selection_ability_list)<-c("product code","start date","end date","stock selection","sig level","time selection","sig level")
write.csv(stock_selection_ability_list, "stock_selection_ability_list_0812.csv")
