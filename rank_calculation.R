# This script is used to get rank of a fund among comparative funds
# To run this script, you need to install and library package "WindR" first

# define fundemental variables
fund_code<-c("000940.OF","002692.OF","006751.OF","257070.OF","001513.OF","320007.OF","007490.OF","001071.OF","519674.OF","000697.OF","519772.OF",
             "000404.OF","110013.OF","002939.OF","610002.OF")
comparative_fund_list<-read.csv("comparative fund.csv",header = T)
original_start_date_series<-c("2011-06-30","2015-01-01","2015-06-18","2015-10-01","2016-01-01","2018-03-02","2019-01-01")
original_end_date_series<-c("2014-12-31","2015-06-17","2015-09-30","2015-12-31","2018-03-01","2018-12-30","2020-06-30")
rank_result<-data.frame(matrix(NA, ncol=6, nrow = 0))

for (j in 1:length(fund_code)) {

  # get calculation interval for specific fund
  fund_redmstart_date<-as.Date(w.wss(fund_code[j],'fund_redmstartdate')$Data$FUND_REDMSTARTDATE,origin="1899-12-30")
  manager_start_date<-as.Date(w.wss(fund_code[j],'fund_manager_startdate','order=1')$Data$FUND_MANAGER_STARTDATE,origin="1899-12-30")
  fund_setup_date<-as.Date(w.wss(fund_code[j],'fund_setupdate')$Data$FUND_SETUPDATE,origin="1899-12-30")
  
  rank_result_single<-data.frame(matrix(NA, ncol=6, nrow = 0))
  
  if(fund_setup_date==manager_start_date){
    start_date<-fund_redmstart_date
  }else{
    start_date<-manager_start_date
  }
  
  if(sum(start_date>original_start_date_series)<length(original_start_date_series)){
    location<-sum(start_date>original_start_date_series)+1
    start_date_series<-c(start_date,original_start_date_series[location:length(original_start_date_series)])
    
    end_date_series<-original_end_date_series[sum(start_date>original_start_date_series):length(original_end_date_series)]
    
    # filter funds existing in above intervals and get interval return
    for(i in 1:length(start_date_series)){
      comparative_fund<-comparative_fund_list[comparative_fund_list$setup_date<=start_date_series[i],]$fund_code
      comparative_fund_return<-w.wss(comparative_fund,'return','annualized=0',paste0('startDate=',start_date_series[i]),
                                     paste0('endDate=',end_date_series[i]))$Data
      comparative_fund_return$rank<-paste(rank(-comparative_fund_return$RETURN),
                                          length(comparative_fund_return$CODE),sep="/")
      comparative_fund_return$rank_pct<-rank(-comparative_fund_return$RETURN)/
        length(comparative_fund_return$CODE)
      
      rank_result_single[i,1]<-fund_code[j]
      rank_result_single[i,2]<-as.character(start_date_series[i])
      rank_result_single[i,3]<-as.character(end_date_series[i])
      rank_result_single[i,4]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$RETURN
      rank_result_single[i,5]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$rank
      rank_result_single[i,6]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$rank_pct
    }
    
  }else{
    end_date<-"2020-06-30"
    comparative_fund<-comparative_fund_list[comparative_fund_list$setup_date<=start_date,]$fund_code
    comparative_fund_return<-w.wss(comparative_fund,'return','annualized=0',paste0('startDate=',start_date),
                                   paste0('endDate=',end_date))$Data
    comparative_fund_return$rank<-paste(rank(-comparative_fund_return$RETURN),
                                        length(comparative_fund_return$CODE),sep="/")
    comparative_fund_return$rank_pct<-rank(-comparative_fund_return$RETURN)/
      length(comparative_fund_return$CODE)
    
    rank_result_single[1,1]<-fund_code[j]
    rank_result_single[1,2]<-as.character(start_date)
    rank_result_single[1,3]<-as.character(end_date)
    rank_result_single[1,4]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$RETURN
    rank_result_single[1,5]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$rank
    rank_result_single[1,6]<-comparative_fund_return[comparative_fund_return$CODE==fund_code[j],]$rank_pct
  }
  rank_result<-rbind(rank_result,rank_result_single)
}


colnames(rank_result)<-c("fund code","start date","end date","interval return","rank","rank percentage")

write.csv(rank_result, "rank_result_all.csv")
