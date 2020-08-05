#This script is used to filter important funds in specific industry(This project takes TMT as example)

#To run this script, you need to install and library package "WindR" and "stringr" first

#Define fundemental variables,如果换了产品池和行业，只需要修改这部分的信息，然后run all

#Fund pool for filtering
product_list<-c("006751.OF","000940.OF","002692.OF")
#Time series of reporting date
w_season_data<-w.tdays("2018-01-01","2019-12-31","Days=Alldays;Period=Q")$Data$DATETIME
#Use top 10 heavy stock for calculation
num_of_heavy_stock<-10
# Define industry List
industry_list<-c('电子','计算机','传媒','通信')

# Determine 1st_sw_industry of the heavy_stock
function_industry<-function(stock_code_list)
{
  industry_result<-w.wss(stock_code_list,'industry_sw','industryType=1')
  con<-is.element(industry_result$Data$INDUSTRY_SW, industry_list)
}

# For each product, calculate quarterly average proportion of required industry in top 10 heavy stock 
main_function<-function(fund_code, w_season_data, num_of_heavy_stock)
{
  required_industry_proportion_global<-0
  num_of_reporting_date<-0
  for (i in 1:length(w_season_data)){
    # 1. Fetch Heavy Stock
    w_wset_data<-w.wset('allfundhelddetail',paste(str_c('rptDate=',w_season_data[i]),str_c('windcode=',fund_code),
                        'field=sec_name,rpt_date,stock_code,stock_name,marketvalueofstockholdings',sep=";"))
    data_top_ten<-w_wset_data$Data[1:10,]
    print ('All top 10 heavy stock are :')
    print (data_top_ten)
    if (ncol(data_top_ten)!=0){
      num_of_reporting_date=num_of_reporting_date+1
      # 2. Calculate total nav of top 10 heavy stock
      total_nav_of_top_10_heavy_stock<-sum(data_top_ten$marketvalueofstockholdings)
      print (paste("Total nav of top 10 heavy stock are:",total_nav_of_top_10_heavy_stock))
      # 3. Filter Industry
      required_industry<-subset(data_top_ten, function_industry(data_top_ten$stock_code))
      print ('Top 10 heavy stock after industry filtering are:')
      print (required_industry)
      if(nrow(required_industry)!= 0){
        # 4. Calculate required_industry_proportion
        required_industry_proportion = sum(required_industry$marketvalueofstockholdings) / total_nav_of_top_10_heavy_stock
        print (paste("Total proportion of required industry are:",required_industry_proportion))
        required_industry_proportion_global = required_industry_proportion_global + required_industry_proportion
        
      }
    }
  }
  if(num_of_reporting_date>=3){
    required_industry_proportion_global = required_industry_proportion_global / num_of_reporting_date
    print(paste("Total number of reporting date are:",num_of_reporting_date))
  } 
  else{
    required_industry_proportion_global='The number of reporting date is insufficient for calculation'}
  return(required_industry_proportion_global)
}

# Apply to 1 product
unit_helper<-function(fund_code)
{
  main_function(fund_code, w_season_data, num_of_heavy_stock)
}

# Apply to all products and get result
t1=proc.time()
filter_result_list<-sapply(product_list, unit_helper)

#Save result in excel
write.csv(filter_result_list, "filter_result_list.csv")
t2=proc.time()
t=t2-t1
print(paste0('Total running time：',t[3][[1]],'s'))