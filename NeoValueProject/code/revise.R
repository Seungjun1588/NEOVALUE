# 2019년 10월 데이터와 2022년 1월 데이터를 변경하는 코드 


# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
library(readxl)

# 상위 디렉토리
pwd = "C:/Users/user/Seungjun/NeoValue/230616_네오밸류 데이터/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)/일별/총매출"

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# 함수 생성
# year = 숫자, start = 시작월(숫자)
MergedDataSet = function(year=2019,start=1,end=12,pwd){
  Year = paste0("/",year,"년")
  file_lst = list.files(paste0(pwd,Year))
  
  
  # 2019년 4월에서12월까지만 병합.
  sales19 = read_excel(paste0(pwd,Year,"/",file_lst[start]),skip=11) %>% 
    slice(-1,-n()) %>% 
    select(-"합계",-1) %>%
    rename("SHOP_NAME"="...3") 
  
  makeyear = function(year,month,day){
    days = (year-2000)*10000+month*100+day
    return(days)
  }
  newcolnames = colnames(sales19)[-(1:2)] %>% as.numeric %>% sapply(makeyear,month=start,year=year)
  colnames(sales19)[3:length(colnames(sales19))] = newcolnames
  colnames(sales19)[1] = "SHOP_CD"
  
  for(month in (start+1):end){
    # 해당 월 파일 가져오기
    sales_month = read_excel(paste0(pwd,Year,"/",file_lst[month]),skip=11) %>% 
      slice(-1,-n()) %>% 
      select(-"합계",-1) %>%
      rename("SHOP_NAME"="...3")
    
    # 열이름 변경
    newcolnames = colnames(sales_month)[-(1:2)] %>% as.numeric %>% sapply(makeyear,year=year,month=month)
    print(newcolnames)
    print("*************************")
    colnames(sales_month)[3:length(colnames(sales_month))] = newcolnames
    colnames(sales_month)[1] = "SHOP_CD"
    
    # 병합
    sales19 = merge(sales19,sales_month,by=c("SHOP_CD","SHOP_NAME"),all=TRUE)
  }
  return(sales19)
}

Year=2019
Year = paste0("/",year,"년")
file_lst = list.files(paste0(pwd,Year))

sales10m = read_excel(paste0(pwd,Year,"/",file_lst[10]),skip=11) %>% 
  slice(-1,-n()) %>% 
  select(-"합계",-1) %>%
  rename("SHOP_NAME"="...3") 

makeyear = function(year,month,day){
  days = (year-2000)*10000+month*100+day
  return(days)
}  
  
  



file_lst[9]
file_lst[10]
read_excel(file_lst[10]) %>% head(15)
read_excel("for_example.xlsx") %>% head(15)





