# 2019년 10월 데이터와 2022년 1월 데이터를 변경하는 코드 


# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
library(readxl)

# 상위 디렉토리
pwd = "C:/Seungjun/neovalue/NEOVALUE/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)/일별/총매출"

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
## 2019 10월 ###
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

year=2019
Year = paste0("/",year,"년")
file_lst = list.files(paste0(pwd,Year))

sales10m = read_excel(paste0(pwd,Year,"/",file_lst[10]),skip=11) %>% 
  slice(-1,-n()) %>% 
  select(-"합계",-1) %>%
  rename("SHOP_NAME"="...3","SHOP_CD"="매장코드") 

  
# 일별 매출건수
salescnt19 = MergedDataSet(year=2019,start=4,end=9,pwd=dir)
salescnt19_12 = MergedDataSet(year=2019,start=11,end=12,pwd=dir)
salescnt20 = MergedDataSet(year=2020,start=1,end=12,pwd=dir)
salescnt19 %>% select(SHOP_CD,SHOP_NAME) %>% head(5)

TotalSalesCnt = full_join(salescnt19,salescnt19_12,salescnt20,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesCnt %>% select(SHOP_CD,SHOP_NAME) %>% head(5)

sales10m %>% select(SHOP_CD,SHOP_NAME) 
sales10m  %>%
  select(SHOP_CD) %>% 
  left_join(TotalSalesCnt%>% select(SHOP_CD,SHOP_NAME),by=c("SHOP_CD")) -> recover
  
recover$SHOP_NAME[is.na(recover$SHOP_NAME)] = "키오스크(더부쓰 비어위크)"


# save
library(openxlsx)
Membership = createWorkbook("2019recover")
addWorksheet(Membership,"2019")
writeDataTable(Membership,"2019",recover)
saveWorkbook(Membership,file ="2019recover.xlsx")

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
## 2022 1월 ###

year=2022
Year = paste0("/",year,"년")
file_lst = list.files(paste0(pwd,Year))

salescnt21 = MergedDataSet(year=2021,start=1,end=12,pwd=dir)
salescnt22 = MergedDataSet(year=2022,start=2,end=10,pwd=dir)

TotalSalesCnt = full_join(salescnt21,salescnt22,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesCnt %>% select(SHOP_CD,SHOP_NAME) %>% head(5)

sales1m = read_excel(paste0(pwd,Year,"/",file_lst[1]),skip=11) %>% 
  slice(-1,-n()) %>% 
  select(-"합계",-1) %>%
  rename("SHOP_NAME"="...3","SHOP_CD"="매장코드") 

sales1m  %>%
  select(SHOP_CD) %>% 
  left_join(TotalSalesCnt%>% select(SHOP_CD,SHOP_NAME),by=c("SHOP_CD")) -> recover2

dim(sales1m)
dim(recover2)

recover2 %>% is.na() %>% colSums()

# save
library(openxlsx)
Membership = createWorkbook("2022recover")
addWorksheet(Membership,"2022")
writeDataTable(Membership,"2022",recover2)
saveWorkbook(Membership,file ="2022recover.xlsx")








