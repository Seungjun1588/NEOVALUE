# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
library(readxl)
library(openxlsx)

#---------------------------------------------------------------------#
# 매출 데이터 통합
#---------------------------------------------------------------------#
# 상위 디렉토리

pwd = "C:/Seungjun/neovalue/NEOVALUE/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)/일별"

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
#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# 매출건수

dir = paste0(pwd,"/매출건수")

# 일별 매출건수
salescnt19 = MergedDataSet(year=2019,start=4,pwd=dir)
salescnt20 = MergedDataSet(year=2020,start=1,pwd=dir)
salescnt21 = MergedDataSet(year=2021,start=1,pwd=dir)
salescnt22 = MergedDataSet(year=2022,start=1,end=10,pwd=dir)
TotalSalesCnt = full_join(salescnt19,salescnt20,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesCnt = full_join(TotalSalesCnt,salescnt21,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesCnt = full_join(TotalSalesCnt,salescnt22,by=c("SHOP_CD","SHOP_NAME"))


dim(TotalSalesCnt)
length(unique(TotalSalesCnt$SHOP_CD))
length(unique(TotalSalesCnt$SHOP_NAME))

# 이름은 같으나 코드가 달라진 가게(어떤 변화가 있었는지 확인 필요)
TotalSalesCnt %>% 
  group_by(SHOP_NAME) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

#---------------------------------------------------------------------#
#---------------------------------------------------------------------#
# 총매출

dir = paste0(pwd,"/총매출")

# 일별 매출건수
sales19 = MergedDataSet(year=2019,start=4,pwd=dir)
sales20 = MergedDataSet(year=2020,start=1,pwd=dir)
sales21 = MergedDataSet(year=2021,start=1,pwd=dir)
sales22 = MergedDataSet(year=2022,start=1,end=10,pwd=dir)
TotalSales = full_join(sales19,sales20,by=c("SHOP_CD","SHOP_NAME"))
TotalSales = full_join(TotalSales,sales21,by=c("SHOP_CD","SHOP_NAME"))
TotalSales = full_join(TotalSales,sales22,by=c("SHOP_CD","SHOP_NAME"))


dim(TotalSales)
length(unique(TotalSales$SHOP_CD))
length(unique(TotalSales$SHOP_NAME))

# 이름은 같으나 코드가 달라진 가게(어떤 변화가 있었는지 확인 필요)
TotalSales %>% 
  group_by(SHOP_NAME) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# save
totalsales = createWorkbook("TotalSales")
addWorksheet(totalsales,"sales")
writeDataTable(totalsales,"sales",TotalSales)
saveWorkbook(totalsales,file =paste0(dir,"/","Totalsales.xlsx"),overwrite =T)



#--------------------------------------------------------------------#
#--------------------------------------------------------------------#
## 순매출


dir = paste0(pwd,"/순매출")

# 일별 순매출
sales19net = MergedDataSet(year=2019,start=4,pwd=dir)
sales20net = MergedDataSet(year=2020,start=1,pwd=dir)
sales21net = MergedDataSet(year=2021,start=1,pwd=dir)
sales22net = MergedDataSet(year=2022,start=1,end=10,pwd=dir)
TotalSalesNet = full_join(sales19net,sales20net,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesNet = full_join(TotalSalesNet,sales21net,by=c("SHOP_CD","SHOP_NAME"))
TotalSalesNet = full_join(TotalSalesNet,sales22net,by=c("SHOP_CD","SHOP_NAME"))


dim(TotalSales)
length(unique(TotalSales$SHOP_CD))
length(unique(TotalSales$SHOP_NAME))


