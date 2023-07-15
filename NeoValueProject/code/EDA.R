# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
#------------------------------------------------------------------#
#------------------------------------------------------------------#
## 멤버십 데이터 불러오기 
#------------------------------------------------------------------#
#------------------------------------------------------------------#

setwd("C:/Users/user/Seungjun/NeoValue/230616_네오밸류 데이터/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)")
library(readxl)
#------------------------------------------------------------------#

# 매장코드 sheet(3열까지만 정보가 있고 나머지는 비어있음.)
MDCode = read_excel("멤버십 매출.xlsx",sheet="매장코드(SCON002)")
head(MDCode)
# 필요한 정보만 남김
MDCode = MDCode[-1,] %>% select(STR_CD,SHOP_CD,SHOP_NAME)
head(MDCode)

dim(MDCode) # 전체 길이 365
table(MDCode$STR_CD) # 사업장코드는 전부 같다.
length(unique(MDCode$SHOP_NAME)) # 197 생각보다 겹치는 상호가 많다.
length(unique(MDCode$SHOP_CD)) # 228

# 매장명은 같으나 매장코드가 다른 가게가 존재한다는 것을 알 수 있다. 
# 해당목록들
names  = unique(MDCode %>% select(SHOP_NAME)) %>% pull()
for(i in names){
  num =  MDCode %>% 
    filter(SHOP_NAME %in% i) %>% 
    select(SHOP_CD) %>% unique %>% 
    pull %>% 
    length 
  if(num >1){
    print(i)
  }
}

#------------------------------------------------------------------#
# 회원정보(CRM.CR_CUSTOMER)
MDCustomer = read_excel("멤버십 매출.xlsx",sheet="회원정보(CRM.CR_CUSTOMER)")
head(MDCustomer)
# 여기도 정보가 있는 컬럼이 몇 개 없음 
# 필요한 정보만 남김
MDCustomer = MDCustomer[-1,1:7] 
colnames(MDCustomer) = c("CUST_ID","CUST_NAME","SEX_CD","BIRTH_DT","ADDR1","ADDR2","ADDR3")
head(MDCustomer)

dim(MDCustomer) # 25171
length(unique(MDCustomer$CUST_ID)) # 중복되는 ID는 없음
table(MDCustomer$SEX_CD)
prop.table(table(MDCustomer$SEX_CD)) # 성비

# 생일은 중요하지 않고, 나이가 중요하다고 생각


# 지역
tab = table(MDCustomer$ADDR1)
tab[order(tab,decreasing = T)]
prop.table(tab[order(tab,decreasing = T)])


# 구까지 구분
ADDR12 = paste(MDCustomer$ADDR1,MDCustomer$ADDR2)
tab = table(ADDR12)
tab[order(tab,decreasing = T)]

#------------------------------------------------------------------#
# 일별매출속보(SANL104)
types = c(rep("text",4),rep("numeric",13))
MDSANL = read_excel("멤버십 매출.xlsx",range=cell_cols(1:17),sheet="일별매출속보(SANL104)",col_types=types)

MDSANL = MDSANL[-1,1:17]
# 데이터가 정렬이 안되어 있음
MDSANL = MDSANL %>% arrange(SHOP_NAME)
summary(MDSANL)

MDSANL %>% filter(is.na(MDSANL$WEEK))
#------------------------------------------------------------------#
# 포인트적립(CRM.CR_POINT_ADD)
MDPoint = read_excel("멤버십 매출.xlsx",sheet="포인트적립(CRM.CR_POINT_ADD)")
MDPoint = MDPoint[-1,]
dim(MDPoint)
length(unique(MDPoint %>% select(ADD_ID) %>%  pull)) # 502473
length(unique(MDPoint %>% select(CARD_NO) %>%  pull)) # 17031
length(unique(MDPoint %>% select(APPR_ID) %>%  pull)) # 502473
length(unique(MDPoint %>% select(SHOP_CD) %>%  pull)) # 122
length(unique(MDPoint %>% select(CUST_ID) %>%  pull)) # 17031

sum(!is.na(MDPoint$TRADE_CD))
sum(!is.na(MDPoint$VEN_CD))

MDPoint = MDPoint %>% select(-TRADE_CD,-VEN_CD)
#------------------------------------------------------------------#
# save
library(openxlsx)
Membership = createWorkbook("Membership")
addWorksheet(Membership,"매장코드(SCON002)")
addWorksheet(Membership,"회원정보(CRM.CR_CUSTOMER)")
addWorksheet(Membership,"일별매출속보(SANL104)")
addWorksheet(Membership,"포인트적립(CRM.CR_POINT_ADD)")

writeDataTable(Membership,"매장코드(SCON002)",MDCode)
writeDataTable(Membership,"회원정보(CRM.CR_CUSTOMER)",MDCustomer)
writeDataTable(Membership,"일별매출속보(SANL104)",MDSANL)
writeDataTable(Membership,"포인트적립(CRM.CR_POINT_ADD)",MDPoint)

saveWorkbook(Membership,file ="Membership.xlsx")





