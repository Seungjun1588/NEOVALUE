# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
library(readxl)

#------------------------------------------------------------------#
#------------------------------------------------------------------#
## 멤버십 데이터 불러오기 
#------------------------------------------------------------------#
#------------------------------------------------------------------#

setwd("C:/Seungjun/neovalue/NEOVALUE/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)")
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
MDCustomer %>% 
  mutate(BIRTH_YEAR = substr(BIRTH_DT,1,4),
         BIRTH_MONTH = substr(BIRTH_DT,5,6),
         AGE = 2023-as.numeric(BIRTH_YEAR)) -> MDCustomer

MDCustomer %>% 
  mutate(AGE_INT = case_when(AGE <10 ~ "<10",
                             AGE <21 ~ "10~20",
                             AGE <31 ~ "21~30",
                             AGE <41 ~ "31~40",
                             AGE <51 ~ "41~50",
                             AGE <61 ~ "51~60",
                             AGE >=61 ~">61")) -> MDCustomer
MDCustomer %>% select(AGE_INT) %>% table

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





# 요약 정보
MDSANL %>%
  group_by(SHOP_NAME) %>%
  summarise(FIRST_SALE = first(SALE_DT),
            LAST_SALE=last(SALE_DT),
            MIN_SALE=min(TOT_SALE_AMT),
            MAX_SALE =max(TOT_SALE_AMT),
            MEAN_SALE=mean(TOT_SALE_AMT),
            MAD_SALE =median(TOT_SALE_AMT)) -> SUMMARY_SANL

SUMMARY_SANL %>%
  arrange(desc(MEAN_SALE)) %>%
  head(20) %>% select(SHOP_NAME) -> meantop20

SUMMARY_SANL %>%
  arrange(desc(MAD_SALE)) %>%
  head(20) %>%  select(SHOP_NAME) -> madtop20

saletop = intersect(meantop20,madtop20)

SUMMARY_SANL %>% 
  filter(SHOP_NAME %in% saletop[,,drop=T]) %>%
  arrange(desc(MEAN_SALE))



# 변수 추가
MDSANL %>% 
  mutate(POSTIME = paste0(substr(SALE_DT,1,4),"-",
                          substr(SALE_DT,5,6),"-",
                          substr(SALE_DT,7,8)),
         date = as.POSIXct(POSTIME)) -> MDSANL

# 상위 18개의 가게 매출 그래프
for(i in 1:dim(saletop)[1]){
  plot = MDSANL %>% 
    filter(SHOP_NAME == saletop[i,,drop=T]) %>%
    ggplot(aes(x=date,y=TOT_SALE_AMT)) +
    ggtitle(paste(saletop[i,,drop=T])) +
    geom_line()
  print(plot)
  
}
# 개별 가게를 연도 확인 코드
MDSANL %>% filter(SHOP_NAME =="외부팝업행사") %>% select(date) %>% unique %>% view()

# 상위 18개의 가게 매출 그래프 겹쳐 그리기
MDSANL %>% 
  filter(SHOP_NAME %in% saletop[,,drop=T],SHOP_NAME != "CU편의점") %>% 
  ggplot(aes(x=date,y=TOT_SALE_AMT)) +
  geom_line(aes(color=SHOP_NAME),alpha=0.3)
  

# 특정 가게 뜯어보기
MDSANL %>% 
  filter(SHOP_NAME =="도쿄등심") %>% 
  ggplot(aes(x=DAY_WEEK,y=TOT_SALE_AMT)) +
  geom_boxplot(aes(color=as.factor(DAY_WEEK)))

# 카테고리 붙이기
category = read_excel("임대계약관리(코드 기초자료).xlsx")
category = category[-1,]
category %>% select(SHOP_CD,CATE_NAME) -> cate

left_join(MDSANL,cate,by=c("SHOP_CD"))  -> df
df$CATE_NAME[is.na(df$CATE_NAME)] = "양식"
sum(is.na(df$CATE_NAME))

df %>% 
  group_by(CATE_NAME,date) %>% 
  summarise(MEAN_SALE = mean(TOT_SALE_AMT)) %>% 
  ggplot(aes(x=date,y=MEAN_SALE)) +
  geom_line(aes(color=CATE_NAME),alpha=0.3)

SHOP_lst = unique(df$CATE_NAME)
for(i in 1:19){
  df %>% 
    filter(CATE_NAME %in% SHOP_lst[i]) %>% 
    ggplot(aes(x=date,y=TOT_SALE_AMT)) +
    ggtitle(SHOP_lst[i]) +
    geom_line(aes(color=SHOP_NAME),alpha=0.3) -> plot
  print(plot)
}


df %>% 
  filter(SHOP_NAME =="식물원") %>% 
  ggplot() +
  geom_point(aes(x=DAY_WEEK,y=TOT_SALE_AMT,color=as.factor(DAY_WEEK)),alpha=0.5)

# boxplot
df %>% 
  filter(SHOP_NAME =="식물원") %>% 
  ggplot() +
  geom_boxplot(aes(x=DAY_WEEK,y=TOT_SALE_AMT,color=as.factor(DAY_WEEK)),alpha=0.5)

df %>% 
  mutate(MONTH = substr(SALE_DT,5,6),
         YEAR = substr(SALE_DT,1,4),
         DATE = substr(SALE_DT,7,8),
         newdate = paste0(MONTH,"-",DATE),
         newdate = as.POSIXct(SALE_DT,format="%Y-%m-%d")) %>% 
  group_by(YEAR,MONTH,date,SHOP_NAME) %>% 
  summarise(SHOP_NAME = first(SHOP_NAME),
            MEAN_SALE= mean(TOT_SALE_AMT)) %>% 
  ungroup() %>% 
  filter(SHOP_NAME =="플레이위드미") %>% 
  ggplot(aes(x=date,y=MEAN_SALE,group=YEAR)) +
  ggtitle("플레이위드미")+
  geom_line(aes(color=YEAR),alpha=0.4)

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

# 포인트 적립 하는 사람들 횟수 히스토그램
MDPoint %>% 
  group_by(CUST_ID) %>% 
  summarise(n=n()) %>% 
  select(CUST_ID,n) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(n)) +
  geom_histogram(aes(y=after_stat(count/sum(count))))

# 생각보다 포인트 적립을 열심히 하는 사람들이 많다. 
MDPoint %>% 
  group_by(CUST_ID) %>% 
  summarise(n=n()) %>% 
  mutate(cnt = case_when(n<2~"n=1",
                         n<5~"<5",
                         n<10~"5~10",
                         n<20~"10~20",
                         n<30~"20~30",
                         n>=30~">30"),
         cnt=factor(cnt,levels=c("n=1","<5","5~10","10~20","20~30",">30"))) %>% 
  ggplot() +
    geom_bar(aes(x=cnt,y=after_stat(count/sum(count))))

# 재방문이 높은 사람들이 주로 가는 가게, 소비 금액 파악해보기
MDPoint %>% 
  group_by(CUST_ID) %>% 
  summarise(n=n()) %>% 
  filter(n>30) %>%  select(CUST_ID) -> loyalID  # 재방문 높은 아이디

# 특정 가게 방문 횟수
MDPoint %>% 
  filter(CUST_ID %in% loyalID[,,drop=T]) %>% 
  group_by(SHOP_CD) %>% 
  summarise(n=n(),MEAN_SALE=mean(as.numeric(TRADE_AMT))) %>% 
  arrange(desc(n)) -> SHOP_VISIT
  
# 가게 이름 가져와서 붙이기
MDCode %>% 
  group_by(SHOP_CD) %>% 
  summarise(SHOP_LIST = first(SHOP_NAME)) -> SHOP_LIST

left_join(SHOP_VISIT,SHOP_LIST,by=c("SHOP_CD")) %>% 
  select(SHOP_CD,SHOP_LIST,n,MEAN_SALE) # SHOP_CD =0인건 확인안됨...

# 경로 파악
 

newdf = pivot_wider(df2,names_from = SHOP_CD,values_from=n)
newdf %>% replace(.,is.na(.),0) -> newdf


library(psych)
faaa = fa(t(data.matrix(newdf)),nfactors=4,fm-"ml")

clusters = kmeans(faaa$loadings,centers=4,iter.max=100)
clusters$centers
clusters$cluster

# 방문 횟수와 사용 금액에 따라서 색상표시?

MDPoint %>% 
  group_by(CUST_ID) %>% 
  summarise(n=n(),COST=sum(as.numeric(TRADE_AMT))) -> MDsumm


clusters = kmeans(MDsumm[,2:3],centers=10,iter.max=1000)
clusters$centers
clusters$cluster

MDsumm$cluster = clusters$cluster


MDsumm %>% 
  filter(COST < 2e+07) %>% 
  ggplot() + 
  geom_point(aes(x=COST,y=n,color=as.factor(cluster)))

#---------------------------------#
# n만 고려
clusters = kmeans(MDsumm[,2],centers=4,iter.max=100)
clusters$centers
clusters$cluster

MDsumm$cluster = clusters$cluster


MDsumm %>% 
  filter(COST < 2e+07) %>% 
  ggplot() + 
  geom_point(aes(x=COST,y=n,color=as.factor(cluster)))
#---------------------------------#
# 금액만 고려
clusters = kmeans(MDsumm[,3],centers=4,iter.max=100)
clusters$centers
clusters$cluster

MDsumm$cluster = clusters$cluster


MDsumm %>% 
  filter(COST < 2e+07) %>% 
  ggplot() + 
  geom_point(aes(x=COST,y=n,color=as.factor(cluster)))











View(tbl)
View(df2)
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





