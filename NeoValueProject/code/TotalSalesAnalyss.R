# install.packages("data.table")
library(data.table)
library(readxl)
library(tidyverse)
library(readxl)
library(openxlsx)

pwd = "C:/Seungjun/neovalue/NEOVALUE/230616_네오밸류 데이터(발송용)/네오밸류 데이터/02_운영데이터 등/230531_광교 데이터/후지쯔 매출내역(Open~2210)/일별"
dir = paste0(pwd,"/총매출")

# 2022년도까지의 KICC 내역들
TotalSales = 
  read_excel(paste0(dir,"/Totalsales.xlsx"),
             col_types=c(rep("text",2),rep("numeric",1310)))

# NA를 전부 0으로 처리
TotalSales %>% 
  replace(.,is.na(.),0) -> TotalSales

dim(TotalSales)
length(unique(TotalSales$SHOP_CD))
length(unique(TotalSales$SHOP_NAME))

# SHOP_NAME이 같은 것들은 하나로 합침(이 때 SHOP_CD는 임의로 하나로 통일)
Rev_SHOP_CD = TotalSales %>% 
  select(SHOP_CD,SHOP_NAME) %>% 
  group_by(SHOP_NAME) %>% 
  summarise(SHOP_CD = first(SHOP_CD)) 

# 하나로 합치는게 합당한지 체크(문제없다고 판단)
TotalSales %>% 
  select(SHOP_NAME) %>% 
  group_by(SHOP_NAME) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  filter(n == 2) %>% 
  select(SHOP_NAME) -> NotUniqueName

for(i in 1:dim(NotUniqueName)[1]){
  TotalSales %>% 
    filter(SHOP_NAME == NotUniqueName[i,,drop=T]) %>%
    pivot_longer(cols=-c("SHOP_NAME","SHOP_CD"),names_to = "DATE",values_to="SALES") %>% 
    mutate(DATE = as.POSIXct(DATE,format="%y%m%d")) %>% 
    ggplot() + 
    geom_line(aes(x=DATE,y=SALES,group=SHOP_CD,color=(SHOP_CD),alpha=0.3))+
    ggtitle(NotUniqueName[i,,drop=T]) -> plot
  print(plot)
}

# 통합하고 피벗을 바꾼 데이터
TotalSales %>% 
  select(-SHOP_CD) %>% 
  group_by(SHOP_NAME) %>% 
  summarise_all(.fun=sum) %>% 
  left_join(Rev_SHOP_CD,by=c("SHOP_NAME")) -> RevTotalSales
dim(RevTotalSales)

RevTotalSales %>% 
  pivot_longer(cols=-c("SHOP_NAME","SHOP_CD"),names_to = "DATE",values_to="SALES")  %>% 
  mutate(DATE = as.POSIXct(DATE,format="%y%m%d")) -> RevTotalSales

#----------------------------------------------------#
# 공휴일 쉬는 날 반영

RevTotalSales %>% 
  group_by(DATE) %>% 
  summarise(SALE = sum(SALES)) -> AlleySales
# "190912","190913"

holiday_lst = c("200125","200224","201001","210212","210921","220201","220909","220910")

MakeDate = function(date){
  return(as.POSIXct(date,format="%y%m%d"))
}


ImpHoliday = function(lst,w,data){
  for(date in lst){
    data %>% 
      filter(DATE > MakeDate(date)-days(w),DATE < MakeDate(date)+days(w)) %>% 
      filter(DATE != MakeDate(date)) %>% 
      select(SALE) %>% 
      summarise(SUM = mean(SALE)) %>% 
      pull() -> Impval
    data$SALE[data$DATE == MakeDate(date)] = Impval
  }
  return(data)
}

df = ImpHoliday(lst=holiday_lst,w=3,data=AlleySales)
# "190912","190913" -> 따로 처리
df %>% 
  filter(DATE > MakeDate("190912")-days(3),DATE < MakeDate("190912")+days(4)) %>% 
  filter(DATE != MakeDate("190912"),DATE != MakeDate("190913")) %>% 
  select(SALE) %>% 
  summarise(SUM = mean(SALE)) %>% 
  pull() -> Impval
df$SALE[df$DATE == MakeDate("190912")] = Impval
df$SALE[df$DATE == MakeDate("190913")] = Impval


# 전체 기간 데이터
AlleySales %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log(SALE))) +
  geom_hline(yintercept = log(20000000),linetype="dashed") +
  ggtitle("기존 데이터")

df %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log(SALE))) +
  geom_hline(yintercept = log(20000000),linetype="dashed") +
  ggtitle("수정한 데이터")

#--------------------------------------------------#

# 전체 기간 데이터
AlleySales %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log(SALE)))

# 일부 기간 데이터(22년 6월 12일)
# 201025 할로윈
#   filter(DATE > "2020-08-01",DATE <"2020-12-31") %>% 

#   filter(DATE > "2020-08-01",DATE <"2020-12-31") %>% 
# 20년 3월 12일 키즈 패션 위크
df %>% 
  mutate(log_SALE = log(SALE)) %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log_SALE)) +
  geom_vline(xintercept = as.POSIXct("200312",format="%y%m%d"),linetype="dashed") +
  geom_vline(xintercept = as.POSIXct("200321",format="%y%m%d"),linetype="dashed") 
df %>% 
  mutate(log_SALE = log(SALE)) %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=SALE)) +
  geom_vline(xintercept = as.POSIXct("200312",format="%y%m%d"),linetype="dashed") +
  geom_vline(xintercept = as.POSIXct("200321",format="%y%m%d"),linetype="dashed") 

df %>% 
  mutate(log_SALE = log(SALE), log_SALE7 = log_SALE -diff(log_SALE,7)) %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log_SALE)) +
  geom_vline(xintercept = as.POSIXct("200312",format="%y%m%d"),linetype="dashed") +
  geom_vline(xintercept = as.POSIXct("200321",format="%y%m%d"),linetype="dashed") 




# 20년 8월 배달 서비스
# 태풍 바비가 온 시점
df %>% 
  filter(DATE > "2020-06-01", DATE < "2020-12-31") %>% 
  mutate(SALE = SALE -diff(SALE,7)) %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log(SALE))) +
  geom_vline(xintercept = as.POSIXct("200801",format="%y%m%d"),linetype="dashed")+
  geom_vline(xintercept = as.POSIXct("200831",format="%y%m%d"),linetype="dashed")


# 21년 초
df %>% 
  filter(DATE > "2020-06-01", DATE < "2020-12-31") %>% 
  mutate(SALE = SALE -diff(SALE,7)) %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log(SALE))) +
  geom_vline(xintercept = as.POSIXct("200801",format="%y%m%d"),linetype="dashed")+
  geom_vline(xintercept = as.POSIXct("200831",format="%y%m%d"),linetype="dashed")


#--------------------------------------------------#
# ARIMAX를 이용한 개입분석

# 20년 3월 12일 키즈 패션 위크(예시)
df %>% 
  mutate(log_SALE = log(SALE)) -> df 

df %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=SALE)) +
  geom_vline(xintercept = as.POSIXct("200312",format="%y%m%d"),linetype="dashed") +
  geom_vline(xintercept = as.POSIXct("200321",format="%y%m%d"),linetype="dashed") +
  ggtitle("앨리웨이 총매출 시계열 그래프")
  
# 분산 안정화를 위한 로그 변환
df %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log_SALE)) +
  geom_vline(xintercept = as.POSIXct("200312",format="%y%m%d"),linetype="dashed") +
  geom_vline(xintercept = as.POSIXct("200321",format="%y%m%d"),linetype="dashed") +
  ggtitle("앨리웨이 총매출 시계열 그래프(로그변환)")

# 필요한 부분만 잘라서 이용 
df %>% 
  filter(DATE >"2019-06-01",DATE < "2021-01-01") -> kids

df %>% 
  filter(DATE > "2019-06-01",DATE < "2020-03-21") -> kids1

df %>% 
  filter(DATE > "2020-03-22",DATE < "2021-01-01") -> kids2

kids1 %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=SALE)) +
  ggtitle("19.06.01~20.03.21 앨리웨이 총매출 시계열 그래프")

kids1 %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log_SALE)) +
  ggtitle("20.06.01~21.01.01 앨리웨이 총매출 시계열 그래프(로그변환)")

kids2 %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=SALE)) +
  ggtitle("20.03.21~21.01.01 앨리웨이 총매출 시계열 그래프")

kids2 %>% 
  ggplot() + 
  geom_line(aes(x=DATE,y=log_SALE)) +
  ggtitle("20.03.21~21.01.01 앨리웨이 총매출 시계열 그래프(로그변환)")





# 정상 시계열인지를 확인하기 위한 단위근 검정(추세,등분산 검정)
# library(tseries)
# adf.test(kids %>% select(log_SALE) %>%  pull)  
library(aTSA)
adf.test(kids1 %>% select(log_SALE) %>%  pull)  
adf.test(kids2 %>% select(log_SALE) %>%  pull)  

# TYPE1을 봤을 때 추세가 존재하기 때문에 차분 시행
kids1 %>% 
  mutate(SALE1=SALE - diff(SALE,1)) %>% 
  mutate(SALE17=SALE1 - diff(SALE1,7)) %>% 
  mutate(log_SALE1=log_SALE - diff(log_SALE,1)) %>% 
  mutate(log_SALE17=log_SALE1 - diff(log_SALE1,7)) -> kids1

kids2 %>% 
  mutate(SALE1=SALE - diff(SALE,1)) %>% 
  mutate(SALE17=SALE1 - diff(SALE1,7)) %>% 
  mutate(log_SALE1=log_SALE - diff(log_SALE,1)) %>% 
  mutate(log_SALE17=log_SALE1 - diff(log_SALE1,7)) -> kids2


# 1차 차분
kids1 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=log_SALE1)) +
  ggtitle("1차 차분(로그)")

# 1차 차분
kids1 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALE1)) +
  ggtitle("1차 차분")

# 1,7차 차분
kids1 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=log_SALE17))+
  ggtitle("1차&7차 차분(로그)")


# 1,7차 차분
kids1 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALE17))+
  ggtitle("1차&7차 차분")

# 1차 차분
kids2 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=log_SALE1)) +
  ggtitle("1차 차분(로그)")

# 1차 차분
kids2 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALE1)) +
  ggtitle("1차 차분")

# 1,7차 차분
kids2 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=log_SALE17))+
  ggtitle("1차&7차 차분(로그)")

# 1,7차 차분
kids2 %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALE17))+
  ggtitle("1차&7차 차분")


# 다시 한번 검정
adf.test(kids1 %>% select(log_SALE) %>%  pull)  
adf.test(kids1 %>% select(log_SALE17) %>%  pull)  
adf.test(kids1 %>% select(SALE17) %>%  pull)  

adf.test(kids2 %>% select(log_SALE) %>%  pull)  
adf.test(kids2 %>% select(log_SALE17) %>%  pull)  
adf.test(kids2 %>% select(SALE17) %>%  pull)  

# 둘 다 로그 안취한게 낫다는 결론(1차 7차 시행)


#### ARIMA모형 추정
library(forecast)
#-----------------------------------------------#
library(tsibble)
library(tsibbledata)
library(dplyr)

# 앞 기간에 대한 모델 찾기
par(mfrow=c(1,2))
acf(kids1 %>% select(SALE17),lag=30)
pacf(kids1 %>% select(SALE17),lag=30)
par(mfrow=c(1,1))


fit <- arima(kids1 %>% select(SALE) %>% pull ,
             order=c(0,1,2), seasonal = list(order=c(0,1,2), period=7), method = "ML")
summary(fit)
acf(fit$residuals, lag.max = 35)
pacf(fit$residuals, lag.max = 35)


library(TSA)
# 2020-03-21 -> index 294



library(forecast)

# 단위 조정
kids %>% 
  mutate(SALE_SCALED = SALE/10000000)  -> kids


# 개입시점 생성
level = 1*(seq(dim(kids)[1])>294)

# 개입모형 적합합
fit = Arima(kids %>% select(SALE_SCALED),order=c(0,1,2),
      seasonal = list(order=c(0,1,2), period=7),
      xreg = level,
      method="CSS")

# 결과 : 1.15(천만원) 정도 평균이 차이 난다.
summary(fit)

# fit.i = arimax(
#   kids %>% select(SALE_SCALED),
#   order=c(0,1,2),
#   seasonal = list(order=c(0,1,2), period=7),
#   xtransf = data.frame(level=level),
#   transfer = list(c(0,1)),
#   method="CSS"
# )
# summary(fit.i)

# 사후분석
acf(fit$residuals, lag.max = 35)
pacf(fit$residuals, lag.max = 35)

# fitting 결과(색상 구분이 필요할듯)

kids %>% 
  filter(DATE > "2019-06-01",DATE < "2020-03-21") -> kids1

kids %>% 
  filter(DATE >= "2020-03-21",DATE < "2021-01-01") -> kids2

# 시각화(뭔가 플랏이 아쉬움..더 좋은 방법..?)
ggplot() + 
  geom_line(data = kids,aes(x=DATE,y=SALE_SCALED)) +
  geom_smooth(data = kids,aes(x=DATE,y=SALE_SCALED),se=F,color="black") +
  geom_line(data = kids1,aes(x=DATE,y=SALE_SCALED),color="blue",alpha=0.3) +
  geom_smooth(data = kids1,aes(x=DATE,y=SALE_SCALED),se=F) +
  geom_line(data = kids2,aes(x=DATE,y=SALE_SCALED),color="red") +
  geom_smooth(data = kids2,aes(x=DATE,y=SALE_SCALED),color="red",se=F,alpha=0.3)
  

# summary stat 
summary(kids1 %>% select(SALE_SCALED))
summary(kids2 %>% select(SALE_SCALED))
sd(kids1 %>% select(SALE_SCALED) %>% pull)
sd(kids2 %>% select(SALE_SCALED) %>% pull)

#-----------------------------------------------#
