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
  

#--------------------------------------------------#
# ARIMAX를 이용한 개입분석
RevTotalSales %>% 
  filter(DATE>as.POSIXct("190430",format="%y%m%d"),SHOP_NAME == "밀도") -> sampledat
library(imputeTS)

sampledat$SALES[sampledat$SALES == 0] = NA

sampledat %>% 
  mutate_at("SALES",funs(na_ma(sampledat$"SALES",k = 4))) -> sampledat

# 분산 안정화,차분

auto.arima(sampledat$SALES)


sampledat %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES)) +
  ggtitle("original plot")

library(gridExtra)
sampledat %>% 
  mutate(SALES = difference(SALES,1)) %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES)) +
  ggtitle("lag=1") ->g1

sampledat %>% 
  mutate(SALES = difference(SALES,7)) %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES))+
  ggtitle("lag=7") ->g2
sampledat %>% 
  mutate(SALES = difference(SALES,30)) %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES)) +
  ggtitle("lag=30") ->g3

grid.arrange(g1,g2,g3,nrow=3)

sampledat %>% 
  mutate(SALES = difference(difference(SALES,7)),1) %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES)) +
  ggtitle("double diff 7,1") ->g4

sampledat %>% 
  mutate(SALES = difference(difference(difference(SALES,30),7)),1) %>% 
  ggplot() +
  geom_line(aes(x=DATE,y=SALES)) +
  ggtitle("tripple diff 30,7,1") -> g5
grid.arrange(g4,g5,nrow=2)

as_tsibble(sampledat,key=SHOP_NAME,index=DATE,regular = F) %>% 
  ACF(SALES) %>% 
  autoplot()

sampledat %>% as_tsibble(key=SHOP_NAME,index=DATE,regular = F) %>% 
  gg_tsdisplay(SALES,plot_type = "partial")

                
sampledat %>% 
  mutate(SALES=difference(SALES,7)) %>% 
  as_tsibble(key=SHOP_NAME,index=DATE,regular = F) %>% 
  gg_tsdisplay(SALES,plot_type = "partial")


fit <- arima(sampledat %>% select(SALES), order=c(0,1,0), seasonal = list(order=c(1,0,0), period=7), method = "ML")
summary(fit)
acf(fit$residuals, lag.max = 35)
pacf(fit$residuals, lag.max = 35)

checkresiduals(fit)

library(TSA)

fit.i = arimax(
  sampledat %>% select(SALES), 
  order=c(1,0,0), seasonal = list(order=c(0,0,2), period=7), 
  method = "ML",
  xtransf=data.frame(event=1*(seq(sampledat$SALES)>=204), begin=1*(seq(sampledat$SALES)>=245)),
  transfer=list(c(1,0),c(1,0) ),
)

summary(fit.i)

acf(fit.i$residuals, lag.max = 35)
pacf(fit.i$residuals, lag.max = 35)

