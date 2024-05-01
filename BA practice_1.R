#데이터 읽어오기 
#install.packages("foreign")
library(foreign)

online_member=read.dta("C:/Users/82107/OneDrive/문서/2024-1/mobile commerce/OnlineMember.dta")
head(online_member)
str(online_member)

mobile_member=read.dta("C:/Users/82107/OneDrive/문서/2024-1/mobile commerce/MobileMember.dta")
head(mobile_member)
str(mobile_member)

online_order=read.dta("C:/Users/82107/OneDrive/문서/2024-1/mobile commerce/OnlineOrder.dta")
head(online_order)
str(online_order)

mobile_order=read.dta("C:/Users/82107/OneDrive/문서/2024-1/mobile commerce/MobileOrder.dta")
head(mobile_order)
str(mobile_order)


##1번 
#생일 numeric으로 바꾸기
online_member$Birth<-as.numeric(as.character(online_member$Birth))
mobile_member$Birth<-as.numeric(as.character(mobile_member$Birth))

#Age 행 추가
online_member$Age<-2011 - online_member$Birth
mobile_member$Age<-2011 - mobile_member$Birth

#평균 Age 구하기
mean(online_member$Age, na.rm=TRUE)
mean(mobile_member$Age, na.rm=TRUE)

#두 그룹 간 age 차이 t-test 
t.test(online_member$Age, mobile_member$Age)


##2번 
#proportion of female in adopter, non-adopter group
gender_data<-rbind(table(online_member$Gender), table(mobile_member$Gender))
rownames(gender_data)<-c('non-adopter','adopter')
head(gender_data)

prop.table(gender_data,margin=1)

#gender 분포와 두 그룹이 독립인지 아닌지 chi-square test
chisq.test(gender_data)


##3번 
#mobile dummy variable 생성
mobile_order$mobile<-ifelse(mobile_order$Mall=="03"&(mobile_order$AccessRoute=="1000132495"|mobile_order$AccessRoute=="1000132496"|mobile_order$AccessRoute =="1000013091"),1,0)
head(mobile_order)
str(mobile_order)

# mobile 과 pc 거래의 order price 값의 차이
boxplot(OrderPrice ~ mobile, data = mobile_order,
        xlab = "Mobile", ylab = "Order Price")

aggregate(mobile_order$OrderPrice, list(mobile_order$mobile), mean)

#두 그룹 간 order price 차이 t-test 수행 
t.test(mobile_order$OrderPrice~mobile_order$mobile)


##4번
#CRate dummy variable 생성 
mobile_order$CRate<-ifelse(mobile_order$OrderQuantity==mobile_order$ConfirmedQuan,1,0)

#mobile과 pc거래의 confirmation rate 
agg_CRate<-aggregate(CRate~mobile, data=mobile_order,sum)
agg_transac<-aggregate(BasketID~mobile,data=mobile_order,length)

agg_CRate$CRate/agg_transac$BasketID

#두 그룹 간 CRate 차이 t-test 수행 
t.test(mobile_order$CRate~mobile_order$mobile)


##5번 
#pc order와 mobile order 그룹 나누기
pc_order_group<-mobile_order[mobile_order$mobile == 0,]
mobile_order_group<-mobile_order[mobile_order$mobile==1,]

# 각 그룹에서 각 인증서에 의존하는 주문 비율 계산
pc_cert<-sum(pc_order_group$OkSeller=="Y"|pc_order_group$QuickSeller=="Y"|pc_order_group$BigSeller=="Y")/nrow(pc_order_group)
mobile_cert<-sum(mobile_order_group$OkSeller=="Y"|mobile_order_group$QuickSeller=="Y"|mobile_order_group$BigSeller=="Y")/nrow(mobile_order_group)
pc_cert
mobile_cert

# confirmation rate 와 두 그룹이 독립인지 아닌지 카이제곱 검정 수행
chisq.test(prop.table(table(mobile_order$OkSeller,mobile_order$mobile),margin=2))
chisq.test(prop.table(table(mobile_order$QuickSeller,mobile_order$mobile),margin=2))
chisq.test(prop.table(table(mobile_order$BigSeller,mobile_order$mobile),margin=2))

chisq.test(cbind(c(pc_cert,1-pc_cert),c(mobile_cert,1-mobile_cert)))

