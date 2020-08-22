load("C:/Users/sec/Desktop/개인 업무 자료/DRA/R 캠프 앙상블/day5Data.RData")

#tr <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/train.csv")

#te <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/test.csv")
# 구조 확인  
str(tr)
str(te)

### EDA & Feature Engineering

# factor 데이터 type 확인
# 자 이렇게 factor 타입이 다른 데이터들이 있을 수 있다. - ganwon, gwangju는 학습에, jeju는 테스트 데이터에 존재... 이런 경우 어떻게 해야할까?
table(tr$sido)
table(te$sido)

# dental_clinic은 train에만 있고 test에는 없다.
# "" 공란의 level도 하나 존재한다.
table(tr$instkind)
table(te$instkind)

table(tr$ownerChange)
table(te$ownerChange)

# 이 경우에는 level들을 다 통일해줘야 한다. 모든 경우를 다 포함하는 합집합의 level를 공통 레벨로 삼게 한다.
Sido <- c("busan", "choongbuk", "choongnam","daegu","daejeon","gangwon","gwangju","gyeongbuk","gyeonggi","gyeongnam","incheon",
          "jeonbuk","jeonnam","sejong","seoul","ulsan","jeju")

Instkind <- c("", "clinic", "dental_clinic", "general_hospital","hospital","nursing_hospital","traditional_clinic","traditional_hospital")

tr$sido <- factor(tr$sido, levels = Sido)
te$sido <- factor(te$sido, levels= Sido)

tr$instkind <- factor(tr$instkind, levels= Instkind)
te$instkind <- factor(te$instkind, levels=Instkind)


# train과 test간의 type이 다른 employee1, employee2
# train은 int형으로 되어 있지만, test는 factor로 되어 있음
# factor형을 int형으로 바꾸어 주기로 하자.
# 다만, ','가 들어가 있는 것을 빼주는 전처리를 먼저 한다.
te$employee1
te$employee2
# ','를 빼는 이유!
a <- as.factor("1,234")
a
a <- as.numeric(a)
a
te$employee1 <- gsub(",", "", te$employee1)
te$employee2 <- gsub(",", "", te$employee2)
te$employee1 <- as.integer(te$employee1)
te$employee2 <- as.integer(te$employee2)

# NA 갯수 확인
colSums(is.na(tr))
colSums(is.na(te))

# tr의 경우 NA로 가득한 관측치가 있다. 
rowSums(is.na(tr))
rowSums(is.na(te))
# 그러한 관측치는 제거
tr <- tr[rowSums(is.na(tr))<48,]

# 수치형태로 전환
tr$OC <- ifelse(tr$OC=='open',1,0)
tr$OC <- as.factor(tr$OC)
## 단순하게 fill로 채우는 과정을 거쳐봄
library(tidyr)
fill_na <- function(df, col){
  fill(df, col, .direction='updown') -> df
  return(df)
}
colSums(is.na(tr))  
# 나머지 결측치 제거
tr <- fill_na(tr, 'bedCount')
tr <- fill_na(tr, 'employee1')
tr <- fill_na(tr, 'employee2')
tr <- fill_na(tr, 'ownerChange')
colSums(is.na(tr))

colnames(te)

for(i in 3:58){
  te <- fill_na(te, colnames(te)[i])
}

colSums(is.na(te))

tr$openDate
## 개업 날짜 데이터 전처리
tr$openDate <- sapply(tr$openDate, FUN= function(x){as.factor(x%/%10000)})
te$openDate <- sapply(te$openDate, FUN= function(x){as.factor(x%/%10000)})

levels(tr$openDate)
levels(te$openDate)

setdiff(levels(tr$openDate), levels(te$openDate))
setdiff(levels(te$openDate), levels(tr$openDate))

years <- c( "2007","2016","2000" ,"2005", "2002", "1982", "1987", "2006", "2008", "2013", "1981", "2012", "1994", "1996", "2004", "1997", "2003", "2001", "2014", "2011", "1988",
            "1998", "2009" ,"2017", "1993", "1983", "2010", "1999", "1985", "2015", "1984", "1978", "1989", "1980", "1986", "1992", "1995", "1976")

tr$openDate <- factor(tr$openDate, levels=years)
te$openDate <- factor(te$openDate, levels=years)

# factor 변수인 -- sido, openDate, instkind, ownerChange 제외하고 전부 gather로 묶어서 정리
colnames(tr)
trim_tr <- tr[,c(-1,-3,-5,-7,-58)]
trim_tr
library(dplyr)
trim_tr %>% gather(key=variable, value, -OC) -> gather_tr
gather_tr
library(ggplot2)
## 피처의 전반적인 특징을 확인할 수 있다..!
gather_tr %>% ggplot(aes(x=OC,y=value))+facet_wrap(~variable, scales='free') + geom_boxplot()

# 변수 간의 관계도 확인해볼 수 있다...!
library(corrplot)
trim_tr <- trim_tr[,-1]
cors <- cor(trim_tr)
corrplot(cors)

# factor 형 변수의 영향력도 확인할 수 있다.
table(tr$sido, tr$OC)
table(tr$openDate, tr$OC)
table(tr$instkind, tr$OC)
table(tr$ownerChange, tr$OC)

## 새로운 파생 변수도 추가할 수 있다...!

# 수익 차 변수 추가
tr$revenue0 <- tr$revenue1 - tr$revenue2
te$revenue0 <- te$revenue1 - te$revenue2

# 이윤 차 변수 추가
tr$profit0 <- tr$profit1 - tr$profit2
te$profit0 <- te$profit1 - te$profit2

# 설명 변수 column들
# 일단 모든 피처를 다 집어 넣었을 때
name <- names(tr)[3:ncol(tr)]
name

fomula2 <- as.formula(paste('OC',paste(name,collapse='+'),sep='~'))
fomula2

set.seed(10)
library(randomForest)
first_rf <- randomForest(fomula2, tr)
varImpPlot(first_rf)

#sub <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/submission_sample.csv")

## 피처는 중요도나 전반적으로 비슷한 의미를 지니는 변수들끼리 묶어서 표현...!
library(randomForest)
set.seed(10)
rf <- randomForest(OC~sido+sgg+openDate+bedCount+instkind, tr)
pre0 <- predict(rf, te)
sub <- cbind(sub, pre0)
sub
#sub$pre0 <- ifelse(sub$pre0=='open',1,0)
sub$OC <- ifelse(sub$pre0==1, 1, 0)
sub

# 수익
set.seed(10)
rf <- randomForest(OC~revenue1+profit1+noi1+surplus1+revenue2+profit2+noi2+surplus2, tr)
pre1 <- predict(rf, te)
sub <- cbind(sub, pre1)
#sub$pre1 <- ifelse(sub$pre1 =='open',1,0)
sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1 == 1, 1, 0)
sub

# 비용
set.seed(10)
rf <- randomForest(OC~salescost1+sga1+salary1+noe1+interest1+ctax1+salescost2+sga2+salary2+noe2+interest2+ctax2, tr)
pre2 <- predict(rf, te)
sub <- cbind(sub, pre2)
#sub$pre2 <- ifelse(sub$pre2=='open',1,0)
sub$OC <- ifelse(sub$pre0==1 & sub$pre1==1 & sub$pre2 ==1, 1, 0 )
sub

# 자산
set.seed(10)
rf <- randomForest(OC~liquidAsset1+quickAsset1+receivableS1+inventoryAsset1+nonCAsset1+tanAsset1+OnonCAsset1+receivableL1+
                     liquidAsset2+quickAsset2+receivableS2+inventoryAsset2+nonCAsset2+tanAsset2+OnonCAsset2+receivableL2, tr)
pre3 <- predict(rf,te)
sub <- cbind(sub,pre3)
#sub$pre3 <- ifelse(sub$pre3=='open',1,0)
sub$OC <- ifelse(sub$pre0 ==1 & sub$pre1==1 & sub$pre2 ==1 & sub$pre3 ==1, 1, 0)
sub

# 부채
set.seed(10)
rf <- randomForest(OC~debt1+liquidLiabilities1+shortLoan1+NCLiabilities1+longLoan1+netAsset1+
                     debt2+liquidLiabilities2+shortLoan2+NCLiabilities2+longLoan2+netAsset2, tr)
pre4 <- predict(rf, te)
sub <- cbind(sub, pre4)
sub$OC <- ifelse(sub$pre0 == 1 & sub$pre1 ==1 & sub$pre2 == 1 & sub$pre3 == 1 & sub$pre4 == 1, 1, 0)
sub

# 17년과 16년 사이의 수익차
set.seed(10)
rf <- randomForest(OC~revenue0, tr)
pre5 <- predict(rf, te)
sub <- cbind(sub, pre5)
sub$OC <- ifelse(sub$pre0 == 1 & sub$pre1 ==1 & sub$pre2 == 1 & sub$pre3 == 1 & sub$pre4 == 1 & sub$pre5 ==1, 1, 0)
sub

# 17년과 16년 사이의 이윤차
set.seed(10)
rf <- randomForest(OC~profit0, tr)
pre6 <- predict(rf, te)
sub <- cbind(sub, pre6)
sub$OC <- ifelse(sub$pre0 == 1 & sub$pre1 ==1 & sub$pre2 == 1 & sub$pre3 == 1 & sub$pre4 == 1 & sub$pre5 ==1 & sub$pre6 == 1, 1, 0)
sub


result <- sub['inst_id']
result$OC <- sub$OC
result

#tr2 <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/train.csv", na.strings=c("",NA,"-")) # na.strings = ?? : 특정 문자를 NA로 저장
#te2 <- read.csv("C:/Users/sec/Desktop/병원 개폐업  예측 데이터/test.csv", na.strings=c("",NA,"-"))

te2$employee1 <- gsub(",", "", te2$employee1)
te2$employee2 <- gsub(",", "", te2$employee2)

# train 데이터와의 통일을 위해 integer 형태로 전환해준다.

te2$employee1 <- as.integer(te2$employee1)
te2$employee2 <- as.integer(te2$employee2)

te2$OC <- as.factor(te2$OC)

# 그런 다음 levels를 해당에 맞게 적용시켜준다 factor화 시켜준다.
tr2$sido <- factor(tr2$sido, levels = Sido)
te2$sido <- factor(te2$sido, levels= Sido)

tr2$instkind <- factor(tr2$instkind, levels= Instkind)
te2$instkind <- factor(te2$instkind, levels=Instkind)

tr2$OC <- ifelse(tr2$OC=='open',1,0)

tr2$revenue0 <- tr2$revenue1 - tr2$revenue2
te2$revenue0 <- te2$revenue1 - te2$revenue2

tr2$profit0 <- tr2$profit1 - tr2$profit2
te2$profit0 <- te2$profit1 - te2$profit2

tr2$openDate <- sapply(tr2$openDate, FUN= function(x){as.factor(x%/%10000)})
te2$openDate <- sapply(te2$openDate, FUN= function(x){as.factor(x%/%10000)})


years <- c( "2007","2016","2000" ,"2005", "2002", "1982", "1987", "2006", "2008", "2013", "1981", "2012", "1994", "1996", "2004", "1997", "2003", "2001", "2014", "2011", "1988",
            "1998", "2009" ,"2017", "1993", "1983", "2010", "1999", "1985", "2015", "1984", "1978", "1989", "1980", "1986", "1992", "1995", "1976")

tr2$openDate <- factor(tr2$openDate, levels=years)
te2$openDate <- factor(te2$openDate, levels=years)


## 모델링 설정

new_name <- name

tr2
train_x <- data.matrix(tr2[,new_name])
train_y <- data.matrix(tr2$OC)


test_x <- data.matrix(te2[,new_name])

library(xgb.DMatrix)
x_mt <- xgb.DMatrix(data=train_x,label=train_y)

xg_hospital <- xgboost(data=x_mt,eta=0.025, max_depth=15,nrounds=1000,objective='reg:linear',eval_metric="rmse",verbose=F)

result$prob_xg <- predict(xg_hospital, test_x)
result

tapply(result$prob_xg, result$OC, mean)
result$OC <- ifelse(result$prob_xg<0.9,0,1)
result

result <- result[,-3]
result

write.csv(result, "submission_test.csv", row.names = F)
save(tr,te,sub,tr2,te2, file="day5Data.RData")
