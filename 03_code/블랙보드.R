###############
# ~ 23.01.29
###############
library("ggplot2")
library(dplyr)

# 데이터 불러오기
data <- read.csv("블랙보드만족도조사_학생_데이터분석공모전.csv", header=T, fileEncoding = "UTF-8", check.names = F)
summary(data)

# int -> factor 변환
for (i in c(4:19,21,23:25,27,28,30,32,34,36,38,40,42,44,46,48,51)){
  data[,i] <- as.factor(data[,i])
}

# level 변환
for (i in c(6,11,24,27,28,30,32,34,36,38,40,42,44,46,48)){
  levels(data[,i]) <- c('매우 불만족','불만족','보통','만족','매우 만족')
}

data[,7] <- factor(data[,7], levels=c('하루에 한 번','하루에 3회 이상','1주에 1회 정도','1주에 3회 정도','1달에 1회 정도','접속하지 않음'))
data[,8] <- factor(data[,8], levels=c('오전 (6:00-12:00)','오후1 (12:00-15:00)','오후2 (15:00-18:00)','저녁 (18:00-24:00)','새벽 (24:00-6:00)'))

for (i in c(9,12,13,14,25)){
  levels(data[,i]) <- c('전혀 그렇지 않다','그렇지 않다','보통이다','그렇다','매우 그렇다')
}

data[,10] <- factor(data[,10], levels=c('크롬(Chrome)','Mac용 사파리','Mac용 파이어폭스','엣지(Edge)','기타'))
data[,15] <- factor(data[,15], levels=c('실시간 화상강의(Zoom)','실시간 화상강의(Collaborate)','녹화영상(영상 출석콘텐츠)','외부 URL 또는 YOUTUBE 제공','온라인수업(비대면)을 선호하지 않음'))
data[,16] <- factor(data[,16], levels=c('영상 출석콘텐츠 외 자료 제공','과제','퀴즈/시험','토론','조별 활동','질의 응답','없음'))
data[,17] <- factor(data[,17], levels=c('녹화 강의 영상의 질','실시간 화상강의 수업의 질','내용 이해를 돕는 학습 활동의 부족','교수-학생·학생-학생 간 의사소통의 부족','시험의 공정성','과제 수행의 어려움','없음'))
data[,18] <- factor(data[,18], levels=c('온라인수업 시 집중 저하','안정적인 수업 수강 장소 부재','시스템 불안정','네트워크 불안정으로 접속 끊김 현상','블랙보드 기능 사용의 어려움','없음'))
data[,19] <- factor(data[,19], levels=c('매우 불만족','불만족','보통','만족','매우 만족','영상 출석콘텐츠 수업 경험 없음'))
data[,21] <- factor(data[,21], levels=c('매우 불만족','불만족','보통','만족','매우 만족','실시간 화상강의 수업 경험 없음'))
data[,23] <- factor(data[,23], levels=c('메시지','과제·퀴즈 피드백','토론방','블로그·위키 피드백','수업용 화상강의 외 별도의 추가적인 화상강의(Zoom, Collaborate)','없음'))
data[,51] <- factor(data[,51], levels=c('현재 학습관리시스템(LMS) 블랙보드 유지','차세대 학습관리시스템(LMS) 도입 희망','상관없음'))

# 블랙보드 만족도 현황
prop.table(table(data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?`, y=(..count..)/sum(..count..), fill=data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 시스템에 전반적으로 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족하는 편 -> 81.4%
# 불만족하는 편 -> 9.1%

sum(as.integer(data[,6]))/length(data[,6])
# 3.868182

###############

## random forest
library(randomForest)
library(gridExtra)

data_copy <- data

# 컬럼 이름 변경
for (i in 1:50){
  colnames(data_copy)[i+5] <- print(paste0("Q", i))
}

# 컬럼 제외(영향X, 복수응답, 주관식)
data_copy <- data_copy[-c(1,2,3,20,22,26,29,31,33,35,37,39,41,43,45,47,49,50,51,52,53,54,55)]
# 복수응답 : 15,17,21
# 주관식 : 24,26,28,30,32,34,36,38,40,42,44,45,47,48,49,50

# ntree=300 / 전체 변수 : 32개(y 포함)
set.seed(1)

rf <- randomForest(Q1~., data = data_copy, importance = TRUE,
                   ntree = 300, mtry = 31)
rf

rf_imp <- data.frame(importance(rf)[,3])
rf_imp <- cbind(data.frame(rownames(rf_imp)), rf_imp)
rownames(rf_imp) <- NULL
colnames(rf_imp) <- c("variable","MeanDecreaseAccuracy")

RF <- ggplot(rf_imp, aes(x=reorder(variable, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy+5)) +
  geom_bar(stat="identity", fill="skyblue") +
  scale_y_continuous(labels = function(y) y-5) +
  coord_flip() +
  xlab("") + ylab("Mean Decrease Accuracy") +
  theme(axis.text.x = element_text(size=9,face='bold'),
        axis.text.y = element_text(size=9,face='bold')) +
  ggtitle("Random forest (ntree=300, mtry=31)")

RF

###############

## 블랙보드 만족하는 그룹
data_s <- data %>% filter(data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?` == '만족' | data$`1. 블랙보드 시스템에 전반적으로 만족하십니까?` == '매우 만족')
# 179명 (220명 중)

# 접속 빈도
prop.table(table(data_s$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기중 블랙보드에는 얼마나 자주 접속하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 하루에 한 번 -> 37.4%
# 하루에 3회 이상 -> 36.3%

# 접속 시간대
prop.table(table(data_s$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`, y=(..count..)/sum(..count..), fill=data_s$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 12:00-15:00(오후1) -> 29.1%
# 15:00-18:00(오후2) -> 25.7%

# 접속이 신속한가(페이지 접속, 이동, 로그인 등)
prop.table(table(data_s$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`, y=(..count..)/sum(..count..), fill=data_s$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 그렇다 -> 57.0%
# 매우 그렇다 -> 29.6%

# 이용하는 브라우저
prop.table(table(data_s$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#F8766D","#A3A500","#00B0F6","#E76BF3"))

# 크롬 -> 80.4%
# 사파리 -> 9.5%

###############

# (블랙보드 만족 -> 차세대 희망X) 그룹
data_s_nh <- data_s %>% filter(data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?` == '현재 학습관리시스템(LMS) 블랙보드 유지')
# 88명 (179명 중)

###############

(score_23_1 <- sum(as.integer(data_s_nh[,28]))/length(data_s_nh[,28]))
(score_25_1 <- sum(as.integer(data_s_nh[,30]))/length(data_s_nh[,30]))
(score_27_1 <- sum(as.integer(data_s_nh[,32]))/length(data_s_nh[,32]))
(score_29_1 <- sum(as.integer(data_s_nh[,34]))/length(data_s_nh[,34]))
(score_31_1 <- sum(as.integer(data_s_nh[,36]))/length(data_s_nh[,36]))
(score_33_1 <- sum(as.integer(data_s_nh[,38]))/length(data_s_nh[,38]))
(score_35_1 <- sum(as.integer(data_s_nh[,40]))/length(data_s_nh[,40]))
(score_37_1 <- sum(as.integer(data_s_nh[,42]))/length(data_s_nh[,42]))
(score_39_1 <- sum(as.integer(data_s_nh[,44]))/length(data_s_nh[,44]))
(score_41_1 <- sum(as.integer(data_s_nh[,46]))/length(data_s_nh[,46]))
(score_43_1 <- sum(as.integer(data_s_nh[,48]))/length(data_s_nh[,48]))

score_data_1 <- rbind(score_23_1, score_25_1, score_27_1, score_29_1,
                      score_31_1, score_33_1, score_35_1, score_37_1,
                      score_39_1, score_41_1, score_43_1)
score_data_1 <- data.frame(score_data_1)
colnames(score_data_1) <- "score"
score_data_1 <- score_data_1 %>% arrange(desc(score))

###############
# ~ 23.01.26
###############

## 만족도 점수 결과
# 5점 척도 : 4,6,7,8,9,19,20,22,23,25,27,29,31,33,35,37,39,41,43 (19개)

(score_4 <- sum(as.integer(data_s[,9]))/length(data_s[,9]))
(score_6 <- sum(as.integer(data_s[,11]))/length(data_s[,11]))
(score_7 <- sum(as.integer(data_s[,12]))/length(data_s[,12]))
(score_8 <- sum(as.integer(data_s[,13]))/length(data_s[,13]))
(score_9 <- sum(as.integer(data_s[,14]))/length(data_s[,14]))
(score_19 <- sum(as.integer(data_s[,24]))/length(data_s[,24]))
(score_20 <- sum(as.integer(data_s[,25]))/length(data_s[,25]))
(score_22 <- sum(as.integer(data_s[,27]))/length(data_s[,27]))
(score_23 <- sum(as.integer(data_s[,28]))/length(data_s[,28]))
(score_25 <- sum(as.integer(data_s[,30]))/length(data_s[,30]))
(score_27 <- sum(as.integer(data_s[,32]))/length(data_s[,32]))
(score_29 <- sum(as.integer(data_s[,34]))/length(data_s[,34]))
(score_31 <- sum(as.integer(data_s[,36]))/length(data_s[,36]))
(score_33 <- sum(as.integer(data_s[,38]))/length(data_s[,38]))
(score_35 <- sum(as.integer(data_s[,40]))/length(data_s[,40]))
(score_37 <- sum(as.integer(data_s[,42]))/length(data_s[,42]))
(score_39 <- sum(as.integer(data_s[,44]))/length(data_s[,44]))
(score_41 <- sum(as.integer(data_s[,46]))/length(data_s[,46]))
(score_43 <- sum(as.integer(data_s[,48]))/length(data_s[,48]))

score_data <- rbind(score_4, score_6, score_7, score_8,
                    score_9, score_19, score_20, score_22,
                    score_23, score_25, score_27, score_29,
                    score_31, score_33, score_35, score_37,
                    score_39, score_41, score_43)
score_data <- data.frame(score_data)
colnames(score_data) <- "score"
score_data <- score_data %>% arrange(desc(score))

###############

## 만족도가 높은 문항

# 글자 크기 적정한가
prop.table(table(data_s$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`, y=(..count..)/sum(..count..), fill=data_s$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 그렇다 -> 44.7%
# 매우 그렇다 -> 43.0%

# 코스 메뉴 공지사항 기능
prop.table(table(data_s$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("코스 메뉴 공지사항 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 48.0%
# 매우 만족 -> 40.8%

# 성적 기능
prop.table(table(data_s$`43. 성적 기능에 얼마나 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`43. 성적 기능에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`43. 성적 기능에 얼마나 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`43. 성적 기능에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("성적 기능에 얼마나 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 49.7%
# 매우 만족 -> 36.9%

###############

## 만족도가 낮은 문항

# 모바일 app 관련
prop.table(table(data_s$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 보통 -> 31.3%
# 만족 -> 30.2%
# 불만족 -> 20.7%

# 그룹활동 기능
prop.table(table(data_s$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 46.4%
# 보통 -> 28.5%
# 매우 만족 -> 16.2%

# 구성
prop.table(table(data_s$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 57.5%
# 보통 -> 18.4%
# 매우 만족 -> 17.3%

###############

## random forest
library(randomForest)
library(gridExtra)

data_s_copy <- data_s

# 컬럼 이름 변경
for (i in 1:50){
  colnames(data_s_copy)[i+5] <- print(paste0("Q", i))
}

# 컬럼 제외(영향X, 복수응답, 주관식)
data_s_copy <- data_s_copy[-c(1,2,3,17,18,20,22,26,29,31,33,35,37,39,41,43,45,47,49,50,51,52,53,54,55)]
# 복수응답 : 15,17,21
# 주관식 : 24,26,28,30,32,34,36,38,40,42,44,45,47,48,49,50

# factor level 변경
data_s_copy$Q1 <- droplevels(data_s_copy$Q1)

# ntree=300 / 전체 변수 : 30개(y 포함)
set.seed(11)

rf_s <- randomForest(Q1~., data = data_s_copy, importance = TRUE,
                     ntree = 300, mtry = 29)
rf_s

rf_s_imp <- data.frame(importance(rf_s)[,3])
rf_s_imp <- cbind(data.frame(rownames(rf_s_imp)), rf_s_imp)
rownames(rf_s_imp) <- NULL
colnames(rf_s_imp) <- c("variable","MeanDecreaseAccuracy")

RF_S <- ggplot(rf_s_imp, aes(x=reorder(variable, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy+3)) +
  geom_bar(stat="identity", fill="skyblue") +
  scale_y_continuous(labels = function(y) y-3) +
  coord_flip() +
  xlab("") + ylab("Mean Decrease Accuracy") +
  ggtitle("Random forest (ntree=300, mtry=29)")

RF_S

###############

# 차세대 희망 현황
prop.table(table(data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 희망한다 -> 23.5%
# 희망하지 않는다 -> 49.2%

###############

# (블랙보드 만족 -> 차세대 희망X) 그룹
data_s_nh <- data_s %>% filter(data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?` == '현재 학습관리시스템(LMS) 블랙보드 유지')
# 88명 (179명 중)

###############

(score_23_1 <- sum(as.integer(data_s_nh[,28]))/length(data_s_nh[,28]))
(score_25_1 <- sum(as.integer(data_s_nh[,30]))/length(data_s_nh[,30]))
(score_27_1 <- sum(as.integer(data_s_nh[,32]))/length(data_s_nh[,32]))
(score_29_1 <- sum(as.integer(data_s_nh[,34]))/length(data_s_nh[,34]))
(score_31_1 <- sum(as.integer(data_s_nh[,36]))/length(data_s_nh[,36]))
(score_33_1 <- sum(as.integer(data_s_nh[,38]))/length(data_s_nh[,38]))
(score_35_1 <- sum(as.integer(data_s_nh[,40]))/length(data_s_nh[,40]))
(score_37_1 <- sum(as.integer(data_s_nh[,42]))/length(data_s_nh[,42]))
(score_39_1 <- sum(as.integer(data_s_nh[,44]))/length(data_s_nh[,44]))
(score_41_1 <- sum(as.integer(data_s_nh[,46]))/length(data_s_nh[,46]))
(score_43_1 <- sum(as.integer(data_s_nh[,48]))/length(data_s_nh[,48]))

score_data_1 <- rbind(score_23_1, score_25_1, score_27_1, score_29_1,
                    score_31_1, score_33_1, score_35_1, score_37_1,
                    score_39_1, score_41_1, score_43_1)
score_data_1 <- data.frame(score_data_1)
colnames(score_data_1) <- "score"
score_data_1 <- score_data_1 %>% arrange(desc(score))

###############
# ~ 23.01.22
###############

# 차세대 희망 현황
prop.table(table(data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`, y=(..count..)/sum(..count..), fill=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 희망한다 -> 23.5%
# 희망하지 않는다 -> 49.2%

### (블랙보드 만족 -> 차세대 희망X) 그룹
data_s_nh <- data_s %>% filter(data_s$`46. 차세대 학습관리시스템(LMS)를 도입하는 것에 대해 어떻게 생각하십니까?` == '현재 학습관리시스템(LMS) 블랙보드 유지')
# 88명 (179명 중)

###############

# (블랙보드 만족 -> 차세대 희망X) 그룹의 컬럼 분석

### 블랙보드 경험 및 인식
## 접속 관련
# 접속 빈도
prop.table(table(data_s_nh$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`2. 학기중 블랙보드에는 얼마나 자주 접속하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기중 블랙보드에는 얼마나 자주 접속하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 하루에 3회 이상 -> 39.8%
# 하루에 한 번 -> 37.5%

# 접속 시간대
prop.table(table(data_s_nh$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`3. 학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기중 블랙보드를 가장 많이 활용한 시간대는 언제입니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 12:00-15:00(오후1) -> 31.8%
# 15:00-18:00(오후2) -> 27.3%

# 접속이 신속한가(페이지 접속, 이동, 로그인 등)
prop.table(table(data_s_nh$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`4. 학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("학기 중 블랙보드 사이트 접속이 신속하게 이루어집니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 그렇다 -> 61.4%
# 매우 그렇다 -> 22.7%

# 이용하는 브라우저
prop.table(table(data_s_nh$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`5. 어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("어떤 브라우저를 사용하여 블랙보드를 주로 접속하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#F8766D","#A3A500","#00B0F6","#E76BF3"))

# 크롬 -> 77.3%
# 사파리 -> 12.5%

## 블랙보드 시가화
# 구성 및 디자인
prop.table(table(data_s_nh$`6. 블랙보드 메인 화면의 구성 및 디자인에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`6. 블랙보드 메인 화면의 구성 및 디자인에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`6. 블랙보드 메인 화면의 구성 및 디자인에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`6. 블랙보드 메인 화면의 구성 및 디자인에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 메인 화면의 구성 및 디자인에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 58.0%
# 매우 만족 -> 27.3%

# 메뉴가 직관적인가
prop.table(table(data_s_nh$`7. 블랙보드 강의실 내 메뉴는 직관적이라고 생각하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`7. 블랙보드 강의실 내 메뉴는 직관적이라고 생각하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`7. 블랙보드 강의실 내 메뉴는 직관적이라고 생각하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`7. 블랙보드 강의실 내 메뉴는 직관적이라고 생각하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 강의실 내 메뉴는 직관적이라고 생각하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 그렇다 -> 46.6%
# 매우 그렇다 -> 31.8%

# 분류가 잘 되어있는가(안내 페이지, 개인정보 화면, 코스 목록 등)
prop.table(table(data_s_nh$`8. 블랙보드에서 제공되는 정보들은 잘 분류되어 있습니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`8. 블랙보드에서 제공되는 정보들은 잘 분류되어 있습니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`8. 블랙보드에서 제공되는 정보들은 잘 분류되어 있습니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`8. 블랙보드에서 제공되는 정보들은 잘 분류되어 있습니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드에서 제공되는 정보들은 잘 분류되어 있습니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 그렇다 -> 46.6%
# 매우 그렇다 -> 39.8%

# 글자 크기 적정한가
prop.table(table(data_s_nh$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`9. 블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내 글자 크기(메뉴 텍스트, 본문, 코스 정보 등)는 적정합니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 그렇다 -> 42.0%
# 매우 그렇다 -> 47.7%

## 블랙보드 내 선호하는 기능
# 선호하는 온라인수업
prop.table(table(data_s_nh$`10. 블랙보드 내에서 가장 선호하는 온라인수업(비대면) 운영 방식은 무엇입니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`10. 블랙보드 내에서 가장 선호하는 온라인수업(비대면) 운영 방식은 무엇입니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`10. 블랙보드 내에서 가장 선호하는 온라인수업(비대면) 운영 방식은 무엇입니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`10. 블랙보드 내에서 가장 선호하는 온라인수업(비대면) 운영 방식은 무엇입니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내에서 가장 선호하는 온라인수업(비대면) 운영 방식은 무엇입니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 실시간화상강의(zoom) -> 39.8%
# 녹화영상(영상 출석콘텐츠) -> 30.7%

# 효과적인 학습활동
prop.table(table(data_s_nh$`11. 귀하가 생각하는 가장 효과적인 블랙보드 내 학습활동은 무엇입니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`11. 귀하가 생각하는 가장 효과적인 블랙보드 내 학습활동은 무엇입니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`11. 귀하가 생각하는 가장 효과적인 블랙보드 내 학습활동은 무엇입니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`11. 귀하가 생각하는 가장 효과적인 블랙보드 내 학습활동은 무엇입니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("귀하가 생각하는 가장 효과적인 블랙보드 내 학습활동은 무엇입니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#F8766D","#C49A00","#53B400","#A58AFF","#FB61D7"))

# 영상 출석콘텐츠 외 자료 제공 -> 44.3%
# 과제 -> 40.9%

## 블랙보드 내 한계
# 학습의 한계
prop.table(table(data_s_nh$`12. 블랙보드 내 학습의 한계는 무엇이라고 생각하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`12. 블랙보드 내 학습의 한계는 무엇이라고 생각하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`12. 블랙보드 내 학습의 한계는 무엇이라고 생각하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`12. 블랙보드 내 학습의 한계는 무엇이라고 생각하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내 학습의 한계는 무엇이라고 생각하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 의사소통의 부족 -> 45.5%
# 시험의 공정성, 없음 -> 15.9%

# 수강 환경의 한계
prop.table(table(data_s_nh$`13. 블랙보드 내 수강 환경의 한계는 무엇이라고 생각하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`13. 블랙보드 내 수강 환경의 한계는 무엇이라고 생각하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`13. 블랙보드 내 수강 환경의 한계는 무엇이라고 생각하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`13. 블랙보드 내 수강 환경의 한계는 무엇이라고 생각하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내 수강 환경의 한계는 무엇이라고 생각하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 온라인수업 시 집중 저하 -> 43.2%
# 네트워크 불안정으로 접속 끊김 현상 -> 22.7%

## 영상 출석콘텐츠(녹화영상) 관련
# 만족도
prop.table(table(data_s_nh$`14. 블랙보드 영상 출석콘텐츠 수업 방식에 대해 전반적으로 만족합니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`14. 블랙보드 영상 출석콘텐츠 수업 방식에 대해 전반적으로 만족합니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`14. 블랙보드 영상 출석콘텐츠 수업 방식에 대해 전반적으로 만족합니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`14. 블랙보드 영상 출석콘텐츠 수업 방식에 대해 전반적으로 만족합니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 영상 출석콘텐츠 수업 방식에 대해 전반적으로 만족합니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 48.9%
# 매우 만족 -> 19.3%

# 한계(복수)
# 복수응답 분리
data_s_nh_15 <- unlist(strsplit(data_s_nh$`15. 블랙보드 영상 출석콘텐츠 수업 방식의 한계는 무엇이라고 생각하십니까? (복수응답 가능)`, split=";"))

prop.table(table(data_s_nh_15))*100

ggplot() +
  geom_bar(aes(x=data_s_nh_15, y=(..count..)/sum(..count..), fill=data_s_nh_15), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh_15, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 영상 출석콘텐츠 수업 방식의 한계는 무엇이라고 생각하십니까? (복수응답 가능)") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 출석 반영 오류 -> 23.4%
# 상호작용 불가로 인한 질의응답 어려움 -> 16.1%

# (불만족, 매우 불만족)의 경우
# 출석 반영 오류 -> 33.3%
# 상호작용 불가로 인한 질의응답 어려움 -> 27.8%

## 실시간 화상강의(zoom, collaborate) 관련
# 만족도
prop.table(table(data_s_nh$`16. 실시간 화상강의(Zoom, Collaborate) 수업 방식에 대해 전반적으로 만족합니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`16. 실시간 화상강의(Zoom, Collaborate) 수업 방식에 대해 전반적으로 만족합니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`16. 실시간 화상강의(Zoom, Collaborate) 수업 방식에 대해 전반적으로 만족합니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`16. 실시간 화상강의(Zoom, Collaborate) 수업 방식에 대해 전반적으로 만족합니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("실시간 화상강의(Zoom, Collaborate) 수업 방식에 대해 전반적으로 만족합니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#B79F00","#00BA38","#00BFC4","#619CFF","#F564E3"))

# 만족 -> 62.5%
# 매우 만족 -> 22.7%

# 한계
# 복수응답 분리
data_s_nh_17 <- unlist(strsplit(data_s_nh$`17. 실시간 화상강의(Zoom, Collaborate)를 활용한 온라인수업의 한계는 무엇이라고 생각하십니까? (복수응답 가능)`, split=";"))

prop.table(table(data_s_nh_17))*100

ggplot() +
  geom_bar(aes(x=data_s_nh_17, y=(..count..)/sum(..count..), fill=data_s_nh_17), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh_17, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("실시간 화상강의(Zoom, Collaborate)를 활용한 온라인수업의 한계는 무엇이라고 생각하십니까? (복수응답 가능)") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 오류로 인한 수업 끊김 -> 38.1%
# 음향이 작거나 울림, 없음 -> 16.1%

# (불만족, 매우 불만족)의 경우
# 수업 내용 이해의 어려움 -> 50.0%
# 접속 오류로 인한 수업 끊김 -> 50.0%

## 블랙보드 내 선호하는 기능
# 소통 방식
prop.table(table(data_s_nh$`18. 블랙보드 내에서 가장 선호하는 소통 방식을 선택하여 주십시오.`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`18. 블랙보드 내에서 가장 선호하는 소통 방식을 선택하여 주십시오.`, y=(..count..)/sum(..count..), fill=data_s_nh$`18. 블랙보드 내에서 가장 선호하는 소통 방식을 선택하여 주십시오.`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`18. 블랙보드 내에서 가장 선호하는 소통 방식을 선택하여 주십시오.`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 내에서 가장 선호하는 소통 방식을 선택하여 주십시오.") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#F8766D","#B79F00","#00BA38","#619CFF","#F564E3"))

# 과제·퀴즈 피드백 -> 50.00%
# 없음 -> 17.05%

### 블랙보드 시스템 구성 및 기능 만족도

## 기능
# 만족도
prop.table(table(data_s_nh$`19. 블랙보드 기능에 전반적으로 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`19. 블랙보드 기능에 전반적으로 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`19. 블랙보드 기능에 전반적으로 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`19. 블랙보드 기능에 전반적으로 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 기능에 전반적으로 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 64.8%
# 매우 만족 -> 25.0%

# 사용이 용이한가
prop.table(table(data_s_nh$`20. 블랙보드 기능은 전반적으로 사용하기 쉽습니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`20. 블랙보드 기능은 전반적으로 사용하기 쉽습니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`20. 블랙보드 기능은 전반적으로 사용하기 쉽습니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`20. 블랙보드 기능은 전반적으로 사용하기 쉽습니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 기능은 전반적으로 사용하기 쉽습니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#00BF7D","#00B0F6","#E76BF3"))

# 그렇다 -> 55.7%
# 매우 그렇다 -> 35.2%

# 오류(복수)
# 복수응답 분리
data_s_nh_21 <- unlist(strsplit(data_s_nh$`21. 블랙보드 이용 시 오류를 겪었다면, 어떤 유형의 오류였는지 표기해주십시오. (복수응답 가능)`, split=";"))

prop.table(table(data_s_nh_21))*100

ggplot() +
  geom_bar(aes(x=data_s_nh_21, y=(..count..)/sum(..count..), fill=data_s_nh_21), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh_21, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 이용 시 오류를 겪었다면, 어떤 유형의 오류였는지 표기해주십시오. (복수응답 가능)") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 실시간 화상강의(zoom, collaborate) 참여, 코스 화면 접속 -> 16.4%
# 없음 -> 15.0%
# SSO 통합 로그인 -> 13.6%

## 구성
# 만족도
prop.table(table(data_s_nh$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`22. 블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("블랙보드 홈페이지는 LMS를 사용하는데 필요한 정보를 손쉽게 찾고 관련 안내를 신속하게 전달할 수 있도록 구성되었습니다. 이 구성에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 59.1%
# 매우 만족 -> 23.9%

## 모바일 app 관련
# 만족도
prop.table(table(data_s_nh$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`23. 모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("모바일 APP Blackboard는 웹에서 이용하는 주요 기능을 모바일 기기에서 이용 가능하도록 구성되었습니다. 모바일 APP 이용에 얼마나 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 30.7%
# 보통 -> 23.9%
# 불만족 -> 19.3%

# 개선점(주관식)
data_s_nh$`24. 모바일 APP 이용 시 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 코스 화면 구성
# 만족도
prop.table(table(data_s_nh$`25. 코스 화면에서는 과거 코스, 현재 수강 중인 코스 목록을 손쉽게 파악할 수 있도록 구성되어 있습니다. 코스 화면 구성에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`25. 코스 화면에서는 과거 코스, 현재 수강 중인 코스 목록을 손쉽게 파악할 수 있도록 구성되어 있습니다. 코스 화면 구성에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`25. 코스 화면에서는 과거 코스, 현재 수강 중인 코스 목록을 손쉽게 파악할 수 있도록 구성되어 있습니다. 코스 화면 구성에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`25. 코스 화면에서는 과거 코스, 현재 수강 중인 코스 목록을 손쉽게 파악할 수 있도록 구성되어 있습니다. 코스 화면 구성에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("코스 화면에서는 과거 코스, 현재 수강 중인 코스 목록을 손쉽게 파악할 수 있도록 구성되어 있습니다. 코스 화면 구성에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 46.6%
# 매우 만족 -> 29.5%

# 개선점(주관식)
data_s_nh$`26. 코스 화면에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 코스 메뉴 구성
# 만족도
prop.table(table(data_s_nh$`27. 코스 홈 접속 후 좌측에 나타나는 코스 메뉴 구성에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`27. 코스 홈 접속 후 좌측에 나타나는 코스 메뉴 구성에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`27. 코스 홈 접속 후 좌측에 나타나는 코스 메뉴 구성에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`27. 코스 홈 접속 후 좌측에 나타나는 코스 메뉴 구성에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("코스 홈 접속 후 좌측에 나타나는 코스 메뉴 구성에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 58.0%
# 매우 만족 -> 29.5%

# 개선점(주관식)
data_s_nh$`28. 코스 메뉴 구성에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 코스 메뉴 공지사항 기능
# 만족도
prop.table(table(data_s_nh$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`29. 코스 메뉴 공지사항 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("코스 메뉴 공지사항 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 46.6%
# 매우 만족 -> 42.0%

# 개선점(주관식)
data_s_nh$`30. 코스 메뉴 공지사항 기능 중 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 영상 출결 콘텐츠 기능
# 만족도
prop.table(table(data_s_nh$`31. 영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능에 만족하십니까? `))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`31. 영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능에 만족하십니까? `, y=(..count..)/sum(..count..), fill=data_s_nh$`31. 영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능에 만족하십니까? `), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`31. 영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능에 만족하십니까? `, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 51.1%
# 매우 만족 -> 27.3%

# 개선점(주관식)
data_s_nh$`32. 영상 출결 콘텐츠(커먼즈, 유튜브, 구글 드라이브) 기능 중 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 과제 제출 및 피드백 기능
# 만족도
prop.table(table(data_s_nh$`33. 과제 제출 및 피드백 기능(턴잇인, SafeAssign)에 얼마나 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`33. 과제 제출 및 피드백 기능(턴잇인, SafeAssign)에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`33. 과제 제출 및 피드백 기능(턴잇인, SafeAssign)에 얼마나 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`33. 과제 제출 및 피드백 기능(턴잇인, SafeAssign)에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("과제 제출 및 피드백 기능(턴잇인, SafeAssign)에 얼마나 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 51.1%
# 매우 만족 -> 37.5%

# 개선점(주관식)
data_s_nh$`34. 과제 제출 및 피드백 기능에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 그룹활동 기능
# 만족도
prop.table(table(data_s_nh$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`35. 그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("그룹활동(토론, 블로그, 저널) 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 51.1%
# 매우 만족 -> 19.3%

# 개선점
data_s_nh$`36. 그룹활동(토론, 블로그, 저널) 기능에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 녹화영상 출석 현황 기능
# 만족도
prop.table(table(data_s_nh$`37.  영상 출결 콘텐츠  출석 현황 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`37.  영상 출결 콘텐츠  출석 현황 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`37.  영상 출결 콘텐츠  출석 현황 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`37.  영상 출결 콘텐츠  출석 현황 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("영상 출결 콘텐츠  출석 현황 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#F8766D","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 45.5%
# 매우 만족 -> 37.5%

# 개선점
data_s_nh$`38. 녹화영상 출석 현황에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 시험/락다운브라우저 기능
# 만족도
prop.table(table(data_s_nh$`39. 시험/락다운브라우저 기능에 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`39. 시험/락다운브라우저 기능에 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`39. 시험/락다운브라우저 기능에 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`39. 시험/락다운브라우저 기능에 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("시험/락다운브라우저 기능에 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1))

# 만족 -> 40.9%
# 매우 만족 -> 36.4%

# 개선점
data_s_nh$`40. 시험/락다운브라우저 기능 중 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 실시간 강의 도구 기능
# 만족도
prop.table(table(data_s_nh$`41. 실시간 강의 도구(ZOOM, Collaborate) 기능에 만족하십니까? `))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`41. 실시간 강의 도구(ZOOM, Collaborate) 기능에 만족하십니까? `, y=(..count..)/sum(..count..), fill=data_s_nh$`41. 실시간 강의 도구(ZOOM, Collaborate) 기능에 만족하십니까? `), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`41. 실시간 강의 도구(ZOOM, Collaborate) 기능에 만족하십니까? `, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("실시간 강의 도구(ZOOM, Collaborate) 기능에 만족하십니까? ") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#A3A500","#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 54.5%
# 매우 만족 -> 39.8%

# 개선점
data_s_nh$`42. 실시간 강의 도구(ZOOM, Collaborate) 기능에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

## 성적 기능
# 만족도
prop.table(table(data_s_nh$`43. 성적 기능에 얼마나 만족하십니까?`))*100

ggplot() +
  geom_bar(aes(x=data_s_nh$`43. 성적 기능에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..), fill=data_s_nh$`43. 성적 기능에 얼마나 만족하십니까?`), stat="count") +
  geom_text(aes(label=paste0(sprintf("%.1f",(..count..)*100/sum(..count..)),"%"), x=data_s_nh$`43. 성적 기능에 얼마나 만족하십니까?`, y=(..count..)/sum(..count..)), stat="count", vjust=-0.5) +
  xlab("성적 기능에 얼마나 만족하십니까?") +
  ylab("비율") +
  theme(legend.position='none') +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(labels=scales::percent, limits = c(0,1)) +
  scale_fill_manual(values = c("#00BF7D","#00B0F6","#E76BF3"))

# 만족 -> 52.3%
# 매우 만족 -> 36.4%

# 개선점
data_s_nh$`44. 성적 기능에서 개선되었으면 하는 점을 자유롭게 작성하여 주십시오.`
# 텍스트마이닝 필요

###############

# 만족도 점수 결과
# 랜포에 들어가는 문항 : 2,3,4,5,6,7,8,9,10,11,14,16,18,19,20
# 5점 척도 아닌 것 제외 : 2,3,5,10,11,14,16,18

(score_4 <- sum(as.integer(data_s_nh[,9]))/length(data_s_nh[,9]))
(score_6 <- sum(as.integer(data_s_nh[,11]))/length(data_s_nh[,11]))
(score_7 <- sum(as.integer(data_s_nh[,12]))/length(data_s_nh[,12]))
(score_8 <- sum(as.integer(data_s_nh[,13]))/length(data_s_nh[,13]))
(score_9 <- sum(as.integer(data_s_nh[,14]))/length(data_s_nh[,14]))
(score_19 <- sum(as.integer(data_s_nh[,24]))/length(data_s_nh[,24]))
(score_20 <- sum(as.integer(data_s_nh[,25]))/length(data_s_nh[,25]))
(score_22 <- sum(as.integer(data_s_nh[,27]))/length(data_s_nh[,27]))
(score_23 <- sum(as.integer(data_s_nh[,28]))/length(data_s_nh[,28]))
(score_25 <- sum(as.integer(data_s_nh[,30]))/length(data_s_nh[,30]))
(score_27 <- sum(as.integer(data_s_nh[,32]))/length(data_s_nh[,32]))
(score_29 <- sum(as.integer(data_s_nh[,34]))/length(data_s_nh[,34]))
(score_31 <- sum(as.integer(data_s_nh[,36]))/length(data_s_nh[,36]))
(score_33 <- sum(as.integer(data_s_nh[,38]))/length(data_s_nh[,38]))
(score_35 <- sum(as.integer(data_s_nh[,40]))/length(data_s_nh[,40]))
(score_37 <- sum(as.integer(data_s_nh[,42]))/length(data_s_nh[,42]))
(score_39 <- sum(as.integer(data_s_nh[,44]))/length(data_s_nh[,44]))
(score_41 <- sum(as.integer(data_s_nh[,46]))/length(data_s_nh[,46]))
(score_43 <- sum(as.integer(data_s_nh[,48]))/length(data_s_nh[,48]))

score_data <- rbind(score_4, score_6, score_7, score_8,
                    score_9, score_19, score_20, score_22,
                    score_23, score_25, score_27, score_29,
                    score_31, score_33, score_35, score_37,
                    score_39, score_41, score_43)
score_data <- data.frame(score_data)
colnames(score_data) <- "score"
score_data <- score_data %>% arrange(desc(score))

###############

## 블랙보드 개선점 & 기타
data_s_nh$`45. 학습관리시스템(LMS) 블랙보드와 관련하여 개선되었으면 하는 사항 및 기타 하고 싶으신 말씀에 대해 작성해주십시오.`
# 텍스트마이닝 필요

###############

## 차세대 LMS
# 차세대 희망X 이유
data_s_nh$`47. 차세대 학습관리시스템(LMS)을 도입을 희망하지 않는 이유에 대해 작성하여 주십시오.`
# 텍스트마이닝 필요

# 차세대 도입 시, 반드시 유지해야 하는 기능
data_s_nh$`49. 차세대 학습관리시스템(LMS)을 도입하게 된다면, 반드시 유지해야 하는 기능에 대해 작성하여 주십시오.`
# 모두 빈칸

# 차세대 도입 시, 새롭게 도입되길 희망하는 기능
data_s_nh$`50. 차세대 학습관리시스템(LMS)을 도입하게 된다면, 새롭게 도입되길 희망하는 기능에 대해 작성하여 주십시오.`
# 모두 빈칸

###############
