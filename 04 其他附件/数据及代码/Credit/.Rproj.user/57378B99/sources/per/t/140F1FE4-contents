# *******************
# 一. 数据预处理 ####
# *******************

# 导入数据并初步处理
loan_data1 <- read.csv("./data/renrenloan1.csv", header = TRUE, stringsAsFactors = FALSE)
loan_data1[, "年龄"] <- 2015 - as.numeric(substring(loan_data1[,"出生年月"], 1, 4))
loan_data2 <- read.csv("./data/renrenloan2.csv", header = TRUE, stringsAsFactors = FALSE)
loan_data2[, "年龄"] <- 2016 - as.numeric(substring(loan_data2[,"出生年月"], 1, 4))
data_init <- rbind(loan_data1, loan_data2)
data_tidy <- subset(data_init, 
                    select=c('标的状态', 
                             
                             # 个人基础性息
                             '性别', '年龄', '学历', '婚姻', 
                             '公司性质', '公司规模', '工作城市', '工作时间',
                             
                             # 标的信息
                             '投资人次', '标的类型', '借款性质', '标的总额', 
                             '年利率', '期限', 
                             
                             # 资产信息
                             '收入', '房产', '房贷', '车产', '车贷', 
                             
                             
                             # 历史信用信息
                             '信用评级', '信用额度', '借款总额', '申请借款', 
                             '成功借款', '还清笔数', '逾期次数', '严重逾期',
                             
                             # 其他分析所需
                             '岗位职位', '标题', 'passTime'))

# 规范变量类型
str(data_tidy)
data_tidy$投资人次 <- as.numeric(data_tidy$投资人次)
for(i in 1:(dim(data_tidy)[2]-3)) {
  ifelse(typeof(data_tidy[,i]) == 'character', 
         data_tidy[,i] <- as.factor(data_tidy[,i]),
         print(paste(names(data_tidy)[i], typeof(data_tidy[,i]), sep = ': ')))
}

summary(data_tidy)
levels(data_tidy$标的状态) <- c('逾期','还清')
levels(data_tidy$学历) <- c(NA, '大专或本科', '大专或本科', '大专或本科', '大专或本科', 
                          '高中及以下', '高中及以下', '研究生及以上')
levels(data_tidy$婚姻) <- c('离婚',NA,'已婚','未婚','已婚')
levels(data_tidy$公司性质) <- c(NA, '国企', '民企', '单位', '其他', '优企', '单位',
                                '外企', '其他', '国企', '民企', '优企')
levels(data_tidy$公司规模) <- c(NA, '10-100人', '10-100人', '100-500人', 
                                '10人以下', '500人以上',  '500人以上')
levels(data_tidy$工作城市)[1] <- NA
levels(data_tidy$工作时间) <- c(NA, '5年以上', '1-3年', '1-3年', '1年以内', 
                                '3-5年', '5年以上', '无')
data_tidy$投资人次[data_tidy$投资人次 == '#N/A'] <- NA
levels(data_tidy$标的类型)[1] <- NA
levels(data_tidy$借款性质) <- c(NA, '资金周转', '资金周转', '个人消费', '购车购房', '购车购房',
                                '婚礼筹备', '教育培训', '其他借款', '投资创业', '投资创业',
                                '医疗支出', '装修借款', '装修借款')
levels(data_tidy$收入) <- c(NA, '10000-20000元', '5000元以下', '5000-10000元', '10000-20000元',
                            '5000元以下', '5000元以下', '5000元以下', '5000元以下', 
                            '5000元以下', '5000元以下', '20000-50000元', '20000-50000元',
                            '5000-10000元', '5000-10000元', '5000-10000元', '5000-10000元',
                            '5000-10000元', '5000-10000元', '5000-10000元', '50000元以上')
levels(data_tidy$房产) <- c('FALSE', 'FALSE', 'TRUE', 'TRUE')
levels(data_tidy$房贷) <- c('FALSE', 'FALSE', 'TRUE', 'TRUE')
levels(data_tidy$车产) <- c('FALSE', 'FALSE', 'TRUE', 'TRUE')
levels(data_tidy$车贷) <- c('FALSE', 'FALSE', 'TRUE', 'TRUE')


loan_data <- data_tidy
data_tidy <- data_tidy[,1:28]

# 缺失值处理1-删除记录或变量
library(VIM)
aggr_plot <- aggr(data_tidy, col = c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data_tidy), cex.axis=.7, gap=3, prop = FALSE,
                  ylab=c("各变量缺失条数", "数据缺失模式"))
index <- which( is.na(data_tidy$学历) | (is.na(data_tidy$公司性质) & is.na(data_tidy$公司规模)) )
data_tidy <- data_tidy[-index,]
data_tidy <- na.omit(data_tidy)


# 划分数据集
library(caret); set.seed(1234)
index <- createDataPartition(y = data_tidy$标的状态, p = 0.8, list = F)
data_train <- data_tidy[index,]; data_test <- data_tidy[-index,]
prop.table(table(data_train$标的状态)); prop.table(table(data_test$标的状态))

# 缺失值处理2-插补处理
aggr_plot <- aggr(data_tidy, col = c('navyblue', 'red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data_tidy), cex.axis=.7, gap=3, prop = FALSE,
                  ylab=c("各变量缺失条数", "数据缺失模式"))
# library(DMwR)
# rad_pred <- predict(class_mod, BostonHousing[is.na(BostonHousing$rad), ])
# 
# imputation_bag <- preProcess(data_train, method=c("knnImpute"))
# train <- predict(imputation_bag, data_train)
# test <- predict(imputation_bag, data_test)

# 非平衡样本处理
# 使用 SMOTE 来平衡数据
# perc.over = xx 表示 少样本变成原来的（1+xx/100）倍 
# perc.under=yy 表示多样本变成少样本的 yy/100 *(xx/100)倍
library(DMwR)
train <- SMOTE(标的状态 ~ ., data_train, perc.over=200, perc.under=150)
prop.table(table(train$标的状态))



train <- train[,-8]
dmy <- dummyVars(~.,data = train, fullRank=T)
train <- data.frame(predict(dmy,newdata=train))
test <- data.frame(predict(dmy,newdata=data_test[,-8]))


write.csv(train,file = 'train.csv')
write.csv(test,file = 'test.csv')

# *******************
# 二. 探索性分析 ####
# *******************

attach(data_train)
library(lattice)
densityplot(~年利率|标的状态)
