lines <- readLines("clean_data.csv", encoding = "big5")
utf8_data <- iconv(lines, "big5", "utf8")

data <- read.csv(text = utf8_data)
attach(data)

#----EDA----
# 先對連續變數繪製直方圖，看是否有需要做transform
hist(單價元平方公尺); sd(單價元平方公尺); mean(單價元平方公尺)
hist(建物移轉總面積平方公尺);hist(公設比);hist(屋齡);hist(移轉層次_數字);
hist(總樓層數_數字);hist(移轉層次_數字);hist(建物現況格局_房);hist(建物現況格局_廳);hist(建物現況格局_衛)


# 觀察連續變數間的關聯
pairs(data[,c(5,6,7,8,11,14,15,18,19)]) 

cor(data$單價元平方公尺, data$公設比, use = "complete.obs")  # 檢查公設比
cor(data$單價元平方公尺, data$屋齡, use = "complete.obs")  # 檢查屋齡
cor(data$公設比, data$屋齡, use = "complete.obs")  # 檢查公設比與屋齡

#觀察類別變數，繪製箱型圖
boxplot(單價元平方公尺~鄉鎮市區,data=data)
boxplot(單價元平方公尺~建物型態,data=data)
boxplot(單價元平方公尺~主要用途,data=data)
boxplot(單價元平方公尺~有無管理組織,data=data)
boxplot(單價元平方公尺~車位類別,data=data)
boxplot(單價元平方公尺~電梯,data=data)
boxplot(單價元平方公尺~豪宅,data=data)
boxplot(單價元平方公尺~面積大小,data=data)
boxplot(單價元平方公尺~移轉層次_級距,data=data)


#----MODEL----
data$屋齡2=data$屋齡^2

##以每坪單價為y----
#模型1，考量資料內容不重複的變數
lm1=lm(單價元平方公尺~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +主要用途+車位類別+電梯+公設比+屋齡+移轉層次_數字+總樓層數_數字
       +豪宅+鄉鎮市區+有無管理組織,data=data)
summary(lm1) #Multiple R-squared:  0.5077,	Adjusted R-squared:  0.5037

#更改一些類別變數的基礎，
#主要用途係數上來看雖然應改為農業用，但根據boxplot覺得不適合
#車位類別係數上改為"其他"較好"，但解讀上還是以"無車位"比較適合
data$鄉鎮市區 <- relevel(as.factor(data$鄉鎮市區), ref = "北投區")
data$車位類別 <- relevel(as.factor(data$車位類別), ref = "無車位")


#加上屋齡的平方
lm2=lm(單價元平方公尺~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +主要用途+車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_數字+總樓層數_數字
       +豪宅+鄉鎮市區+有無管理組織,data=data)
summary(lm2) #Multiple R-squared:  0.5259,	Adjusted R-squared:  0.5219
anova(lm2,lm1) #顯著


#考量交乘項
##地區與用途:同樣的用途在不同的地區也可能會有不同的影響程度
lm3=lm(單價元平方公尺~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_數字+總樓層數_數字
       +豪宅+主要用途*鄉鎮市區+有無管理組織,data=data)
summary(lm3) #Multiple R-squared:  0.5402,	Adjusted R-squared:  0.5316
             #加入交乘項後共線性太高或類別變數太多，無法執行vif。不考慮。
anova(lm3,lm2) #顯著

##樓層與電梯:樓層越高的住戶越需要電梯
lm3b=lm(單價元平方公尺~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
        +車位類別+公設比+屋齡+屋齡2+電梯*(移轉層次_數字+總樓層數_數字)
        +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data)
summary(lm3b) #Multiple R-squared:  0.5486,	Adjusted R-squared:  0.5446 
              #交乘項後共線性太高。不考慮。




#取log，考量變數轉換，將移轉層數限定>0會損失34個樣本
lm4=lm(單價元平方公尺~log(建物移轉總面積平方公尺)+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+log(移轉層次_數字)+log(總樓層數_數字)
       +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data[data$移轉層次_數字>0,])
summary(lm4) #Multiple R-squared:  0.538,	Adjusted R-squared:  0.5341 
             #面積與移轉層次的顯著程度提高


#使用級距代表移轉層次與面積
lm5=lm(單價元平方公尺~面積大小+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_級距+log(總樓層數_數字)
       +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data)
summary(lm5) #Multiple R-squared:  0.5355,	Adjusted R-squared:  0.531 
             #有效果，但沒有比取LOG好，不過lm5共線性太高，而無法計算VIF。
             #分別替換執行後發現是"移轉層次_級距"的原因。

#以lm4為基礎刪減變數
lm6 <- update(lm4, . ~ . - 有無管理組織)
summary(lm6) #Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5341 
anova(lm6,lm4) #不顯著，可剃除

lm7 <- update(lm6, . ~ . - 建物現況格局.廳)
summary(lm7) #Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5342 
anova(lm7,lm6) #不顯著，可剃除

lm8 <- update(lm7, . ~ . - 車位類別)
summary(lm8) #Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5342 
anova(lm8,lm7) #接近1%的顯著，不可剃除




##以總價為y----
lm1.1=lm(總價元~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +主要用途+車位類別+電梯+公設比+屋齡+移轉層次_數字+總樓層數_數字
       +豪宅+鄉鎮市區+有無管理組織,data=data)
summary(lm1.1) #Multiple R-squared:  0.817,	Adjusted R-squared:  0.8155 


#加上屋齡的平方
lm2.1=lm(總價元~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +主要用途+車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_數字+總樓層數_數字
       +豪宅+鄉鎮市區+有無管理組織,data=data)
summary(lm2.1) #Multiple R-squared:  0.8182,	Adjusted R-squared:  0.8167 
anova(lm2.1,lm1.1) #顯著


#考量交乘項
##地區與用途:同樣的用途在不同的地區也可能會有不同的影響程度
lm3.1=lm(總價元~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_數字+總樓層數_數字
       +豪宅+主要用途*鄉鎮市區+有無管理組織,data=data)
summary(lm3.1) #Multiple R-squared:  0.8228,	Adjusted R-squared:  0.8195 
               #加入交乘項後共線性太高或類別變數太多，無法執行vif。不考慮。
anova(lm3.1,lm2.1) #顯著

##樓層與電梯:樓層越高的住戶越需要電梯，
lm3.1b=lm(總價元~建物移轉總面積平方公尺+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
        +車位類別+公設比+屋齡+屋齡2+電梯*(移轉層次_數字+總樓層數_數字)
        +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data)
summary(lm3.1b) #Multiple R-squared:  0.8198,	Adjusted R-squared:  0.8182
                #交乘項共線性太高。不考慮。


#取log，考量變數轉換，將移轉層數限定>0會損失34個樣本
lm4.1=lm(總價元~log(建物移轉總面積平方公尺)+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+log(移轉層次_數字)+log(總樓層數_數字)
       +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data[data$移轉層次_數字>0,])
summary(lm4.1) #Multiple R-squared:  0.6296,	Adjusted R-squared:  0.6265 
               #面積顯著程度提高，移轉層次的顯著程度降低


#使用級距代表移轉層次與面積
lm5.1=lm(總價元~面積大小+建物型態+建物現況格局.房+建物現況格局.衛+建物現況格局.廳
       +車位類別+電梯+公設比+屋齡+屋齡2+移轉層次_級距+log(總樓層數_數字)
       +豪宅+主要用途+鄉鎮市區+有無管理組織,data=data)
summary(lm5.1) #Multiple R-squared:  0.5266,	Adjusted R-squared:  0.5225
               #顯著程度還在，但解釋能力也大幅下降。

#以lm2.1為基礎刪減變數
lm6.1 <- update(lm2.1, . ~ . - 有無管理組織)
summary(lm6.1) #Multiple R-squared:  0.8181,	Adjusted R-squared:  0.8166 
anova(lm6.1,lm2.1) #有10%的顯著，不可剃除

lm7.1 <- update(lm2.1, . ~ . - 車位類別)
summary(lm7.1) #Multiple R-squared:  0.8177,	Adjusted R-squared:  0.8164  
anova(lm7.1,lm6.1) #不顯著，可剃除


#以lm7和lm7.1繼續分析
#lm7共線性分析
library(car)
vif(lm7, type="predictor")
vif(lm7)
##發現在電梯以及屋齡及屋齡2中存在較高共線性##電梯可能與建物型態有高度相關(已被包含)
##考慮刪除電梯項或將建物型態與電梯變成交乘項
lm7_1<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 
            建物型態 + 建物現況格局.房 + 建物現況格局.衛 + 
            車位類別  + 公設比 + 屋齡 + 屋齡2 + log(移轉層次_數字) + 
            log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
          data = data[data$移轉層次_數字 > 0, ])
vif(lm7_1)
lm7_2<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 
            建物型態*電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
            車位類別  + 公設比 + 屋齡 + 屋齡2 + log(移轉層次_數字) + 
            log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
          data = data[data$移轉層次_數字 > 0, ])
vif(lm7_2)

data$建物型態_電梯 <- interaction(data$建物型態, data$電梯)
lm7_3<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
          + 建物現況格局.房 + 建物現況格局.衛 + 
            車位類別  + 公設比 + 屋齡 + 屋齡2 + log(移轉層次_數字) + 
            log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
          data = data[data$移轉層次_數字 > 0, ])
vif(lm7_3)
summary(lm7)  ##Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5342 
summary(lm7_1)##Multiple R-squared:  0.5367,	Adjusted R-squared:  0.5331 
summary(lm7_3)##Multiple R-squared:  0.5379,  Adjusted R-squared:  0.5342 
anova(lm7,lm7_3)
##刪去電梯後，儘管電梯像是顯著的，但整體解釋力並無下降太多，且共線性過高的問題也獲得解決
##直接將電梯與建物型態交乘後共線性太高或類別變數太多，無法執行vif
##使用 interaction() 將電梯與建物型交互項作為單一變數，發現解釋能力無下降，而且共線性過高的問體也解決
lm7_4<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
          + 建物現況格局.房 + 建物現況格局.衛 + 
            車位類別  + 公設比 + 屋齡 + 屋齡2 + log(移轉層次_數字) + 
            log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
          data = data[data$移轉層次_數字 > 0, ])
lm7_5<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
          + 建物現況格局.房 + 建物現況格局.衛 + 
            車位類別  + 公設比 + 屋齡 + log(移轉層次_數字) + 
            log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
          data = data[data$移轉層次_數字 > 0, ])
anova(lm7_4,lm7_5)
##剩下vif值較高的部分為屋年與屋齡2，考量屋齡2為屋齡^2本身即存在一定的共線性，在目前皆不超過5的情況下暫不進行變換
##且進行anova後發現屋齡2顯著不為0，因此暫時不做更動
##採用lim7_3繼續分析
##cookdistance
cooklm7_3 <-cooks.distance(lm7_3)
plot(cooklm7_3, xlab="ID number")
cooklm7_3[cooklm7_3 > 1.0]
cooklm7_3[cooklm7_3> 0.5]

##leverage
levlm7_3 <- hatvalues(lm7_3)
k <- 3; n <- nrow(lm7_3)
( thr3 <- 3*(k+1)/n )
( thr2 <- 2*(k+1)/n )
plot(levlm7_3, xlab="ID Number", ylab="Leverage")
abline(h=thr3, lty=2); abline(h=thr2,lty=3)
levlm7_3[levlm7_3 > thr3]

## Outlier (studentized residuals)
reslm7_3 <- rstandard(lm7_3)
hist(reslm7_3, breaks=20)
reslm7_3[reslm7_3<(-3) | reslm7_3>3]

##remove outlier
outlier_indices <- which(reslm7_3 < -3 | reslm7_3 > 3)
data_clean <- data[-outlier_indices, ]
lm7a  <- lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
            + 建物現況格局.房 + 建物現況格局.衛 + 
              車位類別  + 公設比 + 屋齡 + 屋齡2 + log(移轉層次_數字) + 
              log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
            data = data_clean[data_clean$移轉層次_數字 > 0, ])
vif(lm7a)
summary(lm7a)##Multiple R-squared:  0.5403,	Adjusted R-squared:  0.5365
##plot
plot(residuals(lm7a)) 
abline(h=0)

plot(fitted(lm7a), 
     residuals(lm7a), 
     xlab="Fitted", 
     ylab="Residuals"); abline(h=0)

hist(residuals(lm7a))
qqnorm(residuals(lm7a), ylab="Residuals"); qqline(residuals(lm7a))





##
##lm7.1共線性分析
library(car)
vif(lm7.1, type="predictor")
vif(lm7.1)
##發現依舊在電梯以及屋齡及屋齡2中存在較高共線性##電梯可能與建物型態有高度相關(已被包含)
##考慮刪除電梯項或將建物型態與電梯變成交成項
lm7.11<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 
             屋齡 + 屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data)

vif(lm7.11)
lm7.12<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態*電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 
             屋齡 + 屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data)
vif(lm7.12)
data$建物型態_電梯 <- interaction(data$建物型態, data$電梯)
lm7.13<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 
             屋齡 + 屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data)
vif(lm7.13)
vif(lm7.1)
summary(lm7.1)##Multiple R-squared:  0.8177,	Adjusted R-squared:  0.8164  
summary(lm7.11)##Multiple R-squared:  0.8175,	Adjusted R-squared:  0.8163 
summary(lm7.13)##Multiple R-squared:  0.8177,	Adjusted R-squared:  0.8164 
##刪去電梯後，儘管電梯像是顯著的，但整體解釋力並無下降太多，且共線性過高的問題也獲得解決
##使用 interaction() 將電梯與建物型交互項作為單一變數，發現解釋能力無下降，而且共線性過高的問體也解決
lm7.14<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 電梯 + 公設比 + 
             屋齡 + 屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data)
lm7.15<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 電梯 + 公設比 + 
             屋齡+ 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data)
anova(lm7.14,lm7.15)
##剩下vif值較高的部分為屋年與屋齡2，考量屋齡2為屋齡^2本身即存在一定的共線性，在目前皆不超過5的情況下暫不進行變換
##且進行anova後發現屋齡2顯著不為0，因此暫時放著
##依lm7.13繼續分析
## cookdistance
cooklm7.13 <-cooks.distance(lm7.13)
plot(cooklm7.13, xlab="ID number")
cooklm7.13[cooklm7.13 > 1.0]
cooklm7.13[cooklm7.13> 0.5]

##leverage
levlm7.13 <- hatvalues(lm7.13)
k <- 3; n <- nrow(lm7.13)
( thr3 <- 3*(k+1)/n )
( thr2 <- 2*(k+1)/n )
plot(levlm7.13, xlab="ID Number", ylab="Leverage")
abline(h=thr3, lty=2); abline(h=thr2,lty=3)
levlm7.13[levlm7.13 > thr3]

## Outlier (studentized residuals)
reslm7.13 <- rstandard(lm7.13)
hist(reslm7.13, breaks=20)
reslm7.13[reslm7.13<(-3) | reslm7.13>3]

##remove outlier
outlier_indices <- which(reslm7.13 < -3 | reslm7.13 > 3)
data_clean2 <- data[-outlier_indices, ]
lm7b <- lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 
             屋齡 + 屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 
             豪宅 + 鄉鎮市區 + 有無管理組織, data = data_clean2)
vif(lm7b)
summary(lm7b)##Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8926 
##plot
plot(residuals(lm7b)) 
abline(h=0)

plot(fitted(lm7b), 
     residuals(lm7b), 
     xlab="Fitted", 
     ylab="Residuals"); abline(h=0)

hist(residuals(lm7b))
qqnorm(residuals(lm7b), ylab="Residuals"); qqline(residuals(lm7b))



summary(lm7a)##Multiple R-squared:  0.5403,	Adjusted R-squared:  0.5365
##嘗試變數轉換lm7a
#考慮屋齡與屋齡2的高共線性
data_clean$avg_屋齡=(data_clean$屋齡+data_clean$屋齡2)/2
lm7a.1<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + avg_屋齡 + log(移轉層次_數字) + 
             log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])

summary(lm7a.8)#Multiple R-squared:  0.4911,	Adjusted R-squared:  0.4871
qqnorm(residuals(lm7a.11), ylab="Residuals"); qqline(residuals(lm7a.11))
##考量樓層之間的關係，可能也有高度共線性可以考略刪除看看
lm7a.2<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + 屋齡+屋齡2  + 
             log(總樓層數_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
##Multiple R-squared:  0.5351,	Adjusted R-squared:  0.5315
lm7a.3<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + avg_屋齡 + log(移轉層次_數字) + 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.3)#Multiple R-squared:  0.4878,	Adjusted R-squared:  0.4839 
data_clean$avg_樓層=(data_clean$移轉層次_數字+data_clean$總樓層數_數字)/2
lm7a.4<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + avg_屋齡 + avg_樓層 + 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.4)#Multiple R-squared:  0.4912,	Adjusted R-squared:  0.4873
##只留下移轉層次，並進行變數轉換加上平方項(從後面的變數轉換中發現!)
data_clean$sq移轉層次_數字=(data_clean$移轉層次_數字)^2
lm7a.12<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺)+ 建物型態_電梯
            + 建物現況格局.房 + 建物現況格局.衛 + 
              車位類別  + 公設比 + 屋齡 + 屋齡2+ 移轉層次_數字+sq移轉層次_數字+ 豪宅 + 主要用途 + 鄉鎮市區, 
            data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.12)#Multiple R-squared:  0.5481,	Adjusted R-squared:  0.5445 
plot(residuals(lm7a.12)) 
abline(h=0)

plot(fitted(lm7a.12), 
     residuals(lm7a.12), 
     xlab="Fitted", 
     ylab="Residuals"); abline(h=0)

hist(residuals(lm7a.12))
qqnorm(residuals(lm7a.12), ylab="Residuals"); qqline(residuals(lm7a.12))

##倒數ref
data_clean$ref建物移轉總面積平方公尺=1/(data_clean$建物移轉總面積平方公尺)
data_clean$ref屋齡=1/(data_clean$屋齡)
data_clean$ref屋齡2=1/(data_clean$屋齡2)
data_clean$ref公設比=1/(data_clean$公設比+1)
data_clean$ref移轉層次_數字=1/(data_clean$移轉層次_數字+1)
data_clean$ref總樓層次_數字=1/(data_clean$總樓層次_數字+1)
##嘗試屋齡設導數 因為可能屋齡越大導致價鉻越低
lm7a.5<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + ref屋齡 +  log(移轉層次_數字) + 
             log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.5)#Multiple R-squared:  0.4802,	Adjusted R-squared:  0.4761
lm7a.6<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + ref屋齡 + ref屋齡2+ log(移轉層次_數字) + 
             log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.6)##Multiple R-squared:  0.4884,	Adjusted R-squared:  0.4843 
lm7a.7<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺) + 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + ref公設比 + 屋齡 + 屋齡2+ log(移轉層次_數字) + 
             log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.7)#Multiple R-squared:  0.5401,	Adjusted R-squared:  0.5364
lm7a.8<-lm(單價元平方公尺 ~ ref建物移轉總面積平方公尺+ 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + 屋齡 + 屋齡2+ log(移轉層次_數字) + 
             log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.8)#Multiple R-squared:  0.5375,	Adjusted R-squared:  0.5337(雖然沒有提升解釋度，但有可能代表土地越大單位價格越小)
##平方
##sq
data_clean$sq建物移轉總面積平方公尺=(data_clean$建物移轉總面積平方公尺)^2
data_clean$sq公設比=(data_clean$公設比)^2
data_clean$sq移轉層次_數字=(data_clean$移轉層次_數字)^2
data_clean$sq總樓層次_數字=(data_clean$總樓層次_數字)^2
data_clean$sq豪宅=(data_clean$豪宅)^2
lm7a.9<-lm(單價元平方公尺 ~ sq建物移轉總面積平方公尺+ 建物型態_電梯
           + 建物現況格局.房 + 建物現況格局.衛 + 
             車位類別  + 公設比 + 屋齡 + 屋齡2+ log(移轉層次_數字) + 
             log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
           data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.9)#Multiple R-squared:  0.5311,	Adjusted R-squared:  0.5273
lm7a.10<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺)+ 建物型態_電梯
            + 建物現況格局.房 + 建物現況格局.衛 + 
              車位類別  + sq公設比 + 屋齡 + 屋齡2+ log(移轉層次_數字) + 
              log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
            data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.11)
lm7a.11<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺)+ 建物型態_電梯
            + 建物現況格局.房 + 建物現況格局.衛 + 
              車位類別  + 公設比 + 屋齡 + 屋齡2+ sq移轉層次_數字+ 
              log(總樓層數_數字)+ 豪宅 + 主要用途 + 鄉鎮市區, 
            data = data_clean[data_clean$移轉層次_數字 > 0, ])
summary(lm7a.11)#Multiple R-squared:  0.5409,	Adjusted R-squared:  0.5372 
lm7a.12<-lm(單價元平方公尺 ~ log(建物移轉總面積平方公尺)+ 建物型態_電梯
            + 建物現況格局.房 + 建物現況格局.衛 + 
              車位類別  + 公設比 + 屋齡 + 屋齡2+ 移轉層次_數字+sq移轉層次_數字+ 豪宅 + 主要用途 + 鄉鎮市區, 
            data = data_clean[data_clean$移轉層次_數字 > 0, ])
vif(lm7a)
summary(lm7a.12)#Multiple R-squared:  0.5481,	Adjusted R-squared:  0.5445 
plot(residuals(lm7a.12)) 
abline(h=0)

plot(fitted(lm7a.12), 
     residuals(lm7a.12), 
     xlab="Fitted", 
     ylab="Residuals"); abline(h=0)
hist(residuals(lm7a.12))
qqnorm(residuals(lm7a.12), ylab="Residuals"); qqline(residuals(lm7a.12))

summary(lm7b)#Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8926 
##變數轉換lm7.1
##嘗試變數轉換lm7b
#考慮屋齡與屋齡2的高共線性
data_clean2$avg_屋齡=(data_clean2$屋齡+data_clean2$屋齡2)/2
lm7b.1<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 +
             avg_屋齡 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)

summary(lm7b.1)#Multiple R-squared:  0.8878,	Adjusted R-squared:  0.887 
qqnorm(residuals(lm7b.1), ylab="Residuals"); qqline(residuals(lm7b.1))
##考量樓層之間的關係，可能也有高度共線性可以考略刪除看看
lm7b.2<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 +
             屋齡+屋齡2  + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.2)#Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8926
##Multiple R-squared:  0.5351,	Adjusted R-squared:  0.5315
lm7b.3<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 +
             屋齡+屋齡2 +移轉層次_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.3)#Multiple R-squared:  0.8904,	Adjusted R-squared:  0.8897 
data_clean2$avg_樓層=(data_clean2$移轉層次_數字+data_clean2$總樓層數_數字)/2
lm7b.4<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 +
             屋齡+屋齡2 + avg_樓層 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.4)#Multiple R-squared:  0.892,	Adjusted R-squared:  0.8913  
##倒數ref
data_clean2$ref建物移轉總面積平方公尺=1/(data_clean2$建物移轉總面積平方公尺)
data_clean2$ref屋齡=1/(data_clean2$屋齡)
data_clean2$ref屋齡2=1/(data_clean2$屋齡2)
data_clean2$ref公設比=1/(data_clean2$公設比+1)
data_clean2$ref移轉層次_數字=1/(data_clean2$移轉層次_數字+1)
data_clean2$ref豪宅=1/(data_clean2$豪宅+1)
##嘗試屋齡設導數 因為可能屋齡越大導致價鉻越低
lm7b.5<-lm(總價元 ~ ref建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 屋齡 + 
             屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.5)#Multiple R-squared:  0.5899,	Adjusted R-squared:  0.587
lm7b.6<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + ref公設比 + 屋齡 + 
             屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.6)##MMultiple R-squared:  0.893,	Adjusted R-squared:  0.8923  
##屋齡大小運用倒數可能回有效果?屋齡越大價格越低
lm7b.7<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + ref屋齡 + 
             + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.7)#Multiple R-squared:  0.8874,	Adjusted R-squared:  0.8866 
lm7b.8<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + ref屋齡 + 
             ref屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.8)#Multiple R-squared:  0.8883,	Adjusted R-squared:  0.8875
##考慮踢掉屋齡2，因為兩者本身有一定關係
lm7b.9<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
             建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
             建物現況格局.廳 + 主要用途 + 公設比 + 屋齡 + 
             + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
             鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.9)#Multiple R-squared:  0.8902,	Adjusted R-squared:  0.8895
##sq
data_clean2$sq建物移轉總面積平方公尺=(data_clean2$建物移轉總面積平方公尺)^2
data_clean2$sq公設比=(data_clean2$公設比)^2
data_clean2$sq移轉層次_數字=(data_clean2$移轉層次_數字)^2
data_clean2$sq總樓層次_數字=(data_clean2$總樓層次_數字)^2
data_clean2$sq豪宅=(data_clean2$豪宅)^2
lm7b.10<-lm(總價元 ~ 建物移轉總面積平方公尺+sq建物移轉總面積平方公尺 + 
              建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
              建物現況格局.廳 + 主要用途 + 公設比 + 屋齡 + 
              屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
              鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.10)#Multiple R-squared:  0.8933,	Adjusted R-squared:  0.8926 
lm7b.11<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
              建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
              建物現況格局.廳 + 主要用途 + 公設比+sq公設比 + 屋齡 + 
              屋齡2 + 移轉層次_數字 + 總樓層數_數字 + 豪宅 + 
              鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.11)#Multiple R-squared:  0.8935,	Adjusted R-squared:  0.8927
lm7b.12<-lm(總價元 ~ 建物移轉總面積平方公尺 + 
              建物型態_電梯 + 建物現況格局.房 + 建物現況格局.衛 + 
              建物現況格局.廳 + 主要用途 + 公設比 + 屋齡 + 
              屋齡2 +移轉層次_數字+ sq移轉層次_數字  + 豪宅 + 
              鄉鎮市區 + 有無管理組織, data = data_clean2)
summary(lm7b.12)#Multiple R-squared:  0.8936,	Adjusted R-squared:  0.8928 
vif(lm7b.12)
vif(lm7b)
summary(lm7a.12)
plot(residuals(lm7a.12)) 
abline(h=0)

plot(fitted(lm7a.12), 
     residuals(lm7a.12), 
     xlab="Fitted", 
     ylab="Residuals"); abline(h=0)
hist(residuals(lm7a.12))
qqnorm(residuals(lm7a.12), ylab="Residuals"); qqline(residuals(lm7a.12))
#----
#----
#電梯的係數為負不正常，可能是建物型態變數本身就有包含電梯的資訊，應該刪除一個，減少共線性。
#兩個屋齡和兩個樓層應該也有共線性，可能各要挑一個刪
#車位類別係數的基礎為"無車位"，鄉鎮市區的基礎是北投，主要用途的基礎是工業用，建物型態的基礎是公寓
#屋齡的轉折點在lm7(單價)中是88、89年，在lm7.1(總價)是70、71年。相差有點大。

#----
#電梯的係數為負不正常，可能是建物型態變數本身就有包含電梯的資訊，應該刪除一個，減少共線性。
#兩個屋齡和兩個樓層應該也有共線性，可能各要挑一個刪
#車位類別係數的基礎為"無車位"，鄉鎮市區的基礎是北投，主要用途的基礎是工業用，建物型態的基礎是公寓
#屋齡的轉折點在lm7(單價)中是88、89年，在lm7.1(總價)是70、71年。相差有點大。
