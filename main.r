install.packages("quantmod")
library("quantmod", lib.loc="c:/Program Files/R/R-3.2.2/library")
library("quantmod")
tw2330 = getSymbols("6105.TWO", auto.assign=FALSE)
chartSeries(tw2330["2013-01::2016-01"], theme=chartTheme("white"))
addBBands()

ma_20<-runMean(tw2330[,4],n=20)
ma_60<-runMean(tw2330[,4],n=60)
#說明：過去60天的收盤平均。

addTA(ma_20,on=1,col="blue")
#說明：將20日均線(ma_20)的圖形，用藍色線條加到目前K線圖上。

addTA(ma_60,on=1,col="red")
#說明：將60日均線(ma_60)的圖形，用紅色線條加到目前K線圖上。


####
position<-Lag(ifelse(ma_20>ma_60, 1,0))
#解說：position為一個時間序列，以日為單位，如果20ma大於60ma，設值為1；否則設值為0。由於我們是日資料，訊號發生時只能隔天做交易，故將這向量全部往後遞延一天。
return<-Cl(tw2330)
return<-ROC(Cl(tw2330))*position
#解說：ROC計算：log(今天收盤價/昨天收盤價)，乘上poistion代表。若1則持有，若0則空手。

return<-return['2013-01-01/2016-01-01']
#解說：由於我們策略條件是60ma>20ma之後才會交易，故統計值從2007-03-20開始；另外APPLE在2004年有配發股利，故我們只統計2007年到2013年的資料

return<-exp(cumsum(return))
#解說：cumsum計算累計值，即將每一分量之前的值累加起來。取exp函數是要計算累計損亦。(這裡運用國中數學:log(a)+log(b)=log(ab)，exp(log(ab))=ab)

plot(return)
#解說：將累計損益圖畫出來。

#此策略的損益圖形如下，橫軸為時間軸，縱軸為報酬率，1代表原始自有資金100%。
