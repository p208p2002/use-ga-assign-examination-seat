# 用基因演算法分配考試座位

## 程式資訊
- 2016年末旁聽夜間部研究所基因演算法的實作
- By Philip Huang

## Fitness function設計概念
- 學生會有操性成績，操行成績越低的應該要坐前面;假設老師坐在台前監考
- 學生鄰座應該要異性;仿效梅花座
- 鄰座同學的平時成績應該要相近;就算作弊你也很難拿到高分
- 與鄰座同學的關係分數要越低越好;這樣不容易共謀作弊

## 檔案說明
- myR.r: 主程式
- rdata.csv: 學生資料
> 如有亂碼請以utf-8編碼開啟

## 初始設置
- mac請於程式70、71行修改導入資料路徑
- windows請於程式74、75行修改導入資料路徑
- 可於82行設定教室大小(必須是一個矩陣)
``` 
seatMatrix<-matrix(c(1:x),nrow=y,ncol=z,byrow=TRUE)

x = y*z 

x表示有多少學生, y表示教室座位有幾列, z表示教室座位有幾行
```
- 914行可進行一些GA的參數設置

## 程式結果
- 繪製每個世代的最高值 `$ plot(log)` 
- 此次演算結果的最高值 `$ caculateFitnessValue(seatMatrix,decode(logMaxChromosome,row,col),logMaxChromosome)`
- 顯示最終教室的排序矩陣 `$ decode(logMaxChromosome,row,col)`
