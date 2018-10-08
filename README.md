# 用基因演算法分配考試座位

## 程式資訊
- 2016年末旁聽夜間部研究所基因演算法的實作
- 2018年9月以16年版本為基礎進行重構
- By Philip Huang

## Fitness function設計概念
- 學生會有操性成績，操行成績越低的應該要坐前面;假設老師坐在台前監考
- 學生鄰座應該要異性;仿效梅花座
- 鄰座同學的平時成績應該要相近;就算作弊你也很難拿到高分
- 與鄰座同學的關係分數要越低越好;這樣不容易共謀作弊

## 檔案說明
- main.r : 主程式
- ~~paper.pdf :~~
- ~~rdata.csv : 學生資料~~
> 如有亂碼請以utf-8編碼開啟

## 設定
- line 1~37
- line 242

## 程式結果
- 此次演算結果的最高值 `$ caculateFitnessValue(logMaxChromosome)`
- 顯示最終教室的排序矩陣 `$ decode(logMaxChromosome)`
