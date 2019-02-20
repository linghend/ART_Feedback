rm(ls=list())

library(tidyverse)
#library(readxl)

ART_FB=read_xlsx("DataSet\\RawData.xlsx",sheet="RawData")
ART_FB %>% group_by(TimeStamp) %>% summarize(avg_RiskProb = mean(RiskProb))
Rec_Cnt<-ART_FB %>% group_by(TimeStamp) %>% summarize(RecCnt = length(unique(Cnno)))
Int_Cnt<-ART_FB %>% group_by(TimeStamp) %>% filter((!is.na(CAR_Interaction_ID))) %>% summarize(IntCnt = length(unique(Cnno)))

Uti=merge(Rec_Cnt,Int_Cnt)
Uti$Ratio=Uti$IntCnt*1.0/Uti$RecCnt                                          
