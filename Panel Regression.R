library(AER)
library(plm)
library(stargazer)
library(panelAR)
library(dplyr)
library(Hmisc)
library(fastDummies)

data <- all_data_rensad_3
pc<-pc_7[,1:3]


df <-data.frame(pc,data)

names(df)<-c("pc1", "pc2", "pc3", "Month", "Turnover Ratio", "Zero Trading", "Bid ask",
             "LHH", "Spread Percentage", "Amihud", "IP", "UR", "INF","SDeposit", "VSTOXX","CBOE","Y10",
             "REPO","M3","OMXS_Vol","PTB","EPS","VOL","MV","ID")


df$MV=log(1+df$MV/10^6)
 
df$VOL<-Lag(df$VOL,-1)
df$MV<-Lag(df$MV,-1)
df$PTB<-Lag(df$PTB,-1)


df$Yield_Spread<- df$Y10-df$M3

df = subset(df,select = -c(Y10,M3))

# df$size<-character()
# df$size[df$MV>4e+10]<-"Large"
# df$size[df$MV<1.5e+9]<-"Small"
# df$size[1.5e+9<df$MV & df$MV<4e+10]<-"Mid"
# df$size<-as.factor(df$size)

#rownames(df) <- make.names(df$size, unique = TRUE)

contrasts(df$size)

corr<-cor(df$SDeposit,df$REPO)
corr<-cor(df$VSTOXX,df$CBOE)

dummies<-dummy_cols(df$ID,remove_first_dummy = TRUE, remove_selected_columns =TRUE)

dummy_vol<-t(df$VOL)*dummies
dummy_PTB<-t(df$PTB)*dummies
dummy_MV<-t(df$MV)*dummies

df<-data.frame(df,dummy_MV)
df<-data.frame(df,dummy_vol)
df<-data.frame(df,dummy_PTB)

sg<- df[5:24]
sg = subset(sg,select = -c(ID,EPS))
stargazer(sg)

model1 <- plm(pc1~IP+UR+VSTOXX+CBOE+INF+Yield_Spread+REPO+OMXS_Vol+SDeposit+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model1)

model2<- plm(pc1~IP+UR+VSTOXX+CBOE+Yield_Spread+REPO+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model2)

model3 <- plm(pc1~UR+VSTOXX+CBOE+Yield_Spread+REPO+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model3)

model4 <- plm(pc1~UR+VSTOXX+CBOE+Yield_Spread+REPO+VOL+size,+PTB,data = df,model="within", index=c("ID","Month"))
summary(model4)

model5 <- plm(pc1~UR+VSTOXX+CBOE+Yield_Spread+REPO+df+dummy_PTB+dummy_vol,data = df,model="within", index=c("ID","Month"))
summary(model5)

fit<-fitted(model4)
fe<-fixef(model4)

model6<-lm(pc1~UR+VSTOXX+CBOE+Yield_Spread+REPO+VOL+size,data = df)
summary(model6)





model5 <- plm(pc1~IP+UR+VSTOXX+Yield_Spread+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
              .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(model5)

fit<-fitted(model5)

par(mfrow=c(2,1))
plot(fit[1:236])
plot(df$pc1[1:236])



plot(model5$residuals)



model6 <- plm(df$`Bid ask`~UR+VSTOXX+CBOE+Yield_Spread+REPO+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model6)


model5 <- plm(df$`Turnover Ratio`~UR+VSTOXX+CBOE+Yield_Spread+REPO+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model5)

model5 <- plm(df$`Zero Trading`~UR+VSTOXX+CBOE+Yield_Spread+REPO+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model5)

plot(model5$residuals)

stargazer(model1)

model2<- plm(pc2~UR+OMXS_Vol+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model2)

model3<- plm(pc3~UR+VSTOXX+OMXS_Vol+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model3)

eps<-df$EPS



##############################################MACRO VARIABLES#######################################################################

#########################Univariate##################################

m1<-plm(pc1~IP,data=df,model="within",index=c("ID","Month"))
summary(m1)
coeftest(m1, vcov=vcovHC(m1,type="sss",cluster="group"))
woodridge1<-pwartest(m1)

plot(res)
res <- (residuals(m1))
y<-df$pc1
#Multiple R-Squared (Coefficient of Determination)
SSyy=sum((y-mean(y))**2)
SSE=sum(residuals(m1)**2)
(SSyy-SSE)/SSyy
#Alternatively
1-SSE/SSyy

m_1<-panelAR(pc1~IP,data=df,panelVar = "ID",timeVar = "Month", autoCorr="ar1",panelCorrMethod = "pcse")

m2<-plm(pc1~UR,data=df,index=c("ID","Month"))
summary(m2)
coeftest(m2, vcov=vcovHC(m2,type="sss",cluster="group"))

m3<-plm(pc1~INF,data=df,index=c("ID","Month"))
summary(m3)
coeftest(m3, vcov=vcovHC(m3,type="sss",cluster="group"))

m4<-plm(pc1~SDeposit,data=df,index=c("ID","Month"))
summary(m4)
coeftest(m4, vcov=vcovHC(m4,type="sss",cluster="group"))

m5<-plm(pc1~VSTOXX,data=df,index=c("ID","Month"))
summary(m5)
coeftest(m5, vcov=vcovHC(m5,type="sss",cluster="group"))

m6<-plm(pc1~CBOE,data=df,index=c("ID","Month"))
summary(m6)
coeftest(m6, vcov=vcovHC(m6,type="sss",cluster="group"))

m7<-plm(pc1~REPO,data=df,index=c("ID","Month"))
summary(m7)
coeftest(m7, vcov=vcovHC(m7,type="sss",cluster="group"))
woodridge7<-pwartest(m7)
# m8<-plm(pc1~OMXS_Vol,data=df,index=c("ID","Month"))
# summary(m8)

m9<-plm(pc1~Yield_Spread,data=df,index=c("ID","Month"))
summary(m9)
coeftest(m9, vcov=vcovHC(m9,type="sss",cluster="group"))

########################Bivariate#####################################

m10<-plm(pc1~REPO+VSTOXX,data=df,index=c("ID","Month"))
summary(m10)

m11<-plm(pc1~INF+REPO,data=df,index=c("ID","Month"))
summary(m11)

m12<-plm(pc1~UR+REPO,data=df,index=c("ID","Month"))
summary(m12)

m13<-plm(pc1~IP+REPO,data=df,index=c("ID","Month"))
summary(m13)

m14<-plm(pc1~VSTOXX+INF,data=df,index=c("ID","Month"))
summary(m14)

m15<-plm(pc1~VSTOXX+UR,data=df,index=c("ID","Month"))
summary(m15)

m16<-plm(pc1~IP+VSTOXX,data=df,index=c("ID","Month"))
summary(m16)

######################Multivariate#####################################

m17<-plm(pc1~REPO+VSTOXX+UR,data=df,index=c("ID","Month"))
summary(m17)

m18<-plm(pc1~REPO+VSTOXX+IP,data=df,index=c("ID","Month"))
summary(m18)

m19<-plm(pc1~REPO+VSTOXX+INF,data=df,index=c("ID","Month"))
summary(m19)

m20<-plm(pc1~IP+UR+VSTOXX+REPO,data=df,model="within",index=c("ID","Month"))
summary(m20)

m21<-plm(pc1~IP+UR+VSTOXX+REPO+CBOE,data=df,model="within",index=c("ID","Month"))
summary(m21)

partial<-pFtest(m21,m20)


####################################FIRM SPECIFIC###################################################################

f1<-plm(pc1~ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
+ .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
  .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
  .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
  .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
  .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
  .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
  .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
  .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
  .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89,data=df,model = "within",index=c("ID","Month"))

summary(f1)

f2<-plm(pc1~ + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2,data=df,model = "within",index=c("ID","Month"))

summary(f2)

f3<-plm(pc1~.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        , data = df,model="within", index=c("ID","Month"))

summary(f3)

f4<-plm(pc1~ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
+ .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
  .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
  .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
  .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
  .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
  .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
  .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
  .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
  .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89+ df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
  .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
  .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
  .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
  .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
  .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
  .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
  .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
  .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
  .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2,data=df,model = "within",index=c("ID","Month"))


summary(f4)

f5<-plm(pc1~.data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89+.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        , data = df,model="within", index=c("ID","Month"))

summary(f5)

f6<-plm(pc1~ + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
  .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
  .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
  .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
  .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
  .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
  .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
  .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
  .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
  .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2+.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
    .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
    .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
    .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
    .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
    .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
    .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
    .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
    .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
    .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
  , data = df,model="within", index=c("ID","Month"))

summary(f6)


f7<-plm(pc1~ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
   + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
     .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
     .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
     .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
     .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
     .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
     .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
     .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
     .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
   
   + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
     .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
     .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
     .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
     .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
     .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
     .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
     .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
     .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
     .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
   
   +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
     .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
     .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
     .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
     .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
     .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
     .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
     .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
     .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
     .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
   
   , data = df,model="within", index=c("ID","Month"))

summary(f7)

f8<-plm(pc1~INF+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
        
        + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
        
        +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        
        , data = df,model="within", index=c("ID","Month"))

summary(f8)

f9<-plm(pc1~UR+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
        
        + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
        
        +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        
        , data = df,model="within", index=c("ID","Month"))

summary(f9)

f10<-plm(pc1~VSTOXX+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
        
        + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
        
        +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        
        , data = df,model="within", index=c("ID","Month"))

summary(f10)

f11<-plm(pc1~IP+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
        
        + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
        
        +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        
        , data = df,model="within", index=c("ID","Month"))

summary(f11)

f12<-plm(pc1~REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
        + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
          .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
          .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
          .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
          .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
          .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
          .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
          .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
          .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
        
        + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
          .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
          .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
          .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
          .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
          .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
          .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
          .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
          .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
          .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
        
        +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
          .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
          .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
          .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
          .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
          .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
          .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
          .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
          .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
          .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
        
        , data = df,model="within", index=c("ID","Month"))

summary(f12)


##########################################MACRO + FIRM###############################################################################

mf1 <- plm(pc1~REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf1)

mf2 <- plm(pc1~VSTOXX+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf2)

mf3 <- plm(pc1~VSTOXX+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf3)

mf4 <- plm(pc1~UR+VSTOXX+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf4)

mf5 <- plm(pc1~IP+UR+VSTOXX+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,effet="individual" ,model="within", index=c("ID","Month"))
summary(mf5)

stargazer(mf5)

mf6 <- plm(pc1~IP+UR+VSTOXX+CBOE+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf6)

mf7 <- plm(pc1~CBOE+UR+VSTOXX+REPO+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
              + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
                .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
                .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
                .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
                .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
                .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
                .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
                .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
                .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
              
              + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
                .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
                .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
                .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
                .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
                .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
                .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
                .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
                .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
                .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
              
              +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
                .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
                .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
                .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
                .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
                .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
                .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
                .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
                .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
                .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
              
              , data = df,model="within", index=c("ID","Month"))
summary(mf7)

fit<-fitted(mf5)

par(mfrow=c(2,1))
plot(fit[1:236])
plot(df$pc1[1:236])

mf8 <- plm(pc1~UR+VSTOXX+REPO+INF+ .data_1 + .data_2 + .data_3 + .data_4 + .data_5 + .data_6
           + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
             .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
             .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
             .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
             .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
             .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
             .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
             .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
             .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
           
           + df$.data_1.2+ .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
             .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
             .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
             .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
             .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
             .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
             .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
             .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
             .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
             .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
           
           +.data_1.1+.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
             .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
             .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
             .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
             .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
             .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
             .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
             .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
             .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
             .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
           
           , data = df,model="within", index=c("ID","Month"))
summary(mf8)

mf8 <- plm(pc1~UR+VSTOXX+REPO+INF+ .data_2 + .data_3 + .data_4 + .data_5 + .data_6
           + .data_7 + .data_8 + .data_9 + .data_10 + .data_11 + .data_12 + .data_13 + .data_14 +
             .data_15 + .data_16 + .data_17 + .data_18 + .data_19 + .data_20 + .data_21 + .data_22+.data_23+.data_24+
             .data_25+.data_26+.data_27+.data_28+.data_29+.data_30+
             .data_31+.data_32+.data_33+.data_34+.data_35+.data_36+.data_37+.data_38+.data_39+.data_40+
             .data_41+.data_42+.data_43+.data_44+.data_45+.data_46+.data_47+.data_48+.data_49+.data_50+
             .data_51+.data_52+.data_53+.data_54+.data_55+.data_56+.data_57+.data_58+.data_59+
             .data_60+.data_61+.data_62+.data_63+.data_64+.data_65+.data_66+.data_67+.data_68+.data_69+
             .data_70+.data_71+.data_72+.data_73+.data_74+.data_75+.data_76+.data_77+.data_78+.data_79 + .data_80 +
             .data_81 + .data_82 + .data_83 + .data_84 + .data_85 + .data_86 + .data_87 + .data_88 + .data_89
           
           + .data_2.2 + .data_3.2 + .data_4.2 + .data_5.2 + 
             .data_6.2+ .data_7.2 + .data_8.2 + .data_9.2 + .data_10.2 + .data_11.2 + .data_12.2 + .data_13.2 + .data_14.2 +
             .data_15.2 + .data_16.2 + .data_17.2 + .data_18.2 + .data_19.2 + .data_20.2 + .data_21.2 + .data_22.2+.data_23.2+.data_24.2+
             .data_25.2+.data_26.2+.data_27.2+.data_28.2+.data_29.2+.data_30.2+
             .data_31.2+.data_32.2+.data_33.2+.data_34.2+.data_35.2+.data_36.2+.data_37.2+.data_38.2+.data_39.2+.data_40.2+
             .data_41.2+.data_42.2+.data_43.2+.data_44.2+.data_45.2+.data_46.2+.data_47.2+.data_48.2+.data_49.2+.data_50.2+
             .data_51.2+.data_52.2+.data_53.2+.data_54.2+.data_55.2+.data_56.2+.data_57.2+.data_58.2+.data_59.2+
             .data_60.2+.data_61.2+.data_62.2+.data_63.2+.data_64.2+.data_65.2+.data_66.2+.data_67.2+.data_68.2+.data_69.2+
             .data_70.2+.data_71.2+.data_72.2+.data_73.2+.data_74.2+.data_75.2+.data_76.2+.data_77.2+.data_78.2+.data_79.2 + .data_80.2 +
             .data_81.2 + .data_82.2 + .data_83.2 + .data_84.2 + .data_85.2 + .data_86.2 + .data_87.2 + .data_88.2 + .data_89.2
           
           +.data_2.1 + .data_3.1 + .data_4.1 + .data_5.1 + 
             .data_6.1+ .data_7.1 + .data_8.1 + .data_9.1 + .data_10.1 + .data_11.1 + .data_12.1 + .data_13.1 + .data_14.1 +
             .data_15.1 + .data_16.1 + .data_17.1 + .data_18.1 + .data_19.1 + .data_20.1 + .data_21.1 + .data_22.1+.data_23.1+.data_24.1+
             .data_25.1+.data_26.1+.data_27.1+.data_28.1+.data_29.1+.data_30.1+
             .data_31.1+.data_32.1+.data_33.1+.data_34.1+.data_35.1+.data_36.1+.data_37.1+.data_38.1+.data_39.1+.data_40.1+
             .data_41.1+.data_42.1+.data_43.1+.data_44.1+.data_45.1+.data_46.1+.data_47.1+.data_48.1+.data_49.1+.data_50.1+
             .data_51.1+.data_52.1+.data_53.1+.data_54.1+.data_55.1+.data_56.1+.data_57.1+.data_58.1+.data_59.1+
             .data_60.1+.data_61.1+.data_62.1+.data_63.1+.data_64.1+.data_65.1+.data_66.1+.data_67.1+.data_68.1+.data_69.1+
             .data_70.1+.data_71.1+.data_72.1+.data_73.1+.data_74.1+.data_75.1+.data_76.1+.data_77.1+.data_78.1+.data_79.1 + .data_80.1 +
             .data_81.1 + .data_82.1 + .data_83.1 + .data_84.1 + .data_85.1 + .data_86.1 + .data_87.1 + .data_88.1 + .data_89.1
           
           , data = df,model="within", index=c("ID","Month"))
summary(mf8)
coeftest(mf8, vcov=vcovHC(mf8,type="sss",cluster="group"))
woodridge<-pwartest(mf8)
plot(residuals(mf8))

res<-residuals(mf8)
df2 <- cbind(as.vector(res), attr(res, "index"))
names(df2) <- c("resid", "firm", "month")

plot(df2$resid,type = "l")


pred.20 <- data.frame(df[1:20791,], 
                      pred = predict(mf8, interval = "prediction"),
                      conf = predict(mf8, interval = "confidence"),
                      e = df2$resid)
elims.20 <- abs(max(pred.20$e)) * c(-1,1)

#Residuals 
ggplot(data = pred.20, aes(x = pred, y = e)) + 
  geom_point(size = 0.5) +
  geom_hline(yintercept = 0) +
  expand_limits(y = elims.20) + 
  labs(x = "Fitted value", y = "Residuals")

#QQ plot
qq20 <- ggplot(data = pred.20, aes(sample=e)) + 
  geom_qq(size = 1) + 
  geom_qq_line() + 
  labs(x = "Theoretical", y = "Sample")

hist20 <- ggplot(data = pred.20, aes(x = e)) +
  geom_histogram(bins = 100) + 
  labs(y = "Frequency", x = "Residuals")


