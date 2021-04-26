library(AER)
library(plm)
library(stargazer)
library(plm)

data <- all_data_rensad_2
pc<-pc_7[,1:3]

df <-data.frame(pc,data)

names(df)<-c("pc1", "pc2", "pc3", "Month", "Turnover Ratio", "Zero Trading", "Bid ask",
             "LHH", "Spread Percentage", "Amihud", "IP", "UR", "INF","SDeposit", "VSTOXX","CBOE","Y10",
             "REPO","M3","OMXS_Vol","PTB","EPS","VOL","MV","ID")

df = subset(df,select = -c(IP))

names(df)<-c("pc1", "pc2", "pc3", "Month", "Turnover Ratio", "Zero Trading", "Bid ask",
             "LHH", "Spread Percentage", "Amihud", "IP", "UR", "INF","SDeposit", "VSTOXX","CBOE","Y10",
             "REPO","M3","OMXS_Vol","PTB","EPS","VOL","MV","ID")

df$size<-character()
df$size[df$MV>4e+10]<-"Large"
df$size[df$MV<1.5e+9]<-"Small"
df$size[1.5e+9<df$MV & df$MV<4e+10]<-"Mid"
df$size<-as.factor(df$size)

rownames(df) <- make.names(df$size, unique = TRUE)

contrasts(df$size)

model1 <- plm(pc1~UR+VSTOXX+CBOE+Y10+REPO+M3+OMXS_Vol+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model)

model2<- plm(pc2~UR+Y10+OMXS_Vol+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model2)

model3<- plm(pc3~UR+VSTOXX+Y10+OMXS_Vol+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model3)

eps<-df$EPS

