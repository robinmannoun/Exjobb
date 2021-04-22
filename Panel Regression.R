library(AER)
library(plm)
library(stargazer)

data <- all_data_rensad
pc<-pc_4

df <-data.frame(pc,data)


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

model1<-plm(pc1~size,data=df)

model <- plm(pc1~UR+VSTOXX+CBOE+Y10+REPO+M3+OMXS_Vol+PTB+VOL+size,data = df,model="within", index=c("ID","Month"))
summary(model)
