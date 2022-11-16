df1 = read.csv("Ex2_Data_R.csv")
str(df1)
summary(df1)

#Linear_Regression
lm.model1 <- lm(UNITS1 ~ REGPR1 + FEAT1 + RATING1, data = df1)


# Create log UNITS (not creating LOG(Price), since the values are already normalised)
df1$logUNITS1 <- log(df1$UNITS1)
df1$logUNITS2 <- log(df1$UNITS2)
df1$logUNITS3 <- log(df1$UNITS3)
df1$logREGRP1<- log(df1$REGPR1)
df1$logREGRP2<- log(df1$REGPR2)
df1$logREGRP3<- log(df1$REGPR3)


#Semi Log
lm.semi_log <- lm(logUNITS1 ~ REGPR1, data = df1)

#For App1
lm.linear_app1   <- lm(UNITS1 ~ REGPR1+FEAT1+RATING1, data = df1)
lm.semi_log_app1 <- lm(logUNITS1 ~ REGPR1+FEAT1+RATING1, data = df1)
lm.loglog_app1 <- lm(logUNITS1 ~ logREGRP1+FEAT1+RATING1, data = df1)
summary(lm.linear_app1)
summary(lm.semi_log_app1)
summary(lm.loglog_app1)

#For App2
lm.linear_app2   <- lm(UNITS2 ~ REGPR2+FEAT2+RATING2, data = df1)
lm.semi_log_app2 <- lm(logUNITS2 ~ REGPR2+FEAT2+RATING2, data = df1)
lm.loglog_app2 <- lm(logUNITS2 ~ logREGRP2+FEAT1+RATING1, data = df1)
summary(lm.linear_app2)
summary(lm.semi_log_app2)
summary(lm.loglog_app2)
#For App3
lm.linear_app3   <- lm(UNITS3 ~ REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app3 <- lm(logUNITS3 ~ REGPR3+FEAT3+RATING3, data = df1)
lm.loglog_app3 <- lm(logUNITS3 ~ logREGRP3+FEAT1+RATING1, data = df1)
summary(lm.linear_app3)
summary(lm.semi_log_app3)
summary(lm.loglog_app3)


#2

#APP1

lm.linear_app11   <- lm(UNITS1 ~ REGPR2+FEAT2+RATING2+REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app11 <- lm(logUNITS1 ~ REGPR2+FEAT2+RATING2+REGPR3+FEAT3+RATING3, data = df1)



#APP2
lm.linear_app21   <- lm(UNITS2 ~ REGPR1+FEAT1+RATING1+REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app21 <- lm(logUNITS2 ~ REGPR1+FEAT1+RATING1+REGPR3+FEAT3+RATING3, data = df1)


#APP3
lm.linear_app31   <- lm(UNITS3 ~ REGPR2+FEAT2+RATING2+REGPR1+FEAT1+RATING1, data = df1)
lm.semi_log_app31 <- lm(logUNITS3 ~ REGPR2+FEAT2+RATING2+REGPR1+FEAT1+RATING1, data = df1)


summary(lm.linear_app11)
summary(lm.semi_log_app11)
summary(lm.linear_app21)
summary(lm.semi_log_app21)
summary(lm.linear_app31)
summary(lm.semi_log_app31)

# Removing rating from marketing activity

lm.linear_app111   <- lm(UNITS1 ~ REGPR2+FEAT2+REGPR3+FEAT3, data = df1)
lm.semi_log_app111 <- lm(logUNITS1 ~ REGPR2+FEAT2+REGPR3+FEAT3, data = df1)



#APP2
lm.linear_app211   <- lm(UNITS2 ~ REGPR1+FEAT1+REGPR3+FEAT3, data = df1)
lm.semi_log_app211 <- lm(logUNITS2 ~ REGPR1+FEAT1+REGPR3+FEAT3, data = df1)


#APP3
lm.linear_app311   <- lm(UNITS3 ~ REGPR2+FEAT2+REGPR1+FEAT1, data = df1)
lm.semi_log_app311 <- lm(logUNITS3 ~ REGPR2+FEAT2+REGPR1+FEAT1, data = df1)
summary(lm.linear_app111)
summary(lm.semi_log_app111)
summary(lm.linear_app211)
summary(lm.semi_log_app211)
summary(lm.linear_app311)
summary(lm.semi_log_app311)


#3
#ACROSS APPS

#App1 vs RoA
lm.linear_app123   <- lm(UNITS1 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app123 <- lm(logUNITS1 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)

#App2 vs RoA
lm.linear_app223   <- lm(UNITS2 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app233 <- lm(logUNITS2 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)

#App3 vs RoA
lm.linear_app321   <- lm(UNITS3 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)
lm.semi_log_app322 <- lm(logUNITS3 ~ REGPR1+FEAT1+RATING1+REGPR2+FEAT2+RATING2 +REGPR3+FEAT3+RATING3, data = df1)


#RESULTS
summary(lm.model1)
summary(lm.semi_log)
summary(lm.linear_app1)
summary(lm.semi_log_app1)
summary(lm.linear_app2)
summary(lm.semi_log_app2)
summary(lm.linear_app3)
summary(lm.semi_log_app3)
summary(lm.linear_app123)
summary(lm.semi_log_app123)
summary(lm.linear_app223)
summary(lm.semi_log_app233)
summary(lm.linear_app321)
summary(lm.semi_log_app322)
