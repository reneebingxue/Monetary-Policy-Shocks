#File: Homework_8.R
#Course: Advanced Macroeconomics Theory
#Author: Renee Li
#Date: 2021-05-13


#CLEAR UP MY CONSOLE##################
cat("\014")
#PACKAFES SET UP######################
install.packages("RcppArmadillo")
install.packages("stargazer")
install.packages("knitr")
install.packages("zoo")
install.packages("outliers")
require("foreign")    # to import dta files
require("sandwich")   # for HC standard errors
require("AER")        # contains ivreg command
require("table1")     # customize summary table
require("stargazer")  # customize regression outcome
require("dplyr")
require("knitr")
require("zoo")
require("outliers")

#IMPORT THE DATA#####################
monetary <- read.csv("/Users/renee/Desktop/monetary_homework.csv")
monetary <- monetary[c(14,15,11,1, 8,2,3,4,5,6,7,9,10,12,13)]
#DATA CLEANING AREA#################
monetary$quarter = as.yearqtr(monetary$date,format="%Yq%q")
monetary$qvar = as.Date(monetary$quarter)
monetary$year = as.numeric(format(monetary$qvar, "%Y"))
monetary$month = as.numeric(format(monetary$qvar, "%m"))
monetary$quarter1 <- (monetary$month>=1)*1
monetary$quarter2 <- (monetary$month>=4)*1
monetary$quarter3 <- (monetary$month>=7)*1
monetary$quarter4 <- (monetary$month>=10)*1
monetary$quarter <- monetary$quarter1+monetary$quarter2+monetary$quarter3+monetary$quarter4
monetary <- monetary[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]
monetary$dd <- monetary$year+monetary$quarter*0.25

#CH1:INFLATION FORECAST & REAL INTEREST RATE#############
#a) Construct measure of inflation with log gdp price deflator.Anualize.Time Series Plot. 
monetary$inflation <- c(NA,diff(log(monetary$pgdp),lag=1))
monetary$inflation <- monetary$inflation*400
par(mfrow = c(1,1))
plot (monetary$dd, monetary$inflation,
      main="Forecasting Inflation",
      type="o",
      col="black",
      pch=20,
      cex=0.4,
      xlab="Time (by Quarter)",
      ylab="Inflation Rate (Annualized)")

#b)Forecast for Inflation Using Information from prior period. Plot Actual Inflation Against Predicted
#  4 Lags of inflation and unemployment
iftp1 <- lm(inflation ~ lag(inflation)+lag(inflation,2)
            +lag(inflation,3)+lag(inflation,4)
            +lag(uerate)+lag(uerate,2)
            +lag(uerate,3)+lag(uerate,4),data=monetary)
monetary$fittedift1 <- c(NA,NA,NA,NA,NA,fitted(iftp1))
points(monetary$dd,monetary$fittedift1,
       type="o",
       pch=20,
       col="blue",
       cex=0.3)
# 4 lags of inflation and output gap 
iftp2 <- lm(inflation ~ lag(inflation)+lag(inflation,2)
            +lag(inflation,3)+lag(inflation,4)
            +lag (ygap_frb)+lag(ygap_frb,2)
            +lag(ygap_frb,3)+lag(ygap_frb,4),data=monetary)
monetary$fittedift2 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,fitted(iftp2))
points(monetary$dd,monetary$fittedift2, 
       type="o", 
       pch=18, 
       col="red", 
       cex=0.3)
legend("topleft", 
       c("Inflation-Unemployment","Inflation-Output Gap"), 
       pch=c(20,18), 
       col=c("blue","red"),
       cex = 0.5)

#c)Inflation Series to Real Fed Funds Rate. (Timing!)
monetary$rffr <- monetary$ffr-lead(monetary$inflation)
plot(monetary$dd,monetary$ffr,
     main="Time Series of Federal Funds Rate and Real Federal Funds Rate",
       type="o", 
       pch=18, 
       col="black",
       xlab="Time (By Quarter)",
       ylab="FFR",
       ylim=c(-3,18),
       cex=0.5)
points(monetary$dd,monetary$rffr,
       type="o", 
       pch=18, 
       col="blue", 
       cex=0.5)
legend("topleft", 
       c("Real Fed Funds Rate","Fed Funds Rate"), 
       pch=c(18,18), 
       col=c("blue","black"),
       cex = 0.5)

#CH2:THE TYLOR RULE###################
#a)Regress ffr on inflation and ygap_frb
tylr <- lm(ffr ~ inflation + ygap_frb, data=monetary)
#b)Regress a) after 1978
tylrpost <- lm(ffr ~ inflation + ygap_frb,data=subset(monetary,dd>=1978.00))
#c)Regress a) prior to 1978
tylrpre <- lm(ffr ~ inflation + ygap_frb,data=subset(monetary,dd<1978.00))
#d1)Plot ffr against fitted pre 1978
monetarypre <- subset(monetary,dd<1978.00)
monetarypre$fittedffr1 <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,fitted(tylrpre))
par(mfrow = c(1,2))
plot(monetarypre$dd, monetarypre$ffr,
     main="Time Series Plot of Federal Funds Rate (Pre-1978)",
     type="o",
     col="black",
     pch=20,
     cex=0.4,
     xlab="Time (by Quarter)",
     ylab="FFR, Predicted FFR")
points(monetarypre$dd, monetarypre$fittedffr1,
       type="o", 
       pch=18, 
      col="blue",
      cex=0.4)
legend("topleft", 
       c("FFR","Taylor-Rule-Predicted FFR"), 
       pch=c(18,18), 
       col=c("black","blue"),
       cex = 0.3)
#d2)Plot ffr against fitted post 1978
monetaryps <- subset(monetary,dd>=1978.00)
monetaryps$fittedffr2 <- fitted(tylrpost)
plot(monetaryps$dd, monetaryps$ffr,
     main="Time Series Plot of Federal Funds Rate (Post-1978)",
     type="o",
     col="black",
     pch=20,
     cex=0.4,
     xlab="Time (by Quarter)",
     ylab="FFR, Predicted FFR")
points(monetaryps$dd, monetaryps$fittedffr2,
       type="o", 
       pch=18, 
       col="blue",
       cex=0.4)
legend("topleft", 
       c("FFR","Taylor-Rule-Predicted FFR"), 
       pch=c(18,18), 
       col=c("black","blue"),
       cex = 0.3)
#e)Do these coefficient estimates come close to the Tylor rule?
stargazer(tylr,tylrpost,tylrpre,title="Tolyor Rule Examination")

#CH3:THE EFFECT OF MONETARY POLICY ON THE REAL ECONOMY#########
#a)Construct h period changes of lgdp,lpgdp,linvest,lconsnds,lemp,uerate. 
monetary$lgdp1 <- log(monetary$gdp)-lag(log(monetary$gdp))
monetary$lgdp2 <- log(monetary$gdp)-lag(log(monetary$gdp),2)
monetary$lgdp3 <- log(monetary$gdp)-lag(log(monetary$gdp),3)
monetary$lgdp4 <- log(monetary$gdp)-lag(log(monetary$gdp),4)
monetary$lgdp5 <- log(monetary$gdp)-lag(log(monetary$gdp),5)
monetary$lgdp6 <- log(monetary$gdp)-lag(log(monetary$gdp),6)
monetary$lgdp7 <- log(monetary$gdp)-lag(log(monetary$gdp),7)
monetary$lgdp8 <- log(monetary$gdp)-lag(log(monetary$gdp),8)
monetary$lgdp9 <- log(monetary$gdp)-lag(log(monetary$gdp),9)
monetary$lgdp10 <- log(monetary$gdp)-lag(log(monetary$gdp),10)
monetary$lgdp11 <- log(monetary$gdp)-lag(log(monetary$gdp),11)
monetary$lgdp12 <- log(monetary$gdp)-lag(log(monetary$gdp),12)
#
monetary$lpgdp1 <- log(monetary$pgdp)-lag(log(monetary$pgdp))
monetary$lpgdp2 <- log(monetary$pgdp)-lag(log(monetary$pgdp),2)
monetary$lpgdp3 <- log(monetary$pgdp)-lag(log(monetary$pgdp),3)
monetary$lpgdp4 <- log(monetary$pgdp)-lag(log(monetary$pgdp),4)
monetary$lpgdp5 <- log(monetary$pgdp)-lag(log(monetary$pgdp),5)
monetary$lpgdp6 <- log(monetary$pgdp)-lag(log(monetary$pgdp),6)
monetary$lpgdp7 <- log(monetary$pgdp)-lag(log(monetary$pgdp),7)
monetary$lpgdp8 <- log(monetary$pgdp)-lag(log(monetary$pgdp),8)
monetary$lpgdp9 <- log(monetary$pgdp)-lag(log(monetary$pgdp),9)
monetary$lpgdp10 <- log(monetary$pgdp)-lag(log(monetary$pgdp),10)
monetary$lpgdp11 <- log(monetary$pgdp)-lag(log(monetary$pgdp),11)
monetary$lpgdp12 <- log(monetary$pgdp)-lag(log(monetary$pgdp),12)
#
monetary$linvest1 <- log(monetary$invest)-lag(log(monetary$invest))
monetary$linvest2 <- log(monetary$invest)-lag(log(monetary$invest),2)
monetary$linvest3 <- log(monetary$invest)-lag(log(monetary$invest),3)
monetary$linvest4 <- log(monetary$invest)-lag(log(monetary$invest),4)
monetary$linvest5 <- log(monetary$invest)-lag(log(monetary$invest),5)
monetary$linvest6 <- log(monetary$invest)-lag(log(monetary$invest),6)
monetary$linvest7 <- log(monetary$invest)-lag(log(monetary$invest),7)
monetary$linvest8 <- log(monetary$invest)-lag(log(monetary$invest),8)
monetary$linvest9 <- log(monetary$invest)-lag(log(monetary$invest),9)
monetary$linvest10 <- log(monetary$invest)-lag(log(monetary$invest),10)
monetary$linvest11 <- log(monetary$invest)-lag(log(monetary$invest),11)
monetary$linvest12 <- log(monetary$invest)-lag(log(monetary$invest),12)
#
monetary$lconsnds1 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds))
monetary$lconsnds2 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),2)
monetary$lconsnds3 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),3)
monetary$lconsnds4 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),4)
monetary$lconsnds5 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),5)
monetary$lconsnds6 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),6)
monetary$lconsnds7 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),7)
monetary$lconsnds8 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),8)
monetary$lconsnds9 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),9)
monetary$lconsnds10 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),10)
monetary$lconsnds11 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),11)
monetary$lconsnds12 <- log(monetary$cons_nds)-lag(log(monetary$cons_nds),12)
#
monetary$lemp1 <- log(monetary$empl)-lag(log(monetary$empl))
monetary$lemp2 <- log(monetary$empl)-lag(log(monetary$empl),2)
monetary$lemp3 <- log(monetary$empl)-lag(log(monetary$empl),3)
monetary$lemp4 <- log(monetary$empl)-lag(log(monetary$empl),4)
monetary$lemp5 <- log(monetary$empl)-lag(log(monetary$empl),5)
monetary$lemp6 <- log(monetary$empl)-lag(log(monetary$empl),6)
monetary$lemp7 <- log(monetary$empl)-lag(log(monetary$empl),7)
monetary$lemp8 <- log(monetary$empl)-lag(log(monetary$empl),8)
monetary$lemp9 <- log(monetary$empl)-lag(log(monetary$empl),9)
monetary$lemp10 <- log(monetary$empl)-lag(log(monetary$empl),10)
monetary$lemp11 <- log(monetary$empl)-lag(log(monetary$empl),11)
monetary$lemp12 <- log(monetary$empl)-lag(log(monetary$empl),12)
#
monetary$uerate1 <- monetary$uerate-lag(monetary$uerate)
monetary$uerate2 <- monetary$uerate-lag(monetary$uerate,2)
monetary$uerate3 <- monetary$uerate-lag(monetary$uerate,3)
monetary$uerate4 <- monetary$uerate-lag(monetary$uerate,4)
monetary$uerate5 <- monetary$uerate-lag(monetary$uerate,5)
monetary$uerate6 <- monetary$uerate-lag(monetary$uerate,6)
monetary$uerate7 <- monetary$uerate-lag(monetary$uerate,7)
monetary$uerate8 <- monetary$uerate-lag(monetary$uerate,8)
monetary$uerate9 <- monetary$uerate-lag(monetary$uerate,9)
monetary$uerate10 <- monetary$uerate-lag(monetary$uerate,10)
monetary$uerate11 <- monetary$uerate-lag(monetary$uerate,11)
monetary$uerate12 <- monetary$uerate-lag(monetary$uerate,12)

monetary$llgdp <- log(monetary$gdp)-lag(log(monetary$gdp))
monetary$llpgdp <- log(monetary$pgdp)-lag(log(monetary$pgdp))
monetary$llinvest <- log(monetary$invest)-lag(log(monetary$invest))
monetary$llconsnds <- log(monetary$cons_nds)-lag(log(monetary$cons_nds))
monetary$llemp <- log(monetary$empl)-lag(log(monetary$empl))
monetary$luerate <- monetary$uerate-lag(monetary$uerate)

#b) Plot Romer&Romer romer surprise as measure for monetary policy shocks 
par(mfrow = c(1,1))
plot(monetary$dd,monetary$romer_shock,
     main="Time Series Plot of Monetary Policy Shocks Measured by R&R",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     xlab="Time (by Quarter)",
     ylab="Romer Surprise")

#c)Estimate the output 
#x=lgdp 
#time horizon h=1 (include inflation, unemployment, and past )
lgdprg1 <- lm(lgdp1 ~ lag(llgdp)+lag(llgdp,2)+lag(llgdp,3)+lag(llgdp,4)+lag(llgdp,5)
                +lag(romer_shock)
                +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
                +lag(uerate)+lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)
              ,data=monetary)
lgdprg2 <- lm(lgdp2 ~ lag(llgdp,2)+lag(llgdp,3)+lag(llgdp,4)+lag(llgdp,5)+lag(llgdp,6)
              +lag(romer_shock,2)
              +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
              +lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)
              ,data=monetary)
lgdprg3 <- lm(lgdp3 ~ lag(llgdp,3)+lag(llgdp,4)+lag(llgdp,5)+lag(llgdp,6)+lag(llgdp,7)
              +lag(romer_shock,3)
              +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
              +lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)
              ,data=monetary)
lgdprg4 <- lm(lgdp4 ~ lag(llgdp,4)+lag(llgdp,5)+lag(llgdp,6)+lag(llgdp,7)+lag(llgdp,8)
              +lag(romer_shock,4)
              +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
              +lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)
              ,data=monetary)
lgdprg5 <- lm(lgdp5 ~ lag(llgdp,5)+lag(llgdp,6)+lag(llgdp,7)+lag(llgdp,8)+lag(llgdp,9)
              +lag(romer_shock,5)
              +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
              +lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)
              ,data=monetary)
lgdprg6 <- lm(lgdp6 ~ lag(llgdp,6)+lag(llgdp,7)+lag(llgdp,8)+lag(llgdp,9)+lag(llgdp,10)
              +lag(romer_shock,6)
              +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
              +lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)
              ,data=monetary)
lgdprg7 <- lm(lgdp7 ~ lag(llgdp,7)+lag(llgdp,8)+lag(llgdp,9)+lag(llgdp,10)+lag(llgdp,11)
              +lag(romer_shock,7)
              +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
              +lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)
              ,data=monetary)
lgdprg8 <- lm(lgdp8 ~ lag(llgdp,8)+lag(llgdp,9)+lag(llgdp,10)+lag(llgdp,11)+lag(llgdp,12)
              +lag(romer_shock,8)
              +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
              +lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)
              ,data=monetary)
lgdprg9 <- lm(lgdp9 ~ lag(llgdp,9)+lag(llgdp,10)+lag(llgdp,11)+lag(llgdp,12)+lag(llgdp,13)
              +lag(romer_shock,9)
              +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
              +lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)
              ,data=monetary)
lgdprg10 <- lm(lgdp10 ~ lag(llgdp,10)+lag(llgdp,11)+lag(llgdp,12)+lag(llgdp,13)+lag(llgdp,14)
              +lag(romer_shock,10)
              +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
              +lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)
              ,data=monetary)
lgdprg11 <- lm(lgdp11 ~ lag(llgdp,11)+lag(llgdp,12)+lag(llgdp,13)+lag(llgdp,14)+lag(llgdp,15)
               +lag(romer_shock,11)
               +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
               +lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)
               ,data=monetary)
lgdprg12 <- lm(lgdp12 ~ lag(llgdp,12)+lag(llgdp,13)+lag(llgdp,14)+lag(llgdp,15)+lag(llgdp,16)
               +lag(romer_shock,12)
               +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
               +lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)+lag(uerate,16)
               ,data=monetary)

gdpresponse1 <- summary(lgdprg1)$coefficients
gdpresponse1 <- t(gdpresponse1[7,])
gdpresponse2 <- coeftest(lgdprg2, vcov.=NeweyWest(lgdprg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse2 <- t(gdpresponse2)
gdpresponse3 <- coeftest(lgdprg3, vcov.=NeweyWest(lgdprg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse3 <- t(gdpresponse3)
gdpresponse4 <- coeftest(lgdprg4, vcov.=NeweyWest(lgdprg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse4 <- t(gdpresponse4)
gdpresponse5 <- coeftest(lgdprg5, vcov.=NeweyWest(lgdprg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse5 <- t(gdpresponse5)
gdpresponse6 <- coeftest(lgdprg6, vcov.=NeweyWest(lgdprg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse6 <- t(gdpresponse6)
gdpresponse7 <- coeftest(lgdprg7, vcov.=NeweyWest(lgdprg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse7 <- t(gdpresponse7)
gdpresponse8 <- coeftest(lgdprg8, vcov.=NeweyWest(lgdprg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse8 <- t(gdpresponse8)
gdpresponse9 <- coeftest(lgdprg9, vcov.=NeweyWest(lgdprg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse9 <- t(gdpresponse9)
gdpresponse10 <- coeftest(lgdprg10, vcov.=NeweyWest(lgdprg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse10 <- t(gdpresponse10)
gdpresponse11 <- coeftest(lgdprg11, vcov.=NeweyWest(lgdprg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse11 <- t(gdpresponse11)
gdpresponse12 <- coeftest(lgdprg12, vcov.=NeweyWest(lgdprg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
gdpresponse12 <- t(gdpresponse12)

gdpresponse <- rbind(gdpresponse1, gdpresponse2,gdpresponse3,gdpresponse4,gdpresponse5,gdpresponse6,
                     gdpresponse7,gdpresponse8,gdpresponse9,gdpresponse10,gdpresponse11,gdpresponse12)

gdpresponse <- data.frame(gdpresponse)
gdpresponse$ub <- gdpresponse$Estimate+1.64*gdpresponse$Std..Error
gdpresponse$lb <- gdpresponse$Estimate-1.64*gdpresponse$Std..Error
plot(gdpresponse$Estimate,
     main="GDP Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.01),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(gdpresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(gdpresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#GDP Deflator 
lpgdprg1 <- lm(lpgdp1 ~ lag(llpgdp)+lag(llpgdp,2)+lag(llpgdp,3)+lag(llpgdp,4)+lag(llpgdp,5)
               +lag(romer_shock)
               +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
               +lag(uerate)+lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)
               ,data=monetary)
lpgdprg2 <- lm(lpgdp2 ~ lag(llpgdp,2)+lag(llpgdp,3)+lag(llpgdp,4)+lag(llpgdp,5)+lag(llpgdp,6)
               +lag(romer_shock,2)
               +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
               +lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)
               ,data=monetary)
lpgdprg3 <- lm(lpgdp3 ~ lag(llpgdp,3)+lag(llpgdp,4)+lag(llpgdp,5)+lag(llpgdp,6)+lag(llpgdp,7)
               +lag(romer_shock,3)
               +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
               +lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)
               ,data=monetary)
lpgdprg4 <- lm(lpgdp4 ~ lag(llpgdp,4)+lag(llpgdp,5)+lag(llpgdp,6)+lag(llpgdp,7)+lag(llpgdp,8)
               +lag(romer_shock,4)
               +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
               +lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)
               ,data=monetary)
lpgdprg5 <- lm(lpgdp5 ~ lag(llpgdp,5)+lag(llpgdp,6)+lag(llpgdp,7)+lag(llpgdp,8)+lag(llpgdp,9)
               +lag(romer_shock,5)
               +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
               +lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)
               ,data=monetary)
lpgdprg6 <- lm(lpgdp6 ~ lag(llpgdp,6)+lag(llpgdp,7)+lag(llpgdp,8)+lag(llpgdp,9)+lag(llpgdp,10)
               +lag(romer_shock,6)
               +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
               +lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)
               ,data=monetary)
lpgdprg7 <- lm(lpgdp7 ~ lag(llpgdp,7)+lag(llpgdp,8)+lag(llpgdp,9)+lag(llpgdp,10)+lag(llpgdp,11)
               +lag(romer_shock,7)
               +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
               +lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)
               ,data=monetary)
lpgdprg8 <- lm(lpgdp8 ~ lag(llpgdp,8)+lag(llpgdp,9)+lag(llpgdp,10)+lag(llpgdp,11)+lag(llpgdp,12)
               +lag(romer_shock,8)
               +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
               +lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)
               ,data=monetary)
lpgdprg9 <- lm(lpgdp9 ~ lag(llpgdp,9)+lag(llpgdp,10)+lag(llpgdp,11)+lag(llpgdp,12)+lag(llpgdp,13)
               +lag(romer_shock,9)
               +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
               +lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)
               ,data=monetary)
lpgdprg10 <- lm(lpgdp10 ~ lag(llpgdp,10)+lag(llpgdp,11)+lag(llpgdp,12)+lag(llpgdp,13)+lag(llpgdp,14)
                +lag(romer_shock,10)
                +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
                +lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)
                ,data=monetary)
lpgdprg11 <- lm(lpgdp11 ~ lag(llpgdp,11)+lag(llpgdp,12)+lag(llpgdp,13)+lag(llpgdp,14)+lag(llpgdp,15)
                +lag(romer_shock,11)
                +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
                +lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)
                ,data=monetary)
lpgdprg12 <- lm(lpgdp12 ~ lag(llpgdp,12)+lag(llpgdp,13)+lag(llpgdp,14)+lag(llpgdp,15)+lag(llpgdp,16)
                +lag(romer_shock,12)
                +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
                +lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)+lag(uerate,16)
                ,data=monetary)

pgdpresponse1 <- summary(lpgdprg1)$coefficients
pgdpresponse1 <- t(pgdpresponse1[7,])
pgdpresponse2 <- coeftest(lpgdprg2, vcov.=NeweyWest(lpgdprg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse2 <- t(pgdpresponse2)
pgdpresponse3 <- coeftest(lpgdprg3, vcov.=NeweyWest(lpgdprg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse3 <- t(pgdpresponse3)
pgdpresponse4 <- coeftest(lpgdprg4, vcov.=NeweyWest(lpgdprg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse4 <- t(pgdpresponse4)
pgdpresponse5 <- coeftest(lpgdprg5, vcov.=NeweyWest(lpgdprg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse5 <- t(pgdpresponse5)
pgdpresponse6 <- coeftest(lpgdprg6, vcov.=NeweyWest(lpgdprg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse6 <- t(pgdpresponse6)
pgdpresponse7 <- coeftest(lpgdprg7, vcov.=NeweyWest(lpgdprg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse7 <- t(pgdpresponse7)
pgdpresponse8 <- coeftest(lpgdprg8, vcov.=NeweyWest(lpgdprg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse8 <- t(pgdpresponse8)
pgdpresponse9 <- coeftest(lpgdprg9, vcov.=NeweyWest(lpgdprg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse9 <- t(pgdpresponse9)
pgdpresponse10 <- coeftest(lpgdprg10, vcov.=NeweyWest(lpgdprg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse10 <- t(pgdpresponse10)
pgdpresponse11 <- coeftest(lpgdprg11, vcov.=NeweyWest(lpgdprg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse11 <- t(pgdpresponse11)
pgdpresponse12 <- coeftest(lpgdprg12, vcov.=NeweyWest(lpgdprg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
pgdpresponse12 <- t(pgdpresponse12)

pgdpresponse <- rbind(pgdpresponse1, pgdpresponse2,pgdpresponse3,pgdpresponse4,pgdpresponse5,pgdpresponse6,
                      pgdpresponse7,pgdpresponse8,pgdpresponse9,pgdpresponse10,pgdpresponse11,pgdpresponse12)

pgdpresponse <- data.frame(pgdpresponse)
pgdpresponse$ub <- pgdpresponse$Estimate+1.64*pgdpresponse$Std..Error
pgdpresponse$lb <- pgdpresponse$Estimate-1.64*pgdpresponse$Std..Error


plot(pgdpresponse$Estimate,
     main="PGDP Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.01),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(pgdpresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(pgdpresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#Investment 
linvestrg1 <- lm(linvest1 ~ lag(llinvest)+lag(llinvest,2)+lag(llinvest,3)+lag(llinvest,4)+lag(llinvest,5)
                 +lag(romer_shock)
                 +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
                 +lag(uerate)+lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)
                 ,data=monetary)
linvestrg2 <- lm(linvest2 ~ lag(llinvest,2)+lag(llinvest,3)+lag(llinvest,4)+lag(llinvest,5)+lag(llinvest,6)
                 +lag(romer_shock,2)
                 +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
                 +lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)
                 ,data=monetary)
linvestrg3 <- lm(linvest3 ~ lag(llinvest,3)+lag(llinvest,4)+lag(llinvest,5)+lag(llinvest,6)+lag(llinvest,7)
                 +lag(romer_shock,3)
                 +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
                 +lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)
                 ,data=monetary)
linvestrg4 <- lm(linvest4 ~ lag(llinvest,4)+lag(llinvest,5)+lag(llinvest,6)+lag(llinvest,7)+lag(llinvest,8)
                 +lag(romer_shock,4)
                 +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
                 +lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)
                 ,data=monetary)
linvestrg5 <- lm(linvest5 ~ lag(llinvest,5)+lag(llinvest,6)+lag(llinvest,7)+lag(llinvest,8)+lag(llinvest,9)
                 +lag(romer_shock,5)
                 +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
                 +lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)
                 ,data=monetary)
linvestrg6 <- lm(linvest6 ~ lag(llinvest,6)+lag(llinvest,7)+lag(llinvest,8)+lag(llinvest,9)+lag(llinvest,10)
                 +lag(romer_shock,6)
                 +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
                 +lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)
                 ,data=monetary)
linvestrg7 <- lm(linvest7 ~ lag(llinvest,7)+lag(llinvest,8)+lag(llinvest,9)+lag(llinvest,10)+lag(llinvest,11)
                 +lag(romer_shock,7)
                 +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
                 +lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)
                 ,data=monetary)
linvestrg8 <- lm(linvest8 ~ lag(llinvest,8)+lag(llinvest,9)+lag(llinvest,10)+lag(llinvest,11)+lag(llinvest,12)
                 +lag(romer_shock,8)
                 +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
                 +lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)
                 ,data=monetary)
linvestrg9 <- lm(linvest9 ~ lag(llinvest,9)+lag(llinvest,10)+lag(llinvest,11)+lag(llinvest,12)+lag(llinvest,13)
                 +lag(romer_shock,9)
                 +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
                 +lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)
                 ,data=monetary)
linvestrg10 <- lm(linvest10 ~ lag(llinvest,10)+lag(llinvest,11)+lag(llinvest,12)+lag(llinvest,13)+lag(llinvest,14)
                  +lag(romer_shock,10)
                  +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
                  +lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)
                  ,data=monetary)
linvestrg11 <- lm(linvest11 ~ lag(llinvest,11)+lag(llinvest,12)+lag(llinvest,13)+lag(llinvest,14)+lag(llinvest,15)
                  +lag(romer_shock,11)
                  +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
                  +lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)
                  ,data=monetary)
linvestrg12 <- lm(linvest12 ~ lag(llinvest,12)+lag(llinvest,13)+lag(llinvest,14)+lag(llinvest,15)+lag(llinvest,16)
                  +lag(romer_shock,12)
                  +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
                  +lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)+lag(uerate,16)
                  ,data=monetary)

investresponse1 <- summary(linvestrg1)$coefficients
investresponse1 <- t(investresponse1[7,])
investresponse2 <- coeftest(linvestrg2, vcov.=NeweyWest(linvestrg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse2 <- t(investresponse2)
investresponse3 <- coeftest(linvestrg3, vcov.=NeweyWest(linvestrg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse3 <- t(investresponse3)
investresponse4 <- coeftest(linvestrg4, vcov.=NeweyWest(linvestrg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse4 <- t(investresponse4)
investresponse5 <- coeftest(linvestrg5, vcov.=NeweyWest(linvestrg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse5 <- t(investresponse5)
investresponse6 <- coeftest(linvestrg6, vcov.=NeweyWest(linvestrg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse6 <- t(investresponse6)
investresponse7 <- coeftest(linvestrg7, vcov.=NeweyWest(linvestrg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse7 <- t(investresponse7)
investresponse8 <- coeftest(linvestrg8, vcov.=NeweyWest(linvestrg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse8 <- t(investresponse8)
investresponse9 <- coeftest(linvestrg9, vcov.=NeweyWest(linvestrg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse9 <- t(investresponse9)
investresponse10 <- coeftest(linvestrg10, vcov.=NeweyWest(linvestrg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse10 <- t(investresponse10)
investresponse11 <- coeftest(linvestrg11, vcov.=NeweyWest(linvestrg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse11 <- t(investresponse11)
investresponse12 <- coeftest(linvestrg12, vcov.=NeweyWest(linvestrg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
investresponse12 <- t(investresponse12)

investresponse <- rbind(investresponse1, investresponse2,investresponse3,investresponse4,investresponse5,investresponse6,
                        investresponse7,investresponse8,investresponse9,investresponse10,investresponse11,investresponse12)

investresponse <- data.frame(investresponse)
investresponse$ub <- investresponse$Estimate+1.64*investresponse$Std..Error
investresponse$lb <- investresponse$Estimate-1.64*investresponse$Std..Error


plot(investresponse$Estimate,
     main="Investment Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.08,0.08),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(investresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(investresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#consumption expenditure for non durable goods
lconsndsrg1 <- lm(lconsnds1 ~ lag(llconsnds)+lag(llconsnds,2)+lag(llconsnds,3)+lag(llconsnds,4)+lag(llconsnds,5)
                  +lag(romer_shock)
                  +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
                  +lag(uerate)+lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)
                  ,data=monetary)
lconsndsrg2 <- lm(lconsnds2 ~ lag(llconsnds,2)+lag(llconsnds,3)+lag(llconsnds,4)+lag(llconsnds,5)+lag(llconsnds,6)
                  +lag(romer_shock,2)
                  +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
                  +lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)
                  ,data=monetary)
lconsndsrg3 <- lm(lconsnds3 ~ lag(llconsnds,3)+lag(llconsnds,4)+lag(llconsnds,5)+lag(llconsnds,6)+lag(llconsnds,7)
                  +lag(romer_shock,3)
                  +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
                  +lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)
                  ,data=monetary)
lconsndsrg4 <- lm(lconsnds4 ~ lag(llconsnds,4)+lag(llconsnds,5)+lag(llconsnds,6)+lag(llconsnds,7)+lag(llconsnds,8)
                  +lag(romer_shock,4)
                  +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
                  +lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)
                  ,data=monetary)
lconsndsrg5 <- lm(lconsnds5 ~ lag(llconsnds,5)+lag(llconsnds,6)+lag(llconsnds,7)+lag(llconsnds,8)+lag(llconsnds,9)
                  +lag(romer_shock,5)
                  +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
                  +lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)
                  ,data=monetary)
lconsndsrg6 <- lm(lconsnds6 ~ lag(llconsnds,6)+lag(llconsnds,7)+lag(llconsnds,8)+lag(llconsnds,9)+lag(llconsnds,10)
                  +lag(romer_shock,6)
                  +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
                  +lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)
                  ,data=monetary)
lconsndsrg7 <- lm(lconsnds7 ~ lag(llconsnds,7)+lag(llconsnds,8)+lag(llconsnds,9)+lag(llconsnds,10)+lag(llconsnds,11)
                  +lag(romer_shock,7)
                  +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
                  +lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)
                  ,data=monetary)
lconsndsrg8 <- lm(lconsnds8 ~ lag(llconsnds,8)+lag(llconsnds,9)+lag(llconsnds,10)+lag(llconsnds,11)+lag(llconsnds,12)
                  +lag(romer_shock,8)
                  +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
                  +lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)
                  ,data=monetary)
lconsndsrg9 <- lm(lconsnds9 ~ lag(llconsnds,9)+lag(llconsnds,10)+lag(llconsnds,11)+lag(llconsnds,12)+lag(llconsnds,13)
                  +lag(romer_shock,9)
                  +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
                  +lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)
                  ,data=monetary)
lconsndsrg10 <- lm(lconsnds10 ~ lag(llconsnds,10)+lag(llconsnds,11)+lag(llconsnds,12)+lag(llconsnds,13)+lag(llconsnds,14)
                   +lag(romer_shock,10)
                   +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
                   +lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)
                   ,data=monetary)
lconsndsrg11 <- lm(lconsnds11 ~ lag(llconsnds,11)+lag(llconsnds,12)+lag(llconsnds,13)+lag(llconsnds,14)+lag(llconsnds,15)
                   +lag(romer_shock,11)
                   +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
                   +lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)
                   ,data=monetary)
lconsndsrg12 <- lm(lconsnds12 ~ lag(llconsnds,12)+lag(llconsnds,13)+lag(llconsnds,14)+lag(llconsnds,15)+lag(llconsnds,16)
                   +lag(romer_shock,12)
                   +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
                   +lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)+lag(uerate,16)
                   ,data=monetary)

consndsresponse1 <- summary(lconsndsrg1)$coefficients
consndsresponse1 <- t(consndsresponse1[7,])
consndsresponse2 <- coeftest(lconsndsrg2, vcov.=NeweyWest(lconsndsrg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse2 <- t(consndsresponse2)
consndsresponse3 <- coeftest(lconsndsrg3, vcov.=NeweyWest(lconsndsrg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse3 <- t(consndsresponse3)
consndsresponse4 <- coeftest(lconsndsrg4, vcov.=NeweyWest(lconsndsrg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse4 <- t(consndsresponse4)
consndsresponse5 <- coeftest(lconsndsrg5, vcov.=NeweyWest(lconsndsrg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse5 <- t(consndsresponse5)
consndsresponse6 <- coeftest(lconsndsrg6, vcov.=NeweyWest(lconsndsrg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse6 <- t(consndsresponse6)
consndsresponse7 <- coeftest(lconsndsrg7, vcov.=NeweyWest(lconsndsrg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse7 <- t(consndsresponse7)
consndsresponse8 <- coeftest(lconsndsrg8, vcov.=NeweyWest(lconsndsrg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse8 <- t(consndsresponse8)
consndsresponse9 <- coeftest(lconsndsrg9, vcov.=NeweyWest(lconsndsrg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse9 <- t(consndsresponse9)
consndsresponse10 <- coeftest(lconsndsrg10, vcov.=NeweyWest(lconsndsrg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse10 <- t(consndsresponse10)
consndsresponse11 <- coeftest(lconsndsrg11, vcov.=NeweyWest(lconsndsrg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse11 <- t(consndsresponse11)
consndsresponse12 <- coeftest(lconsndsrg12, vcov.=NeweyWest(lconsndsrg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
consndsresponse12 <- t(consndsresponse12)

consndsresponse <- rbind(consndsresponse1, consndsresponse2,consndsresponse3,consndsresponse4,consndsresponse5,consndsresponse6,
                         consndsresponse7,consndsresponse8,consndsresponse9,consndsresponse10,consndsresponse11,consndsresponse12)

consndsresponse <- data.frame(consndsresponse)
consndsresponse$ub <- consndsresponse$Estimate+1.64*consndsresponse$Std..Error
consndsresponse$lb <- consndsresponse$Estimate-1.64*consndsresponse$Std..Error


plot(consndsresponse$Estimate,
     main="consndsment Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.01,0.005),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(consndsresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(consndsresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#Employment 
lemprg1 <- lm(lemp1 ~ lag(llemp)+lag(llemp,2)+lag(llemp,3)+lag(llemp,4)+lag(llemp,5)
              +lag(romer_shock)
              +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
              +lag(uerate)+lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)
              ,data=monetary)
lemprg2 <- lm(lemp2 ~ lag(llemp,2)+lag(llemp,3)+lag(llemp,4)+lag(llemp,5)+lag(llemp,6)
              +lag(romer_shock,2)
              +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
              +lag(uerate,2)+lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)
              ,data=monetary)
lemprg3 <- lm(lemp3 ~ lag(llemp,3)+lag(llemp,4)+lag(llemp,5)+lag(llemp,6)+lag(llemp,7)
              +lag(romer_shock,3)
              +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
              +lag(uerate,3)+lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)
              ,data=monetary)
lemprg4 <- lm(lemp4 ~ lag(llemp,4)+lag(llemp,5)+lag(llemp,6)+lag(llemp,7)+lag(llemp,8)
              +lag(romer_shock,4)
              +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
              +lag(uerate,4)+lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)
              ,data=monetary)
lemprg5 <- lm(lemp5 ~ lag(llemp,5)+lag(llemp,6)+lag(llemp,7)+lag(llemp,8)+lag(llemp,9)
              +lag(romer_shock,5)
              +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
              +lag(uerate,5)+lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)
              ,data=monetary)
lemprg6 <- lm(lemp6 ~ lag(llemp,6)+lag(llemp,7)+lag(llemp,8)+lag(llemp,9)+lag(llemp,10)
              +lag(romer_shock,6)
              +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
              +lag(uerate,6)+lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)
              ,data=monetary)
lemprg7 <- lm(lemp7 ~ lag(llemp,7)+lag(llemp,8)+lag(llemp,9)+lag(llemp,10)+lag(llemp,11)
              +lag(romer_shock,7)
              +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
              +lag(uerate,7)+lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)
              ,data=monetary)
lemprg8 <- lm(lemp8 ~ lag(llemp,8)+lag(llemp,9)+lag(llemp,10)+lag(llemp,11)+lag(llemp,12)
              +lag(romer_shock,8)
              +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
              +lag(uerate,8)+lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)
              ,data=monetary)
lemprg9 <- lm(lemp9 ~ lag(llemp,9)+lag(llemp,10)+lag(llemp,11)+lag(llemp,12)+lag(llemp,13)
              +lag(romer_shock,9)
              +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
              +lag(uerate,9)+lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)
              ,data=monetary)
lemprg10 <- lm(lemp10 ~ lag(llemp,10)+lag(llemp,11)+lag(llemp,12)+lag(llemp,13)+lag(llemp,14)
               +lag(romer_shock,10)
               +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
               +lag(uerate,10)+lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)
               ,data=monetary)
lemprg11 <- lm(lemp11 ~ lag(llemp,11)+lag(llemp,12)+lag(llemp,13)+lag(llemp,14)+lag(llemp,15)
               +lag(romer_shock,11)
               +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
               +lag(uerate,11)+lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)
               ,data=monetary)
lemprg12 <- lm(lemp12 ~ lag(llemp,12)+lag(llemp,13)+lag(llemp,14)+lag(llemp,15)+lag(llemp,16)
               +lag(romer_shock,12)
               +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
               +lag(uerate,12)+lag(uerate,13)+lag(uerate,14)+lag(uerate,15)+lag(uerate,16)
               ,data=monetary)

lempresponse1 <- summary(lemprg1)$coefficients
lempresponse1 <- t(lempresponse1[7,])
lempresponse2 <- coeftest(lemprg2, vcov.=NeweyWest(lemprg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse2 <- t(lempresponse2)
lempresponse3 <- coeftest(lemprg3, vcov.=NeweyWest(lemprg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse3 <- t(lempresponse3)
lempresponse4 <- coeftest(lemprg4, vcov.=NeweyWest(lemprg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse4 <- t(lempresponse4)
lempresponse5 <- coeftest(lemprg5, vcov.=NeweyWest(lemprg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse5 <- t(lempresponse5)
lempresponse6 <- coeftest(lemprg6, vcov.=NeweyWest(lemprg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse6 <- t(lempresponse6)
lempresponse7 <- coeftest(lemprg7, vcov.=NeweyWest(lemprg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse7 <- t(lempresponse7)
lempresponse8 <- coeftest(lemprg8, vcov.=NeweyWest(lemprg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse8 <- t(lempresponse8)
lempresponse9 <- coeftest(lemprg9, vcov.=NeweyWest(lemprg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse9 <- t(lempresponse9)
lempresponse10 <- coeftest(lemprg10, vcov.=NeweyWest(lemprg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse10 <- t(lempresponse10)
lempresponse11 <- coeftest(lemprg11, vcov.=NeweyWest(lemprg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse11 <- t(lempresponse11)
lempresponse12 <- coeftest(lemprg12, vcov.=NeweyWest(lemprg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
lempresponse12 <- t(lempresponse12)

lempresponse <- rbind(lempresponse1, lempresponse2,lempresponse3,lempresponse4,lempresponse5,lempresponse6,
                      lempresponse7,lempresponse8,lempresponse9,lempresponse10,lempresponse11,lempresponse12)

lempresponse <- data.frame(lempresponse)
lempresponse$ub <- lempresponse$Estimate+1.64*lempresponse$Std..Error
lempresponse$lb <- lempresponse$Estimate-1.64*lempresponse$Std..Error

plot(lempresponse$Estimate,
     main="Employment Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.005),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(lempresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(lempresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#Unemployment
ueraterg1 <- lm(uerate1 ~ lag(luerate)+lag(luerate,2)+lag(luerate,3)+lag(luerate,4)+lag(luerate,5)
                +lag(romer_shock)
                +lag(inflation)+lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)
                ,data=monetary)
ueraterg2 <- lm(uerate2 ~ lag(luerate,2)+lag(luerate,3)+lag(luerate,4)+lag(luerate,5)+lag(luerate,6)
                +lag(romer_shock,2)
                +lag(inflation,2)+lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)
                ,data=monetary)
ueraterg3 <- lm(uerate3 ~ lag(luerate,3)+lag(luerate,4)+lag(luerate,5)+lag(luerate,6)+lag(luerate,7)
                +lag(romer_shock,3)
                +lag(inflation,3)+lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)
                ,data=monetary)
ueraterg4 <- lm(uerate4 ~ lag(luerate,4)+lag(luerate,5)+lag(luerate,6)+lag(luerate,7)+lag(luerate,8)
                +lag(romer_shock,4)
                +lag(inflation,4)+lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)
                ,data=monetary)
ueraterg5 <- lm(uerate5 ~ lag(luerate,5)+lag(luerate,6)+lag(luerate,7)+lag(luerate,8)+lag(luerate,9)
                +lag(romer_shock,5)
                +lag(inflation,5)+lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)
                ,data=monetary)
ueraterg6 <- lm(uerate6 ~ lag(luerate,6)+lag(luerate,7)+lag(luerate,8)+lag(luerate,9)+lag(luerate,10)
                +lag(romer_shock,6)
                +lag(inflation,6)+lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)
                ,data=monetary)
ueraterg7 <- lm(uerate7 ~ lag(luerate,7)+lag(luerate,8)+lag(luerate,9)+lag(luerate,10)+lag(luerate,11)
                +lag(romer_shock,7)
                +lag(inflation,7)+lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)
                ,data=monetary)
ueraterg8 <- lm(uerate8 ~ lag(luerate,8)+lag(luerate,9)+lag(luerate,10)+lag(luerate,11)+lag(luerate,12)
                +lag(romer_shock,8)
                +lag(inflation,8)+lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)
                ,data=monetary)
ueraterg9 <- lm(uerate9 ~ lag(luerate,9)+lag(luerate,10)+lag(luerate,11)+lag(luerate,12)+lag(luerate,13)
                +lag(romer_shock,9)
                +lag(inflation,9)+lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)
                ,data=monetary)
ueraterg10 <- lm(uerate10 ~ lag(luerate,10)+lag(luerate,11)+lag(luerate,12)+lag(luerate,13)+lag(luerate,14)
                 +lag(romer_shock,10)
                 +lag(inflation,10)+lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)
                 ,data=monetary)
ueraterg11 <- lm(uerate11 ~ lag(luerate,11)+lag(luerate,12)+lag(luerate,13)+lag(luerate,14)+lag(luerate,15)
                 +lag(romer_shock,11)
                 +lag(inflation,11)+lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)
                 ,data=monetary)
ueraterg12 <- lm(uerate12 ~ lag(luerate,12)+lag(luerate,13)+lag(luerate,14)+lag(luerate,15)+lag(luerate,16)
                 +lag(romer_shock,12)
                 +lag(inflation,12)+lag(inflation,13)+lag(inflation,14)+lag(inflation,15)+lag(inflation,16)
                 ,data=monetary)

uerateresponse1 <- summary(ueraterg1)$coefficients
uerateresponse1 <- t(uerateresponse1[7,])
uerateresponse2 <- coeftest(ueraterg2, vcov.=NeweyWest(ueraterg2, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse2 <- t(uerateresponse2)
uerateresponse3 <- coeftest(ueraterg3, vcov.=NeweyWest(ueraterg3, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse3 <- t(uerateresponse3)
uerateresponse4 <- coeftest(ueraterg4, vcov.=NeweyWest(ueraterg4, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse4 <- t(uerateresponse4)
uerateresponse5 <- coeftest(ueraterg5, vcov.=NeweyWest(ueraterg5, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse5 <- t(uerateresponse5)
uerateresponse6 <- coeftest(ueraterg6, vcov.=NeweyWest(ueraterg6, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse6 <- t(uerateresponse6)
uerateresponse7 <- coeftest(ueraterg7, vcov.=NeweyWest(ueraterg7, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse7 <- t(uerateresponse7)
uerateresponse8 <- coeftest(ueraterg8, vcov.=NeweyWest(ueraterg8, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse8 <- t(uerateresponse8)
uerateresponse9 <- coeftest(ueraterg9, vcov.=NeweyWest(ueraterg9, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse9 <- t(uerateresponse9)
uerateresponse10 <- coeftest(ueraterg10, vcov.=NeweyWest(ueraterg10, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse10 <- t(uerateresponse10)
uerateresponse11 <- coeftest(ueraterg11, vcov.=NeweyWest(ueraterg11, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse11 <- t(uerateresponse11)
uerateresponse12 <- coeftest(ueraterg12, vcov.=NeweyWest(ueraterg12, lag=4, adjust=TRUE, verbose=TRUE))[7,]
uerateresponse12 <- t(uerateresponse12)

uerateresponse <- rbind(uerateresponse1, uerateresponse2,uerateresponse3,uerateresponse4,uerateresponse5,uerateresponse6,
                        uerateresponse7,uerateresponse8,uerateresponse9,uerateresponse10,uerateresponse11,uerateresponse12)

uerateresponse <- data.frame(uerateresponse)
uerateresponse$ub <- uerateresponse$Estimate+1.64*uerateresponse$Std..Error
uerateresponse$lb <- uerateresponse$Estimate-1.64*uerateresponse$Std..Error

plot(uerateresponse$Estimate,
     main="Unemployment Rate Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.5,0.55),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(uerateresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(uerateresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)

#Plot Together
par(mfrow = c(3,2))
plot(gdpresponse$Estimate,
     main="GDP Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.01),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(gdpresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(gdpresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
plot(pgdpresponse$Estimate,
     main="PGDP Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.01),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(pgdpresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(pgdpresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
plot(investresponse$Estimate,
     main="Investment Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.08,0.08),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(investresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(investresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
plot(consndsresponse$Estimate,
     main="Consumer Expenditure (ND) Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.01,0.005),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(consndsresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(consndsresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
plot(lempresponse$Estimate,
     main="Employment Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.02,0.005),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(lempresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(lempresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
plot(uerateresponse$Estimate,
     main="Unemployment Rate Response to R&R measured Monetary Policy Shock",
     type="o",
     col="black",
     pch=20,
     cex=0.3,
     ylim=c(-0.5,0.55),
     xlab="Time (by Quarter)",
     ylab="Percentage Response")
abline(h=0)
points(uerateresponse$ub,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)
points(uerateresponse$lb,
       type="o",
       col="blue",
       pch=20,
       cex=0.3)









