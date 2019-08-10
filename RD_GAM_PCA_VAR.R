library(gam)
library(vegan)
library(vars)
library(MTS)

# Read the data
DataRD <- read.csv(file="Pollutant_data.csv",sep=";",dec=",",header=TRUE)
attach(DataRD)
names(DataRD)
n<-length(Admissions)  # Sample size
Pollutants<-DataRD[,3:7]  # Pollutant data

# Descriptive statistics
summary(DataRD[,1:14])

# Plots of pollutants and response variable
par(mfrow=c(3,2))
ts.plot(CO)
ts.plot(NO2)
ts.plot(SO2)
ts.plot(PM10)
ts.plot(O3)
ts.plot(Admissions)

# ACF of pollutants
par(mfrow=c(3,2))
acf(CO)
acf(O3)
acf(SO2)
acf(NO2)
acf(PM10)

# Correlation matrix
Variables<-matrix(c(PM10,SO2,NO2,CO,O3,Tmax,Tmin,RH,Admissions), nrow = n, ncol=9,
	dimnames = list(NULL, c("PM10","SO2","NO2","CO","O3","Tmax","Tmin","RH","Admissions")))
cor(Variables,method='pearson')


############################################################################
# GAM Model

Holiday2<-Corpus+Penha
Holiday3<-Carn+Tirad+Indep
M_GAM <- gam(Admissions~CO+NO2+SO2+PM10+O3+Tuesday+Wednesday+Thursday+Friday+
		Saturday+Sunday+Holiday2+Holiday3+s(RHm12,12), family=poisson(link=log))
summary(M_GAM)

# MSE, AIC and BIC
MSE_GAM<-M_GAM$deviance/M_GAM$df.res
AIC_GAM<-M_GAM$aic
# BIC
K=length(M_GAM$coef)
LIKE_GAM <- -(M_GAM$aic - 2*K)/2
BIC_GAM <- -2*LIKE_GAM + K*log(n)
cbind(MSE_GAM,AIC_GAM,BIC_GAM)


############################################################################
# PRINCIPAL COMPONENT ANALYSIS
############################################################################

######Standardizing the pollutants#############################
Stand_Pollutants<- decostand(Pollutants, "standardize")

# Do the PCA 
pca <- prcomp(Stand_Pollutants,center=T,scale=T)
summary(pca)
eigen(cor(Stand_Pollutants))

#Retrieving the PCs
pcs <- pca$x
#Retaining only the first 3 PCs
PCs <- pcs[,1:3]
PCs <- as.data.frame(PCs)
attach(PCs)

#ACF of PCs
par(mfrow=c(2,2))
stats::acf(PC1,main="PC1")
stats::ccf(PC1, PC2, lag.max = NULL, type = c("correlation"), main="PC1 x PC2")
stats::ccf(PC1, PC3, lag.max = NULL, type = c("correlation"), main="PC1 x PC3")
stats::ccf(PC2, PC3, lag.max = NULL, type = c("correlation"), main="PC2 x PC3")



############################################################################
# GAM-PCA Model

M_GAM_PCA <- gam(Admissions~Tuesday+Wednesday+Thursday+Friday+Saturday
		+Sunday+Holiday2+Holiday3+s(RHm12,12)+PC1+PC2+PC3, 
		family=poisson(link=log))
summary(M_GAM_PCA)

# MSE, AIC and BIC
MSE_GAM_PCA<-M_GAM_PCA$deviance/M_GAM_PCA$df.res
AIC_GAM_PCA<-M_GAM_PCA$aic
# BIC
K_PCA=length(M_GAM_PCA$coef)
LIKE_GAM_PCA <- -(M_GAM_PCA$aic - 2*K_PCA)/2
BIC_GAM_PCA <- -2*LIKE_GAM_PCA + K_PCA*log(n)
cbind(MSE_GAM_PCA,AIC_GAM_PCA,BIC_GAM_PCA)

# SAR(1)(1)_7 model to the residuals of GAM_PCA
Res_GAM_PCA <- ts(M_GAM_PCA$res,start=1,frequency=1) 
M_Res<-arima(Res_GAM_PCA, order = c(7,1,0))

#ACF and PACF of residuals from GAM-PCA model
par(mfrow=c(2,1))
acf(M_Res$res)
pacf(M_Res$res)




############################################################################
# GAM-PCA-VAR Model


############################################################################
# VAR and PRINCIPAL COMPONENT ANALYSIS
############################################################################

#VAR Filtering

fitSVAR=sVARMA(Pollutants,order=c(1,0,0),sorder=c(1,0,0),s=7)
stats::acf(fitSVAR)

######Standardizing the VAR filtered pollutants
StanPolutants_VAR <- decostand(fitSVAR$res, "standardize")

# Do the PCA 
pca_VAR <- prcomp(StanPolutants_VAR,center=T,scale=T)
summary(pca_VAR)
eigen(cor(StanPolutants_VAR))

#Retrieving the PCs
pcs_VAR <- pca_VAR$x
#Retaining only the first 3 PCs
PCs_VAR <- pcs_VAR[,1:3]
PCs_VAR <- as.data.frame(PCs_VAR)
attach(PCs_VAR)

#ACF of PCs
par(mfrow=c(2,2))
stats::acf(PC1,main="PC1")
stats::ccf(PC1, PC2, lag.max = NULL, type = c("correlation"), main="PC1 x PC2")
stats::ccf(PC1, PC3, lag.max = NULL, type = c("correlation"), main="PC1 x PC3")
stats::ccf(PC2, PC3, lag.max = NULL, type = c("correlation"), main="PC2 x PC3")

KVAR<-length(PC1)
M_GAM_PCA_VAR <- gam(Admissions[1:KVAR]~Tuesday[1:KVAR]+Wednesday[1:KVAR]+Thursday[1:KVAR]+Friday[1:KVAR]+Saturday[1:KVAR]
		+Sunday[1:KVAR]+Holiday2[1:KVAR]+Holiday3[1:KVAR]+s(RHm12,12)[1:KVAR]+PC1+PC2+PC3, 
		family=poisson(link=log))

# MSE, AIC and BIC
MSE_GAM_PCA_VAR<-M_GAM_PCA_VAR$deviance/M_GAM_PCA_VAR$df.res
AIC_GAM_PCA_VAR<-M_GAM_PCA_VAR$aic
# BIC
K_PCA_VAR=length(M_GAM_PCA_VAR$coef)
LIKE_GAM_PCA_VAR <- -(M_GAM_PCA_VAR$aic - 2*K_PCA_VAR)/2
BIC_GAM_PCA_VAR <- -2*LIKE_GAM_PCA_VAR + K_PCA_VAR*log(KVAR)
cbind(MSE_GAM_PCA_VAR,AIC_GAM_PCA_VAR,BIC_GAM_PCA_VAR)

#ACF and PACF of residuals from GAM-PCA-VAR model
par(mfrow=c(2,1))
acf(M_GAM_PCA_VAR$res)
pacf(M_GAM_PCA_VAR$res)



#################################################################
# Relative risk and confidence intervals

# Interquartile variation of each polutant
   k_CO <- summary(CO)[5]-summary(CO)[2]
   k_NO2 <- summary(NO2)[5]-summary(NO2)[2]
   k_O3 <- summary(O3)[5]-summary(O3)[2]
   k_PM10 <- summary(PM10)[5]-summary(PM10)[2]
   k_SO2 <- summary(SO2)[5]-summary(SO2)[2]

# Example for the GAM model

# Relative risk
IRR_CO = exp(k_CO*M_GAM$coef["CO"])
IRR_NO2 = exp(k_NO2*M_GAM$coef["NO2"])
IRR_O3 = exp(k_O3*M_GAM$coef["O3"])
IRR_PM10 = exp(k_PM10*M_GAM$coef["PM10"])
IRR_SO2 = exp(k_SO2*M_GAM$coef["SO2"])

# 95% Asymptotic Confidence Intervals
IC.CO <- exp(k_CO*M_GAM$coef["CO"]+c(-1,+1)*qnorm(0.025)*summary(M_GAM)$coef[2,2]*k_CO)
IC.NO2 <- exp(k_NO2*M_GAM$coef["NO2"]+c(-1,+1)*qnorm(0.025)*summary(M_GAM)$coef[3,2]*k_NO2)
IC.SO2 <- exp(k_SO2*M_GAM$coef["SO2"]+c(-1,+1)*qnorm(0.025)*summary(M_GAM)$coef[4,2]*k_SO2)
IC.PM10 <- exp(k_PM10*M_GAM$coef["PM10"]+c(-1,+1)*qnorm(0.025)*summary(M_GAM)$coef[5,2]*k_PM10)
IC.O3 <- exp(k_O3*M_GAM$coef["O3"]+c(-1,+1)*qnorm(0.025)*summary(M_GAM)$coef[6,2]*k_O3)

