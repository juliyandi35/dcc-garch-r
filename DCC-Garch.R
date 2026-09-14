# Import Data Keseluruhan
library(readxl)
Data <- read_excel("mgarch.xlsx")
Data <- data.frame(Data)
names(Data)

## Analisis Deskriptif
# Tabel Deskriptif
library(e1071)
Descriptive_tabel <- data.frame(Variabel = names(Data[,-1]),
                                Mean = sapply(Data[,2:10],mean),
                                Skewness = sapply(Data[,2:10],skewness),
                                Kurtosis = sapply(Data[,2:10],kurtosis))
rownames(Descriptive_tabel) <- NULL
writexl::write_xlsx(Descriptive_tabel,"Descriptive Table.xlsx")

# Plot Distribusi
plot(density(Data$Indo),main = "Indonesia Data Distribution")
plot(density(Data$Malaysia),main = "Malaysia Data Distribution")
plot(density(Data$Sgd),main = "Sgd Data Distribution")
plot(density(Data$Laos),main = "Laos Data Distribution")
plot(density(Data$Filipina),main = "Filipina Data Distribution")
plot(density(Data$Thai),main = "Thailand Data Distribution")
plot(density(Data$Vietnam),main = "Vietnam Data Distribution")
plot(density(Data$Kamboja),main = "Kamboja Data Distribution")
plot(density(Data$GEPU),main = "GEPU Data Distribution")

## Memeriksa Stationeritas
# Uji ADF
# Sebuah data dikatakan stationer jika punya p-value < 0.05
library(tseries)
adf.test(Data$Indo) # Stationer
adf.test(Data$Malaysia) # Stationer
adf.test(Data$Sgd) # Stationer
adf.test(Data$Laos) # Stationer
adf.test(Data$Filipina) # Stationer
adf.test(Data$Thai) # Stationer
adf.test(Data$Vietnam) # Stationer
adf.test(Data$Kamboja) # Stationer
adf.test(Data$GEPU) # Tidak stationer

# Differencing
rIndo <- diff(Data$Indo)
rMalaysia <- diff(Data$Malaysia)
rSgd <- diff(Data$Sgd)
rLaos <- diff(Data$Laos)
rFilipina <- diff(Data$Filipina)
rThai <- diff(Data$Thai)
rVietnam <- diff(Data$Vietnam)
rKamboja <- diff(Data$Kamboja)
rGEPU <- diff(Data$GEPU)

Data_Stat <- data.frame(Indo = rIndo,Malaysia = rMalaysia,
                        Sgd = rSgd,Laos = rLaos,
                        Filipina = rFilipina,Thai = rThai,
                        Vietnam = rVietnam,Kamboja = rKamboja,
                        GEPU = rGEPU)

# Memeriksa Heteroskedastisitas
library(lmtest)
bptest(lm(GEPU ~., data = Data_Stat)) # Residu model OLS Homogen, jadi tidak ada heteroskedastistas

# Model DCC-Garch
library(rmgarch)
library(rugarch)
model <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                    distribution.model = "norm")
modelspec <- dccspec(uspec = multispec(replicate(8,model)),
                     dccOrder = c(1,1), distribution = "mvnorm")
modelfit <- dccfit(modelspec,data = Data_Stat[,-9])
modelfit

# Pre-Pandemic
library(readxl)
Data_Pre <- subset(Data,(Monthly >= "2018-03-01" & Monthly <= "2020-04-1"))

## Analisis Deskriptif
# Tabel Deskriptif
library(e1071)
Descriptive_tabel <- data.frame(Variabel = names(Data_Pre[,-1]),
                                Mean = sapply(Data_Pre[,2:10],mean),
                                Skewness = sapply(Data_Pre[,2:10],skewness),
                                Kurtosis = sapply(Data_Pre[,2:10],kurtosis))
rownames(Descriptive_tabel) <- NULL
writexl::write_xlsx(Descriptive_tabel,"Descriptive Table Pre-Pandemic.xlsx")

# Plot Distribusi
plot(density(Data_Pre$Indo),main = "Indonesia Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Malaysia),main = "Malaysia Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Sgd),main = "Sgd Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Laos),main = "Laos Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Filipina),main = "Filipina Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Thai),main = "Thailand Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Vietnam),main = "Vietnam Data Distribution Pre-Pandemic")
plot(density(Data_Pre$Kamboja),main = "Kamboja Data Distribution Pre-Pandemic")
plot(density(Data_Pre$GEPU),main = "GEPU Data Distribution Pre-Pandemic")

## Memeriksa Stationeritas
# Uji ADF
# Sebuah data dikatakan stationer jika punya p-value < 0.05
library(tseries)
adf.test(Data_Pre$Indo) # Tidak Stationer
adf.test(Data_Pre$Malaysia) # Stationer
adf.test(Data_Pre$Sgd) # Tidak Stationer
adf.test(Data_Pre$Laos) # Stationer
adf.test(Data_Pre$Filipina) # Tidak Stationer
adf.test(Data_Pre$Thai) # Tidak Stationer
adf.test(Data_Pre$Vietnam) # Tidak Stationer
adf.test(Data_Pre$Kamboja) # Tidak Stationer
adf.test(Data_Pre$GEPU) # Tidak stationer

# Differencing
rIndo_Pre <- diff(Data_Pre$Indo)
rMalaysia_Pre <- diff(Data_Pre$Malaysia)
rSgd_Pre <- diff(Data_Pre$Sgd)
rLaos_Pre <- diff(Data_Pre$Laos)
rFilipina_Pre <- diff(Data_Pre$Filipina)
rThai_Pre <- diff(Data_Pre$Thai)
rVietnam_Pre <- diff(Data_Pre$Vietnam)
rKamboja_Pre <- diff(Data_Pre$Kamboja)
rGEPU_Pre <- diff(Data_Pre$GEPU)

Data_Stat_Pre <- data.frame(Indo = rIndo_Pre,Malaysia = rMalaysia_Pre,
                        Sgd = rSgd_Pre,Laos = rLaos_Pre,
                        Filipina = rFilipina_Pre,Thai = rThai_Pre,
                        Vietnam = rVietnam_Pre,Kamboja = rKamboja_Pre,
                        GEPU = rGEPU_Pre)

# Memeriksa Heteroskedastisitas
library(lmtest)
bptest(lm(GEPU ~., data = Data_Stat_Pre)) # Residu model OLS Homogen, jadi tidak ada heteroskedastistas

# Model DCC-Garch
library(rmgarch)
library(rugarch)
model_Pre <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                    variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                    distribution.model = "norm")
modelspec_Pre <- dccspec(uspec = multispec(replicate(8,model_Pre)),
                     dccOrder = c(1,1), distribution = "mvnorm")
modelfit_Pre <- dccfit(modelspec_Pre,data = Data_Stat_Pre[,-9])
modelfit_Pre

# Post-Pandemic
Data_Post <- subset(Data,(Monthly >= "2020-04-01" & Monthly <= "2023-05-1"))

## Analisis Deskriptif
# Tabel Deskriptif
library(e1071)
Descriptive_tabel <- data.frame(Variabel = names(Data_Post[,-1]),
                                Mean = sapply(Data_Post[,2:10],mean),
                                Skewness = sapply(Data_Post[,2:10],skewness),
                                Kurtosis = sapply(Data_Post[,2:10],kurtosis))
rownames(Descriptive_tabel) <- NULL
writexl::write_xlsx(Descriptive_tabel,"Descriptive Table Post-Pandemic.xlsx")

# Plot Distribusi
plot(density(Data_Post$Indo),main = "Indonesia Data Distribution")
plot(density(Data_Post$Malaysia),main = "Malaysia Data Distribution")
plot(density(Data_Post$Sgd),main = "Sgd Data Distribution")
plot(density(Data_Post$Laos),main = "Laos Data Distribution")
plot(density(Data_Post$Filipina),main = "Filipina Data Distribution")
plot(density(Data_Post$Thai),main = "Thailand Data Distribution")
plot(density(Data_Post$Vietnam),main = "Vietnam Data Distribution")
plot(density(Data_Post$Kamboja),main = "Kamboja Data Distribution")
plot(density(Data_Post$GEPU),main = "GEPU Data Distribution")

## Memeriksa Stationeritas
# Uji ADF
# Sebuah data dikatakan stationer jika punya p-value < 0.05
library(tseries)
adf.test(Data_Post$Indo) # Stationer
adf.test(Data_Post$Malaysia) # Stationer
adf.test(Data_Post$Sgd) # Stationer
adf.test(Data_Post$Laos) # Stationer
adf.test(Data_Post$Filipina) # Stationer
adf.test(Data_Post$Thai) # Stationer
adf.test(Data_Post$Vietnam) # Stationer
adf.test(Data_Post$Kamboja) # Stationer
adf.test(Data_Post$GEPU) # Tidak stationer

# Differencing
rIndo_Post <- diff(Data_Post$Indo)
rMalaysia_Post <- diff(Data_Post$Malaysia)
rSgd_Post <- diff(Data_Post$Sgd)
rLaos_Post <- diff(Data_Post$Laos)
rFilipina_Post <- diff(Data_Post$Filipina)
rThai_Post <- diff(Data_Post$Thai)
rVietnam_Post <- diff(Data_Post$Vietnam)
rKamboja_Post <- diff(Data_Post$Kamboja)
rGEPU_Post <- diff(Data_Post$GEPU)

Data_Stat_Post <- data.frame(Indo = rIndo_Post,Malaysia = rMalaysia_Post,
                            Sgd = rSgd_Post,Laos = rLaos_Post,
                            Filipina = rFilipina_Post,Thai = rThai_Post,
                            Vietnam = rVietnam_Post,Kamboja = rKamboja_Post,
                            GEPU = rGEPU_Post)

# Memeriksa Heteroskedastisitas
library(lmtest)
bptest(lm(GEPU ~., data = Data_Stat_Post)) # Residu model OLS Homogen, jadi tidak ada heteroskedastistas

# Model DCC-Garch
library(rmgarch)
library(rugarch)
model_Post <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                        variance.model = list(garchOrder = c(1,1), model = "sGARCH"),
                        distribution.model = "norm")
modelspec_Post <- dccspec(uspec = multispec(replicate(8,model_Post)),
                         dccOrder = c(1,1), distribution = "mvnorm")
modelfit_Post <- dccfit(modelspec_Post,data = Data_Stat_Post[,-9])
modelfit_Post
