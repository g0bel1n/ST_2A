
library(comprehenr)
library(urca)
library(stats)

# %%

data <- data.frame(read.csv("valeurs_mensuelles.csv", sep = ";", header = TRUE))
data <- data[-c(1, 2, 3), ]
data$Indice.CVS.CJO.de.la.production.industrielle..base.100.en.2015....Fabrication.de.bière..NAF.rév..2..niveau.classe..poste.11.05. <- as.double(data$Indice.CVS.CJO.de.la.production.industrielle..base.100.en.2015....Fabrication.de.bière..NAF.rév..2..niveau.classe..poste.11.05.)
indice <- ts(data$Indice.CVS.CJO.de.la.production.industrielle..base.100.en.2015....Fabrication.de.bière..NAF.rév..2..niveau.classe..poste.11.05., start = c(1990, 1), frequency = 12)

dindice <- diff(indice)

auto.arima(dindice)

# %%

# %%

indice <- ts(head(indice, -2), start = c(1990, 1), frequency = 12)
last_points <- ts(head(indice, 2), start = c(2022, 1), frequency = 12)
print(indice)
print(last_points)
# %%


# %%
components.ts <- decompose(indice)
plot(components.ts)

# %%
# On obserce une légère saisonnalité (environ 1/40ème des variations)
# Il y aussi une tendance qui se dessine
# Supprimons donc la saisonnalité et regardons la série différenciée à l'ordre 1

# %%
componentsts <- decompose(indice)

seasonnalyadjustedts <- indice - componentsts$seasonal
tsData <- diff(seasonnalyadjustedts, differences = 1)

components.ts <- decompose(tsData)
# plot(components.ts)
# %%

# %%
m <- tseries::adf.test(tsData)
adf.out <- ur.df(tsData)

# %%


# %%
library(forecast)
auto.arima(dindice)
# %%


# %%
pacf(dindice)
# %% suggests AR 8-9

# %%
acf(dindice) # MA 1-2
# %%


# print(m)
# p-value = 0.01
# C'est mieux. Regardons maintenant les autocorrelations :

# acf(tsData)
# Suggests a MA 2 or 3

# %%
model_maxi <- arima(dindice, order = c(9, 0, 3))
residus_maxi <- residuals(model_maxi)
write.csv(confint(model_maxi))

# %%
# %%
model_2 <- arima(tsData, order = c(1, 0, 3))
write.csv(confint(model_2))
# All coefficients are significant

# %%

# %%

model <- arima(dindice, order = c(0, 0, 0))
AIC <- AIC(model)
BIC <- AIC(model, k = log(length(dindice)))
results <- data.frame(AR = c(0), MA = c(0), AIC = c(AIC), BIC = c(BIC))
for (AR in 1:9) {
    for (MA in 1:3) {
        model <- arima(dindice, order = c(AR, 0, MA))
        AIC <- AIC(model)
        BIC <- AIC(model, k = log(length(dindice)))
        results[nrow(results) + 1, ] <- c(AR, MA, AIC, BIC)
    }
}
print(results)
# The lowest BIC is reached for 1,0,3
# %%

# %%
write.csv(results)

# %%
# %%
png("acfrresidus.png")
acf(model_2$residuals) # rien de significatif
dev.off()
# %%
# %%
library(FitAR)
png("LjunBoxTest.png")
boxresult <- LjungBoxTest(model_2$residuals) # p-values au dessus de 0.05 -> pas de significativité, pas de pattern
plot(boxresult[, 3], main = "Ljung-Box Q Test", ylab = "P-values", xlab = "Lag")

dev.off()

# %%
# %%
png("qqnorm.png")
qqnorm(model_2$residuals)
qqline(model_2$residuals) # Le long de la ligne et non pas éparpillés
dev.off()
# %%
# %%
# Bilan pas de raison de penser que les résidus ne sont pas un bruit blanc
# %%

# %%
mean_residuals <- mean(model_2$residuals)
print(model_2$coef[1])

# %%
a <- predict(model_2, 2)

# plot(tsData)
phi <- as.numeric(model_2$coef[1])
psi <- as.numeric(model_2$coef[4])
sigma <- cbind(c(1 + phi + psi, psi + phi), c(psi + phi, 1))
# plot(X_T2 ~ X_T1, data = df, type = "n")
ellipse(center = c(a$pred[2], a$pred[1]), shape = sigma, radius = qchisq(0.05, 2) * model_2$sigma2, draw = TRUE, add = TRUE, lty = 2, fill = TRUE, fill.alpha = 0.1) # nolint
points(a$pred[2], a$pred[1], pch = "+", col = "red")
# %%