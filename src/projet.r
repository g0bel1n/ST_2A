# %%
library(forecast)
library(comprehenr)
library(urca)
library(stats)
library(car)
# %%
# %%
# On commence par lire les valeurs du fichier csv en raemarquant que les valeurs récentes sont en premier
data <- read.csv("data/valeurs_mensuelles.csv", sep = ";", header = TRUE)
indice <- ts(rev(data$Indice.CVS.CJO.de.la.production.industrielle..base.100.en.2015....Fabrication.de.bière..NAF.rév..2..niveau.classe..poste.11.05.), start = c(1990, 1), frequency = 12)
plot(indice)
# %%

# %%
components.ts <- decompose(indice)
plot(components.ts)

# %%

# %%
# On observe tendance qui devrait être supprimés en différenciant à l'ordre 1
componentsts <- decompose(indice)

tsData <- diff(indice, differences = 1)

# %%
# %%
components.ts <- decompose(tsData)
plot(components.ts)
# Ce graphique nous permet de vérifier visuellement la pertinence de la différenciation dans un premier temps

# %%

# %%
last_points <- ts(head(tsData, 2), start = c(2022, 1), frequency = 12)
tsData <- ts(head(tsData, -2), start = c(1990, 1), frequency = 12)
# On garde les deux dernières valeurs en vue de la question 2
# %%

# %%
m <- tseries::adf.test(tsData)
print(m)
# Le test de Dickey Fuller Augmenté nous permet de vérifier si notre série est suffisament différenciée.
# %%

# %%
summary(ur.kpss(tsData))
summary(ur.pp(tsData))
# On complète le test de Dickey Fuller par celui KPSS et celui Philip-Perron
# %%
# %%
library(forecast)
auto.arima(indice, stepwise = FALSE, approximation = FALSE, max.p = 6, max.q = 6, parallel = TRUE)
# Cette ligne nous guidera. C'est en effet un ordre proche de celui-ci que nous devrions trouver.
# %%

# %%
pacf(tsData)
# %% suggests AR 8-9

# %%
acf(tsData) # suggests MA 1-2
# %%


# %%
model_maxi <- arima(dindice, order = c(9, 0, 3))
residus_maxi <- residuals(model_maxi)
write.csv(confint(model_maxi))
# On affiche les intervalles de confiances des coefficients de la modélisation. Sont-ils tous significatifs ?
# %%


# %%
model <- Arima(tsData, order = c(0, 0, 0))
AIC <- AIC(model)
BIC <- AIC(model, k = log(length(dindice)))
results <- data.frame(AR = c(0), MA = c(0), AIC = c(AIC), BIC = c(BIC))
for (AR in 0:9) {
    for (MA in 0:3) {
        if (AR + MA != 0) {
            model <- Arima(tsData, order = c(AR, 0, MA))
            AIC <- AIC(model)
            BIC <- BIC(model)
            results[nrow(results) + 1, ] <- c(AR, MA, AIC, BIC)
        }
    }
}

# On regarde l'AIC et le BIC de tous les ordres compris entre les valeurs maximales trouvées précédemment.

# %%

# %%
print(results[which.min(results$AIC), ])
print(results[which.min(results$BIC), ])
#
# %%
# %%
write.csv(results)
# %%
# %%
png("ressources/root103.png")
Arima(tsData, order = c(5, 0, 3), xreg = seq_along(tsData)) %>%
    autoplot()

dev.off()
# Racines unitaires inverses du modèle
# %%
# %%
png("ressources/root103.png")
Arima(tsData, order = c(0, 0, 2), xreg = seq_along(tsData)) %>%
    autoplot()

dev.off()
# Racines unitaires inverses du modèle
# %%

# %%
png("ressources/root103.png")
Arima(tsData, order = c(1, 0, 3), xreg = seq_along(tsData)) %>%
    autoplot()

dev.off()
# Racines unitaires inverses du modèle
# %%
# %%

model_2 <- Arima(tsData, order = c(1, 0, 3), seasonal = c(0, 0, 0), include.mean = FALSE)
write.csv(confint(model_2))

# On se concentre maintenant sur le modèle choisi => ARIMA(1,0,3)

# %%

# %%
png("ressources/acfrresidus.png")
acf(model_2$residuals) # rien de significatif
dev.off()

# %%
# %%
library(FitAR)
png("ressources/LjunBoxTest.png")
boxresult <- LjungBoxTest(model_2$residuals) # p-values au dessus de 0.05 -> pas de significativité, pas de pattern
plot(boxresult[, 3], main = "Ljung-Box Q Test", ylab = "P-values", xlab = "Lag")
dev.off()

# Test de LjungBoxTest
# %%
# %%

png("ressources/qqnorm.png")
qqnorm(model_2$residuals)
qqline(model_2$residuals)
dev.off()
# Q-Q Plot

# %%
# %%
png("ressources/ellispe.png")
library(car)
a <- predict(model_2, 2)
df <- data.frame(X_T1 = (-1000:700) / 100, X_T2 = (-1000:700) / 100)
phi <- as.numeric(model_2$coef[1])
psi <- as.numeric(model_2$coef[4])
sigma <- cbind(c(1 + phi + psi, psi + phi), c(psi + phi, 1))
plot(X_T2 ~ X_T1, data = df, type = "n")
ellipse(center = c(a$pred[2], a$pred[1]), shape = sigma, radius = qchisq(0.05, 2) * model_2$sigma2, draw = TRUE, add = TRUE, lty = 2, fill = TRUE, fill.alpha = 0.1) # nolint

points(a$pred[2], a$pred[1], pch = "+", col = "green")
points(last_points[2], last_points[1], pch = "+", col = "red")

dev.off()

# Dessin de l'ellipse de confiance selon le calcul du rapport
# %%


# %%
png("ressources/forecast.png")
forecast(model_2, 10, ) %>%
    autoplot()
dev.off()

# Précision
# %%