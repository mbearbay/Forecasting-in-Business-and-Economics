# Forecasting in Business and Economics

[English](#english) | [Dansk](#dansk)

## English

Exam project from the course *Forecasting in Business and Economics* at Copenhagen Business School, written in R. The project models and forecasts quarterly personal consumption expenditure (PCE) using data from FRED (series `ND000349Q`, 2002-2024).

### What the project does

1. **Data:** Fetches the series from FRED, log-transforms it and checks trend and seasonality.
2. **Model 1, deterministic trend and seasonality:** Linear and quadratic trend with seasonal dummies, residual analysis, Durbin-Watson test and regression with ARMA errors.
3. **Model 2, stochastic trend:** De-seasonalisation, Dickey-Fuller unit root test and ARIMA with seasonal dummies.
4. **Model 3, seasonal ARIMA:** Seasonal differencing and SARIMA, selected using AIC.
5. **Model 4, Holt-Winters exponential smoothing.**
6. **Model comparison:** The data is split into a training and a test period, and the models are compared on out-of-sample forecast accuracy.
7. **Forecast:** Final forecast with the chosen model, transformed back from logs.

### Files

| File | Content |
| --- | --- |
| `Forecasting Exam.R` | All code for the project |
| `ND000349Q.csv` | The data series from FRED |

### How to run

The script fetches data with the `fredr` package, which needs a free API key from [FRED](https://fred.stlouisfed.org/docs/api/api_key.html).

```r
install.packages(c("fredr", "forecast", "scales", "texreg", "lmtest"))
fredr::fredr_set_key("YOUR_API_KEY")
source("Forecasting Exam.R")
```

**Tools:** R, forecast, fredr, lmtest

---

## Dansk

Eksamensprojekt fra faget *Forecasting in Business and Economics* på Copenhagen Business School, skrevet i R. Projektet modellerer og forudsiger det kvartalsvise private forbrug (PCE) med data fra FRED (serie `ND000349Q`, 2002-2024).

### Hvad projektet gør

1. **Data:** Henter serien fra FRED, log-transformerer den og undersøger trend og sæson.
2. **Model 1, deterministisk trend og sæson:** Lineær og kvadratisk trend med sæsondummies, residualanalyse, Durbin-Watson-test og regression med ARMA-fejl.
3. **Model 2, stokastisk trend:** Sæsonkorrektion, Dickey-Fuller-test for enhedsrod og ARIMA med sæsondummies.
4. **Model 3, sæson-ARIMA:** Sæsondifferencing og SARIMA, valgt ud fra AIC.
5. **Model 4, Holt-Winters eksponentiel udglatning.**
6. **Sammenligning af modeller:** Data deles i en trænings- og en testperiode, og modellerne sammenlignes på præcisionen af prognoserne uden for stikprøven.
7. **Prognose:** Endelig prognose med den valgte model, transformeret tilbage fra log.

### Filer

| Fil | Indhold |
| --- | --- |
| `Forecasting Exam.R` | Al kode til projektet |
| `ND000349Q.csv` | Dataserien fra FRED |

### Sådan køres koden

Scriptet henter data med pakken `fredr`, som kræver en gratis API-nøgle fra [FRED](https://fred.stlouisfed.org/docs/api/api_key.html).

```r
install.packages(c("fredr", "forecast", "scales", "texreg", "lmtest"))
fredr::fredr_set_key("DIN_API_NØGLE")
source("Forecasting Exam.R")
```

**Værktøjer:** R, forecast, fredr, lmtest
