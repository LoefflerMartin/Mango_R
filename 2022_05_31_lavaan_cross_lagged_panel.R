# based on: http://www.regorz-statistik.de/inhalte/r_lavaan_pfadanalyse_6.html

library(lavaan)
meine_daten = Demo.growth

# Cross-Lagged-Panel-Modell
mein_modell <- '
# Gerichtete Pfade
t2 ~ AR11 * t1 + CL21 * x1
x2 ~ AR22 * x1 + CL12 * t1

# Kovarianzen
t2 ~~ x2
t1 ~~ x1
'

model_fit <- sem(data = meine_daten, model = mein_modell)

summary(model_fit, fit.measures = TRUE)

# Visualisierung mit tidySEM
library(tidySEM)

pfad_layout <- get_layout("t1", "t2",
                          "x1", "x2",
                          rows = 2)

graph_sem(model = model_fit, layout = pfad_layout)


# Signifikanztest auf Unterschied der beiden Cross-Lagged-Pfade

# Schätzung des Modells mit Gleichheitsrestriktion beider CL-Pfade

mein_modell2 <- '
# Gerichtete Pfade
t2 ~ AR11 * t1 + CL21 * x1
x2 ~ AR22 * x1 + CL12 * t1

# Kovarianzen
t2 ~~ x2

# Gleichheitsrestriktion Cross-Lagged-Pfade
CL21 == CL12
'

model_fit2 <- sem(data = meine_daten, model = mein_modell2)

summary(model_fit2)

# Signifikanztest auf Unterschied von beschränktem und unbeschränktem Modell

lavTestLRT(model_fit, model_fit2)
