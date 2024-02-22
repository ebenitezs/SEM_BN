#install.packages("haven")
#install.packages("lavaan")
#install.packages("semPlot")
#install.packages("semptools")
#install.packages("dplyr")
#install.packages("bnlearn")
#install.packages("gplots")
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("Rgraphviz")
#install.packages("igraph")
#install.packages("gRain")
#install.packages("viridisLite")
#install.packages("DiagrammeR")
#install.packages("rgl")
#install.packages("akima")
#install.packages("caret")
#install.packages("ggplot")
#install.packages("lattice")
#install.packages("pROC")
#install.packages("shiny")
#install.packages("plotly")
#install.packages("infotheo")
#install.packages("fmsb")
#install.packages("reshape2")
#if (!require("tidyverse")) install.packages("tidyverse")
#if (!require("openxlsx")) install.packages("openxlsx")
#install.packages("ggpattern")
#if (!require("networkD3")) install.packages("networkD3")
library(networkD3)
library(ggpattern)
library(openxlsx)
library(tidyverse)
library(reshape2)
library(fmsb)
library(infotheo)
library(plotly)
library(shiny)
library(pROC)
library(caret)
library(ggplot2)
library(lattice)
library(akima)
library(rgl)
library(DiagrammeR)
library(gplots)
library(gRain)
library(igraph)
library(Rgraphviz)
library(bnlearn)
library(lavaan)
library(haven)
library(semptools)
library(dplyr)
library(semPlot)
library(viridis)

dat <- read_sav("G:/Mi unidad/NAVARRA/DATOS/ALVARO/base de datos_tesis_V0.sav")

dat$SOI_1  <- 8 - dat$SOC_1
dat$SOI_2  <- 8 - dat$SOC_2
dat$SOI_3  <- 8 - dat$SOC_3
dat$SOI_7  <- 8 - dat$SOC_7
dat$SOI_10 <- 8 - dat$SOC_10


# Lista de columnas para convertir
cols_an <- c("EP_3", "EP_4", "EP_5", "EP_6", "EP_7", "EP_8",
             "EP_23", "EP_24", "EP_26", "EP_27", "EP_28", "EP_29", "EP_30",
             "EP_38", "EP_39", "EP_40", "EP_41",
             "EP_32", "EP_33", "EP_34", "EP_35", "EP_36",
             "Hope_1", "Hope_3", "Hope_5",
             "YLOT_4", "YLOT_6", "YLOT_8", "YLOT_10", "YLOT_12", "YLOT_14",
             "YLOT_3", "YLOT_5", "YLOT_7", "YLOT_9", "YLOT_11", "YLOT_13",
             "AE_1", "AE_4", "AE_5", "AE_6", "AE_7", "AE_8", "AE_9", "AE_10",
             "SOC_6", "SOC_8", "SOC_11",
             "SOI_3", "SOC_5", "SOI_10",
             "SOI_1", "SOC_4", "SOI_7", "SOC_12",
             "VC_1", "VC_3", "VC_5",
             "VC_8", "VC_10", "VC_12",
             "VC_7", "VC_9", "VC_11", "VC_13",
             "VC_15", "VC_17", "VC_19",
             "VC_14", "VC_16", "VC_18", "VC_20",
             "VC_24", "VC_27", "VC_30")

# Crear un nuevo dataframe con solo las columnas especificadas
datn <- dat[, cols_an]


datn <- datn[complete.cases(datn[, cols_an]), ]

# Lista de todas las variables a convertir

for (var in cols_an) {
  datn[[var]] <- as.numeric(as.character(datn[[var]]))
}


model <- '
FAFE =~ EP_3 + EP_4 + EP_5 + EP_6 + EP_7 + EP_8

FAUT =~ EP_23 + EP_24 + EP_26 + EP_27 + EP_28 + EP_29 + EP_30

FREV =~ EP_38 + EP_39 + EP_40 + EP_41

FHUM =~ EP_32 + EP_33 + EP_34 + EP_35 + EP_36

FAGE =~ Hope_1 + Hope_3 + Hope_5

FOPT =~ YLOT_4 + YLOT_6 + YLOT_8 + YLOT_10 + YLOT_12 + YLOT_14

FPES =~ YLOT_3 + YLOT_5 + YLOT_7 + YLOT_9 + YLOT_11 + YLOT_13

FAEF =~ AE_1 + AE_4 + AE_5 + AE_6 + AE_7 + AE_8 + AE_9 + AE_10

FCOM =~ SOC_6 + SOC_8 + SOC_11

FMAN =~ SOI_3 + SOC_5 + SOI_10

FSIG =~ SOI_1 + SOC_4 + SOI_7 + SOC_12

FIGU =~ VC_1 + VC_3 + VC_5

FPER =~ VC_8 + VC_10 + VC_12

FAPO =~ VC_7 + VC_9 + VC_11 + VC_13

FNOR =~ VC_15 + VC_17 + VC_19

FVAL =~ VC_14 + VC_16 + VC_18 + VC_20

FOFE =~ VC_24 + VC_27 + VC_30

FEPA =~ FAFE + FAUT + FREV + FHUM

FPPY =~ FAGE + FOPT + FPES + FAEF + FCOM + FMAN + FSIG

FVIN =~ FPER + FAPO

FCLA =~ FNOR + FVAL

FCEN =~ FOFE + FVIN + FCLA + FIGU

FCEN =~ FPPY
FEPA =~ FPPY

FCEN ~~ FEPA
'


fit <- sem(model, data=datn)

fitMeasures(fit, c("rmsea", "srmr", "cfi"))

# Definir los grupos de variables
grupos <- list(
  FAFE = c("EP_3", "EP_4", "EP_5", "EP_6", "EP_7", "EP_8"),
  FAUT = c("EP_23", "EP_24", "EP_26", "EP_27", "EP_28", "EP_29", "EP_30"),
  FREV = c("EP_38", "EP_39", "EP_40", "EP_41"),
  FHUM = c("EP_32", "EP_33", "EP_34", "EP_35", "EP_36"),
  FAGE = c("Hope_1", "Hope_3", "Hope_5"),
  FOPT = c("YLOT_4", "YLOT_6", "YLOT_8", "YLOT_10", "YLOT_12", "YLOT_14"),
  FPES = c("YLOT_3", "YLOT_5", "YLOT_7", "YLOT_9", "YLOT_11", "YLOT_13"),
  FAEF = c("AE_1", "AE_4", "AE_5", "AE_6", "AE_7", "AE_8", "AE_9", "AE_10"),
  FCOM = c("SOC_6", "SOC_8", "SOC_11"),
  FMAN = c("SOI_3", "SOC_5", "SOI_10"),
  FSIG = c("SOI_1", "SOC_4", "SOI_7", "SOC_12"),
  FIGU = c("VC_1", "VC_3", "VC_5"),
  FPER = c("VC_8", "VC_10", "VC_12"),
  FAPO = c("VC_7", "VC_9", "VC_11", "VC_13"),
  FNOR = c("VC_15", "VC_17", "VC_19"),
  FVAL = c("VC_14", "VC_16", "VC_18", "VC_20"),
  FOFE = c("VC_24", "VC_27", "VC_30")
)

# Mapeo de nombres actualizado
mapeo_nombres <- list(
  FPPY = list(nombre = "Personal Positive Youth Development",         categoria = "1st",          acronimo = "PPYD"),
  FCEN = list(nombre = "Climate and Functioning of the School",       categoria = "1st",          acronimo = "CFS"),
  FEPA = list(nombre = "Positive Parenting",                          categoria = "1st",          acronimo = "PP"),
  FAFE = list(nombre = "Warmth",                                      categoria = "2nd for PP",   acronimo = "War"),
  FAUT = list(nombre = "Autonomy",                                    categoria = "2nd for PP",   acronimo = "Aut"),
  FHUM = list(nombre = "Humor",                                       categoria = "2nd for PP",   acronimo = "Hum"),
  FREV = list(nombre = "Self-disclosure",                             categoria = "2nd for PP",   acronimo = "Dis"),
  FIGU = list(nombre = "Peers",                                       categoria = "2nd for CFS",  acronimo = "Pee"),
  FVIN = list(nombre = "Bonds",                                       categoria = "2nd for CFS",  acronimo = "Bon"),
  FOFE = list(nombre = "Activities proffered",                        categoria = "2nd for CFS",  acronimo = "Pro"),  
  FCLA = list(nombre = "Clarity",                                     categoria = "2nd for CFS",  acronimo = "Cla"),
  FPER = list(nombre = "Belonging",                                   categoria = "3rd for CFS",  acronimo = "Bel"),
  FAPO = list(nombre = "Support",                                     categoria = "3rd for CFS",  acronimo = "Sup"),
  FNOR = list(nombre = "Rules",                                       categoria = "3rd for CFS",  acronimo = "Rul"),
  FVAL = list(nombre = "Values",                                      categoria = "3rd for CFS",  acronimo = "Val"),
  FOPT = list(nombre = "Optimism",                                    categoria = "2nd for PPYD", acronimo = "Opt"),
  FPES = list(nombre = "Pessimism",                                   categoria = "2nd for PPYD", acronimo = "Pes"),
  FAEF = list(nombre = "General Self-efficacy",                       categoria = "2nd for PPYD", acronimo = "GSe"),
  FAGE = list(nombre = "Agency",                                      categoria = "2nd for PPYD", acronimo = "Age"),
  FCOM = list(nombre = "Comprehensibility ",                          categoria = "2nd for PPYD", acronimo = "Com"),
  FMAN = list(nombre = "Manageability",                               categoria = "2nd for PPYD", acronimo = "Man"),
  FSIG = list(nombre = "Meaningfulness",                              categoria = "2nd for PPYD", acronimo = "Mea")
)

model_acronimos <- '
War =~ EP_3 + EP_4 + EP_5 + EP_6 + EP_7 + EP_8
Aut =~ EP_23 + EP_24 + EP_26 + EP_27 + EP_28 + EP_29 + EP_30
Dis =~ EP_38 + EP_39 + EP_40 + EP_41
Hum =~ EP_32 + EP_33 + EP_34 + EP_35 + EP_36
Age =~ Hope_1 + Hope_3 + Hope_5
Opt =~ YLOT_4 + YLOT_6 + YLOT_8 + YLOT_10 + YLOT_12 + YLOT_14
Pes =~ YLOT_3 + YLOT_5 + YLOT_7 + YLOT_9 + YLOT_11 + YLOT_13
GSe =~ AE_1 + AE_4 + AE_5 + AE_6 + AE_7 + AE_8 + AE_9 + AE_10
Com =~ SOC_6 + SOC_8 + SOC_11
Man =~ SOI_3 + SOC_5 + SOI_10
Mea =~ SOI_1 + SOC_4 + SOI_7 + SOC_12
Pee =~ VC_1 + VC_3 + VC_5
Bel =~ VC_8 + VC_10 + VC_12
Sup =~ VC_7 + VC_9 + VC_11 + VC_13
Rul =~ VC_15 + VC_17 + VC_19
Val =~ VC_14 + VC_16 + VC_18 + VC_20
Pro =~ VC_24 + VC_27 + VC_30
PP =~ War + Aut + Dis + Hum
PPYD =~ Age + Opt + Pes + GSe + Com + Man + Mea
Bon =~ Bel + Sup
Cla =~ Rul + Val
CFS =~ Pro + Bon + Cla + Pee
CFS =~ PPYD
PP =~ PPYD
CFS ~~ PP
'


fit_acr <- sem(model_acronimos, data=datn)


semPaths(fit_acr, whatLabels="par", layout="tree2",rotation=1,structural = TRUE)

estandarizadas <- parameterEstimates(fit_acr, standardized = TRUE)



# Agrupar por variable latente y calcular estadísticas
resumen_cargas <- estandarizadas %>%
  filter(op == "=~") %>%
  group_by(lhs) %>%
  summarize(
    Carga_Promedio = mean(std.all, na.rm = TRUE),
    Carga_Min = min(std.all, na.rm = TRUE),
    Carga_Max = max(std.all, na.rm = TRUE)
  )

# Imprimir el resultado
print(n=30, resumen_cargas)





set.seed(123) # Fijar una semilla para reproducibilidad

# Calcular el tamaño del conjunto de entrenamiento (p. ej., 70% del total)
train_size <- floor(0.70 * nrow(datn))

# Crear un índice aleatorio
train_index <- sample(seq_len(nrow(datn)), size = train_size)


# Dividir el dataframe
train_set <- datn[train_index, ]
validation_set <- datn[-train_index, ]

# Aplicar na.omit a tu dataframe
train_set <- na.omit(train_set)
validation <- na.omit(validation_set)

# Función para calcular estadísticas
calcular_estadisticas <- function(df) {
  sapply(df, function(x) {
    c(media = mean(x, na.rm = TRUE),
      maximo = max(x, na.rm = TRUE),
      minimo = min(x, na.rm = TRUE))
  })
}

# Calcular estadísticas para train_set y validation_set
estadisticas_train <- calcular_estadisticas(train_set)
estadisticas_validation <- calcular_estadisticas(validation_set)

# Convertir a dataframes
estadisticas_train_df <- as.data.frame(t(estadisticas_train))
estadisticas_validation_df <- as.data.frame(t(estadisticas_validation))

# Añadir columna para identificar el conjunto
estadisticas_train_df$Conjunto <- 'Entrenamiento'
estadisticas_validation_df$Conjunto <- 'Validacion'

# Fusionar los dataframes
estadisticas_fusionadas <- rbind(estadisticas_train_df, estadisticas_validation_df)
print(estadisticas_fusionadas)

# Asignar la columna 'Grupo' en estadisticas_fusionadas
estadisticas_fusionadas$Grupo <- NA  # Inicializa la columna Grupo con NA

# Asigna el nombre del grupo correspondiente a cada variable
for (nombre_grupo in names(grupos)) {
  grupo_vars <- grupos[[nombre_grupo]]
  for (var in grupo_vars) {
    estadisticas_fusionadas$Grupo[rownames(estadisticas_fusionadas) == var | rownames(estadisticas_fusionadas) == paste0(var, "1")] <- nombre_grupo
  }
}

# Crear un dataframe para almacenar los resultados
resultados_estadisticas <- data.frame(Conjunto = character(), 
                                      Grupo = character(), 
                                      MediaMedias = numeric(), 
                                      SDMedias = numeric(), 
                                      MaxMaximos = numeric(), 
                                      MinMinimos = numeric(), 
                                      stringsAsFactors = FALSE)

# Función para calcular estadísticas para un grupo y conjunto específicos
calcular_estadisticas_por_grupo <- function(df, grupo, conjunto) {
  data.frame(
    Conjunto = conjunto,
    Grupo = grupo,
    MediaMedias = mean(df$media, na.rm = TRUE),
    SDMedias = sd(df$media, na.rm = TRUE),
    MaxMaximos = max(df$maximo, na.rm = TRUE),
    MinMinimos = min(df$minimo, na.rm = TRUE)
  )
}

# Calcular las estadísticas para cada combinación de grupo y conjunto
for (nombre_grupo in names(grupos)) {
  # Datos para el conjunto de entrenamiento
  df_entrenamiento <- subset(estadisticas_fusionadas, Grupo == nombre_grupo & Conjunto == "Entrenamiento")
  if (nrow(df_entrenamiento) > 0) {
    resultados_estadisticas <- rbind(resultados_estadisticas, calcular_estadisticas_por_grupo(df_entrenamiento, nombre_grupo, "Entrenamiento"))
  }
  
  # Datos para el conjunto de validación
  df_validacion <- subset(estadisticas_fusionadas, Grupo == nombre_grupo & Conjunto == "Validacion")
  if (nrow(df_validacion) > 0) {
    resultados_estadisticas <- rbind(resultados_estadisticas, calcular_estadisticas_por_grupo(df_validacion, nombre_grupo, "Validacion"))
  }
}

# Separar los datos de entrenamiento y validación
datos_entrenamiento <- subset(resultados_estadisticas, Conjunto == "Entrenamiento")
datos_validacion <- subset(resultados_estadisticas, Conjunto == "Validacion")

# Renombrar las columnas para preparar la fusión
colnames(datos_entrenamiento)[3:6] <- paste("Entrenamiento", colnames(datos_entrenamiento)[3:6], sep = "_")
colnames(datos_validacion)[3:6] <- paste("Validacion", colnames(datos_validacion)[3:6], sep = "_")

# Eliminar la columna Conjunto ya que ya no es necesaria
datos_entrenamiento <- datos_entrenamiento[-1]
datos_validacion <- datos_validacion[-1]

# Fusionar los datos de entrenamiento y validación por Grupo
datos_finales <- merge(datos_entrenamiento, datos_validacion, by = "Grupo")

# Mostrar los datos finales
print(datos_finales)


# Ajustar el modelo SEM al conjunto de entrenamiento
fit_train <- sem(model, data=train_set)

# Obtener las cargas factoriales
factor_loadings <- parameterEstimates(fit_train, standardized = TRUE)


# Lista de nombres de columnas de las cargas factoriales
factor_columns <- c("FPPY", "FAGE", "FOPT", "FPES", "FAEF", "FCOM", "FMAN", "FSIG", "FEPA", "FAFE", "FAUT", "FHUM", "FREV",
                    "FCEN", "FIGU", "FVIN", "FPER", "FAPO", "FOFE", "FCLA", "FNOR", "FVAL")

# Lista de nodos ocultos (variables latentes)
hidden_nodes   <- c("FPPY", "FAGE", "FOPT", "FPES", "FAEF", "FCOM", "FMAN", "FSIG", "FEPA", "FAFE", "FAUT", "FHUM", "FREV",
                    "FCEN", "FIGU", "FVIN", "FPER", "FAPO", "FOFE", "FCLA", "FNOR", "FVAL")


# Obtener las puntuaciones factoriales para las variables latentes
factor_scores <- lavPredict(fit_train, type = "lv")

# Unir las puntuaciones factoriales con las variables explícitas del conjunto de entrenamiento
combined_data <- cbind(train_set, factor_scores)


# Convertir las variables latentes en quintiles y luego a formato numérico
for (col in factor_columns) {
  combined_data[[col]] <- cut(combined_data[[col]], 
                              breaks = quantile(combined_data[[col]], probs = 0:5 / 5, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE)
  combined_data[[col]] <- as.numeric(combined_data[[col]])
}

# Convertir todas las variables a factores
combined_data_factors <- combined_data
combined_data_factors[] <- lapply(combined_data_factors, factor)

# Definir estructura bayesiana
bn <- model2network("[FEPA][FCEN][FPPY|FEPA:FCEN][FAGE|FPPY][Hope_1|FAGE][Hope_3|FAGE][Hope_5|FAGE][FOPT|FPPY][YLOT_4|FOPT][YLOT_6|FOPT][YLOT_8|FOPT][YLOT_10|FOPT][YLOT_12|FOPT][YLOT_14|FOPT][FPES|FPPY][YLOT_3|FPES][YLOT_5|FPES][YLOT_7|FPES][YLOT_9|FPES][YLOT_11|FPES][YLOT_13|FPES][FAEF|FPPY][AE_1|FAEF][AE_4|FAEF][AE_5|FAEF][AE_6|FAEF][AE_7|FAEF][AE_8|FAEF][AE_9|FAEF][AE_10|FAEF][FCOM|FPPY][SOC_6|FCOM][SOC_8|FCOM][SOC_11|FCOM][FMAN|FPPY][SOI_3|FMAN][SOC_5|FMAN][SOI_10|FMAN][FSIG|FPPY][SOI_1|FSIG][SOC_4|FSIG][SOI_7|FSIG][SOC_12|FSIG][FAFE|FEPA][EP_3|FAFE][EP_4|FAFE][EP_5|FAFE][EP_6|FAFE][EP_7|FAFE][EP_8|FAFE][FAUT|FEPA][EP_23|FAUT][EP_24|FAUT][EP_26|FAUT][EP_27|FAUT][EP_28|FAUT][EP_29|FAUT][EP_30|FAUT][FHUM|FEPA][EP_32|FHUM][EP_33|FHUM][EP_34|FHUM][EP_35|FHUM][EP_36|FHUM][FREV|FEPA][EP_38|FREV][EP_39|FREV][EP_40|FREV][EP_41|FREV][FPER|FVIN][VC_8|FPER][VC_10|FPER][VC_12|FPER][FAPO|FVIN][VC_7|FAPO][VC_9|FAPO][VC_11|FAPO][VC_13|FAPO][FNOR|FCLA][VC_15|FNOR][VC_17|FNOR][VC_19|FNOR][FVAL|FCLA][VC_14|FVAL][VC_16|FVAL][VC_18|FVAL][VC_20|FVAL][FOFE|FCEN][VC_24|FOFE][VC_27|FOFE][VC_30|FOFE][FIGU|FCEN][VC_1|FIGU][VC_3|FIGU][VC_5|FIGU][FCLA|FCEN][FVIN|FCEN]")

###### COMPARACION DE METODOS

# Ajustar el modelo usando hard-EM
bn_fitted_hard_em <- bn.fit(bn, data = combined_data_factors, method = "hard-em")

# Ajustar el modelo usando el método por defecto (exact)
bn_fitted_exact <- bn.fit(bn, data = combined_data_factors)

# Ajustar el modelo usando el aprendizaje bayesiano
bn_fitted_bayes <- bn.fit(bn, data = combined_data_factors, method = "bayes")


# Preparar los datos para la predicción
predictor_data <- combined_data_factors
predictor_data$FPPY <- NULL  # Excluir FPPY para la predicción

# Realizar las predicciones
predictions_hard_em <- predict(bn_fitted_hard_em, node = "FPPY", data = predictor_data)
predictions_exact <- predict(bn_fitted_exact, node = "FPPY", data = predictor_data)
predictions_bayes <- predict(bn_fitted_bayes, node = "FPPY", data = predictor_data)


# Función ajustada para calcular recall y F1-score
calculate_performance_metrics <- function(predictions, actual) {
  cm <- table(actual, predictions)
  
  # Calcular recall (sensibilidad)
  recall <- diag(prop.table(cm, 1))
  
  # Calcular precisión para cada clase
  precision <- diag(prop.table(cm, 2))
  
  # Calcular F1-score para cada clase
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Manejar posibles valores NaN o NA
  recall[is.nan(recall)] <- NA
  f1_score[is.nan(f1_score)] <- NA
  
  # Calcular el promedio (omitir NA)
  avg_recall <- mean(recall, na.rm = TRUE)
  avg_f1_score <- mean(f1_score, na.rm = TRUE)
  
  return(list(recall = avg_recall, f1_score = avg_f1_score))
}

# Función para calcular la exactitud
calculate_accuracy <- function(predictions, actual) {
  cm <- table(actual, predictions)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(accuracy)
}

# Calcular la exactitud para cada modelo
accuracy_hard_em <- calculate_accuracy(predictions_hard_em, combined_data_factors$FPPY)
accuracy_exact <- calculate_accuracy(predictions_exact, combined_data_factors$FPPY)
accuracy_bayes <- calculate_accuracy(predictions_bayes, combined_data_factors$FPPY)

# Calcular métricas de rendimiento para cada modelo
metrics_hard_em <- calculate_performance_metrics(predictions_hard_em, combined_data_factors$FPPY)
metrics_exact <- calculate_performance_metrics(predictions_exact, combined_data_factors$FPPY)
metrics_bayes <- calculate_performance_metrics(predictions_bayes, combined_data_factors$FPPY)

# Calcular la exactitud para cada modelo
accuracy_hard_em <- calculate_accuracy(predictions_hard_em, combined_data_factors$FPPY)
accuracy_exact <- calculate_accuracy(predictions_exact, combined_data_factors$FPPY)
accuracy_bayes <- calculate_accuracy(predictions_bayes, combined_data_factors$FPPY)

# Agregar la exactitud a las métricas calculadas
metrics_hard_em$accuracy <- accuracy_hard_em
metrics_exact$accuracy <- accuracy_exact
metrics_bayes$accuracy <- accuracy_bayes

# Actualizar el dataframe con las métricas de rendimiento
performance_data <- data.frame(
  Method = rep(c("Hard-EM", "Exact", "Bayes"), each = 3),
  Metric = rep(c("Recall", "F1-Score", "Accuracy"), 3),
  Value = c(metrics_hard_em$recall, metrics_hard_em$f1_score, metrics_hard_em$accuracy,
            metrics_exact$recall, metrics_exact$f1_score, metrics_exact$accuracy,
            metrics_bayes$recall, metrics_bayes$f1_score, metrics_bayes$accuracy)
)

# Crear el gráfico de barras con estilos diferenciados
ggplot(performance_data, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            vjust = -0.3, position = position_dodge(0.9), color = "black") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.box = element_blank(),
    legend.background = element_blank()
  ) +
  scale_fill_manual(
    values = c("white", "gray", "lightblue"),
    guide = guide_legend(title = "Metric")
  ) +
  labs(
    y = "Metric Value",
    x = ""
  ) +
  ylim(0, 0.6)  # Establecer los límites del eje y


###### FIGURA 
# Ajustar el modelo SEM al conjunto de entrenamiento
fit_train_fig <- sem(model_acronimos, data=train_set)

# Obtener las cargas factoriales
factor_loadings_fig <- parameterEstimates(fit_train_fig, standardized = TRUE)


# Obtener las puntuaciones factoriales para las variables latentes
factor_scores_fig <- lavPredict(fit_train_fig, type = "lv")

# Unir las puntuaciones factoriales con las variables explícitas del conjunto de entrenamiento
combined_data_fig <- cbind(train_set, factor_scores_fig)


# Lista de nodos ocultos (variables latentes) actualizada con nuevos acrónimos
hidden_nodes_fig <- c("PPYD", "Age", "Opt", "Pes", "GSe", "Com", "Man", "Mea", "PP", "War", "Aut", "Hum", "Dis", "CFS", "Pee", "Bel", "Bon", "Cla", "Sup", "Rul", "Val", "Pro")


# Lista de nombres de columnas de las cargas factoriales
factor_columns_fig <- c("PPYD", "Age", "Opt", "Pes", "GSe", "Com", "Man", "Mea", "PP", "War", "Aut", "Hum", "Dis", "CFS", "Pee", "Bel", "Bon", "Cla", "Sup", "Rul", "Val", "Pro")


# Convertir las variables latentes en quintiles y luego a formato numérico
for (col in factor_columns_fig) {
  combined_data_fig[[col]] <- cut(combined_data_fig[[col]], 
                              breaks = quantile(combined_data_fig[[col]], probs = 0:5 / 5, na.rm = TRUE),
                              include.lowest = TRUE, 
                              labels = FALSE)
  combined_data_fig[[col]] <- as.numeric(combined_data_fig[[col]])
}

# Convertir todas las variables a factores
combined_data_factors_fig <- combined_data_fig
combined_data_factors_fig[] <- lapply(combined_data_factors_fig, factor)


# Definir estructura bayesiana con nuevos acrónimos
bn_fig <- model2network("[PP][CFS][PPYD|PP:CFS][Age|PPYD][Hope_1|Age][Hope_3|Age][Hope_5|Age][Opt|PPYD][YLOT_4|Opt][YLOT_6|Opt][YLOT_8|Opt][YLOT_10|Opt][YLOT_12|Opt][YLOT_14|Opt][Pes|PPYD][YLOT_3|Pes][YLOT_5|Pes][YLOT_7|Pes][YLOT_9|Pes][YLOT_11|Pes][YLOT_13|Pes][GSe|PPYD][AE_1|GSe][AE_4|GSe][AE_5|GSe][AE_6|GSe][AE_7|GSe][AE_8|GSe][AE_9|GSe][AE_10|GSe][Com|PPYD][SOC_6|Com][SOC_8|Com][SOC_11|Com][Man|PPYD][SOI_3|Man][SOC_5|Man][SOI_10|Man][Mea|PPYD][SOI_1|Mea][SOC_4|Mea][SOI_7|Mea][SOC_12|Mea][War|PP][EP_3|War][EP_4|War][EP_5|War][EP_6|War][EP_7|War][EP_8|War][Aut|PP][EP_23|Aut][EP_24|Aut][EP_26|Aut][EP_27|Aut][EP_28|Aut][EP_29|Aut][EP_30|Aut][Hum|PP][EP_32|Hum][EP_33|Hum][EP_34|Hum][EP_35|Hum][EP_36|Hum][Dis|PP][EP_38|Dis][EP_39|Dis][EP_40|Dis][EP_41|Dis][Bel|Bon][VC_8|Bel][VC_10|Bel][VC_12|Bel][Sup|Bon][VC_7|Sup][VC_9|Sup][VC_11|Sup][VC_13|Sup][Rul|Cla][VC_15|Rul][VC_17|Rul][VC_19|Rul][Val|Cla][VC_14|Val][VC_16|Val][VC_18|Val][VC_20|Val][Pro|CFS][VC_24|Pro][VC_27|Pro][VC_30|Pro][Pee|CFS][VC_1|Pee][VC_3|Pee][VC_5|Pee][Cla|CFS][Bon|CFS]")


bn_fitted_fig <- bn.fit(bn_fig, data = combined_data_factors_fig)

# Configuración del resaltado para solo los nodos ocultos
highlight_config <- list(nodes = hidden_nodes_fig, col = "black", fill = "lightgray", textCol = "black")

# Especificar la ruta y el nombre del archivo
output_filepath <- "G:/Mi unidad/NAVARRA/ARTICULOS/REDES BAYESIANAS/FIGURAS/RB08ENE2023.png"

# Definir las dimensiones de la imagen (en pulgadas)
width_in_inches <- 25
height_in_inches <- 10

# Guardar la visualización en un archivo
png(filename = output_filepath, width = width_in_inches, height = height_in_inches, units = "in", res = 300)
graphviz.plot(bn_fitted_fig, highlight = highlight_config, layout = "twopi", fontsize = 15) # Ajusta el tamaño de la fuente aquí
dev.off()  # Cerrar el dispositivo de gráficos



##### validacion
# Obtener las puntuaciones factoriales para las variables latentes del conjunto de validación
factor_scores_validation <- lavPredict(fit_train, newdata = validation_set, type = "lv")

# Unir las puntuaciones factoriales con las variables explícitas del conjunto de validación
combined_data_validation <- cbind(validation_set, factor_scores_validation)

# Convertir las variables en factor_columns a numéricas y luego a quintiles
for (col in factor_columns) {
  combined_data_validation[[col]] <- as.numeric(as.character(combined_data_validation[[col]]))
  combined_data_validation[[col]] <- cut(combined_data_validation[[col]], 
                                         breaks = quantile(combined_data_validation[[col]], probs = 0:5 / 5, na.rm = TRUE),
                                         include.lowest = TRUE, 
                                         labels = FALSE)
  combined_data_validation[[col]] <- as.numeric(combined_data_validation[[col]])
}

# Convertir todas las variables a factores
combined_data_validation_factors <- combined_data_validation
combined_data_validation_factors[] <- lapply(combined_data_validation_factors, factor)

# Crear el objeto predictor_data_validation excluyendo la variable objetivo (FPPY) para la predicción
predictor_data_validation <- combined_data_validation_factors
predictor_data_validation$FPPY <- NULL


# Función para calcular la exactitud
calculate_accuracy <- function(predictions, actual) {
  cm <- table(actual, predictions)
  accuracy <- sum(diag(cm)) / sum(cm)
  return(accuracy)
}

# Realizar validación y calcular todas las métricas para cada modelo ajustado
validate_model <- function(model) {
  predictions <- predict(model, node = "FPPY", data = predictor_data_validation)
  metrics <- calculate_performance_metrics(predictions, combined_data_validation_factors$FPPY)
  accuracy <- calculate_accuracy(predictions, combined_data_validation_factors$FPPY)
  metrics$accuracy <- accuracy
  return(metrics)
}

metrics_hard_em_validation <- validate_model(bn_fitted_hard_em)
metrics_exact_validation <- validate_model(bn_fitted_exact)
metrics_bayes_validation <- validate_model(bn_fitted_bayes)

# Crear un dataframe con las métricas de rendimiento para validación
performance_data_validation <- data.frame(
  Method = rep(c("Hard-EM", "Exact", "Bayes"), each = 3),
  Metric = rep(c("Recall", "F1-Score", "Accuracy"), 3),
  Value = c(metrics_hard_em_validation$recall, metrics_hard_em_validation$f1_score, metrics_hard_em_validation$accuracy,
            metrics_exact_validation$recall, metrics_exact_validation$f1_score, metrics_exact_validation$accuracy,
            metrics_bayes_validation$recall, metrics_bayes_validation$f1_score, metrics_bayes_validation$accuracy)
)


# Crear el gráfico de barras para los datos de validación
ggplot(performance_data_validation, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            vjust = -0.3, position = position_dodge(0.9), color = "black") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.box = element_blank(),
    legend.background = element_blank()
  ) +
  scale_fill_manual(
    values = c("white", "gray", "lightblue"),
    guide = guide_legend(title = "Metric")
  ) +
  labs(
    y = "Metric Value",
    x = ""
  ) +
  ylim(0, 0.6)



#### INFERENCIA #####
# Asegúrate de tener el modelo de SEM ajustado al conjunto completo (fit)
fit_full <- sem(model, data=datn)

# Obtener las puntuaciones factoriales para las variables latentes del conjunto completo
factor_scores_full <- lavPredict(fit_full, type = "lv")

# Unir las puntuaciones factoriales con las variables explícitas del conjunto completo
combined_data_full <- cbind(datn, factor_scores_full)

# Convertir las variables latentes en quintiles y luego a formato numérico
for (col in factor_columns) {
  combined_data_full[[col]] <- cut(combined_data_full[[col]], 
                                   breaks = quantile(combined_data_full[[col]], probs = 0:5 / 5, na.rm = TRUE),
                                   include.lowest = TRUE, 
                                   labels = FALSE)
  combined_data_full[[col]] <- as.numeric(combined_data_full[[col]])
}

# Convertir todas las variables a factores
combined_data_full_factors <- combined_data_full
combined_data_full_factors[] <- lapply(combined_data_full_factors, factor)


# Ajuste del modelo bayesiano con el método 'exact'
bn_full_fitted <- bn.fit(bn, data = combined_data_full_factors, method = "hard-em")


# Lista para almacenar las CPTs
cpt_list <- list()
for (i in seq_along(bn_full_fitted)) {
  node_name <- bn_full_fitted[[i]]$node
  cpt_list[[node_name]] <- bn_full_fitted[[i]]$prob
}

#####
# Asumiendo que bn_full_fitted es el modelo bayesiano ajustado
# y que los nombres de los nodos son 'FAGE' y 'Hope_5'

# Obtener la CPT para 'FAGE' dado 'PPYD'
cpt_fage_given_ppyd <- cpt_list[["FAGE"]]

# Obtener la CPT para 'Hope_5' dado 'FAGE'
cpt_hope_5_given_fage <- cpt_list[["Hope_5"]]

# Mostrar las CPTs
str(cpt_fage_given_ppyd)
str(cpt_hope_5_given_fage)

# Asegurarse de que las CPTs están en formato de array o matriz para realizar cálculos
# Las funciones exactas dependerán de cómo las CPTs están almacenadas en tu entorno

# Inicializar un vector para almacenar la probabilidad condicional de Hope_5 dado PPYD
prob_hope_5_given_ppyd <- numeric(length(cpt_hope_5_given_fage))

# Calcular la probabilidad condicional de Hope_5 dado PPYD
for (fage_state in 1:length(cpt_fage_given_ppyd)) {
  prob_hope_5_given_ppyd <- prob_hope_5_given_ppyd + cpt_hope_5_given_fage[, fage_state] * cpt_fage_given_ppyd[fage_state]
}

# Imprimir la probabilidad condicional resultante
print(prob_hope_5_given_ppyd)



#####

# Asumiendo que tienes una lista cpt_list con las CPTs y que FPPY es una de las claves
cpt_fppy <- cpt_list[["FPPY"]]




# Definir la ruta del directorio
directory <- "G:/Mi unidad/NAVARRA/ARTICULOS/REDES BAYESIANAS/FIGURAS/"

# Función para generar y guardar gráficos de contorno para cada nivel de FPPY
generate_contour_plots_FPPY <- function(cpt_fppy, directory) {
  # Crear el directorio si no existe
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  # Crear una grilla para los ejes x e y
  axis_x <- 1:5  # Estados de FEPA
  axis_y <- 1:5  # Estados de FCEN
  
  # Iterar sobre los niveles de FPPY
  for (level_FPPY in 1:5) {
    # Extraer la matriz correspondiente para el nivel actual de FPPY
    probability_matrix <- cpt_fppy[level_FPPY,,]
    
    # Definir el nombre del archivo para guardar el gráfico
    file_name <- paste0(directory, "contour_FPPY_", level_FPPY, ".jpg")
    
    # Crear y guardar el gráfico de contorno lleno de colores
    jpeg(file_name)
    filled.contour(x = axis_x, y = axis_y, z = probability_matrix, nlevels = 20,
                   color.palette = topo.colors,
                   xlab = "Positive Parenting", 
                   ylab = "Climate and Functioning of the School",
                   #main = paste("Personal Positive Youth Development", level_FPPY),
                   plot.axes = {axis(1); axis(2); contour(axis_x, axis_y, probability_matrix, nlevels = 20, add = TRUE)})
    dev.off()
  }
}

# Llamar a la función
generate_contour_plots_FPPY(cpt_fppy, directory)

#### ENTROPIA

# Calculando la entropía de FPPY
entropia_FPPY <- entropy(combined_data_full$FPPY)

# Función para calcular la ganancia de información
ganancia_informacion <- function(variable_latente) {
  entropia_condicional <- condentropy(combined_data_full$FPPY, combined_data_full[[variable_latente]])
  ganancia <- entropia_FPPY - entropia_condicional
  return(ganancia)
}

# Calculando la ganancia de información para cada variable latente
resultados_ganancia <- list()
for (variable in hidden_nodes) {
  gi <- ganancia_informacion(variable)
  resultados_ganancia[[variable]] <- gi
  cat("Ganancia de información de FPPY con", variable, ":", gi, "\n")
}


# Extraer los nombres y categorías de las variables en el data frame
nombres_variables <- names(resultados_ganancia)
valores_ganancia <- unlist(resultados_ganancia)

# Reemplazar las abreviaturas con los nombres completos y asignar categorías
nombres_completos <- sapply(nombres_variables, function(abbr) mapeo_nombres[[abbr]]$nombre)
categorias <- sapply(nombres_variables, function(abbr) mapeo_nombres[[abbr]]$categoria)

# Calculando los porcentajes relativos
max_ganancia <- max(valores_ganancia)
porcentajes_relativos <- (valores_ganancia / max_ganancia) * 100

# Actualizando el data frame con porcentajes relativos
data <- data.frame(Variable = nombres_completos, Porcentaje = porcentajes_relativos, Layer = categorias)

# Ajustar el orden de los niveles del factor basado en 'Porcentaje'
data$Variable <- reorder(data$Variable, -data$Porcentaje)




# Creando la gráfica
ggplot_object <- ggplot(data, aes(x = Variable, y = Porcentaje, fill = Layer)) +
  geom_bar(stat = "identity") +  # Usar 'fill' para el color de las barras
  scale_fill_manual(values = c("1st" = "red", "2nd for PPYD" = "blue", 
                               "2nd for PP" = "yellow", "2nd for CFS" = "green", 
                               "3th for CFS" = "gray")) +  # Define tus colores aquí
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),  # Establecer el color del texto a negro
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black"),  # Color del título del eje X
    axis.text.x = element_text(color = "black"),  # Color del texto del eje X
    axis.text.y = element_text(color = "black"),  # Color del texto del eje Y
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",  # Mover la leyenda a la izquierda
    legend.text = element_text(color = "black")  # Color del texto de la leyenda
  ) +
  ylab("Gain of information based on PPYD (%)")

# Mostrar la gráfica
print(ggplot_object)

#### SANKEY

# Tus datos actualizados sin la capa "2nd for PPYD"
data <- data.frame(
  Source = c("Warmth", "Autonomy", "Humor", "Self-disclosure",
             "Peers", "Activities proffered",
             "Belonging", "Support", 
             "Rules", "Values",
             "Bonds", "Clarity",
             "Climate and Functioning of the School", "Positive Parenting"),
  Target = c(rep("Positive Parenting", 4),
             rep("Climate and Functioning of the School", 2),
             rep("Bonds", 2),
             rep("Clarity", 2),
             "Climate and Functioning of the School", "Climate and Functioning of the School",
             "Personal Positive Youth Development", "Personal Positive Youth Development"),
  Value = c(11.97, 10.97, 11.33, 7.07, 4.08, 1.73, 5.70, 5.09, 3.62, 3.20, 6.95, 5.18, 7.57, 16.03)
)



# Crear un diagrama de Sankey
nodes <- data.frame(name=c(as.character(data$Source), as.character(data$Target)) %>% unique())
data$IDsource <- match(data$Source, nodes$name)-1
data$IDtarget <- match(data$Target, nodes$name)-1

# Ajustar los valores para influir en el orden de los nodos
# Incrementar los valores puede hacer que un nodo se desplace hacia arriba
data$Value[data$Source == "Belonging"] <- data$Value[data$Source == "Belonging"] + 10
data$Value[data$Source == "Warmth"] <- data$Value[data$Source == "Warmth"] + 10


# Crear la red Sankey
sankeyNetwork(Links = data, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Value", NodeID = "name",
              units = "Percentage", fontSize = 12, nodeWidth = 30)


#### figura de probabilidades condicionales
# Función para crear y guardar gráficos de CPT simplificados
procesar_y_graficar_simplificado <- function(bn_model, variables, parent_var_name, parent_var_label, directory) {
  # Crear lista de CPT
  cpt_list <- setNames(lapply(variables, function(var) bn_model[[var]]$prob), variables)
  
  cpt_list
  # Convertir CPT en data frame en formato largo y usar nombres completos
  df_long <- do.call("rbind", lapply(names(cpt_list), function(var_name) {
    df <- as.data.frame(as.table(cpt_list[[var_name]]))
    colnames(df) <- c("Level", parent_var_label, "Probability")
    df$Variable <- mapeo_nombres[[var_name]]$nombre  # Usar nombres completos
    return(df)
  }))
  
  # Asegúrate de que 'Level' es un factor
  df_long$Level <- as.factor(df_long$Level)
  
  # Crear y guardar gráficos simplificados
  for (i in 1:5) {
    df_subset <- subset(df_long, get(parent_var_label) == i)
    p <- ggplot(df_subset, aes(x = Variable, y = Probability, fill = Level)) +
      geom_bar(stat = "identity", position = position_dodge(), color = "black") +
      scale_fill_manual(values = c("1" = "white", "2" = "gray", "3" = "lightblue", "4" = "blue", "5" = "darkblue")) +
      labs(title = paste("Conditional probability for", parent_var_label, i),
           x = "", y = "Probability") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "bottom",
        legend.box = element_blank(),
        legend.background = element_blank()
      )
    
    # Guardar la gráfica en el directorio especificado
    ggsave(filename = paste0(directory, "CPT_", parent_var_label, "_Level_", i, ".png"), plot = p, width = 8, height = 6)
  }
}

# Directorio para guardar las gráficas
directory <- "G:/Mi unidad/NAVARRA/ARTICULOS/REDES BAYESIANAS/FIGURAS/"

# Llamar a la función con diferentes conjuntos de variables
procesar_y_graficar_simplificado(bn_full_fitted, c("FAFE", "FAUT", "FHUM", "FREV"), "FEPA", "PP", directory)
procesar_y_graficar_simplificado(bn_full_fitted, c("FIGU", "FVIN", "FOFE", "FCLA"), "FCEN", "PCFS", directory)
procesar_y_graficar_simplificado(bn_full_fitted, c("FOPT", "FPES", "FAEF", "FAGE", "FCOM", "FMAN", "FSIG"), "FPPY", "PPYD", directory)
