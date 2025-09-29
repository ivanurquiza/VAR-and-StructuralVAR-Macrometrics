remove(list = ls(all.names = TRUE))
options(warn = -1)

library(readxl)
library(dplyr)
library(seasonal)
library(ggplot2)
library(tidyr)
library(forecast)
library(tseries)
library(tibble)
library(vars)
library(ggthemes)  
#library(showtext)

# Fuente Google elegante
#font_add_google("Lato", "lato")
#showtext_auto()

# Setting del directorio y upload de nuestra base

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

datos <- read_excel("rawdb.xlsx", sheet = 1, skip = 5)

colnames(datos)[1] <- "fecha"

datos$fecha <- as.Date(datos$fecha, origin = "1899-12-30")

names(datos)
head(datos)

################################Pasamos las variables a TS#################################
vars <- c("actividad_chi", "ipc_chi", "tcn_chi", "embi_chi", "tpm_chi",
          "actividad_mex", "ipc_mex", "tcn_mex", "embi_mex", "tpm_mex")

series_ts <- list()

# convertimos a ts
for (var in vars) {
  series_ts[[var]] <- ts(datos[[var]], start = c(2002, 1), frequency = 12)
}

#######################Ya con formato TS desestacionalizo #############################

vars_sa <- c("actividad_chi", "ipc_chi", "actividad_mex", "ipc_mex") #Variables a desestacionalizar

for (var in vars_sa) {
  serie_ts <- ts(datos[[var]], start = c(2002, 1), frequency = 12)
  desest <- seas(serie_ts)
  ajuste <- final(desest)
  
  ajuste_completo <- c(ajuste, rep(NA, nrow(datos) - length(ajuste)))
  
  datos[[paste0(var, "_sa")]] <- ajuste_completo
}



#Tomamos log y multiplicamos por 100 para algunas variables
vars_log <- c("actividad_chi_sa", "actividad_mex_sa", 
              "ipc_chi_sa", "ipc_mex_sa", 
              "tcn_chi", "tcn_mex")

for (var in vars_log) {
  datos[[paste0(var, "_log")]] <- 100 * log(datos[[var]])
}

# Lista de las variables finales
vars_total <- c("embi_chi", "embi_mex", "tpm_chi", "tpm_mex", 
                "tcn_chi_log", "tcn_mex_log", 
                "actividad_chi_sa_log", "actividad_mex_sa_log", 
                "ipc_chi_sa_log", "ipc_mex_sa_log")


# Armamos diferencias mediante test ADF iterado -hasta el rechazo de H0-
  
  # Inicializar tabla de resultados
  resultado_ndiffs <- data.frame(
    variable = vars_total,
    p_value = NA_real_,
    diferencias = NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # Armamos lista para las series
  serie_ts_final <- list()
  
  # Bucle para cada serie
  for (var in vars_total) {
    serie <- ts(datos[[var]], start = c(2002, 1), frequency = 12)
    serie <- na.omit(serie)
    
    d <- 0
    p_val <- 1
    
    # Test ADF y diferenciación sucesiva
    while (p_val >= 0.05 && d <= 2) {
      test <- tryCatch(adf.test(serie), error = function(e) NULL)
      
      if (!is.null(test)) {
        p_val <- test$p.value
        if (p_val < 0.05) break
      }
      
      serie <- diff(serie)
      d <- d + 1
    }
    
    resultado_ndiffs$p_value[resultado_ndiffs$variable == var] <- p_val
    resultado_ndiffs$diferencias[resultado_ndiffs$variable == var] <- d
    serie_ts_final[[var]] <- serie
  }

  resultado_ndiffs
  
#Nos quedamos con la primera diff para el ipc  
adf.test(diff(datos$ipc_chi_sa_log))
  
serie_ts_final$ipc_chi_sa_log <-  diff(ts(datos$ipc_chi_sa_log, start = c(2002, 1), frequency = 12))
serie_ts_final$ipc_mex_sa_log <-  diff(ts(datos$ipc_mex_sa_log, start = c(2002, 1), frequency = 12))


# Unimos todas las series transformadas en una matriz
mts <- do.call(cbind, serie_ts_final)
mts <- mts[complete.cases(mts), ]


#-----------------------------------------------------
# Gráficos series locales

# Convertimos mts a data frame
mts_df <- as.data.frame(mts)
mts_df$fecha <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(mts_df))

# Agregamos una columna que indica país según el nombre de la variable
mts_long <- mts_df %>%
  pivot_longer(-fecha, names_to = "variable", values_to = "valor") %>%
  mutate(pais = case_when(
    grepl("_chi", variable) ~ "Chile",
    grepl("_mex", variable) ~ "México",
    TRUE ~ "Otro"
  ))

# Filtramos por país y graficamos las series

# Todas 
ggplot(mts_long, aes(x = fecha, y = valor)) +
  geom_line(color = "#0072B2", size = 1.2, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +  # Solo 2 columnas para paneles más grandes
  labs(
    title = "Series Diferenciadas",
    subtitle = "Transformadas para estacionariedad (ADF Test)",
    x = "Fecha", y = NULL
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    axis.text = element_text(size = 11, color = "gray30"),
    axis.title.x = element_text(size = 12, margin = margin(t = 6)),
    panel.spacing = unit(1.2, "lines"),  # Más espacio entre paneles
    plot.margin = margin(10, 20, 10, 20)
  )

# México

ggplot(filter(mts_long, pais == "México"), aes(x = fecha, y = valor)) +
  geom_line(color = "#009E73", size = 1.2, alpha = 0.9) +  # Un naranja elegante para México
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +  # 2 columnas para paneles más grandes
  labs(
    title = "Series Diferenciadas - México",
    subtitle = "Transformadas para estacionariedad (ADF Test)",
    x = "Fecha", y = NULL
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    axis.text = element_text(size = 11, color = "gray30"),
    axis.title.x = element_text(size = 12, margin = margin(t = 6)),
    panel.spacing = unit(1.2, "lines"),  # Más espacio entre paneles
    plot.margin = margin(10, 20, 10, 20)
  )

#Chile

ggplot(filter(mts_long, pais == "Chile"), aes(x = fecha, y = valor)) +
  geom_line(color = "#CF010B", size = 1.2, alpha = 0.9) +  # Verde sobrio
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +    # 2 columnas para paneles grandes
  labs(
    title = "Series Diferenciadas - Chile",
    subtitle = "Transformadas para estacionariedad (ADF Test)",
    x = "Fecha", y = NULL
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    axis.text = element_text(size = 11, color = "gray30"),
    axis.title.x = element_text(size = 12, margin = margin(t = 6)),
    panel.spacing = unit(1.2, "lines"),  # Más espacio entre paneles
    plot.margin = margin(10, 20, 10, 20)
  )


# ---------  VAR reducido chile -----------------------

# Seleccionamos nombres de columnas
vars_chile <- grep("_chi", colnames(mts), value = TRUE)
vars_mexico <- grep("_mex", colnames(mts), value = TRUE)

# Crear nuevos mts para cada país
mts_chile <- mts[, vars_chile]
mts_mexico <- mts[, vars_mexico]

#Orden para el VAR estructural -chequear-

orden_var <- c("actividad_chi_sa_log", "ipc_chi_sa_log", "tpm_chi", "embi_chi", "tcn_chi_log")

Y <- mts_chile[, orden_var]

# Lag Order Selection
pmax <- 12 # Maximum lag order

popt <- VARselect(Y, lag.max = pmax, type = "const")
popt

p <- popt$selection[2] # Elegimos HQIC (For log-levels series, see Kilian & L?tkepohl, pp. 373), p > 1 neccesary
p
# Estimation
Y <- Y[(pmax - p + 1):nrow(Y), ] # Para quitar las filas que no tienen obs por los rezagos

VAR <- VAR(Y, p = p, type = "const")
summary(VAR)

m <- VAR$K # Number of variables in the VAR
T <- VAR$obs # Number of effective sample observations, excluding "p" starting values
#plot(VAR)

# Ad hoc Function
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  Cm
}

# Simplification (no feedback from local variables to PCOM) No tenemos ninguna variable exógena
#constraints <- matC(m, p, 1)
#VAR <- restrict(VAR, method = "man", resmat = constraints)



# Model Checking

roots(VAR, modulus = TRUE) #Dan todos menor a 1


# Tenemos problemita con autocorrelación!! (se encarga gorki)

# Breusch-Godfrey Test (ajustado)
h.BG <-6

serial.test(VAR, lags.bg = h.BG, type = "BG") #Chequear esto porque no estoy rechazando

#Portmanteau Test
h.PT <-6

serial.test(VAR, lags.pt = h.PT, type = "PT.asymptotic")

serial.test(VAR, lags.pt = h.PT, type = "PT.adjusted")

# Multivariate Jarque-Bera Test
normality.test(VAR, multivariate.only = FALSE)


# VAR estructural ---------------------------------------------------------
# A Matrix
Amat <- function(m){
  
  Amat <<- diag(m)
  for (i in 2:m) {
    for (j in 1:(i - 1)) {
      Amat[i, j] <- NA
    }
  }
  return(Amat)
}

# B Matrix
Bmat <- function(m){
  Bmat <<- matrix(0, m, m)
  for (i in 1:m) {
    Bmat[i, i] <- NA
  }
  return(Bmat)
}

Amat <-Amat(m)
Bmat <-Bmat(m)

# SVAR estimation (AB model configuration)
SVAR <- SVAR(VAR, Amat = Amat, Bmat = Bmat, lrtest = FALSE, max.iter = 1000)
SVAR

# Structural IRF (manera vieja?)
irf_one_chi <- irf(SVAR, response = "actividad_chi_sa_log", impulse = "tpm_chi", 
               n.ahead = 10, ortho = TRUE, boot = TRUE)

#---Plots IRFs---
irf_matrix <- irf_one_chi$irf$tpm_chi  # matriz de respuestas al impulso tpm_chi

# Construir dataframe
irf_df_chi <- data.frame(
  period = 0:(nrow(irf_matrix)-1),  # horizontes
  response = irf_matrix[, "actividad_chi_sa_log"],  # columna de respuesta
  lower = irf_one_chi$Lower$tpm_chi[, "actividad_chi_sa_log"],
  upper = irf_one_chi$Upper$tpm_chi[, "actividad_chi_sa_log"]
)


# Chile
ggplot(irf_df_chi, aes(x = period, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "#1B4F72", alpha = 0.15) +  # Banda azul oscuro muy tenue
  geom_line(color = "#0B3C5D", size = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  labs(
    title = "Respuesta Impulso-Respuesta (IRF) para Chile",
    subtitle = "Respuesta de la tasa de cambio de la actividad a un shock en la tasa de política monetaria",
    x = "Horizonte (meses)", y = "Cambio en la actividad"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.title = element_text(size = 13)
  )


#Forecast Error Variance Decomposition

#SVARfevd <- fevd(SVAR, n.ahead = 20)
#plot(SVARfevd)

#Matriz P
# SVAR Impact Matrix (Cholesky decomposition)
S <- t(resid(VAR)) %*% resid(VAR) / (T - m * p - 1)

S #Tenemos estimador varianza

P.chol <- t(chol(S))


# SVAR Impact Matrix (implied by AB model)
P <- solve(SVAR$A, SVAR$B) # inv(A) %*% B
S.SVAR <- P %*% t(P)
S.SVAR
#Ambas formas dan muy parecido

# Other SVAR Parameters, esto creo que no es necesario
pars.R <- Bcoef(VAR) # Reduced Form VAR
pars.S <- solve(SVAR$A, pars.R) # Structural Form VAR

# SVAR Analysis #

source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")


H <- 24 # Horizon
H.ERPT <- 240 # Horizon (ERPT)

# IRF
IRF <- SVAR.sirf(SVAR, H)
#plot.sirf(IRF, m, H)

#png("IRF_plot.png", width = 1200, height = 800)
#plot.sirf(IRF, m, H)
#dev.off()

#-------------- IRFs -------------------

# Cambio nombres
shocks <- c("ACTIVIDAD", "IPC", "TPM_CHI", "EMBI", "TCN")
dimnames(IRF)[[2]] <- shocks

# Armamos el dataframe por maleabilidad, porque el objeto IRF está anidado
irf_df <- as.data.frame.table(IRF, responseName = "valor") %>%
  rename(variable = Var1, shock_actual = Var2, periodo = Var3) %>%
  mutate(periodo = as.numeric(as.character(periodo))) %>%
  filter(shock_actual == "TPM_CHI")

head(irf_df)

# IRF para todas las variables, con shock de TPM -1 std?, check!!-
ggplot(irf_df, aes(x = periodo, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", size = 0.8) +
  geom_line(color = "#1B4F72", size = 1.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "IRF: Respuesta de las variables a un shock en TPM_CHI",
    x = "Horizonte (meses)", y = "Respuesta"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank()
  )

# IRF Acumulada 
IRF.c <- SVAR.sirf(SVAR, H, cumulative = TRUE)
#plot.sirf(IRF.c, m, H)

# FEVD (Descomposicion de la varianza)
FEVD <- SVAR.fevd(SVAR, H)

#para distinguir las variables en shocks
dimnames(FEVD)[[2]] <- c("ACTIVIDAD", "IPC", "TPM_CHI", "EMBI", "TCN")


plot.fevd(FEVD, m, H)


# HD (Descomposicion historica)
HD <- SVAR.hd(SVAR)
plot.hd(Y, HD, m)

# Bootstrap Inference ####
R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

# COMMENT ON THE IMPORTANCE OF MULTIVARIATE CONFIDENCE INTERVALS 

#--- Sobreescribimos la función de boot.replicate porque el del source
# no me tira el return (en vez de tocarlo ahí, pisamos la función)
boot.estimate <- function(var.names, Y, m, p, lag.max, ic, resmat = NULL) {
  colnames(Y) <- var.names
  VAR <- VAR(Y, p = p, type = "const")
  if (!is.null(resmat)) {
    VAR <- restrict(VAR, method = "man", resmat = resmat)
  }
  return(VAR)
}
#--------------------------------------------------------------------


# Bootstrap Replications
Y.boot <- boot.replicate(VAR, R, type)

# IRF (Bootstrap)
IRF.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot)
#plot.sirf.boot(IRF.boot, m, H)

# Cumulative IRF (Bootstrap)
IRF.c.boot <- SVAR.sirf.boot(SVAR, Amat, Bmat, H, gamma, Y.boot, cumulative = TRUE)
#plot.sirf.boot(IRF.c.boot, m, H)

#------------- Gráficos con intervalos -------------------------

# IRF puntual t

# Convertimos a dataframe, el objeto está anidado así que lo limpiamos
shocks <- c("ACTIVIDAD", "IPC", "TPM_CHI", "EMBI", "TCN")
dimnames(IRF.boot$pe)[[2]] <- shocks
dimnames(IRF.boot$lb)[[2]] <- shocks
dimnames(IRF.boot$ub)[[2]] <- shocks

irf_df_boot <- as.data.frame.table(IRF.boot$pe, responseName = "valor") %>%
  rename(variable = Var1, shock = Var2, periodo = Var3) %>%
  mutate(
    periodo = as.numeric(as.character(periodo)),
    lb = as.vector(IRF.boot$lb),
    ub = as.vector(IRF.boot$ub)
  ) %>%
  filter(shock == "TPM_CHI")  # solo respuestas al shock de política monetaria

ggplot(irf_df_boot, aes(x = periodo, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#1B4F72", alpha = 0.2) +
  geom_line(color = "#1B4F72", size = 1.1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "IRF con Bootstrap: Respuesta de las variables a un shock en TPM_CHI",
    x = "Horizonte (meses)", y = "Respuesta"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(size = 0.3, color = "gray85"),
    panel.grid.minor = element_blank()
  )


# IRF acumuladas
dimnames(IRF.c.boot$pe)[[2]] <- shocks
dimnames(IRF.c.boot$lb)[[2]] <- shocks
dimnames(IRF.c.boot$ub)[[2]] <- shocks

irf_df_acc <- as.data.frame.table(IRF.c.boot$pe, responseName = "valor") %>%
  rename(variable = Var1, shock = Var2, periodo = Var3) %>%
  mutate(
    periodo = as.numeric(as.character(periodo)),
    lb = as.vector(IRF.c.boot$lb),
    ub = as.vector(IRF.c.boot$ub)
  ) %>%
  filter(shock == "TPM_CHI")


ggplot(irf_df_acc, aes(x = periodo, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", size = 0.8) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#1B4F72", alpha = 0.2) +
  geom_line(color = "#1B4F72", size = 1.1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "IRF acumulada bootstrap: Respuesta a un shock en TPM_CHI",
    x = "Horizonte (meses)", y = "Respuesta acumulada"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank()
  )


#----------------------------------------------------


#Chile con variables internacionales---------------------------------------

# VAR reducido chile + inter ---------------------------------------------------
# Desestacionalización -> Tomamos datos desestacionalizados ya desde las fuentes

datos <- na.omit(datos)


vars_sa_inter <- c("ipc_usa", "tot_chi","tot_mex")

for (var in vars_sa_inter) {
  serie_ts <- ts(datos[[var]], start = c(2002, 1), frequency = 12)
 
  datos[[paste0(var, "_sa")]] <- serie_ts
}


# logs x 100

vars_log_inter <- c("ipc_usa_sa", "tot_chi_sa", "tot_mex_sa")

for (var in vars_log_inter) {
  datos[[paste0(var, "_log")]] <- 100 * log(datos[[var]])
}


# Lista actual 
vars_inter <- c("actividad_global","ipc_usa_sa_log", "tot_chi_sa_log","tot_mex_sa_log", "ebp", "shadow_ffr")

# Estacionariedad variables internacionales

# Tabla de resultados para internacionales
resultado_ndiffs_inter <- data.frame(
  variable = vars_inter,
  p_value = NA_real_,
  diferencias = NA_integer_,
  stringsAsFactors = FALSE
)

# Lista para guardar series transformadas
serie_ts_inter <- list()

# Bucle ADF iterado
for (var in vars_inter) {
  serie <- ts(datos[[var]], start = c(2002, 1), frequency = 12)
  serie <- na.omit(serie)
  
  d <- 0
  p_val <- 1
  
  while (p_val >= 0.05 && d <= 2) {
    test <- tryCatch(adf.test(serie), error = function(e) NULL)
    
    if (!is.null(test)) {
      p_val <- test$p.value
      if (p_val < 0.05) break
    }
    
    serie <- diff(serie)
    d <- d + 1
  }
  
  resultado_ndiffs_inter$p_value[resultado_ndiffs_inter$variable == var] <- p_val
  resultado_ndiffs_inter$diferencias[resultado_ndiffs_inter$variable == var] <- d
  serie_ts_inter[[var]] <- serie
}

# Mirás los resultados
resultado_ndiffs_inter


# Por la configuración de la serie índice de actividad_global de Kilian,
# chequeamos individualmente el test

adf.test(datos$actividad_global)

adf.test(diff(datos$actividad_global))

serie_ts_inter$actividad_global <- datos$actividad_global

# -> Nos quedamos con el índice sin diferenciar

# Mismo check para shadow_ffr,
# chequeamos individualmente el test
adf.test(datos$shadow_ffr)

adf.test(diff(datos$shadow_ffr))

#Aquí la serie es claramente no estacionaria, entonces sí procedemos con diff


#TOT

adf.test(datos$tot_chi_sa_log )

adf.test(diff(datos$tot_chi_sa_log ))

#2 DIFERENCIAS.



# Plots series internacionales transformadas

# Convertimos la lista en una matriz multivariada de series
mts_inter <- do.call(cbind, serie_ts_inter)
mts_inter <- mts_inter[complete.cases(mts_inter), ]

# Lo pasamos a data frame y agregamos fecha
mts_inter_df <- as.data.frame(mts_inter)
mts_inter_df$fecha <- seq(as.Date("2002-01-01"), by = "month", length.out = nrow(mts_inter_df))

# Reorganizamos a formato largo para ggplot
mts_inter_long <- mts_inter_df %>%
  pivot_longer(-fecha, names_to = "variable", values_to = "valor")

#GRAFICO
ggplot(mts_inter_long, aes(x = fecha, y = valor)) +
  geom_line(color = "#3333FF", size = 1.1, alpha = 0.9) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Series Internacionales",
    subtitle = "Diferenciadas hasta lograr estacionariedad (ADF Test)",
    x = "Fecha", y = NULL
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11, color = "gray30"),
    axis.title.x = element_text(size = 12),
    panel.spacing = unit(1.2, "lines")
  )





vars_inter <- c("actividad_global","ipc_usa_sa_log", "tot_chi_sa_log","tot_mex_sa_log", "ebp", "shadow_ffr")

orden_var_int_chi <- c("actividad_global", "ipc_usa_sa_log", "shadow_ffr" ,"ebp",
                   
                   "tot_chi_sa_log","actividad_chi_sa_log", 
                   
                   "ipc_chi_sa_log", "tpm_chi", "embi_chi", "tcn_chi_log")


mts_chile_ <-mts_chile[1:(nrow(mts_inter)), ]

#mts_inter_ <-mts_inter[1:(nrow(mts_chile)), ]

mts_chile_int <- cbind(mts_chile_,mts_inter)

Y_inter_chi <- mts_chile_int[, orden_var_int_chi]

# Selección del orden de rezagos
pmax <- 12
popt_inter_chi <- VARselect(Y_inter_chi, lag.max = pmax, type = "const")
popt_inter_chi


p_inter_chi <- popt_inter_chi$selection[2]  # HQIC

#Estimacion VAR reducido chile con interncionales
Y_inter_chi <- Y_inter_chi[(pmax - p_inter_chi + 1):nrow(Y_inter_chi), ]
VAR_inter_chi <- VAR(Y_inter_chi, p = p_inter_chi, type = "const")
summary(VAR_inter_chi)


# RESTRICCIÓN SOBRE COEF DE REZAGOS!


matC_custom <- function(m, p, bloque_exog) {
  
  # bloque_exog: índices de las variables que deben ser exógenas
  
  bloque_endo <- setdiff(1:m, bloque_exog)
  Cm <- matrix(1, m, m * p + 1)  # 1 = libre
  
  for (i in bloque_exog) {
    for (l in 1:p) {
      for (j in bloque_endo) {
        Cm[i, m * (l - 1) + j] <- 0  # Bloquear feedback de endógenas a exógenas
      }
    }
  }
  return(Cm)
}


m <- ncol(Y_inter_chi)
p <- p_inter_chi

# Las primeras 6 variables son internacionales
bloque_exog <- 1:5

# Generamos matriz de restricciones
constraints <- matC_custom(m, p, bloque_exog)

# Aplicamos restricciones al VAR
VAR_inter_chi_restrict <- restrict(VAR_inter_chi, method = "man", resmat = constraints)

summary(VAR_inter_chi_restrict)

m_inter_chi_restrict <- VAR_inter_chi$K
T_inter_chi_restrict <- VAR_inter_chi$obs

roots(VAR_inter_chi_restrict, modulus = TRUE)

serial.test(VAR_inter_chi_restrict, lags.bg = 12, type = "ES")


# VAR estructural chile + inter ---------------------------------------------------

# Matrices A y B
Amat_inter_chi <- function(m){
  Amat <<- diag(m)
  for (i in 2:m) {
    for (j in 1:(i - 1)) {
      Amat[i, j] <- NA
    }
  }
  return(Amat)
}

Bmat_inter_chi <- function(m){
  Bmat <<- matrix(0, m, m)
  for (i in 1:m) {
    Bmat[i, i] <- NA
  }
  return(Bmat)
}

Amat_inter_chi <- Amat_inter_chi(m_inter_chi_restrict)
Bmat_inter_chi <- Bmat_inter_chi(m_inter_chi_restrict)

# Estimación SVAR con variables internacionales
SVAR_inter_chi <- SVAR(VAR_inter_chi_restrict, Amat = Amat_inter_chi, Bmat = Bmat_inter_chi, lrtest = FALSE, max.iter = 1000)
SVAR_inter_chi

# Análisis SVAR inter ------------------------------------------------------


source("PS2_SVAR_Tools.R")
source("PS2_SVAR_Plots.R")

H <- 24

# IRF
IRF_inter_chi <- SVAR.sirf(SVAR_inter_chi, H)
#plot.sirf(IRF_inter_chi, m_inter_chi, H)

# IRF acumulada
IRF.c_inter_chi <- SVAR.sirf(SVAR_inter_chi, H, cumulative = TRUE)
#plot.sirf(IRF.c_inter_chi, m_inter_chi, H)

# FEVD
FEVD_inter_chi <- SVAR.fevd(SVAR_inter_chi, H)
#plot.fevd(FEVD_inter_chi, m_inter_chi, H)

# HD
#HD_inter_chi <- SVAR.hd(SVAR_inter_chi)
#plot.hd(Y_inter_chi, HD_inter_chi, m_inter_chi)


# Gráficos Chile Internacional

#--------- IRF Chile Internacional ---------------

# Structural IRF (individual)
irf_one_chi_int <- irf(SVAR_inter_chi, response = "actividad_chi_sa_log", impulse = "tpm_chi", 
                   n.ahead = 10, ortho = TRUE, boot = TRUE)

#---Plots IRFs---
irf_matrix_chi_int <- irf_one_chi_int$irf$tpm_chi  # matriz de respuestas al impulso tpm_chi

# Construir dataframe
irf_df_chi_int <- data.frame(
  period = 0:(nrow(irf_matrix_chi_int)-1),  # horizontes
  response = irf_matrix_chi_int[, "actividad_chi_sa_log"],  # columna de respuesta
  lower = irf_one_chi_int$Lower$tpm_chi[, "actividad_chi_sa_log"],
  upper = irf_one_chi_int$Upper$tpm_chi[, "actividad_chi_sa_log"]
)


# Chile con internacionales

ggplot(irf_df_chi_int, aes(x = period, y = response)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "#1B4F72", alpha = 0.15) +
  geom_line(color = "#0B3C5D", size = 1.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  labs(
    title = "Función Impulso-Respuesta (IRF) para Chile con variables internacionales",
    subtitle = "Respuesta de la tasa de cambio de la actividad a un shock en la tasa de política monetaria",
    x = "Horizonte (meses)", y = "Cambio en la actividad"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, margin = margin(b = 10)),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.title = element_text(size = 13)
  )



# Bootstrap Inference ####
R <- 500 # Number of bootstrap replications
type <- "nonparametric"
gamma <- 0.95 # Confidence level

# COMMENT ON THE IMPORTANCE OF MULTIVARIATE CONFIDENCE INTERVALS 

#--- Sobreescribimos la función de boot.replicate porque el del source
# no me tira el return (en vez de tocarlo ahí, pisamos la función)
boot.estimate <- function(var.names, Y, m, p, lag.max, ic, resmat = NULL) {
  colnames(Y) <- var.names
  VAR <- VAR(Y, p = p, type = "const")
  if (!is.null(resmat)) {
    VAR <- restrict(VAR, method = "man", resmat = resmat)
  }
  return(VAR)
}
#--------------------------------------------------------------------


# Bootstrap Replications
Y.boot <- boot.replicate(VAR_inter_chi_restrict, R, type)

# IRF (Bootstrap)
IRF.boot <- SVAR.sirf.boot(SVAR_inter_chi, Amat_inter_chi, Bmat_inter_chi, H, gamma, Y.boot)
#plot.sirf.boot(IRF.boot, m, H)

# Cumulative IRF (Bootstrap)
IRF.c.boot <- SVAR.sirf.boot(SVAR_inter_chi, Amat_inter_chi, Bmat_inter_chi, H, gamma, Y.boot, cumulative = TRUE)
#plot.sirf.boot(IRF.c.boot, m, H)

#------------- Gráficos con intervalos -------------------------

# IRF puntual t

# Convertimos a dataframe, el objeto está anidado así que lo limpiamos
shocks <- c("1","2","3","4","5", "ACTIVIDAD", "IPC", "TPM_CHI", "EMBI", "TCN")
dimnames(IRF.boot$pe)[[2]] <- shocks
dimnames(IRF.boot$lb)[[2]] <- shocks
dimnames(IRF.boot$ub)[[2]] <- shocks

# Filtramos y armamos el dataframe solo con las variables de la 6 en adelante
variables_mostrar <- dimnames(IRF.boot$pe)[[1]][6:length(dimnames(IRF.boot$pe)[[1]])]

irf_df_boot <- as.data.frame.table(IRF.boot$pe, responseName = "valor") %>%
  rename(variable = Var1, shock = Var2, periodo = Var3) %>%
  mutate(
    periodo = as.numeric(as.character(periodo)),
    lb = as.vector(IRF.boot$lb),
    ub = as.vector(IRF.boot$ub)
  ) %>%
  filter(
    shock == "TPM_CHI",
    variable %in% variables_mostrar
  )

# Graficamos solo las respuestas de las variables seleccionadas
ggplot(irf_df_boot, aes(x = periodo, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#1B4F72", alpha = 0.2) +
  geom_line(color = "#1B4F72", size = 1.1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "IRF con Bootstrap: Respuesta de las variables a un shock en TPM_CHI",
    x = "Horizonte (meses)", y = "Respuesta"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(size = 0.3, color = "gray85"),
    panel.grid.minor = element_blank()
  )


# IRF acumuladas
dimnames(IRF.c.boot$pe)[[2]] <- shocks
dimnames(IRF.c.boot$lb)[[2]] <- shocks
dimnames(IRF.c.boot$ub)[[2]] <- shocks

irf_df_acc <- as.data.frame.table(IRF.c.boot$pe, responseName = "valor") %>%
  rename(variable = Var1, shock = Var2, periodo = Var3) %>%
  mutate(
    periodo = as.numeric(as.character(periodo)),
    lb = as.vector(IRF.c.boot$lb),
    ub = as.vector(IRF.c.boot$ub)
  ) %>%
  filter(shock == "TPM_CHI")


ggplot(irf_df_acc, aes(x = periodo, y = valor)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", size = 0.8) +
  geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#1B4F72", alpha = 0.2) +
  geom_line(color = "#1B4F72", size = 1.1) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  labs(
    title = "IRF acumulada: Respuesta a un shock en TPM_CHI",
    x = "Horizonte (meses)", y = "Respuesta acumulada"
  ) +
  theme_minimal(base_family = "lato") +
  theme(
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray85", size = 0.3),
    panel.grid.minor = element_blank()
  )





