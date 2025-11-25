library(psych)

vars <- c(
  "dist_autoservicio",
  "dist_departamental",
  "dist_plaza",
  "HubDist",
  "Cobertura",
  "BA1_NoEqSa",
  "BA8_NoMerc",
  "P_PorcElec",
  "P_AguaPot",
  "AlumP_sum"
)

library(tidyverse)
library(ggplot2)
library(glue)
library(cowplot)
library(psych)   # para tabla de correlaciones bonita

# ---------------------------
# VARIABLES A ANALIZAR
# ---------------------------
vars <- c(
  "dist_autoservicio",
  "dist_departamental",
  "dist_plaza",
  "HubDist",
  "Cobertura",
  "BA1_NoEqSa",
  "BA8_NoMerc",
  "P_PorcElec",
  "P_AguaPot",
  "AlumP_sum"
)

# ---------------------------
# FUNCION PARA GRAFICAR
# ---------------------------
make_scatter <- function(df, var, y_var, logx = FALSE, save_name = "") {
  
  if (logx) {
    p <- ggplot(df, aes_string(x = glue("log10({y_var})"), y = var)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = glue("{var} vs log10({y_var})"),
        x = glue("log10({y_var})"),
        y = var
      )
  } else {
    p <- ggplot(df, aes_string(x = y_var, y = var)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", color = "red") +
      theme_minimal() +
      labs(
        title = glue("{var} vs {y_var}"),
        x = y_var,
        y = var
      )
  }
  
  # versión facetada
  p_facet <- p + facet_wrap(~room_type)
  
  # guardar
  ggsave(glue("{save_name}_{var}.png"), p_facet, width = 10, height = 6)
  
  return(list(normal = p, facet = p_facet))
}

# ---------------------------
# TABLAS DE CORRELACIÓN
# ---------------------------
make_corr_table <- function(df, var, y_vars, save_name = "") {
  
  sub <- df |>
    select(all_of(c(var, y_vars))) |>
    drop_na()
  
  corr <- psych::corr.test(sub)
  
  # guardar tabla como imagen
  png(glue("{save_name}_{var}_corr.png"), width = 1200, height = 1000)
  corrplot::corrplot(cor(sub), method = "number", type = "upper")
  dev.off()
  
  return(corr)
}


# ---------------------------
# LOOP PRINCIPAL
# ---------------------------

# revenue → usar log10
# occupancy → sin log
for (v in vars) {
  
  # 1) Scatter vs REVENUE
  make_scatter(
    df = completo_distancias,
    var = v,
    y_var = "estimated_revenue_l365d",
    logx = TRUE,
    save_name = "revenue"
  )
  
  # 2) Scatter vs OCUPACIÓN
  make_scatter(
    df = completo_distancias,
    var = v,
    y_var = "estimated_occupancy_l365d",
    logx = FALSE,
    save_name = "occupancy"
  )
  
  # 3) Tabla de correlaciones
  make_corr_table(
    df = completo_distancias,
    var = v,
    y_vars = c("estimated_revenue_l365d", "estimated_occupancy_l365d"),
    save_name = "corr"
  )
}

####
#
#
#
#
#
###


library(reshape2)

completo_distancias <- completo_distancias |>
  mutate(
    "log10(estimated_revenue_l365d)" = log10(estimated_revenue_l365d+0.001)
  )

# Variables territoriales a analizar
vars <- c(
  "dist_autoservicio", "dist_departamental", "dist_plaza",
  "HubDist", "Cobertura", "BA1_NoEqSa", "BA8_NoMerc",
  "P_PorcElec", "P_AguaPot", "AlumP_sum"
)

# Subconjunto de datos
vars_all <- c(vars, "log10(estimated_revenue_l365d)", "estimated_occupancy_l365d")

# Matriz de correlaciones
corr <- cor(completo_distancias[vars_all], use = "pairwise.complete.obs")

# Seleccionar solo correlación con revenue y ocupación
corr_sub <- corr[vars, c("log10(estimated_revenue_l365d)", "estimated_occupancy_l365d")]

# Derretir matriz para ggplot
melted <- melt(corr_sub)

# Heatmap
ggplot(melted, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", high = "blue", mid = "white",
    midpoint = 0, limits = c(-1, 1)
  ) +
  geom_text(aes(label = round(value, 2)), size = 4) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Correlaciones entre variables territoriales
y desempeño de Airbnb (Ingreso y Ocupación)",
    subtitle = "Elaborado con diversos datos del GOBCDMX",
    x = "",
    y = "",
    caption = "Elaboración propia"
  ) +
  theme(
  
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  )

#####

library(broom)
library(purrr)

completo_distancias <- completo_distancias |>
  mutate(
    revenue_log = ifelse(estimated_revenue_l365d > 0,
                         log10(estimated_revenue_l365d),
                         NA_real_)

    completo_distancias <- completo_distancias |>
      mutate(
        revenue_log = ifelse(estimated_revenue_l365d > 0,
                             log10(estimated_revenue_l365d),
                             NA_real_)
      )
    
    # Variables territoriales
    vars <- c(
      "dist_autoservicio", "dist_departamental", "dist_plaza",
      "HubDist", "Cobertura", "BA1_NoEqSa", "BA8_NoMerc",
      "P_PorcElec", "P_AguaPot", "AlumP_sum"
    )
    
    # Función para correr modelo robusto a NA
    get_coef <- function(var, outcome) {
      
      form <- as.formula(paste0(outcome, " ~ ", var))
      
      mod <- lm(
        formula = form,
        data = completo_distancias,
        na.action = na.exclude  # <- clave
      )
      
      broom::tidy(mod) %>%
        filter(term != "(Intercept)") %>%
        mutate(
          predictor = var,
          outcome = outcome
        )
    }
    
    # Unir coeficientes de revenue log y ocupación
    coefs <- map_dfr(vars, ~get_coef(.x, "revenue_log")) %>%
      bind_rows(
        map_dfr(vars, ~get_coef(.x, "estimated_occupancy_l365d"))
      )
    
    ggplot(coefs, aes(estimate, predictor, color = outcome)) +
      
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # <--- CLAVE
      
      geom_point(size = 3, na.rm = TRUE) +
      
      geom_errorbarh(
        aes(xmin = estimate - 1.96*std.error,
            xmax = estimate + 1.96*std.error),
        height = 0.2,
        na.rm = TRUE
      ) +
      
      scale_x_continuous(expand = expansion(mult = 0.15)) +  # <--- deja espacio para negativos
      
      theme_minimal(base_size = 13) +
      
      labs(
        title = "Efectos marginales de variables territoriales",
        subtitle = "Revenue (log corregido) vs Ocupación",
        x = "Coeficiente (±95% CI)",
        y = "",
        color = "Outcome"
      )

    

