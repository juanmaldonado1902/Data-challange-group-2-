#Main
library(readr)
library(ggplot2)
library(tidyverse)
library(sf)
library(scales)
airbnb <- read_csv("listings_rendimiento_con_coords.csv")
View(airbnb)

###Tratamiento shp manzanas

transp <- st_read("Número de sistemas de transporte disponibles por manzana/manzanas_zmvm.shp")
transp <- transp |> 
  filter(str_starts(CVEGEO, "09"))

st_write(transp, "Número de sistemas de transporte disponibles por manzana/manzanas__cdmx_zmvm.shp")

####
plot(density(log(na.omit(airbnb$price))))

airbnb <- airbnb |>
  mutate(
    avg_revenue = price * estimated_occupancy_l365d
  )


summary(airbnb$estimated_revenue_l365d)
plot(barplot(na.omit(airbnb$estimated_revenue_l365d)))

airbnb |> ggplot() + 
  geom_density(aes(x = estimated_revenue_l365d)) + 
  scale_x_log10()

airbnb |> ggplot() + 
  geom_boxplot(aes(x = estimated_revenue_l365d)) 




### Promedio rendimiento por colonia 
renta_colonia <- airbnb %>%
  group_by(clave_colonia, room_type) %>%
  summarise(
    avg_revenue = mean(estimated_revenue_l365d, na.rm = TRUE),   # promedio sin NA
    n_total = n()
  )

renta_tipo <- airbnb |>
  group_by(room_type) |>
  summarise(
    avg_revenue = mean(estimated_revenue_l365d, na.rm = TRUE),   # promedio sin NA
    sd_revenue = sd(estimated_revenue_l365d, na.rm = TRUE),
    avg_ocup = mean(estimated_occupancy_l365d, na.rm = TRUE),
    n_total = n()
  )
######

####Distancia al metro OMITIR SI YA TENEMOS LAS BASES

completo_distancias <- read_csv("completo_distancias.csv")

completo_distancias <- completo_distancias |>
  rename(
    dist_metro = HubDist,
    nombre_metro = HubName
  )

write.csv(completo_distancias, "dist_metro.csv")


completo_distancias <- read_csv("completo_autoservicio.csv")

completo_distancias <- completo_distancias |>
  rename(
    dist_autoservicio = HubDist
  ) |>
  select(-HubName)
write.csv(completo_distancias, "dist_autoservicio.csv")
##OTRA TIENDA 


completo_distancias <- read_csv("dist_departamental.csv")

completo_distancias <- completo_distancias |>
  rename(
    dist_departamental = HubDist,
    nombre_departamental = HubName
  ) 
write.csv(completo_distancias, "dist_departamental.csv")

####importación final FINAL


final_monumentos <- read_csv("final_corredor.csv")
completo_distancias <- final_monumentos

######3

ggplot(completo_distancias) + 
  geom_point(aes(y = HubDist, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = HubDist, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = HubDist, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = HubDist, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)


cor(completo_distancias$estimated_revenue_l365d,completo_distancias$HubDist)
#### NO HAY RELACIÓN SIGNIFICATIVA

comercios <- st_read("cypc/C_PComerciales.shp")


tipos_comercios <- comercios |>
  group_by(TIPO) |>
  tally()

###limpiar la tabla
comercios <- comercios %>% 
  mutate(
    tipo_clean = case_when(
      # --- CENTRO / PLAZA COMERCIAL ---
      str_detect(TIPO, regex("centro|plaza|comerc", ignore_case = TRUE)) ~ "CENTRO / PLAZA COMERCIAL",
      
      # --- TIENDAS DE AUTOSERVICIO ---
      str_detect(TIPO, regex("auto ?servic", ignore_case = TRUE)) ~ "TIENDA DE AUTOSERVICIO",
      
      # --- TIENDAS DEPARTAMENTALES ---
      str_detect(TIPO, regex("departament", ignore_case = TRUE)) ~ "TIENDA DEPARTAMENTAL",
      
      # --- MIXTO ---
      str_detect(TIPO, regex("mixto", ignore_case = TRUE)) ~ "MIXTO",
      
      # --- NAs o vacíos ---
      is.na(TIPO) ~ NA_character_,
      
      TRUE ~ TIPO  # por si algo no entra en ninguna categoría
    )
  )
#######HACER DATAFRAMES DE TIPO DE AL


plazas <- comercios |>
  filter(tipo_clean == "CENTRO / PLAZA COMERCIAL")

autoservicio <- comercios |>
  filter(tipo_clean == "TIENDA DE AUTOSERVICIO")

departamental <- comercios |>
  filter(tipo_clean == "TIENDA DEPARTAMENTAL")

mixto <- comercios |>
  filter(tipo_clean == "MIXTO")

##GUARDARLOS EN SHP

st_write(plazas, "shapes_comercios/plazas.shp", delete_dsn = TRUE)
st_write(autoservicio, "shapes_comercios/autoservicio.shp", delete_dsn = TRUE)
st_write(departamental, "shapes_comercios/departamental.shp", delete_dsn = TRUE)
st_write(mixto, "shapes_comercios/mixto.shp", delete_dsn = TRUE)



### DESPUES DE HACER LA BASE DE DATOS E IR A QGIS

######3
###Distancia_autoservicio
ggplot(completo_distancias) + 
  geom_point(aes(y = dist_autoservicio, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_autoservicio, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = dist_autoservicio, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_autoservicio, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

###Distancia_departamental
ggplot(completo_distancias) + 
  geom_point(aes(y = dist_departamental, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_departamental, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = dist_departamental, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_departamental, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

###Distancia_plaza
ggplot(completo_distancias) + 
  geom_point(aes(y = dist_plaza, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_plaza, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = dist_plaza, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = dist_plaza, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)


###Reforma-insurgentes
ggplot(completo_distancias) + 
  geom_point(aes(y = HubDist, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = HubDist, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = HubDist, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = HubDist, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

###Cobertura Transporte público
ggplot(completo_distancias) + 
  geom_point(aes(y = Cobertura, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = Cobertura, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = Cobertura, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = Cobertura, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

##Equipamento salud
ggplot(completo_distancias) + 
  geom_point(aes(y = BA1_NoEqSa, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = BA1_NoEqSa, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = BA1_NoEqSa, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = BA1_NoEqSa, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)


##no Mercados
ggplot(completo_distancias) + 
  geom_point(aes(y = BA8_NoMerc, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = BA8_NoMerc, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = BA8_NoMerc, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = BA8_NoMerc, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

##Porcentaje cobertura eléctrica
ggplot(completo_distancias) + 
  geom_point(aes(y = log10(P_PorcElec), x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = log10(P_PorcElec), x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = log10(P_PorcElec), x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = log10(P_PorcElec), x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)

##Porcentaje agua potable
ggplot(completo_distancias) + 
  geom_point(aes(y = log10(P_AguaPot), x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = log10(P_AguaPot), x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = log10(P_AguaPot), x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = log10(P_AguaPot), x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)


##Alumbrado público
ggplot(completo_distancias) + 
  geom_point(aes(y = AlumP_sum, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = AlumP_sum, x = log10(estimated_revenue_l365d)), method = "lm")

ggplot(completo_distancias) + 
  geom_point(aes(y = AlumP_sum, x = log10(estimated_revenue_l365d))) + 
  geom_smooth(aes(y = AlumP_sum, x = log10(estimated_revenue_l365d)), method = "lm") + 
  facet_wrap(~room_type)




####Revenue y ocupación


comp_ocupacion_revenue <- completo_distancias |> 
  group_by(room_type) |>
  summarise(
    ocupacion_promedio = mean(estimated_occupancy_l365d, na.rm = TRUE),
    ingreso_promedio = mean(estimated_revenue_l365d, na.rm = TRUE)
  )

escala <- max(comp_ocupacion_revenue$ingreso_promedio, na.rm = TRUE) /
  max(comp_ocupacion_revenue$ocupacion_promedio, na.rm = TRUE)

comp_ocupacion_revenue <- comp_ocupacion_revenue |>
  mutate(
    ocupacion_escalada = ocupacion_promedio * escala
  )

comp_ocupacion_revenue <- comp_ocupacion_revenue |>
  arrange(desc(ingreso_promedio)) |>
  mutate(room_type = factor(room_type, levels = room_type))

comp_ocupacion_revenue <- comp_ocupacion_revenue |>
  pivot_longer(cols = c(ocupacion_escalada,ingreso_promedio), names_to = "Variable", values_to = "Valor")

ggplot(comp_ocupacion_revenue, aes(x = room_type, y = Valor, fill = Variable)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("ingreso_promedio" = "steelblue", "ocupacion_escalada" = "tomato"),
    labels = c("Ingreso promedio anual", "Ocupación promedio anual")
  ) +
  scale_y_continuous(
    name = "Ingreso promedio anual MXN",
    labels = label_dollar(prefix = "$", accuracy = 1),
    sec.axis = sec_axis(~ . / escala, name = "Ocupación promedio")
  ) +
  labs(
    title = "Ingreso promedio anual y ocupación promedio anual
por tipo de alojamiento.",
    subtitle = "Elaborado con AIRBNB webscrapping",
    fill = "Variable",
    x = ""
  ) +
  theme_minimal() + 
  theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5)
    
  )

####hist

hist(completo_distancias$HubDist)

summary(completo_distancias$HubDist)
summary(completo_distancias$Cobertura)
completo_distancias |>
  ggplot(aes(x = HubDist)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "blue", color = "white", alpha = 0.5) +
  geom_density(color = "red", size = 1) + 
  labs(
    x = "Distancia (m)",
    title = "Densidad de distanacia entre Airbnb´s y el corredor
económico (Av. Insurgentes y/o Av. Reforma)", 
    subtitle = "Elaborado con datos del GOBCDMX",
    caption = "Elaboración propia"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  )


####Plot de cobertura transporte público 

completo_distancias |>
  ggplot(aes(x = Cobertura)) +
  geom_histogram(fill = "steelblue",  binwidth = .5) +
  labs(
    x = "Cantidad de transporte público disponible por Airbnb",
    title = "Histograma de cobertura de transporte público
en por manzana de los Airbnb´s", 
    subtitle = "Elaborado con datos del GOBCDMX",
    caption = "Elaboración propia"
  ) + 
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5)
  )


####REGRESION

reg_table <- completo_distancias |>
  filter(!is.na(`log10(estimated_revenue_l365d)`), `log10(estimated_revenue_l365d)` >= 0)

reg_table <- reg_table |> 
  select(`log10(estimated_revenue_l365d)`, estimated_revenue_l365d, room_type, estimated_occupancy_l365d,dist_autoservicio, dist_departamental, dist_plaza, HubDist, Cobertura, BA1_NoEqSa)

##Agregar dummies

reg_table <- reg_table |>
  mutate(dummie_entire = ifelse(room_type == "Entire home/apt", 1, 0),
         dummie_private = ifelse(room_type == "Private room", 1, 0),
         dummie_hotel = ifelse(room_type == "Hotel room", 1, 0),
         dummie_shared = ifelse(room_type == "Shared room", 1, 0))

reg_table <- reg_table |>
  filter(!is.na(Cobertura))

reg_table <- reg_table |>
  rename(dist_corredor = HubDist)

reg_table <- reg_table |>
  mutate(
    "log_dist_corredor" = log10(dist_corredor)
  )

#MODELOS

#INGRESO

lm_reg_1 <- lm(`estimated_revenue_l365d` ~ dist_autoservicio + dist_departamental  
               + dist_corredor + Cobertura + BA1_NoEqSa ,
               data = reg_table)

lm_reg_2 <- lm(`estimated_revenue_l365d` ~ dist_autoservicio + dist_departamental  
               + log_dist_corredor + Cobertura + BA1_NoEqSa + dummie_entire + 
                 dummie_private + dummie_hotel,
               data = reg_table)
lm_reg_3 <- lm(`estimated_revenue_l365d` ~ dist_autoservicio + dist_departamental  
               + log_dist_corredor + Cobertura + BA1_NoEqSa + dummie_entire + 
                 dummie_private + dummie_hotel + log_dist_corredor*Cobertura,
               data = reg_table)

lm_reg_log_1 <- lm(`log10(estimated_revenue_l365d)` ~ dist_autoservicio + dist_departamental  
              + dist_corredor +Cobertura + BA1_NoEqSa,
             data = reg_table)

lm_reg_log_2 <- lm(`log10(estimated_revenue_l365d)` ~ dist_autoservicio + dist_departamental  
                   + log_dist_corredor + Cobertura + BA1_NoEqSa + dummie_entire + 
                     dummie_private + dummie_hotel,
                   data = reg_table)

lm_reg_log_3 <- lm(`log10(estimated_revenue_l365d)` ~ dist_autoservicio + dist_departamental  
                   + log_dist_corredor + Cobertura + BA1_NoEqSa + dummie_entire + 
                     dummie_private + dummie_hotel + log_dist_corredor*Cobertura,
                   data = reg_table)

models <- list(
  "Niveles: (1)" = lm_reg_1,
  "Niveles: (2)" = lm_reg_2,
  "Niveles: (3)" = lm_reg_3,
  "Log: (1)"     = lm_reg_log_1,
  "Log: (2)"     = lm_reg_log_2,
  "Log: (3)"     = lm_reg_log_3
)

vcov_list <- lapply(models, function(m) vcovHC(m, type = "HC3"))

library(gt)

tbl <- modelsummary(
  models,
  vcov = vcov_list,
  statistic = "({statistic}) {stars}",
  stars = TRUE,
  coef_map = c(
    "dist_autoservicio" = "Distancia autoservicio",
    "dist_departamental" = "Distancia departamental",
    "dist_corredor" = "Distancia corredor",
    "log_dist_corredor" = "Log distancia corredor",
    "Cobertura" = "Cobertura TP",
    "BA1_NoEqSa" = "BA1",
    "log_dist_corredor:Cobertura" = "Interacción log corredor × cobertura",
    "dummie_entire" = "Entire home",
    "dummie_private" = "Private room",
    "dummie_hotel" = "Hotel room"
  ),
  title = "Estimaciones MCO con errores robustos (HC3)",
  output = "gt"
)


tbl <- tbl |>
  tab_spanner(
    label = "Modelos en Nivel",
    columns = c("Niveles: (1)", "Niveles: (2)", "Niveles: (3)")
  ) |>
  tab_spanner(
    label = "Modelos en Log10",
    columns = c("Log: (1)", "Log: (2)", "Log: (3)")
  )


tbl


summary(lm_reg_1)

modelsummary(
  lm_reg_,
  vcov = vcovHC(lm_reg_1, type = "HC3"),
  statistic = "({statistic}) {stars}",   # estadístico (t) con estrellas
  fmt = 3,
  stars = TRUE,
  coef_map = c(
    "dist_autoservicio" = "Distancia autoservicio",
    "dist_departamental" = "Distancia departamental",
    "dist_corredor" = "Distancia corredor",
    "log_dist_corredor" = "Log distancia corredor",
    "Cobertura" = "Cobertura TP",
    "BA1_NoEqSa" = "BA1",
    "log_dist_corredor:Cobertura" = "Interacción log corredor × cobertura",
    "dummie_entire" = "Entire home",
    "dummie_private" = "Private room",
    "dummie_hotel" = "Hotel room"
  ),
  title  = "Estimaciones MCO con errores robustos (HC3)",
  
  note = list(
    "Errores heterocedásticos (HC3) entre paréntesis bajo los coeficientes."
  )
)

summary(lm_reg)

bptest(lm_reg)  

lm_reg <- lm(`estimated_revenue_l365d` ~ dist_autoservicio + dist_departamental + dist_plaza 
             + dist_corredor + Cobertura + BA1_NoEqSa + dist_corredor*Cobertura + dummie_entire + 
               dummie_private + dummie_hotel,
             data = reg_table)

summary(lm_reg)


library(modelsummary)
library(gt)
library(webshot2)

modelsummary(
  lm_reg,
  vcov = vcovHC(lm_reg, type = "HC3"),
  statistic = "({statistic}) {stars}",
  stars = TRUE
)

tab <- modelsummary(
  lm_reg,
  vcov = vcovHC(lm_reg, type = "HC3"),
  statistic = "({statistic}) {stars}",
  stars = TRUE,
  output = "gt"       # <- tabla GT (indispensable)
)

gtsave(tab, filename = "regresion_robusta.png")
### hacer efectos parciales


# ---- 1. Coeficientes con errores robustos (HC3) ----

robust_coefs <- coeftest(lm_reg, vcov = vcovHC(lm_reg, type = "HC3"))

df_coefs <- tibble(
  term      = rownames(robust_coefs),
  estimate  = robust_coefs[, "Estimate"],
  std.error = robust_coefs[, "Std. Error"]
)

# ---- 3. Quitar intercepto ----
df_coefs <- df_coefs |> filter(term != "(Intercept)")

# ---- 4. Agregar intervalos de confianza ----
df_coefs <- df_coefs |>
  mutate(
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error
  )



ggplot(df_coefs, aes(x = estimate, y = term)) +
  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  geom_point(size = 3, color = "#0072B2") +
  
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.25,
    color = "#0072B2"
  ) +
  
  theme_minimal(base_size = 14) +
  
  labs(
    title = "Efectos Parciales (Forest Plot)",
    subtitle = "Coeficientes con Intervalos de Confianza 95% (HC3)",
    x = "Coeficiente",
    y = "Variable"
  )
