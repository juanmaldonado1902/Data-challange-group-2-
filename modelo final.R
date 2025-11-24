
#Generación del cálculo de metros cuadrados
pkgs <- c("readr","dplyr","stringr")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))


ruta_in  <- "/Users/juanpablo/Desktop/data challange/bases de datos/listings_scrapped.csv"
ruta_out <- "/Users/juanpablo/Desktop/data challange/bases de datos/listings_scrapped_m2.csv"


df <- readr::read_csv(ruta_in, show_col_types = FALSE)
df2 <- df %>%
  mutate(
    
    bedrooms_num  = suppressWarnings(as.numeric(bedrooms)),
    bathrooms_num = suppressWarnings(as.numeric(bathrooms)),
    bathrooms_num = ifelse(
      is.na(bathrooms_num) & !is.na(bathrooms_text),
      readr::parse_number(bathrooms_text),
      bathrooms_num
    ),
    bdr = dplyr::coalesce(bedrooms_num, 0),
    bth = dplyr::coalesce(bathrooms_num, 0),
    m2_estimado = 30 + 35*bdr + 10*bth,
    
    m2_estimado = pmin(pmax(m2_estimado, 25), 250),
    m2_estimado = ifelse(bdr==0 & bth==0, NA_real_, m2_estimado)
  ) %>%
  select(-bdr, -bth)

readr::write_csv(df2, ruta_out)

message("Listo. Archivo guardado en: ", ruta_out)


# Calculo rendimiento
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr)

ruta_in  <- "/Users/juanpablo/Desktop/data challange/bases de datos/listings_valores_final_m2.csv"
ruta_out <- "/Users/juanpablo/Desktop/data challange/bases de datos/listings_rendimiento_con_revenue_real.csv"


df <- readr::read_csv(ruta_in, show_col_types = FALSE)

df <- df %>%
  mutate(
    estimated_revenue_l365d = as.numeric(estimated_revenue_l365d),
    valor_m2_final = as.numeric(valor_m2_final),
    m2_est = as.numeric(m2_est)
  )

df <- df %>%
  mutate(
    rendimiento_real = estimated_revenue_l365d / (valor_m2_final * m2_est),
    rendimiento_pct = rendimiento_real * 100
  )

readr::write_csv(df, ruta_out)


cat("✅ Archivo final guardado en:\n", ruta_out, "\n",
    "Filas totales:", nrow(df), "\n",
    "Rendimiento promedio (%):", round(mean(df$rendimiento_pct, na.rm = TRUE), 2), "\n",
    "Percentiles (10–50–90):",
    paste(round(quantile(df$rendimiento_pct, c(.1,.5,.9), na.rm = TRUE), 2), collapse = " / "), "\n")




# Generación mapa con trimming 2.5%

library(sf)
library(dplyr)
library(ggplot2)
library(readr)


sf::sf_use_s2(FALSE)


ruta_shp <- "/Users/juanpablo/Desktop/data challange/bases de datos/colonias_iecm.shp"

colonias_raw <- st_read(ruta_shp, quiet = FALSE)

colonias_ll <- colonias_raw |>
  st_make_valid() |>            
  st_buffer(0) |>               
  st_transform(4326) |>          
  dplyr::select(NOMDT, NOMUT, CVEUT, geometry)  


ruta_listings <- "/Users/juanpablo/Desktop/listings_dummies_colonia_y_top20_amenities.csv"
df <- read_csv(ruta_listings, show_col_types = FALSE)

df_pos <- df |>
  filter(!is.na(rendimiento_real),
         rendimiento_real > 0,
         !is.na(longitude),
         !is.na(latitude)) |>
  mutate(log_rend = log(rendimiento_real))


p2.5  <- quantile(df_pos$log_rend, 0.025, na.rm = TRUE)
p97.5 <- quantile(df_pos$log_rend, 0.975, na.rm = TRUE)

df_trim <- df_pos |>
  filter(log_rend >= p2.5,
         log_rend <= p97.5) |>
  
  select(longitude, latitude, rendimiento_real, log_rend)

cat("Obs originales:       ", nrow(df), "\n")
cat("Sin ceros y con coords:", nrow(df_pos), "\n")
cat("Tras trimming 2.5%:   ", nrow(df_trim), "\n\n")


pts <- st_as_sf(
  df_trim,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)


pts_col <- st_join(pts, colonias_ll, join = st_within, left = TRUE)

prop_sin_colonia <- mean(is.na(pts_col$NOMUT))
cat("Proporción de listings sin colonia asignada:",
    round(prop_sin_colonia, 4), "\n")


rend_colonia <- pts_col |>
  st_drop_geometry() |>
  group_by(NOMDT, NOMUT, CVEUT) |>
  summarise(
    rend_medio_log = mean(log_rend, na.rm = TRUE),
    rend_medio     = mean(rendimiento_real, na.rm = TRUE),
    n_listings     = n(),
    .groups = "drop"
  )


colonias_rend <- colonias_ll |>
  left_join(rend_colonia, by = c("NOMDT", "NOMUT", "CVEUT"))


ggplot(colonias_rend) +
  geom_sf(aes(fill = rend_medio_log), color = NA) +
  scale_fill_viridis_c(
    option   = "magma",
    na.value = "grey90",
    name     = "log(rendimiento\nmedio)"
  ) +
  theme_minimal() +
  labs(
    title    = "Rendimiento medio de Airbnb por colonia (CDMX)",
    subtitle = "Sólo listings con rendimiento_real > 0 y trimming 2.5% en log(rendimiento)",
    caption  = "Cálculo propio con datos de Airbnb e IECM",
    x = NULL, y = NULL
  )


# Modelo final 

library(readr)
library(dplyr)
library(stargazer)


ruta_csv  <- "/Users/juanpablo/Desktop/listings_con_roomtype_dummies.csv"
df <- read_csv(ruta_csv)

df <- df %>%
  filter(
    !is.na(rendimiento_real),
    rendimiento_real > 0
  ) %>%
  mutate(
    log_rend = log(rendimiento_real)
  )


col_dums  <- grep("^col_",      names(df), value = TRUE)
amen_dums <- grep("^amen_",     names(df), value = TRUE)
room_dums <- grep("^roomtype",  names(df), value = TRUE)  # ej. roomtypeEntire_home_apt

cat("N dummies de colonias:", length(col_dums),  "\n")
cat("N dummies de amenities:", length(amen_dums), "\n")
cat("N dummies de room_type:", length(room_dums), "\n")


if (length(col_dums) == 0) stop("No se encontraron columnas 'col_'")

col_counts <- df %>%
  summarise(across(all_of(col_dums), ~ sum(.x, na.rm = TRUE))) %>%
  as.numeric()

col_base <- col_dums[which.max(col_counts)]
col_dums_use <- setdiff(col_dums, col_base)

cat(">>> Colonia base (omitida en la regresión):", col_base, "\n")


if (length(room_dums) > 0) {
  room_counts <- df %>%
    summarise(across(all_of(room_dums), ~ sum(.x, na.rm = TRUE))) %>%
    as.numeric()
  
  room_base     <- room_dums[which.max(room_counts)]
  room_dums_use <- setdiff(room_dums, room_base)
  
  cat(">>> Room_type base (omitido en la regresión):", room_base, "\n")
} else {
  room_dums_use <- character(0)
  cat(">>> No se encontraron dummies de room_type.\n")
}


dummies_todas <- c(col_dums_use, amen_dums, room_dums_use)

df <- df %>%
  mutate(
    across(
      all_of(dummies_todas),
      ~ as.numeric(ifelse(is.na(.x), 0, .x))
    )
  )


fml_m1 <- as.formula(
  paste("log_rend ~", paste(col_dums_use, collapse = " + "))
)


fml_m2 <- as.formula(
  paste("log_rend ~", paste(c(col_dums_use, amen_dums), collapse = " + "))
)


fml_m3 <- as.formula(
  paste(
    "log_rend ~ review_scores_rating +",
    paste(c(col_dums_use, amen_dums, room_dums_use),
          collapse = " + ")
  )
)


fml_m4 <- as.formula(
  paste(
    "log_rend ~ review_scores_rating +",
    paste(c(col_dums_use, room_dums_use),
          collapse = " + ")
  )
)


fml_m5 <- as.formula(
  paste(
    "log_rend ~ review_scores_rating +",
    paste(c(amen_dums, room_dums_use),
          collapse = " + ")
  )
)


m1 <- lm(fml_m1, data = df)
m2 <- lm(fml_m2, data = df)
m3 <- lm(fml_m3, data = df)
m4 <- lm(fml_m4, data = df)
m5 <- lm(fml_m5, data = df)


na_coefs <- unique(c(
  names(coef(m1))[is.na(coef(m1))],
  names(coef(m2))[is.na(coef(m2))],
  names(coef(m3))[is.na(coef(m3))],
  names(coef(m4))[is.na(coef(m4))],
  names(coef(m5))[is.na(coef(m5))]
))

na_coefs <- na_coefs[!is.na(na_coefs) & na_coefs != "(Intercept)"]

if (length(na_coefs) > 0) {
  cat(">>> Variables con coeficiente NA (por colinealidad) que se ocultarán en la tabla:\n")
  print(na_coefs)
  omit_regex <- paste(na_coefs, collapse = "|")
} else {
  omit_regex <- NULL
}


ruta_html <- "/Users/juanpablo/Desktop/modelos_colonias_amen_rooms_con_M5.html"

stargazer(
  m1, m2, m3, m4, m5,
  type            = "html",
  out             = ruta_html,
  title           = "Modelos de log(rendimiento_real) con colonias, amenidades y room_type",
  dep.var.labels  = "log(rendimiento_real)",
  column.labels   = c(
    "Colonias",
    "Colonias + amenities",
    "Cols + amen + room + rating",
    "Cols + room + rating",
    "Amen + room + rating"
  ),
  omit            = omit_regex,     # oculta dummies problemáticas
  omit.stat       = c("f", "ser"),
  digits          = 4,
  single.row      = FALSE
)

cat("\n>>> Archivo HTML generado en:\n", ruta_html, "\n")
cat(">>> Colonia base (efecto fijo omitido):", col_base, "\n")
if (length(room_dums_use) > 0) {
  cat(">>> Room_type base (omitido):", room_base, "\n")
}
