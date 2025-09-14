## code to prepare `col_admin` dataset goes here

usethis::use_data(col_admin, overwrite = TRUE)


data-raw/col_admin.R
library(sf)
library(dplyr)
library(stringr)
library(usethis)

# 1) Leer el SHP (ajusta el nombre si lo cambiaste)
muni <- sf::st_read("data-raw/Municipio, Distrito y Area no municipalizada.shp", quiet = TRUE)

# 2) Asegurar CRS WGS84 (EPSG:4326)
muni <- sf::st_transform(muni, 4326)

# 3) Estandarizar nombres de columnas a lo que usará el paquete
#    - codigo_dane : código DANE del municipio (5 dígitos)
#    - mpio        : nombre de municipio
#    - dpto        : nombre de departamento
#    - dpto_code   : código DANE del departamento (2 dígitos, derivado de codigo_dane)
muni <- muni %>%
  rename(
    codigo_dane_raw = MpCodigo,   # 91405, etc.
    mpio            = MpNombre,   # "La Chorrera", etc.
    dpto            = Depto       # "Amazonas", etc.
  ) %>%
  mutate(
    codigo_dane_raw = as.character(codigo_dane_raw),
    # asegura ancho correcto con ceros a la izquierda
    codigo_dane     = str_pad(codigo_dane_raw, width = 5, pad = "0"),
    dpto            = str_to_title(dpto),
    mpio            = str_to_title(mpio),
    dpto_code       = substr(codigo_dane, 1, 2)
  ) %>%
  select(codigo_dane, dpto_code, dpto, mpio, geometry)

# 4) (opcional) simplifica geometría para que el paquete pese menos
muni <- sf::st_simplify(muni, dTolerance = 0.0005, preserveTopology = TRUE)

# 5) Crear también una capa de departamentos a partir de los municipios (por si la quieres)
depart <- muni %>%
  group_by(dpto_code, dpto) %>%
  summarise(.groups = "drop")  # une geometrías por dpto

# 6) Guardar como datos INTERNOS del paquete
col_municipios    <- muni
col_departamentos <- depart
usethis::use_data(col_municipios, col_departamentos, internal = TRUE, overwrite = TRUE)




















