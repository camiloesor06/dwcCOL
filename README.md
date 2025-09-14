# dwcCOL

**Darwin Core para Colombia** en R. Construye tablas DwC listas para SIB/GBIF:

Este paquete lo hice para ahorrar horas de vida armando la matriz Darwin Core que usualmente entregamos desde la Universidad Icesi al SIB/GBIF.
Tú pasas lo mínimo (especie + fecha + lat/lon) y el paquete te devuelve una tabla DwC lista, con:

✅ Departamento / Municipio / código DANE a partir de lat/lon (capas internas).

✅ Taxonomía GBIF (incluye authorship del nombre aceptado y nombre común en español priorizando Colombia).

✅ Orden de columnas tal cual el header que usamos.

✅ Chequeos de calidad (QC) y exportación a CSV (amigable para Excel).

---

## Instalación

```r
# Instalar desde GitHub 
install.packages("remotes")
remotes::install_github("camiloesor06/dwcCOL")
---

Que debo preparar para que el código funcione?

| scientificName | decimalLatitude | decimalLongitude | eventDate  | recordedBy              |
| -------------- | --------------: | ---------------: | ---------- | ----------------------- |
| Scinax wandae  |        2.871077 |       -72.058634 | 2022-02-15 | Camilo Andrés Estupiñan |

## Reglas simples:

decimalLatitude y decimalLongitude en grados decimales (punto como separador; oeste es negativo).

Datum: WGS84 (si no lo pones, el paquete lo asume).

eventDate como fecha (YYYY-MM-DD).

scientificName bien escrito

```r
## Ejemplo rápido:

library(dwcCOL)
library(tibble)
library(dplyr)

# (Opcional) Pon tu "perfil" por defecto para no repetir
dwc_set_defaults(list(
  institutionCode = "Universidad ICESI",
  collectionCode  = "Herpetología Mapiripán",
  language        = "es",
  geodeticDatum   = "WGS84",
  accessRights    = "Para uso investigativo"
))

# Tus datos mínimos
ej <- tibble(
  scientificName   = "Scinax wandae",
  decimalLatitude  = 2.871077,
  decimalLongitude = -72.058634,
  eventDate        = as.Date("2022-02-15"),
  recordedBy       = "Camilo Andrés Estupiñan"
)

# Construir la tabla DwC (¡todo en una!)
out <- dwc_build(
  ej,
  use_gbif = TRUE,          # completa taxonomía
  vernacular_es_co = TRUE,  # nombre común en ES priorizando Colombia
  generate_id = TRUE,       # occurrenceID determinístico
  id_prefix = "COL-DWC"
)

# Ver lo importante
out %>% select(stateProvince, municipality, locationID,
               kingdom, family, genus, specificEpithet,
               scientificNameAuthorship, vernacularName)

# Guardar CSV con el orden exacto del header (Excel-friendly)
dir.create("export", showWarnings = FALSE)
write_dwc_csv(out, "export/ejemplo_dwc.csv", excel_friendly = TRUE)




