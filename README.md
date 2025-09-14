# dwcCOL

**Darwin Core para Colombia** en R. Construye tablas DwC listas para SIB/GBIF:

- Georreferencia **Departamento / Municipio / código DANE** a partir de lat/lon (capas internas).
- Completa **taxonomía GBIF** (incluye *authorship* del nombre aceptado y nombre común en español priorizando Colombia).
- Exporta en el **orden exacto** de tu header DwC (CSV, UTF-8).
- Incluye chequeos de calidad (QC) básicos.

---

## Instalación

```r
# Instalar desde GitHub 
install.packages("remotes")
remotes::install_github("camiloesor06/dwcCOL")
