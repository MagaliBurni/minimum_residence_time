# Análisis del Tiempo Mínimo de Residencia (TMR)

Este repositorio contiene un script en **R** para calcular y analizar el **Tiempo Mínimo de Residencia (TMR)** de especies leñosas a dos escalas geográficas: **Provincia de Córdoba** y **Centro-Este de Argentina**.  
El análisis aplica modelos GLM con distribución Gamma, diagnóstico estadístico y visualización de resultados mediante boxplots.

---

## Contenido del repositorio
- `TMR.R`: script principal con todo el análisis.
- El presente README.md con la descripción del script.

**Nota:** Los archivos de datos (`TR LOCAL.csv` y `TR REGIONAL.csv`) no están incluidos en este repositorio por motivos de confidencialidad. Para ejecutar el script, es necesario contar con bases de datos con estructura similar (columnas de especie, año de registro y status invasor).

---

## Requisitos
El script fue desarrollado en **R** y requiere los siguientes paquetes:

```R
library(car)
library(dplyr)
library(ggplot2)
library(patchwork)
library(svglite)
