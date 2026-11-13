# GUIA COMPLETA - TALLER INTEGRADOR DE CALIDAD DE DATOS EN R
# Basado en los ejemplos de la guia. Ejecutar POR SECCIONES (Ctrl+Enter).
# Los scripts R/01...R/07 contienen el procesamiento completo del taller.
# Abrir primero encuesta_calidad.Rproj. NO ejecutar YAML ni comandos bash en R.

# ================= ETAPA 0: CONFIGURACION =================
# Verificar raiz del proyecto y archivo de entrada
getwd()
here::here()
file.exists(here::here('data_raw','encuesta_socioeconomica_2026.csv'))
# Si faltan paquetes, instalar UNA VEZ desde la consola:
# install.packages(c('here','readr','dplyr','tidyr','ggplot2','knitr'))
source(here::here('R','00_configuracion.R'))
# Paso de aprendizaje: distinguir ausencias y conversion numerica
normalizar_ausencia(c('NA','', '0','450.00'))
a_numero(c('100.50','NA','USD 410','0'))

# Pregunta: ¿Por que no reemplazar 0 por NA? Porque 0 es un valor real.

# Las claves se leen como texto para conservar todos sus caracteres y evitar que R 
# cambie códigos o elimine ceros iniciales.

# ¿Por que claves como texto? Para conservar ceros iniciales y formatos.

# Respuesta: No se deben convertir todos los ceros en NA porque cero puede ser una ,
# respuesta verdadera, por ejemplo, una persona sin ingresos. 

# ================= ETAPA 1: IMPORTACION Y PERFILADO =================
# Paso 1.1: importar conservando el texto original
source(here::here('R','00_configuracion.R'))
personas_raw <- leer_texto(
  here::here('data_raw','encuesta_socioeconomica_2026.csv')
)

# Pregunta:¿por qué read_csv() no debe decidir automáticamente todos los tipos de datos?

# Respuesta: read_csv() no debe decidir automáticamente todos los tipos porque podría,
# transformar códigos en números, alterar formatos o interpretar incorrectamente,
# ingresos escritos con coma. Primero leemos como texto y luego revisamos cada variable.

# Paso 1.2: Comprender estructura y frecuencias
#dimensiones, variables, primeras filas y frecuencias
# dplyr proporciona %>% y count()
library(dplyr)
dim(personas_raw)
names(personas_raw)
head(personas_raw, 6)
personas_raw %>% count(zona)

# Paso 1.3: Distinguir ausencia, cero y formato inválido
personas_raw %>% summarise(
  faltantes = sum(is.na(normalizar_ausencia(ingreso_mensual_usd))),
  ceros = sum(a_numero(ingreso_mensual_usd) == 0, na.rm = TRUE),
  negativos = sum(a_numero(ingreso_mensual_usd) < 0, na.rm = TRUE)
)

# Paso 1.4: Construir tabla de perfilado
source(here::here('R','01_importar_perfilar.R'))
# Evidencias: resultados/01_datos.rds y resultados/tablas/01_diagnostico_variables.csv
print(file.exists(here::here('resultados','01_datos.rds')))
print(file.exists(here::here('resultados','tablas','01_diagnostico_variables.csv')))

# Pregunta: Abra el CSV y explique cuáles variables requieren prioridad de revisión.


# Respuesta: La tabla de perfilado permite ver qué variables tienen más
# valores faltantes y cuántos valores distintos aparecen. Primero revisaríamos las variables
# obligatorias con más ausencias o valores sospechosos. Para nombrarlas necesitamos abrir el 
# CSV generado.

# Al revisar la tabla de diagnóstico, identificamos que las variables que requieren mayor atención son las siguientes:
- Ingreso mensual (ingreso_mensual_usd): presenta 24 valores ausentes, equivalentes al 1 % del total de registros. Es la variable con más datos faltantes.
- Edad (edad): presenta 12 valores ausentes, que representan el 0,5 %. Es importante revisarla porque permite conocer las características de las personas encuestadas.
- Nivel de educación (nivel_educacion): tiene 10 valores ausentes, equivalentes al 0,42 %, por lo que también necesita revisión.
- Identificador del hogar (id_hogar): presenta 5 valores ausentes, equivalentes al 0,21 %. Es importante porque permite relacionar a cada persona con su hogar.
- Identificador de persona (id_persona): aunque no presenta valores ausentes, tiene 2.382 valores distintos entre 2.400 registros. Esto indica que existen identificadores
  repetidos que debemos investigar.

View(diagnostico)
table(diagnostico)


# ================= ETAPA 2: VALIDACION DE VARIABLES =================
# Paso 2.1: reglas de dominio (catalogo de educacion)
# El script de etapa 1 debe haber creado el objeto 'personas'.
edu_validas <- readr::read_csv(
  here::here('diccionario','catalogo_educacion.csv'),
  show_col_types = FALSE
)$nivel_educacion
x_edu <- normalizar_ausencia(personas$nivel_educacion)
alerta_educacion <- !is.na(x_edu) & !(x_edu %in% edu_validas)

# Paso 2.2: crear tabla de alertas para revisar casos
alertas_edu <- crear_alerta(
  personas, alerta_educacion, 'V07', 'nivel_educacion',
  'Categoria educativa fuera del catalogo literal'
)
head(alertas_edu)
# Ejecutar el proceso completo de la etapa 2
source(here::here('R','02_validar_variables.R'))


# Pregunta: Seleccionen tres reglas, identifiquen una fila y expliquen si es error confirmado, 
# advertencia o ausencia.

# Respuesta: La tabla de alertas muestra cuál es el registro, 
# qué variable tiene el problema y qué regla no cumple. Una alerta no siempre 
# significa un error confirmado, debemos investigar antes de corregir.

# Al revisar el archivo 02_alertas_variables.csv, seleccionamos tres reglas que nos permiten identificar diferentes problemas en la base de datos.

# Regla	Fila CSV	Valor encontrado	Clasificación
# V02 – Edad fuera del rango	360	-2 años	Error confirmado
# V03 – Edad sin respuesta	9	NA	Ausencia
# V04 – Formato de ingreso	121	950,5	Advertencia

# 1. Regla V02 – Edad fuera del rango (fila 360)
# 
# Encontramos una edad de -2 años, lo cual es incorrecto porque una persona no puede
# tener una edad negativa. Por eso, lo consideramos un error confirmado. Debemos revisar 
# el registro para conocer cuál es la edad correcta.
# 
# 2. Regla V03 – Edad sin respuesta (fila 9)
# 
# En esta fila encontramos el valor NA, que significa que no existe información registrada
# sobre la edad de la persona. Por eso, lo clasificamos como una ausencia. Es necesario 
# revisar si podemos recuperar ese dato.
# 
# 3. Regla V04 – Formato de ingreso (fila 121)
# 
# Encontramos un ingreso registrado como 950,5. Este valor podría ser correcto, pero utiliza
# una coma decimal en lugar del punto que espera el programa. Por eso, lo consideramos una advertencia, 
# ya que debemos revisar su formato antes de utilizarlo en los cálculos.
# 

# ================= ETAPA 3: INTEGRIDAD Y CONSISTENCIA =================
# Paso 3.1: Encontrar claves repetidas
duplicados <- personas %>%
  filter(!is.na(normalizar_ausencia(id_persona))) %>%
  add_count(periodo_referencia, id_persona, name = 'repeticiones') %>%
  filter(repeticiones > 1)
print(nrow(duplicados))

# Paso 3.2: Detectar claves huérfanas
# Cargar hogares desde la fuente guardada por etapa 1, si no existe en memoria.
if (!exists('hogares')) {
  ruta_hogares <- here::here('data_raw','hogares_sinteticos_2026.csv')
  hogares <- leer_texto(ruta_hogares)
}
huerfanos <- personas %>%
  filter(!is.na(normalizar_ausencia(id_hogar))) %>%
  anti_join(hogares, by = c('id_hogar', 'periodo_referencia'))
print(nrow(huerfanos))

# Paso 3.3: Evaluar consistencia entre variables
# 'horas_n' se crea en los scripts del proyecto. No inventar esa columna.
if ('horas_n' %in% names(personas)) {
  alerta_ocupado <- personas$condicion_actividad == 'Ocupado' &
    !is.na(personas$horas_n) & personas$horas_n == 0
  print(sum(alerta_ocupado, na.rm = TRUE))
} else {
  message('horas_n aun no esta en personas; la crea/revisa el script de etapa 3.')
}
source(here::here('R','03_integridad_consistencia.R'))

# ================= ETAPA 4: DEPURACION PROPUESTA =================
# Paso 4.1: Normalizar etiquetas conocidas
canon_zona <- c('urbana'='Urbana', 'rural'='Rural')
canonizar(c('URBANA','Rural ','99'), canon_zona)
# Paso 4.2: Registrar cambios con identificador estable
# Paso 4.3: Comprender la imputación antes de simularla
median(c(100, 300, 500, 9000))
mean(c(100, 300, 500, 9000))
# La imputacion didactica no sustituye el ingreso original.
source(here::here('R','04_depuracion_bitacora.R'))

# Pregunta: Compare tres normalizaciones y un caso que se mantiene pendiente de revisión.

# Respuesta: La media puede subir mucho cuando hay ingresos extremos; 
# la mediana cambia menos. Imputar significa estimar un dato faltante, no recuperar su
# valor real. En este taller se ensaya por grupos sin alterar el ingreso original.


# En nuestra práctica trabajamos con 2.400 registros y realizamos propuestas para mejorar la información sin modificar los datos originales. Seleccionamos tres ejemplos de normalización y un caso que necesita mayor revisión.
# Variable	Valor original	Normalización propuesta
# Provincia	PICHINCHA	Pichincha
# Zona	URBANA	Urbana
# Nivel de educación	SUPERIOR	Superior
# Edad	-2	Pendiente de revisión
# 
# 1. Normalización de provincia (fila 112)
# 
# Encontramos la provincia escrita como PICHINCHA, completamente en mayúsculas. 
# La propuesta consiste en cambiarla a Pichincha, para mantener una misma forma 
# de escritura en todos los registros.
# 
# 2. Normalización de zona (fila 682)
# 
# Encontramos el valor URBANA en mayúsculas. Proponemos cambiarlo a Urbana, 
# para que coincida con las demás categorías y evitar que el programa las 
# considere diferentes.
# 
# 3. Normalización del nivel de educación (fila 198)
# 
# Encontramos la categoría SUPERIOR. Proponemos cambiarla a Superior, 
# manteniendo el mismo significado, pero utilizando una escritura uniforme.
# 
# 4. Caso pendiente de revisión: edad (fila 360)
# 
# Encontramos una persona con una edad registrada de -2 años. Como este 
# valor no es posible, no podemos corregirlo sin conocer la edad verdadera.
# Por eso, debemos mantenerlo pendiente hasta verificar la información.

# ================= ETAPA 5: INDICADORES Y TABLAS =================
# Los indicadores requieren numerador, denominador y regla.
# Completitud = 100 * campos presentes / campos evaluados.
# Validez = 100 * controles cumplidos / controles aplicables.
# Unicidad = 100 * filas sin clave repetida / filas con clave presente.
# Consistencia = 100 * relaciones correctas / relaciones aplicables.
# Ejemplo didactico:
100 * 980 / 1000
100 * 475 / 500
# Paso 5.6: comparar periodos original/propuesta usando script de la guia.
source(here::here('R','05_indicadores_tablas.R'))
# Inspeccionar resultados reales, sin inventar cifras:
indicadores <- readr::read_csv(
  here::here('resultados','tablas','05_indicadores.csv'),
  show_col_types = FALSE
)
print(indicadores, n = Inf)

#  Pregunta: Evidencia E5: 05_indicadores.csv, 05_indicadores_periodo.csv,
#  05_resumen_ingresos.csv, 05_educacion_zona.csv, 05_pobreza_zona.csv, 05_resumen_alertas.csv.
#  Explique una diferencia en puntos porcentuales y compruebe el numerador/denominador de un indicador.
# 
# Respuesta: Al comparar los resultados de octubre y noviembre de 2026, observamos que el indicador de validez 
# presentó diferencias entre ambos períodos. En octubre de 2026, la validez del escenario original 
# fue del 99,67 %, mientras que en noviembre fue del 99,48 %. Esto representa una 
# disminución de 0,19 puntos porcentuales.
# Esto significa que, en noviembre, la proporción de datos que cumplían con las reglas de validación
# fue ligeramente menor que en octubre.
# Comprobación del indicador
# 
# Para comprobar este resultado, tomamos el indicador de validez de octubre, donde el numerador 
# es 5.955 datos válidos y el denominador es 5.975 datos evaluados.


VALIDEZ — OCTUBRE 2026

Numerador: datos válidos
5.955

Denominador: datos evaluados
5.975


5.955/ 5.975* 100=99,67%

# Con esto comprobamos que el porcentaje coincide con el resultado 
# de nuestra tabla. Esta comparación nos permitió identificar pequeñas 
# diferencias en la calidad de los datos entre ambos períodos, sin

# ================= ETAPA 6: GRAFICOS =================
# El script original genera 5 PNG: indicadores, educacion, ingresos,
# alertas y pobreza didactica. No extrapolar a Ecuador.
source(here::here('R','06_graficos_seguimiento.R'))
print(list.files(here::here('resultados','figuras'), pattern='\\.png$'))


# Pregunta: Seleccione un gráfico y explique el denominador, una conclusión 
# que sí se justifica y una conclusión que sería indebida.
# 
# Gráfico seleccionado: Alertas detectadas por regla.
# 
# Respuesta:
  
# Seleccionamos el gráfico de alertas detectadas por regla, donde observamos 
# que la regla I01 presenta 36 alertas, seguida de V06 con 24 e I05 con 14.
# 
# Denominador: Este gráfico muestra cantidades de alertas, no porcentajes, 
# por lo que no utiliza un denominador directamente. Si quisiéramos calcular 
# el porcentaje de alertas de cada regla, utilizaríamos como denominador el 
# total de alertas registradas.
# 
# Conclusión que sí se justifica: La regla I01 presenta la mayor cantidad de
# alertas, por lo que sería conveniente darle prioridad durante la revisión de los datos.
# 
# Conclusión indebida: No podemos afirmar que la regla I01 representa el problema más 
# grave de la base de datos, porque tener más alertas no significa necesariamente que 
# los errores sean más graves.
# 
# En conclusión, el gráfico nos ayuda a identificar qué reglas presentan más alertas 
# y cuáles requieren mayor revisión, pero no permite determinar por sí solo la gravedad 
# de los problemas encontrados.

# ================= ETAPA 7: PAQUETE RDS =================
# Paso 7.2: ejemplo explicito de empaquetado de tablas y alertas
res <- readRDS(here::here('resultados','05_tablas.rds'))
paquete_ejemplo <- list(
  metadatos = list(titulo='Encuesta socioeconomica sintetica'),
  tablas = res$tablas,
  alertas = res$alertas,
  bitacora = res$bitacora
)
# El script original agrega mas metadatos y figuras y guarda el paquete final.
source(here::here('R','07_construir_rds_reporte.R'))
# Paso 7.3: comprobar el RDS final
x <- readRDS(here::here('resultados','07_paquete_reporte.rds'))
names(x)
str(x, max.level = 2)
nrow(x$tablas$indicadores)

source(here::here('R','07_construir_rds_reporte.R'))

# ================= ETAPA 8: REPORTE QUARTO HTML Y WORD =================
# El encabezado YAML y los bloques ```{r} van en el archivo .qmd,
# NUNCA dentro de este script .R. Consultar la guia en Paso 8.1-8.3.
# Comprobar que exista el reporte Quarto con tablas y graficos:
ruta_qmd <- here::here('reportes','reporte_calidad.qmd')
stopifnot(file.exists(ruta_qmd))
# Paso 8.4: equivalente en R de 'quarto render ... --to html/docx'
# Requiere que Quarto CLI este instalado y disponible.
if (nzchar(Sys.which('quarto'))) {
  system2('quarto', c('render', shQuote(ruta_qmd), '--to', 'html'))
  system2('quarto', c('render', shQuote(ruta_qmd), '--to', 'docx'))
} else {
  message('Quarto no se encuentra en PATH. En Terminal comprobar: quarto --version')
}
print(file.exists(here::here('reportes','reporte_calidad.html')))
print(file.exists(here::here('reportes','reporte_calidad.docx')))

quarto render reportes/reporte_calidad.qmd --to html

#Respuesta: La respuesta se encuentra en el documento de word en la carpera "Preguntas y respuestas por etapa" "Resultados tarea" 
# ================= ETAPA 9: SITIO WEB (OPCIONAL) =================
# Requiere web/_quarto.yml, web/index.qmd y web/metodologia.qmd
archivos_web <- here::here('web',c('_quarto.yml','index.qmd','metodologia.qmd'))
print(data.frame(archivo=archivos_web, existe=file.exists(archivos_web)))
if (all(file.exists(archivos_web)) && nzchar(Sys.which('quarto'))) {
  system2('quarto', c('render', shQuote(here::here('web'))))
} else {
  message('Etapa 9 no ejecutada: revisar archivos web o instalacion de Quarto.')
}

#Respuesta: La respuesta se encuentra en el documento de word en la carpera "Preguntas y respuestas por etapa" "Resultados tarea" 

# ================= VERIFICACION FINAL =================
# Se hace DESPUES de las etapas, no sustituye a la Etapa 8.
requeridos <- c(
  'resultados/01_datos.rds',
  'resultados/tablas/01_diagnostico_variables.csv',
  'resultados/tablas/02_alertas_variables.csv',
  'resultados/tablas/03_alertas_integridad.csv',
  'resultados/tablas/03_huerfanos.csv',
  'resultados/tablas/04_bitacora_propuesta.csv',
  'resultados/tablas/05_indicadores.csv',
  'resultados/tablas/05_indicadores_periodo.csv',
  'resultados/figuras/06_indicadores.png',
  'resultados/figuras/06_educacion.png',
  'resultados/figuras/06_ingresos.png',
  'resultados/figuras/06_alertas.png',
  'resultados/figuras/06_pobreza.png',
  'resultados/07_paquete_reporte.rds',
  'reportes/reporte_calidad.html'
)
verificacion <- data.frame(
  archivo = requeridos,
  existe = file.exists(here::here(requeridos))
)
print(verificacion, row.names = FALSE)
readr::write_csv(verificacion, here::here('resultados','tablas','00_verificacion_entregables.csv'))
# Revisar cualquier FALSE antes de dar por finalizada la entrega.
