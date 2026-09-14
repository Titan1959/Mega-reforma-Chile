# Ejecutar desde la raíz del proyecto: Rscript scripts/procesar_datos.R
# Análisis exclusivamente en R. Los archivos originales nunca se modifican.
if (.Platform$OS.type == 'windows') invisible(Sys.setlocale('LC_CTYPE','Spanish_Chile.utf8'))
suppressPackageStartupMessages({library(readxl); library(dplyr); library(tidyr); library(readr); library(jsonlite); library(rvest)})
options(stringsAsFactors=FALSE)
dir.create('datos',showWarnings=FALSE)
guardar <- function(x,nombre) {stopifnot(nrow(x)>0); write_csv(x,file.path('datos',paste0(nombre,'.csv')),na='')}
num <- function(d,r,cols) as.numeric(unlist(d[r,cols],use.names=FALSE))

# DIPRES: promedio anual de cargos, hoja General del archivo aportado.
f <- '01_empleo_publico/Personal disponible en el sector publico.xlsx'
e <- read_excel(f,sheet='General',col_names=FALSE,.name_repair='minimal')
stopifnot(e[[1]][2]=='Sector Público')
empleo <- bind_rows(lapply(c(2,5,8,11,14,17,20),function(r) data.frame(anio=2021:2025,dependencia=e[[1]][r],cargos=num(e,r,2:6))))
stopifnot(all(empleo$cargos>=0), !anyDuplicated(empleo[c('anio','dependencia')]))
tot <- empleo |> filter(dependencia=='Sector Público')
sumas <- empleo |> filter(dependencia!='Sector Público') |> group_by(anio) |> summarise(cargos=sum(cargos),.groups='drop')
stopifnot(all(tot$cargos==sumas$cargos))
guardar(empleo,'empleo_publico')

# FONASA: sumar conteos publicados, no contar filas como personas.
# 2018-2020 se auditan pero no se incorporan al gráfico por claves repetidas.
archivos <- list.files('02_salud_fonasa',pattern='[.]csv$',full.names=TRUE)
fonasa <- list(); calidad <- list()
for(f in archivos) {
  d <- read.csv(f,check.names=FALSE,colClasses='character')
  n <- ncol(d); v <- as.numeric(d[[n]])
  periodo <- unique(d[[1]]); stopifnot(length(periodo)==1,all(is.finite(v)),all(v>=0),all(v==floor(v)))
  anio <- as.integer(substr(periodo,1,4)); dup <- sum(duplicated(d[-n]))
  calidad[[length(calidad)+1]] <- data.frame(archivo=basename(f),anio=anio,filas=nrow(d),claves_repetidas=dup,beneficiarios=sum(v),publicado_en_grafico=anio>=2021)
  if(anio>=2021) {
    stopifnot(dup==0)
    tr <- which(names(d)%in%c('TRAMO','TRAMO_FONASA')); stopifnot(length(tr)==1)
    a <- aggregate(v,list(tramo=d[[tr]]),sum);names(a)[2]<-'beneficiarios';a$anio<-anio
    stopifnot(setequal(a$tramo,c('A','B','C','D')),sum(a$beneficiarios)==sum(v))
    fonasa[[length(fonasa)+1]]<-a
  }
}
guardar(bind_rows(calidad) |> arrange(anio),'auditoria_fonasa')
guardar(bind_rows(fonasa) |> select(anio,tramo,beneficiarios) |> arrange(anio,tramo),'fonasa_tramos')

# SII: valores almacenados como fracción del PIB, convertidos a porcentaje.
f <- list.files('03_tributacion/datos_originales/sii_pib',pattern='xlsx$',full.names=TRUE)
stopifnot(length(f)==1)
s <- read_excel(f,sheet='Consolidados',col_names=FALSE,.name_repair='minimal')
stopifnot(s[[1]][246]=='TOTAL INGRESOS TRIBUTARIOS',grepl('VALOR AGREGADO',s[[2]][97]))
tributos <- data.frame(anio=2009:2025,total=num(s,246,7:23)*100,renta=num(s,5,7:23)*100,iva=num(s,97,7:23)*100)
tributos$otros <- tributos$total-tributos$renta-tributos$iva
stopifnot(all(tributos$otros>=0),all(tributos$total<40),all(tributos$total>0))
guardar(tributos,'tributacion')

# INE: estimaciones anuales (no promedio artesanal de trimestres superpuestos).
f <- '04_empleo_formal/datos_originales/ene_anual.xlsx'
ene <- bind_rows(lapply(c(AS='Total',H='Hombres',M='Mujeres'),function(sexo) {
  hoja <- names(c(AS='Total',H='Hombres',M='Mujeres'))[match(sexo,c('Total','Hombres','Mujeres'))]
  d <- read_excel(f,sheet=hoja,col_names=FALSE,.name_repair='minimal')
  filas <- which(suppressWarnings(as.numeric(d[[1]]))%in%2010:2025)
  data.frame(anio=as.integer(d[[1]][filas]),sexo=sexo,ocupados_miles=as.numeric(d[[7]][filas]),desocupacion_pct=as.numeric(d[[23]][filas]),ocupacion_pct=as.numeric(d[[25]][filas]),participacion_pct=as.numeric(d[[27]][filas]))
}))
stopifnot(nrow(ene)==48,all(ene$ocupacion_pct>=0 & ene$ocupacion_pct<=100),!anyDuplicated(ene[c('anio','sexo')]))
guardar(ene,'empleo_ene')

# DIPRES: totales incluyen operaciones de capital; no confundir con gasto corriente.
d <- read_excel('05_finanzas_publicas/datos_originales/dipres_gct_pib.xlsx',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][34]=='TOTAL INGRESOS',d[[1]][35]=='TOTAL GASTOS',d[[1]][36]=='PRESTAMO NETO/ENDEUDAMIENTO NETO')
fiscal <- data.frame(anio=1990:2025,ingresos_pct_pib=num(d,34,2:37),gastos_pct_pib=num(d,35,2:37),balance_pct_pib=num(d,36,2:37))
stopifnot(max(abs(fiscal$ingresos_pct_pib-fiscal$gastos_pct_pib-fiscal$balance_pct_pib))<0.00001)
guardar(fiscal,'finanzas_publicas')

# MINVU: tabla estadística completa; la capa geográfica no reconcilia el total.
d <- read_excel('06_vivienda/datos_originales/minvu_censos.xlsx',sheet='2024 2002 Completa',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][8]=='Total País')
v <- data.frame(region=d[[1]][10:25],hogares=as.numeric(d[[7]][10:25]),deficit=as.numeric(d[[21]][10:25]))
stopifnot(nrow(v)==16,!anyDuplicated(v$region),all(v$deficit>=0),all(v$hogares>0),sum(v$deficit)==as.numeric(d[[21]][8]),sum(v$hogares)==as.numeric(d[[7]][8]))
v$deficit_por_100_hogares<-100*v$deficit/v$hogares
guardar(v,'vivienda_regiones')

# Banco Central: tabla HTML oficial guardada, relación FBCF/PIB a precios corrientes.
b <- read_html('07_inversion/datos_originales/bcentral_inversion.html') |> html_table(fill=TRUE)
stopifnot(length(b)==1)
b<-b[[1]]; fila<-which(grepl('fijo',b$Serie,ignore.case=TRUE)&grepl('corrientes',b$Serie,ignore.case=TRUE))
stopifnot(length(fila)==1)
cols<-which(grepl('^[0-9]{4}$',names(b)))
inversion<-data.frame(anio=as.integer(names(b)[cols]),fbcf_pct_pib=as.numeric(gsub(',','.',unlist(b[fila,cols]))))
stopifnot(all(is.finite(inversion$fbcf_pct_pib)),all(inversion$fbcf_pct_pib>0))
guardar(inversion,'inversion')
writeLines(capture.output(sessionInfo()),'datos/sesion_R.txt')
cat('Validación completada. Déficit nacional conciliado MINVU:',sum(v$deficit),'\n')
