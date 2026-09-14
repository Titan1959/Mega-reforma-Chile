suppressPackageStartupMessages({library(ggplot2);library(dplyr);library(tidyr);library(readr);library(scales);library(knitr)})
leer <- function(nombre) read_csv(paste0('datos/',nombre,'.csv'),show_col_types=FALSE)
n <- function(x,d=0) number(x,accuracy=10^-d,big.mark='.',decimal.mark=',')
theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),plot.background=element_rect(fill='white',colour=NA),legend.position='bottom',legend.title=element_blank(),plot.margin=margin(12,18,12,12)))
paleta <- c('#1a365d','#bd5315','#26736b','#76618c')
lineas <- function(d,y,grupo=NULL,unidad='') {
  if(is.null(grupo)) {
    g<-ggplot(d,aes(x=anio,y=.data[[y]]))+geom_line(colour=paleta[1],linewidth=1.1)+geom_point(colour=paleta[1],size=2.4)
  } else {
    g<-ggplot(d,aes(x=anio,y=.data[[y]],colour=.data[[grupo]],linetype=.data[[grupo]]))+geom_line(linewidth=1)+geom_point(size=2)+scale_colour_manual(values=paleta)
  }
  g+scale_x_continuous(breaks=sort(unique(d$anio)))+scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','))+expand_limits(y=0)+labs(x=NULL,y=unidad)
}
tabla <- function(d) kable(d,format='html',digits=2,format.args=list(big.mark='.',decimal.mark=','))
