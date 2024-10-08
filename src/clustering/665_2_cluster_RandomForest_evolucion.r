# ideas para un clustering derivado del Machnie Learning
# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("ggplot2")
require("RColorBrewer")
require("ggallin")

require("randomForest")
require("ranger")

PARAM <- list()
PARAM$experimento <- "clu-randomforest"
PARAM$semilla_primigenia <- 990211   # aqui va SU semilla
PARAM$dataset <- "C:/Users/maguf/OneDrive/Documentos/datamining2024/datasets/competencia_01.csv"


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("C:/Users/maguf/OneDrive/Documentos/datamining2024")

# leo el dataset
dataset <- fread(PARAM$dataset)


# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings= FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# campos arbitrarios, solo como ejemplo
# usted DEBE MANDARIAMENTE agregar más campos aqui
# no permita que la pereza se apodere de su alma
campos_cluster <- c("cliente_edad", "cliente_antiguedad", "active_quarter","cliente_vip",
                    "internet","mrentabilidad","mrentabilidad_annual","cproductos","mcuenta_corriente","mcaja_ahorro","mcaja_ahorro_dolares","mcuentas_saldo",
                    "mautoservicio","ctarjeta_debito_transacciones","ctarjeta_visa_transacciones","mtarjeta_visa_consumo","ctarjeta_master_transacciones","mtarjeta_master_consumo",
                    "mprestamos_personales","cprestamos_personales",
                    "mprestamos_prendarios","cprestamos_prendarios","mprestamos_hipotecarios","cprestamos_hipotecarios",
                    "cplazo_fijo","cinversion1","cinversion2","cseguro_vida",
                    "mpayroll","mpayroll2","ccuenta_debitos_automaticos","ctarjeta_visa_debitos_automaticos","ctarjeta_master_debitos_automaticos",
                    "mcomisiones_mantenimiento","ccajeros_propios_descuentos","ctarjeta_visa_descuentos",
                    "cforex", "cforex_buy","cforex_sell","ccheques_emitidos_rechazados",
                    "ccallcenter_transacciones","chomebanking_transacciones","ccajas_transacciones",
                    "catm_trx", "catm_trx_other","cmobile_app_trx","ctrx_quarter",
                    "Master_delinquency", "Visa_delinquency","Visa_msaldopesos", "Visa_msaldodolares", "Master_msaldopesos", "Master_msaldodolares",
                    "Visa_mpagado", "Master_mpagado", 
                    "Master_mlimitecompra", "Visa_mlimitecompra",
                    "Visa_status", "Master_status")


# genero el dataset chico
dchico <- dataset[
  clase_ternaria=="BAJA+2", 
  c("numero_de_cliente",campos_cluster),
  with=FALSE]

# arreglo los valores NA
dchico  <- na.roughfix( dchico )
# no hace falta escalar

# invoco a la distancia de Random Forest
 # ahora, a esperar .. con esta libreria de la prehistoria
#  que NO corre en paralelo

set.seed(PARAM$semilla_primigenia)


modelo <- randomForest( 
  x= dchico[, campos_cluster, with=FALSE ],
  y= NULL,
  ntree= 1000, #se puede aumentar a 10000
  proximity= TRUE,
  oob.prox=  TRUE )

# genero los clusters jerarquicos
# distancia = 1.0 - proximidad
hclust.rf <- hclust( 
  as.dist ( 1.0 - modelo$proximity),
  method= "ward.D2" )


# imprimo un pdf con la forma del cluster jerarquico

pdf( "cluster_jerarquico.pdf" )
plot( hclust.rf )
dev.off()


kclusters <- 5  # cantidad de clusters
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=kclusters & distintos <=kclusters ) )
{
  h <- h - 1
  rf.cluster <- cutree( hclust.rf, h)

  dchico[, cluster := paste0("cluster_", rf.cluster) ]

  distintos <- nrow( dchico[, .N, cluster ] )
  cat( distintos, " " )
}


#--------------------------------------

setorder( dchico, cluster, numero_de_cliente )

fwrite(dchico,
       file= "dchico.txt",
       sep= "\t")

#--------------------------------------
# Analisis de resultados del clustering jerarquico
# cantidad de registros por cluster

dcentroides <- dchico[, lapply(.SD, mean, na.rm=TRUE), 
    by= cluster, 
    .SDcols= campos_cluster ]

dcentroides

fwrite(dcentroides,
       file= "centroides.txt",
       sep= "\t" )

#--------------------------------------
# gafico los clusters en forma bivariada

# Solo voy a mostrar un porcentaje de dchico
dchico[, azar := runif(nrow(dchico)) ]
muestra <- 0.1  # me voy a quedar con los menores a este valor

# calculo la cantidad de campos
n <- length(campos_cluster)


# voy a graficar en escala logaritmica
# cuidado con 

pdf("bivariado.pdf")

for( i in 1:(n-1) ){
  for( j in (i+1):n ){

  grafico <- ggplot( dchico[azar< muestra],
      aes(x= campos_cluster[i],   ####AQUI CORREGI LA FUNCIÓN
                 y= campos_cluster[j],
                 color= "cluster"))  +
      scale_colour_brewer(palette = "Dark2") +
      geom_point(alpha = 0.50) +
      xlab(campos_cluster[i]) +
      # scale_x_continuous(trans = pseudolog10_trans) +
      ylab(campos_cluster[j]) 
      # scale_y_continuous(trans = pseudolog10_trans)

   print( grafico )
  }
}

dev.off()

# -----------------------------------------------------------------------------
# Ahora incorporo la evolucion historica antes de la BAJA

# leo la historia ( desde donde hay,  202101 )
dhistoria <- fread(PARAM$dataset)
thewalkingdead <- dhistoria[ clase_ternaria =="BAJA+2", unique(numero_de_cliente) ]

dwalkingdead <- dhistoria[ numero_de_cliente %in% thewalkingdead ]


# asigno el cluster a los 
dwalkingdead[ dchico,
           on= "numero_de_cliente",
           cluster := i.cluster ]

# asigno cuentra regresiva antes de la BAJA
setorder( dwalkingdead, numero_de_cliente, -foto_mes )

dwalkingdead[, periodo := - rowid(numero_de_cliente)]

# ejemplo
dwalkingdead[numero_de_cliente==1550236937, list( numero_de_cliente, foto_mes, periodo ) ]


# grafico la evolucion de cada < cluster, variable >  univariado ------

# todos los campos menos los que no tiene sentido
campos_totales <- setdiff( colnames(dwalkingdead),
  c("numero_de_cliente","foto_mes","clase_ternaria","cluster","periodo") )



# Genero el grafico intervalo confianza 95%
pdf("evol_RandomForest.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dwalkingdead[periodo >= -6],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()



#--------------------------------------------------------------------
# quito los CEROS  de los graficos

# reemplazo los CEROS  por NA
#  los registros NA no se grafican
dwalkingdead[ dwalkingdead==0, ] <- NA

# Genero el grafico intervalo confianza 95%
pdf("evol_noceros_RandomForest.pdf")

for( campo in campos_totales ) {

  cat( campo, " " )

  grafico <- ggplot( dwalkingdead[periodo >= -6],
    aes_string(x= "periodo",
               y= campo,
               color= "cluster"))  +
    scale_colour_brewer(palette= "Dark2") +
    xlab("periodo") +
    ylab(campo) +
    geom_smooth( method= "loess", level= 0.95,  na.rm= TRUE )

  print( grafico )
}

dev.off()

