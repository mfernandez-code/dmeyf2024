
# limpio la memoria
format(Sys.time(), "%a %b %d %X %Y")
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

require("rlang")
require("yaml")
require("data.table")
envg$EXPENV <- list()
envg$EXPENV$bucket_dir <- "~/buckets/b1"
envg$EXPENV$exp_dir <- "~/buckets/b1/expw333/"
envg$EXPENV$wf_dir <- "~/buckets/b1/flow333/"
envg$EXPENV$repo_dir <- "~/dmeyf2024/"
envg$EXPENV$datasets_dir <- "~/buckets/b1/datasets/"
envg$EXPENV$messenger <- "~/install/zulip_enviar.sh"
# cargo las  "librerias" mlog y exp_lib
dir.create( envg$EXPENV$exp_dir, showWarnings = FALSE)
dir.create( envg$EXPENV$wf_dir, showWarnings = FALSE)

mlog <- paste0( envg$EXPENV$repo_dir,"/src/lib/mlog.r")
source( mlog )

exp_lib <- paste0( envg$EXPENV$repo_dir,"/src/lib/exp_lib.r")
source( exp_lib )

dir.create("~/buckets/b1/exp/lineademuerte/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/lineademuerte/" )

require( "data.table" )

# leo el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_03.csv.gz" )
#_________________________________________________
#___________________________________________________
#catastrophy
CA_catastrophe_base <- function( pinputexps, metodo )
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija
  
  param_local$meta$script <- "/src/wf-etapas/z1201_CA_reparar_dataset.r"
  
  # Opciones MachineLearning EstadisticaClasica Ninguno MICE
  param_local$metodo <- metodo
  param_local$atributos_eliminar <- c( "tmobile_app", "cmobile_app_trx") #, "Visa_mlimitecompra" )
  param_local$semilla <- NULL  # no usa semilla, es deterministico
  
  return( exp_correr_script( param_local ) ) # linea fija}
}
dataset <- CA_catastrophe_base( metodo="MachineLearning")

#_______________________________________________________
#_______________________________________________________
#FE intra mes
FEintra_manual_base <- function( pinputexps )
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija
  
  
  param_local$meta$script <- "/src/wf-etapas/z1301_FE_intrames_manual.r"
  
  param_local$semilla <- NULL  # no usa semilla, es deterministico
  
  return( exp_correr_script( param_local ) ) # linea fija
}
dataset <-FEintra_manual_base()

#______________________________________________________________
#_______________________________________________________________
#data drifting
DR_drifting_base <- function( pinputexps, metodo)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija
  
  
  param_local$meta$script <- "/src/wf-etapas/z1401_DR_corregir_drifting.r"
  
  # valores posibles
  #  "ninguno", "rank_simple", "rank_cero_fijo", "deflacion", "estandarizar"
  param_local$metodo <- metodo
  param_local$semilla <- NULL  # no usa semilla, es deterministico
  
  return( exp_correr_script( param_local ) ) # linea fija
}

dataset<-DR_drifting_base(metodo="deflacion")
#__________________________________________________________________
# Feature Engineering Historico  Baseline
# deterministico, SIN random

FEhist_base <- function( pinputexps)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 ) # linea fija
  
  param_local$meta$script <- "/src/wf-etapas/1501_FE_historia_tend3.r"
  
  param_local$lag1 <- TRUE
  param_local$lag2 <- TRUE # no me engraso con los lags de orden 2
  param_local$lag3 <- TRUE # no me engraso con los lags de orden 3
  
  # no me engraso las manos con las tendencias
  param_local$Tendencias1$run <- TRUE  # FALSE, no corre nada de lo que sigue
  param_local$Tendencias1$ventana <- 6
  param_local$Tendencias1$tendencia <- TRUE
  param_local$Tendencias1$minimo <- TRUE
  param_local$Tendencias1$maximo <- TRUE
  param_local$Tendencias1$promedio <- TRUE
  param_local$Tendencias1$ratioavg <- TRUE
  param_local$Tendencias1$ratiomax <- TRUE
  
  # no me engraso las manos con las tendencias de segundo orden
  param_local$Tendencias2$run <- TRUE
  param_local$Tendencias2$ventana <- 12
  param_local$Tendencias2$tendencia <- TRUE
  param_local$Tendencias2$minimo <- TRUE
  param_local$Tendencias2$maximo <- TRUE
  param_local$Tendencias2$promedio <- TRUE
  param_local$Tendencias2$ratioavg <- TRUE
  param_local$Tendencias2$ratiomax <- TRUE
  
  # no me engraso las manos con las tendencias de segundo orden
  param_local$Tendencias3$run <- TRUE
  param_local$Tendencias3$ventana <- 3
  param_local$Tendencias3$tendencia <- TRUE
  param_local$Tendencias3$minimo <- TRUE
  param_local$Tendencias3$maximo <- TRUE
  param_local$Tendencias3$promedio <- TRUE
  param_local$Tendencias3$ratioavg <- TRUE
  param_local$Tendencias3$ratiomax <- TRUE
  
  param_local$semilla <- NULL # no usa semilla, es deterministico
  
  return( exp_correr_script( param_local ) ) # linea fija
}

dataset<-FEhist_base()

# # Feature Engineering Historico
# cols_lagueables <- copy( setdiff(
#   colnames(dataset),
#   c("numero_de_cliente", "foto_mes", "clase_ternaria")
# ) )
# 
# 
# dataset[, 
#         paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"),
#         by = numero_de_cliente,
#         .SDcols = cols_lagueables
# ]
# 
# dataset[, 
#         paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"),
#         by = numero_de_cliente,
#         .SDcols = cols_lagueables
# ]
# 
# # agrego los delta lags de orden 1
# for (vcol in cols_lagueables)
# {
#   dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
#   dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
# }
#___________________________________________________________________________
#------------------------------------------------------------------------------
#  Agregado de variables de Random Forest, corrido desde LightGBM
#  atencion, parmetros para generar variables, NO para buen modelo
#  azaroso, utiliza semilla

FErf_attributes_base <- function( pinputexps, ratio, desvio)
{
  if( -1 == (param_local <- exp_init())$resultado ) return( 0 )# linea fija
  
  
  param_local$meta$script <- "/src/wf-etapas/z1311_FE_rfatributes.r"
  
  # Parametros de un LightGBM que se genera para estimar la column importance
  param_local$train$clase01_valor1 <- c( "BAJA+2", "BAJA+1")
  param_local$train$training <- c( 202101, 202102, 202103)
  
  # parametros para que LightGBM se comporte como Random Forest
  param_local$lgb_param <- list(
    # parametros que se pueden cambiar
    num_iterations = 20,
    num_leaves  = 16,
    min_data_in_leaf = 1000,
    feature_fraction_bynode  = 0.2,
    
    # para que LightGBM emule Random Forest
    boosting = "rf",
    bagging_fraction = ( 1.0 - 1.0/exp(1.0) ),
    bagging_freq = 1.0,
    feature_fraction = 1.0,
    
    # genericos de LightGBM
    max_bin = 31L,
    objective = "binary",
    first_metric_only = TRUE,
    boost_from_average = TRUE,
    feature_pre_filter = FALSE,
    force_row_wise = TRUE,
    verbosity = -100,
    max_depth = -1L,
    min_gain_to_split = 0.0,
    min_sum_hessian_in_leaf = 0.001,
    lambda_l1 = 0.0,
    lambda_l2 = 0.0,
    
    pos_bagging_fraction = 1.0,
    neg_bagging_fraction = 1.0,
    is_unbalance = FALSE,
    scale_pos_weight = 1.0,
    
    drop_rate = 0.1,
    max_drop = 50,
    skip_drop = 0.5,
    
    extra_trees = FALSE
  )
  
  
  return( exp_correr_script( param_local ) ) # linea fija
}

dataset <- FErf_attributes_base()
#-----------------------------------------------------------------------------
#------------------------------------------------------------------------------


GLOBAL_semilla <- 990211

campos_buenos <- copy( setdiff(
  colnames(dataset), c("clase_ternaria"))
)

set.seed(GLOBAL_semilla, kind = "L'Ecuyer-CMRG")
dataset[, azar:=runif(nrow(dataset))]

dfuture <- dataset[foto_mes==202109]

# undersampling de los CONTINIA al 8%
dataset[, fold_train :=  foto_mes<= 202107 &
          (clase_ternaria %in% c("BAJA+1", "BAJA+2") |
             azar < 0.02 ) ]

dataset[, clase01 := ifelse( clase_ternaria=="CONTINUA", 0, 1 )]

require("lightgbm")

# dejo los datos en el formato que necesita LightGBM
dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[foto_mes==202107, campos_buenos, with = FALSE]),
  label = dataset[foto_mes==202107, clase01],
  free_raw_data = TRUE
)

# aqui se hace la magia informatica con los pesos para poder reutilizar
#  el mismo dataset para training y final_train
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[fold_train == TRUE, campos_buenos, with = FALSE]),
  label = dataset[fold_train == TRUE, clase01],
  weight = dataset[fold_train == TRUE, ifelse( foto_mes<=202106, 1.0, 0.0)],
  free_raw_data = TRUE
)

rm( dataset )
gc(full = TRUE, verbose= FALSE) # garbage collection


nrow( dfuture )
nrow( dvalidate )
nrow( dtrain )

# parametros basicos del LightGBM
param_basicos <- list(
  objective = "binary",
  metric = "auc",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  verbosity = -100,
  force_row_wise = TRUE, # para evitar warning
  seed = GLOBAL_semilla,
  max_bin = 31,
  learning_rate = 0.03,
  feature_fraction = 0.5
)


EstimarGanancia_AUC_lightgbm <- function(x) {
  
  message(format(Sys.time(), "%a %b %d %X %Y"))
  param_train <- list(
    num_iterations = 2048, # valor grande, lo limita early_stopping_rounds
    early_stopping_rounds = 200
  )
  
  param_completo <- c(param_basicos, param_train, x)
  
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = "auc", 
    param = param_completo,
    verbose = -100
  )
  
  AUC <- modelo_train$record_evals$valid$auc$eval[[modelo_train$best_iter]]
  
  # esta es la forma de devolver un parametro extra
  attr(AUC, "extras") <- list("num_iterations"= modelo_train$best_iter)
  
  rm(modelo_train)
  gc(full= TRUE, verbose= FALSE)
  
  return(AUC)
}


# paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


configureMlr(show.learner.output = FALSE)

# configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
# por favor, no desesperarse por lo complejo
obj.fun <- makeSingleObjectiveFunction(
  fn = EstimarGanancia_AUC_lightgbm, # la funcion que voy a maximizar
  minimize = FALSE, # estoy Maximizando AUC
  noisy = FALSE,
  par.set = makeParamSet(
    makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
    makeIntegerParam("min_data_in_leaf", lower = 64L, upper = 8192L)
  ),
  has.simple.signature = FALSE # paso los parametros en una lista
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = "lineademuerte.RDATA"
)

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = 10  # cantidad de iteraciones inteligentes
)

# defino el método estandar para la creacion de los puntos iniciales
#   los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# mas configuraciones
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)



bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)


tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
setorder(tb_bayesiana, -y, -num_iterations) # ordeno en forma descendente por AUC = y
mejores_hiperparametros <- tb_bayesiana[1, # el primero es el de mejor AUC
                                        list(num_leaves, min_data_in_leaf, num_iterations)]

print(mejores_hiperparametros)


set_field(dtrain, "weight", rep(1.0, nrow(dtrain)))


param_final <- c(param_basicos, mejores_hiperparametros)


final_model <- lgb.train(
  data = dtrain,
  param = param_final,
  verbose = -100
)



prediccion <- predict(
  final_model,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)


# genero la tabla de entrega
tb_entrega <- dfuture[, list(numero_de_cliente)]
tb_entrega[, prob := prediccion]

# ordeno por probabilidad descendente
setorder(tb_entrega, -prob)
tb_entrega[, prob := NULL] # ya no necesito prob
tb_entrega[, Predicted := 0L]
tb_entrega[1:11000, Predicted := 1L]

fwrite(tb_entrega, file = "lineademuerte_11001.csv" )

format(Sys.time(), "%a %b %d %X %Y")

