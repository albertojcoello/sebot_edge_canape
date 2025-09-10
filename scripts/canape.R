#==============================================================================#
# 0. Dependencias ----
#==============================================================================#
# Instalación de paquetes necesarios para ejecutar el script. En caso de que
# ya los tengas instalados comenta esta parte o sáltatela.
install.packages("ape") # 'ape' es un paquete para trabajar con árboles
                        # filogenéticos
install.packages("canaper") # 'canaper' es un paquete que incorpora el cálculo
                            # de métricas utilizadas en filogenia espacial
install.packages("terra") # 'terra' se utilizará para hacer figuras con los
                          # diferentes mapas que resultarán al final del taller



#==============================================================================#
# 1. Configuración inicial ----
#==============================================================================#

## 1.1. Activación de paquetes ----
# En R es necesario cargar los paquetes que vayamos a utilizar en cada sesión,
# por lo que vamos a proceder a cargar los paquetes que ya tenemos instalados.
library(ape)
library(canaper)
library(terra)



## 1.2. Cargar los datos ----
# Establecer el directorio de trabajo utilizando la carpeta que hemos descargado
# de GitHub: https://github.com/albertojcoello/sebot_edge_canape. Es importante
# que utilicemos sólo las barras '/', ya que el otro tipo de barra ('\') dan
# problemas en R a la hora de indicar directorios.
setwd("/Users/ajcoello/github/sebot_edge_canape/")

# Cargar los datos de distribución
distribucion <- read.csv("./datos/distribucion.csv")

# Cargar el árbol filogenético
filogenia <- read.tree("./datos/filogenia.tre")

# Mapas para hacer las figuras finales
cuadricula <- vect("./datos/shp/cuadricula.shp")
fronteras <- vect("./datos/shp/fronteras.shp")



## 1.3. Preparación de los datos ----

#### 1.3.1. Distribución ----
# Lo primero que debemos hacer es eliminar aquellas áreas de nuestro territorio
# de estudio que no tienen ningún taxón (ya que si no, los análisis siguientes
# pueden fallar). Además, esto creará un nuevo objeto con la riqueza de taxones
# que utilizaremos más adelante.

# Calculamos la riqueza de taxones para cada área. Nótese que se elimina del
# cálculo la primera columna que se corresponde con el nombre del área
riqueza <- apply(distribucion[, -1], 1, sum)

# Transformamos el vector de riqueza en un 'data frame' para poder trabajar
# luego más fácilmente
riqueza <- data.frame(
    id      = distribucion$id,
    riqueza = riqueza
)

# Mantenemos todas aquellas filas (áreas) que tienen un valor de riqueza de
# tazones mayor de 0
distribucion <- distribucion[riqueza$riqueza > 0, ]

# Hacemos lo mismo con los datos de riqueza
riqueza <- riqueza[riqueza$riqueza > 0, ]




### 1.3.2. Cuadrícula ----
# El siguiente paso va a ser asignar al mapa de la cuadrícula el nombre 'id'
# para la variable correspondiente al nombre de las áreas (única variable que
# tiene). Esto se hace porque así será más fácil combinar los datos cuando
# hagamos los mapas.
names(cuadricula) <- "id"

# Además, es necesario también modificar el sistema de coordenadas para que
# todas las capas tengan el mismo (en nuestro caso, vamos a utilizar el sistema
# WGS84)
cuadricula <- project(cuadricula, "+proj=longlat +datum=WGS84")
fronteras <- project(fronteras, "+proj=longlat +datum=WGS84")



#==============================================================================#
# 2. Estadísticas básicas ----
#==============================================================================#

## 2.1. Riqueza de taxones ----

#### 2.1.1. Mapa ----
# Más arriba ($1.3) hemos calculado la riqueza de especies. Ahora vamos a pasar
# a hacer el mapa para ver su distribución. Para ello, vamos a utilizar 'terra'
# con distintas capas que ya tenemos cargadas.

# Primero unimos agregamos los datos de riqueza a la cuadrícula, indicando que
# vamos a usar cómo referencia el nombre de las áreas ('id')
cuadricula <- merge(cuadricula, riqueza, by = "id", all.x = TRUE)

# A continuación hacemos el mapa de riqueza
plot(
    cuadricula, "riqueza",                                   # Indicamos la capa
                                                             # que vamos a
                                                             # dibujar y la
                                                             # variable
    type   = "continuous",                                   # Tipo de variable
    col    = colorRampPalette(c("#F5EBDC", "#594a31"))(100), # Colores mínimo y
                                                             # máximo
    border = NULL,                                           # Color de borde
    axes   = FALSE,                                          # No dibujar ejes
    legend = TRUE,                                           # Dibujar leyenda
    main   = "Riqueza (R)"                                   # Título
)

# Añadimos los límites de los países para crear un diseno más claro (se pueden
# agregar tantas capas cómo se quiera)
plot(
    fronteras, # Capa que contiene las fronteras de los países
    add = TRUE # Indicamos que se tiene que dibujar sobre el mapa que ya tenemos
)



## 2.2. Endemismo ponderado ----

### 2.2.1 Cálculo ----
# Para calcular el endemismo ponderado primero debemos calcular primero la
# amplitud de la distrubución de cada taxón
amplitud <- apply(distribucion[, -1], 2, sum)

# A continuación pasamos esa amplitud de la distribución a su inverso para tener
# una medida de la rareza de los taxones
rareza <- 1/amplitud

# El siguiente paso es hacer un nuevo data frame de distribución pero
# sustituyendo los valores de presencia de los taxones por su valor de rareza
endemismoPonderado <- distribucion
for(i in names(distribucion)[-1]) {
    value <- rareza[names(rareza) == i]
    endemismoPonderado[names(endemismoPonderado) == i] <-
        endemismoPonderado[names(endemismoPonderado) == i] * value
}

# Finalmente, se calcula la suma de valores de endemismo ponderado para cada
# celda del territorio (se guarda en un data frame tal y cómo se ha hecho
# anteriormente con la riqueza)
endemismoPonderado <- data.frame(
    id                 = endemismoPonderado$id,
    endemismoPonderado = apply(endemismoPonderado[, -1], 1, sum)
)



### 2.2.2. Mapa ----
# Finalmente, añadimos los valores de endemismo ponderado a nuestra cuadrícula,
# creamos el mapa tal y cómo se ha hecho para la riqueza y añadimos la capa de
# las fronteras de los países
cuadricula <- merge(cuadricula, endemismoPonderado, by = "id", all.x = TRUE)
plot(
    cuadricula, "endemismoPonderado",
    type   = "continuous",
    col    = colorRampPalette(c("#F5EBDC", "#594a31"))(100),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Endemismo Ponderado (WE)"
)
plot(
    fronteras,
    add = TRUE
)



#==============================================================================#
# 3. Estadísticas avanzadas ----
#==============================================================================#

## 3.1. Iteraciones ----
# En primer lugar se ha de generar una simulación vara establecer el número de
# iteraciones necesarias para asegurar que se genera una distribución nula
# apropiada para nuestros datos


### 3.1.1. Cálculo ----
# Definir los parámetros para establecer las simulaciones
models <- c("swap", "curveball")
iterations <- c(10000, 100000, 1000000)

# Crear una lista vacía para guardar los resultados
simulacion_iteraciones <- list()

# Generar todas las posibles combinaciones de modelos e iteraciones
for(i in models) {
    for(j in iterations) {
        # Indicamos un mensaje para mostrar que ha comenzado el proceso
        cat(paste0(i, " (", j, " iteraciones)"), sep = "")
        
        r <- cpr_iter_sim(
            comm         = distribucion[, -1], # Datos de distribución
            null_model   = i,                  # Tipo de modelo
            n_iterations = j,                  # Número de iteraciones
            thin         = max(j/100, 1)       # Frecuencia de muestreo de las
                                               # iteraciones
        )
        simulacion_iteraciones[[length(simulacion_iteraciones) + 1]] <- r
        names(simulacion_iteraciones)[length(simulacion_iteraciones)] <- 
            paste0(i, "_", j)
        
        # Indicamos un mensaje para mostrar que ha finalizado el proceso
        cat(" ✓", sep = "\n")
    }
}

### 3.1.2. Gráfico ----
# El siguiente paso va a ser generar un gráfico para ver que modelo y valor de
# iteraciones vamos a utilizar para hacer el test de aleatorización

# Primero dividimos la ventana de gráficos para mostrarlos todos de manera
# simultánea
par(mfrow = c(length(models), length(iterations)))

# Corremos un bucle para dibujar todos los gráficos
for(i in 1:length(simulacion_iteraciones)) {
    plot(
        x    = simulacion_iteraciones[[i]][[1]], # Iteraciones
        y    = simulacion_iteraciones[[i]][[2]], # Resultado de similaridad
        type = "l",                              # Indicar que es un gráfico de
                                                 # tipo línea
        main = names(simulacion_iteraciones)[i], # Nombre del gráfico (modelo e
                                                 # iteraciones máximas)
        xlab = "Iteraciones",                    # Etiqueta del eje X
        ylab = "% Similaridad"                   # Etiqueta del eje Y
    )
}



## 3.2. Test de aleatorización ----
# Una vez se ha hecho la simulación del número de iteraciones se puede elegir
# el modelo y el número de iteraciones que se van a utilizar para nuestro
# análisis. En este caso concreto vamos a usar un modelo Curveball y 60 000
# iteraciones.

# Para correr el test de aleatorización usamos el siguiente comando
aleatorizacion <- cpr_rand_test(
    comm         = distribucion[, -1], # Datos de distribución
    phy          = filogenia,          # Filogenia
    null_model   = "curveball",        # Modelo para calcular iteraciones
    n_reps       = 999,                # Número de comunidades aleatorias a
                                       # replicar
    n_iterations = 60000,              # Número de iteraciones del modelo nulo
    thin         = 1,                  # Parámetro de thinning (no necesario ni
                                       # para swap ni curveball)
    metrics      = c("pd",             # Métricas a calcular
                     "rpd",
                     "pe",
                     "rpe"),
    tbl_out      = FALSE,              # FALSE: generar un data frame en lugar
                                       # de una tibble
    quiet        = FALSE               # FALSE: mostrar los posibles errores
)

# Añadimos el nombre de las celdas a los resultados del test de aleatorización
aleatorizacion$id <- distribucion[[1]]

# Lo siguiente que vamos a hacer es calcular los niveles de significación para
# cada celda para las diferentes métricas que hemos calculado, ya que los
# utilizaremos posteriormente para hacer los mapas

# Diversidad filogenética
aleatorizacion <- cpr_classify_signif(aleatorizacion, c("pd"))

# Endemismo filogenético
aleatorizacion <- cpr_classify_signif(aleatorizacion, c("pe"))

# Diversidad filogenética relativa
aleatorizacion <- cpr_classify_signif(aleatorizacion, c("rpd"))

# Endemismo filogenético relativo
aleatorizacion <- cpr_classify_signif(aleatorizacion, c("rpe"))

# Categorización de centros de endemismo
aleatorizacion <- cpr_classify_endem(aleatorizacion)

# Una vez termine el test de aleatorización debemos guardarlo en nuestro
# ordenador para así poder acceder a él en un futuro sin tener que repetir todo
# el proceso
write.csv(aleatorizacion, "./resultados/aleatorizacion.csv", row.names = FALSE)

# En caso de volver a querer cargar los datos más adelante utilizaremos las
# siguientes líneas
aleatorizacion <- read.csv("./resultados/aleatorizacion.csv")

# Finalmente, unificamos la información del test de aleatorización con la
# cuadrícula
cuadricula <- merge(cuadricula, aleatorizacion, by = "id", all.x = TRUE)



## 3.3. Mapas ----
# Finalmente procederemos a dibujar los mapas finales de todas las variables
# típicad de filogenia espacial que quedaban por mostrar.

### 3.3.1. Diversidad filogenética ----
plot(
    cuadricula, "pd_obs",
    type   = "continuous",
    col    = colorRampPalette(c("#F5EBDC", "#594a31"))(100),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Diversidad filogenética (PD)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.2. Diversidad filogenética (significación) ----
plot(
    cuadricula, "pd_signif",
    type   = "classes",
    sort   = c("< 0.01", "< 0.025", "not significant", "> 0.975", "> 0.99"),
    col    = c("#8b0000", "#ff0000", "#fafad2", "#4876ff", "#27408b"),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Diversidad filogenética (PD)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.3. Endemismo filogenético ----
plot(
    cuadricula, "pe_obs",
    type   = "continuous",
    col    = colorRampPalette(c("#F5EBDC", "#594a31"))(100),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Endemismo filogenético (PE)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.4. Endemismo filogenético (significación) ----
plot(
    cuadricula, "pe_signif",
    type   = "classes",
    sort   = c("< 0.01", "< 0.025", "not significant", "> 0.975", "> 0.99"),
    col    = c("#8b0000", "#ff0000", "#fafad2", "#4876ff", "#27408b"),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Endemismo filogenético (PE)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.5. Diversidad filogenética relativa ----
plot(
    cuadricula, "rpd_signif",
    type   = "classes",
    sort   = c("< 0.01", "< 0.025", "not significant", "> 0.975", "> 0.99"),
    col    = c("#8b0000", "#ff0000", "#fafad2", "#4876ff", "#27408b"),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Diversidad filogenética relativa (RPD)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.6. Endemismo filogenético relativo ----
plot(
    cuadricula, "rpe_signif",
    type   = "classes",
    sort   = c("< 0.01", "< 0.025", "not significant", "> 0.975", "> 0.99"),
    col    = c("#8b0000", "#ff0000", "#fafad2", "#4876ff", "#27408b"),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "Endemismo filogenético relativo (RPE)"
)
plot(
    fronteras,
    add = TRUE
)



### 3.3.7. CANAPE
plot(
    cuadricula, "endem_type",
    type   = "classes",
    sort   = c("neo", "paleo", "not significant", "mixed", "super"),
    col    = c("#ff0000", "#4876ff", "#fafad2", "#cb7fff", "#9d00ff"),
    border = NULL,
    axes   = FALSE,
    legend = TRUE,
    main   = "CANAPE"
)
plot(
    fronteras,
    add = TRUE
)
