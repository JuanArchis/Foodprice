#' R6 Class Representa los módulos 1
#'
#' @description
#' Mes y Ciudad deben estar en formato texto
#'
#' @details
#' Primera versión

#' @name
#' Paquete_Foodprice_ver_0_0_1
#'

library("R6")


Paquete_Foodprice_ver_0_0_1=R6Class(classname="Paquete_Foodprice-ver-0.0.1",

# ------------------------------------------------------------#
#   Definición de parámetros (entradas) de la clase          #
#-----------------------------------------------------------#

public=list(
    Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","openxlsx","tidyverse",
                    "lpSolve","Rglpk","Rsymphony","scatterplot3d","reshape"),
    # Parámetros de la clase
    data_list_precios=NULL,
    data_list_abas=NULL,
    Mes=NULL,
    Año=NULL,
    Ciudad=NULL,
    Margenes=NULL,
    Data_Model=NULL,



    # Parámetros privados

    Data=NULL, # Data final del módulo 1
    DRI_m=NULL,
    DRI_f=NULL,
    Data2=NULL,
    Data3=NULL,


    initialize=function(data_list_precios,data_list_abas,Mes,Año,Ciudad,Margenes=NULL,Data_Model=NULL){

    self$data_list_precios=data_list_precios
    self$data_list_abas=data_list_abas
    self$Mes=Mes
    self$Año=Año
    self$Ciudad=Ciudad

  # Margenes como parámetro opcional
    if (!is.null(Margenes)) {
      if (!is.vector(Margenes) || length(Margenes) != 8) {
        stop("El parámetro 'Margenes' debe ser un vector de longitud 8")
      } else {
        self$Margenes = Margenes
      }
    } else {
      self$Margenes = NULL
    }

      # Data_Model como parámetro opcional
      if (!is.null(Data_Model)) {
      if (!is.data.frame(Data_Model)) {
        stop("El parámetro 'Data_Model' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$Data_Model = Data_Model
      }
    } else {
      self$Data_Model = NULL
    }
    },

    # ---------------------------------------------------------------#
    #          Primer método: Definición de librerias base           # COMPLETO
    #----------------------------------------------------------------#

    Librerias=function(Librerias_base){
    Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","Rsymphony","scatterplot3d","reshape","R6","rio","janitor") # Nombra las librerias necesarias

    if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
    pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes

    cat("Las librerias usadas en el presente paquete son:",Librerias_base_print,"\n")

    },

    # ------------------------------------------------------------#
    #   MÓDULO 1: PREPARACIÓN DE DATOS DE INSUMO A LOS MODELOS   # COMPLETO
    #-----------------------------------------------------------#

    Módulo_1=function(){

    # ---------------------------------------------------------------#
    #   Definición de parámetros privados y constantes del código    # COMPLETO
    #----------------------------------------------------------------#

    Nombres_Meses = c("Enero","Febrero","Marzo","Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre","Octubre","Noviembre","Diciembre")

    Semestres=c("I_Semestre","II_Semestre")

    Enero = seq(from = as.Date(paste(self$Año,"1","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"1","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Febrero = seq(from = as.Date(paste(self$Año,"2","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"2","28", sep = "-"),format = "%Y-%m-%d"), by =1)
    Marzo = seq(from = as.Date(paste(self$Año,"3","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"3","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Abril = seq(from = as.Date(paste(self$Año,"4","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"4","30", sep = "-"),format = "%Y-%m-%d"), by =1)
    Mayo = seq(from = as.Date(paste(self$Año,"4","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"5","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Junio = seq(from = as.Date(paste(self$Año,"6","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"6","30", sep = "-"),format = "%Y-%m-%d"), by =1)
    Julio = seq(from = as.Date(paste(self$Año,"7","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"7","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Agosto = seq(from = as.Date(paste(self$Año,"8","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"8","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Septiembre = seq(from = as.Date(paste(self$Año,"9","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"9","30", sep = "-"),format = "%Y-%m-%d"), by =1)
    Octubre = seq(from = as.Date(paste(self$Año,"10","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"10","31", sep = "-"),format = "%Y-%m-%d"), by =1)
    Noviembre = seq(from = as.Date(paste(self$Año,"11","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"11","30", sep = "-"),format = "%Y-%m-%d"), by =1)
    Diciembre = seq(from = as.Date(paste(self$Año,"12","1", sep = "-"),format = "%Y-%m-%d"), to = as.Date(paste(self$Año,"12","30", sep = "-"),format = "%Y-%m-%d"), by =1)

    Semestre_I = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio")
    Semestre_II = c("Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

    Lista_Mes=list(Enero,Febrero,Marzo,Abril, Mayo, Junio, Julio, Agosto, Septiembre,Octubre,Noviembre,Diciembre);names(Lista_Mes)=Nombres_Meses
    Lista_Semestres = list(Semestre_I, Semestre_II);names(Lista_Semestres)=c("I_Semestre","II_Semestre")
    Fecha=Lista_Mes[[self$Mes]]


    # ---------------------------------------------------------------#
    #   Depuración de la data: Datos Mayoristas  (Fuente: SIPSA)     # COMPLETO
    #----------------------------------------------------------------#

    #-- Lee la data cargada por el usuario, asgina el més con base en la posición de la hoja y depura un poco las columnas combinadas y texto inecesario de excel

    Meses=Nombres_Meses[1:length(self$data_list_precios)-1];Data_Sipsa_Precios=(self$data_list_precios[[which(Meses %in% self$Mes)+1]]) # Se extraen los meses disponibles con base en la data dada

    if(self$Mes %in% Meses==FALSE){cat("Error,",self$Mes," aún no está disponible en los precios mayoristas de SIPSA",sep="")}
    Data_Sipsa_Precios=Data_Sipsa_Precios[-c(1:4,nrow(Data_Sipsa_Precios)),-c(6,7)];Data_Sipsa_Precios=na.omit(Data_Sipsa_Precios) # Un poco de depuración

    colnames(Data_Sipsa_Precios) = c("Fecha", "Grupo", "Alimento", "Mercado", "Precio_kg");Data_Sipsa_Precios$Precio_kg=as.numeric(Data_Sipsa_Precios$Precio_kg);Data_Sipsa_Precios$Fecha=as.Date(paste(self$Año,which(Meses %in% self$Mes),"1", sep = "-"),format = "%Y-%m-%d") # Cambia los nombres y asigna fechas

    # -- Establece la ciudad de interés

    if(self$Ciudad=="Cali") {
        Data_Sipsa_Precios = Data_Sipsa_Precios %>% filter(Mercado %in% c("Cali, Cavasa","Cali, Galería Alameda","Cali, La Floresta","Cali, Santa Elena","Cali, Siloé"))
    } else {cat("Error,",self$Ciudad," aún no está disponible en el paquete",sep="")}


    #-- Crea el vector de alimentos con base en la data de SIPSA precios mayoristas
    Alimentos_Sipsa_Precios = levels(as.factor(Data_Sipsa_Precios$Alimento))

    # Base de datos de recepción para el precio único de cada alimento
    Data_Sipsa_Precios_Unicos = data.frame(Alimentos_Sipsa_Precios, Precio_kg = NA);colnames(Data_Sipsa_Precios_Unicos)=c("Alimento","Precio_kg")

    # El precio único de cada alimento corresponde al precio promedio entre los 5 mercados

    for (i in 1:length(Alimentos_Sipsa_Precios)) {
        Media_Precios = data.frame()
        Media_Precios = Data_Sipsa_Precios[Data_Sipsa_Precios$Alimento == Alimentos_Sipsa_Precios[i],]
        Data_Sipsa_Precios_Unicos$Precio_kg[i] = mean(Media_Precios$Precio_kg)
        rm(Media_Precios)
    }

    #----# Salida: Data_Sipsa_Precios_Unicos #----#

    # ---------------------------------------------------------------#
    # Depuración de la data: Datos Abastecimiento  (Fuente: SIPSA)   # COMPLETO
    #----------------------------------------------------------------#

    # -- Carga y limpia los datos de abastecimiento SIPSA
    Data_Sipsa_Abas=(self$data_list_abas[[as.integer(which(sapply(Lista_Semestres, function(x) self$Mes %in% x)))+1]]) # Se extraen los meses disponibles con base en la data dada
    colnames(Data_Sipsa_Abas) = c("Ciudad_Mercado", "Fecha","Cod_Dep", "Cod_Mun", "Dep_Proc", "Mun_Proc","Grupo", "Alimento", "Cantidad_KG");Data_Sipsa_Abas$Fecha=as.Date(Data_Sipsa_Abas$Fecha)
    Data_Sipsa_Abas <- janitor::remove_empty(Data_Sipsa_Abas, which = "cols") # Elimina las columnas con total NA


    # -- Seleciona la ciudad de interés
    if(self$Ciudad=="Cali") {
        Data_Sipsa_Abas = Data_Sipsa_Abas %>% filter(Ciudad_Mercado %in% c("Cali, Cavasa","Cali, Santa Elena"))} else {cat("Error,",self$Ciudad," aún no está disponible en el paquete",sep="")}


    # --- Crea el abastecimiento mensual de SIPSA

    # Filtrar la información de abastecimiento para Cavasa y SE
    Data_Sipsa_Abas_Cavasa = subset(Data_Sipsa_Abas,Ciudad_Mercado == "Cali, Cavasa",);Data_Sipsa_Abas_SE = subset(Data_Sipsa_Abas,Ciudad_Mercado == "Cali, Santa Elena",);Alimentos_Sipsa_Abs=levels(as.factor(Data_Sipsa_Abas$Alimento))

    # Crea data frame el cual albergará por columnas la suma de abastecimiento en el mes indicado
    Data_Sipsa_Abas_Unicos= data.frame(Alimentos_Sipsa_Abs, Cantidad_Total_KG_Cavasa= NA,Cantidad_Total_KG_SE=NA,Total_cali=NA);colnames(Data_Sipsa_Abas_Unicos)=c("Alimento_abs","Cantidad_Total_KG_Cavasa","Cantidad_Total_KG_SE","Total_cali")

    # Ciclo donde se llena la data a partir de la suma
    for(i in 1:length(Alimentos_Sipsa_Abs)){

        Datos_Alimento_Cavasa=subset(Data_Sipsa_Abas_Cavasa, Alimento==Alimentos_Sipsa_Abs[i])
        Datos_Alimento_SE=subset(Data_Sipsa_Abas_SE, Alimento==Alimentos_Sipsa_Abs[i])

        Data_Sipsa_Abas_Unicos$Cantidad_Total_KG_Cavasa[i]=sum(Datos_Alimento_Cavasa[is.element(Datos_Alimento_Cavasa$Fecha, Fecha),]$Cantidad_KG)
        Data_Sipsa_Abas_Unicos$Cantidad_Total_KG_SE[i]=sum(Datos_Alimento_SE[is.element(Datos_Alimento_SE$Fecha, Fecha),]$Cantidad_KG)

        Data_Sipsa_Abas_Unicos["Cantidad_Total_KG_Cavasa"][Data_Sipsa_Abas_Unicos["Cantidad_Total_KG_Cavasa"] == 0] = NA
        Data_Sipsa_Abas_Unicos["Cantidad_Total_KG_SE"][Data_Sipsa_Abas_Unicos["Cantidad_Total_KG_SE"] == 0] = NA

    }

    Data_Sipsa_Abas_Unicos$Total_cali= rowSums(Data_Sipsa_Abas_Unicos[c("Cantidad_Total_KG_Cavasa","Cantidad_Total_KG_SE")], na.rm =TRUE);Data_Sipsa_Abas_Unicos["Total_cali"][Data_Sipsa_Abas_Unicos["Total_cali"] == 0] = NA

    Data_Sipsa_Abas_Unicos=Data_Sipsa_Abas_Unicos[,c("Alimento_abs","Total_cali")]
    #----# Salida: Data_Sipsa_Abas_Unicos #----#



    # ---------------------------------------------------------------#
    #                                Mapeos                          # REVISAR GENERALIZACIÓN
    #----------------------------------------------------------------#

    # SIPSA (precios mayoristas-abastecimiento)
    data(Mapeo_Precios_Abs, package = "Foodprice",envir=parent.env(environment()))



    # SIPSA-TCAC (Códigos de sipsa a  Composición de Alimentos Colombianos)

    data(Mapeo_Sipsa_TCAC, package = "Foodprice",envir=parent.env(environment()));colnames(Mapeo_Sipsa_TCAC) = c("Alimento", "Codigo_TCAC")
    Mapeo_Sipsa_TCAC = Mapeo_Sipsa_TCAC %>% filter(Codigo_TCAC %in% setdiff(levels(as.factor(Mapeo_Sipsa_TCAC$Codigo_TCAC)), "EX000"))

    # TCAC-GABAS (TCAC con Guías Alimentarias y SIN composición )
    data(Mapeo_Sipsa_TCAC_GABAS_Grupos, package = "Foodprice",envir=parent.env(environment()))
    Variables_Necesarias = c("codigo", "Nombre del Alimento","Grupos  GABAS", "Subgrupos  GABAS",  "Grupo TCAC");Mapeo_Sipsa_TCAC_GABAS_Grupos = Mapeo_Sipsa_TCAC_GABAS_Grupos[Variables_Necesarias]
    colnames(Mapeo_Sipsa_TCAC_GABAS_Grupos) = c("Cod_TCAC", "Alimento", "Grupo_GABAS", "Subgrupo_GABAS", "Grupo_TCAC")


    #--------               -------#
    #    Criterios de exclusión    #
    #-----                  -------#

    data(Primer_Criterio_Lista_Alimentos, package = "Foodprice",envir=parent.env(environment()))


    #--------               -------#
    #    Composición nutricional   #
    #-----                  -------#


   data(Mapeo_Sipsa_TCAC_Carga_2, package = "Foodprice",envir=parent.env(environment()))

    Micro_Macro_Nutrientes_Necesarios = c("codigo", "Nombre del Alimento", "% de parte comestible", "Factor de conversión", "Energia (Kcal)", "Proteina (g)", "Carbohidratos Totales (g)", "Lipidos (g)", "Calcio (mg)",
                                            "Zinc (mg)", "Hierro (mg)", "Magnesio (mg)", "Fosforo (mg)", "Vitamina C (mg)", "Tiamina (mg)", "Riboflavina (mg)",
                                            "Niacina (mg)", "Folatos (mcg)", "Vitamina B12 (mcg)", "Vitamina A (ER)", "Sodio (mg)", "Micr sin inf (por alimento)")

    Sipsa_TCAC=Mapeo_Sipsa_TCAC_Carga_2[Micro_Macro_Nutrientes_Necesarios];colnames(Sipsa_TCAC)[1] = "Cod_TCAC";colnames(Sipsa_TCAC)[2] = "Alimento_TCAC"


    #--------               -------#
    # Intercambios (equivalencias) #
    #-----                  -------#

    # Porciones_GABAS = read_excel("C://Users//kmili//Downloads//Metodologia_CIAT-main//datos_primarios//requerimientos_fuente//Recomendaciones y gramos de intercambios GABAS.xlsx", sheet = 2);colnames(Porciones_GABAS) = c("Cod_TCAC", "Alimentos", "Subgrupo_GABAS", "Energia_100g", "Energia_Int", "Intercambio_g")

    # -----------------------------------------------------------------#
    #                       Seleción de data                           # COMPLETO
    #------------------------------------------------------------------#


    # Asignación de un alimento con abastecimiento a cada producto de la base de datos de precios
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Precios_Unicos, Mapeo_Precios_Abs, by = "Alimento", all.x = TRUE)



    # Asignación del valor de abastecimiento en cada caso
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Abas_Unicos, Data_abs_precios_Sipsa,by = "Alimento_abs", all.x = TRUE)

    # Selección de las variables de interés
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[c("Alimento", "Precio_kg", "Total_cali")]
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[order(Data_abs_precios_Sipsa$Alimento),]
    colnames(Data_abs_precios_Sipsa) = c("Alimento",paste0("Precio_kg_", self$Mes), paste0("Total_Cali_", self$Mes))


    # -----------------------------------------------------------------#
    #                       Criterios de Exc                           # REVISAR GENERALIZACIÓN Y PERFECCIÓN DEL MISMO
    #------------------------------------------------------------------#

    Data_abs_precios_Sipsa_ABS=Data_abs_precios_Sipsa[,c("Alimento",paste0("Total_Cali_",self$Mes))]

    #--------                    -------#
    #  Segundo criterio de exclusión    #
    #-----                       -------#

    # Vector de alimentos incluidos a priori por alguna de las siguientes razones:
    # (1) Unidades de medidas no comparables
    # (2) Niveles de abastecimiento bajo por falta de subdivision (e.g. tipos de quesos)
    # (3) Alimentos no sujetos a factores estacionales

    Alimentos_Exclu = c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA","Huevo rojo extra", "Leche pasteurizada (1.1 L)", "Queso doble crema",
                        "Queso cuajada", "Queso Caquetá", "Pollo entero con visceras","Lomitos de atún en lata", "Galletas saladas", "Sardinas en lata","Chocolate amargo")

    Alimentos_Inclu = setdiff(Data_abs_precios_Sipsa_ABS$Alimento, Alimentos_Exclu)
    criterio_2 = Data_abs_precios_Sipsa_ABS %>% filter(Alimento %in% Alimentos_Inclu)

    # Eliminar niveles NA de abastecimiento (Flujos de carga nulos)
    criterio_2 = criterio_2 %>% drop_na(paste0("Total_Cali_",self$Mes))

    # Calcular cuantiles
    quant = quantile(criterio_2[,2], na.rm = TRUE)

    # Eliminar los alimentos cuyo flujo de carga está abajo del percentil 25
    criterio_2 = criterio_2[criterio_2[,2] < quant[2],]


    #--------                               -------#
    #  Primer  criterio de exclusión: Nutrición    #
    #-----                                  -------#

    Alimentos_Exclu_Criterio_1 = Primer_Criterio_Lista_Alimentos[Primer_Criterio_Lista_Alimentos$`COD. TCAC` == "EX000","Alimento"]

    #--------                               -------#
    # Lista depurada con base en los dos criterios #
    #-----                                  -------#

    # Abastecimiento nulo
    Alimentos_NA = Data_abs_precios_Sipsa_ABS %>% filter(is.na(get(paste0("Total_Cali_", self$Mes))))

    # Construir el vector con la totalidad de alimentos excluidos
    # (criterio 1, criterio 2, flujo de carga nulo y exclusiones ad hoc)

    Alimentos_Excluidos_Criterio_1 = Alimentos_Exclu_Criterio_1["Alimento"]
    Alimentos_Exclu_Criterio_2 = criterio_2$Alimento
    Alimentos_Excluidos_Na = Alimentos_NA$Alimento
    Alimentos_Excluidos = c(Alimentos_Excluidos_Criterio_1, Alimentos_Exclu_Criterio_2, Alimentos_Excluidos_Na,"Queso Caquetá")

    # Exclusión de los alimentos y construcción de la lista definitiva
    Lista_Alimentos_Definitiva = Data_abs_precios_Sipsa_ABS %>% filter(Alimento %in% setdiff(levels(as.factor(Data_abs_precios_Sipsa_ABS$Alimento)), Alimentos_Excluidos))

    # -----------------------------------------------------------------#
    #                        Marginalización                           #  REVISAR GENERALZIACIÓN
    #------------------------------------------------------------------#

    # Carga de datos minoristas

    Nombre_Mercar_mes <- paste0("Mercar_", self$Mes)

    # Cargar el conjunto de datos utilizando get() y el nombre creado
    # Mercar_mes= get(Nombre_Mercar_mes, envir = as.environment("package:Foodprice"))
    assign("Mercar_mes",get("Mercar_Julio", envir = as.environment("package:Foodprice")),envir=parent.env(environment()))
    # Un poco de depuración
    Mercar_mes = Mercar_mes[, colSums(is.na(Mercar_mes)) != nrow(Mercar_mes)];colnames(Mercar_mes) = c("Codigo_mercar", "Descripcion", "Fecha", "Precio", "Unidad");Mercar_mes$Precio = as.numeric(gsub("\\,", "", Mercar_mes$Precio))


    #-- Construcción data por grupos (SIPSA)

    Grupos_Alimentos_Sipsa = Data_Sipsa_Precios[c("Alimento", "Grupo")];Grupos_Alimentos_Sipsa = Grupos_Alimentos_Sipsa[!duplicated(Grupos_Alimentos_Sipsa), ]
    Precios_Grupos_SIPSA = merge(Data_Sipsa_Precios_Unicos, Grupos_Alimentos_Sipsa,by = "Alimento", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
    # Ignorar cavasa por ahora

    #--------                    -------#
    #  Mapeo : Mayorista y Minorista    #  Falta generalizar
    #-----                       -------#
    data(Mapeo_Mayorista_Minorista, package = "Foodprice",envir=parent.env(environment()))
        Precios_Grupos_SIPSA$Precio_minorista_kg = NA


    #--------                    -------#
    #   Mapeo: :Alimentos en gramos     #  Falta generalizar
    #-----                       -------#

    drop = c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA", "Huevo rojo extra")

    Mapeo_Mayorista_Minorista_subset_1 = Mapeo_Mayorista_Minorista %>% filter(Alimento %in% setdiff(levels(as.factor(Precios_Grupos_SIPSA$Alimento)), drop))

    for (j in 1:nrow(Mapeo_Mayorista_Minorista_subset_1)) {
        alpha_1 = Mapeo_Mayorista_Minorista_subset_1[j,]
        levels(as.factor(alpha_1$Alimento))
        alpha_1 = alpha_1[, colSums(is.na(alpha_1)) != nrow(alpha_1)]

        alpha_2 = data.frame(matrix(ncol=3, nrow = ((ncol(alpha_1) - 1)/2)))

        colnames(alpha_2) = c("Alimento", "Codigo_mercar", "Cantidad_g")


        for (i in 1:nrow(alpha_2)) {
        alpha_2[i,] = alpha_1[c("Alimento",
                                paste0("Cod_mercar_", i),
                                paste0("Unidad_",i))]
        }

        alpha_2[alpha_2 == "NA"] = NA
        alpha_2 = merge(alpha_2, Mercar_mes[c("Codigo_mercar", "Descripcion", "Precio")], by = "Codigo_mercar")
        alpha_2$Precio_minorista_kg = (alpha_2$Precio/as.numeric(alpha_2$Cantidad_g))*1000
        x_1 = which(Precios_Grupos_SIPSA$Alimento == levels(as.factor(alpha_2$Alimento)))
        Precios_Grupos_SIPSA$Precio_minorista_kg[x_1] = mean(alpha_2$Precio_minorista_kg)

        rm(alpha_1, alpha_2)
    }


    #--------                    -------#
    # Mapeo: :Alimentos en mililitros   #  Falta generalizar
    #-----                       -------#

    # Aceite vegetal (1000 ml)
    keep = "Aceite vegetal mezcla"
    Mapeo_Mayorista_Minorista_subset_2 = Mapeo_Mayorista_Minorista %>% filter(Alimento %in% keep)

    alpha_1 = Mapeo_Mayorista_Minorista_subset_2
    (levels(as.factor(alpha_1$Alimento)))
    alpha_1 = alpha_1[, colSums(is.na(alpha_1)) != nrow(alpha_1)]
    alpha_2 = data.frame(matrix(ncol=3, nrow = ((ncol(alpha_1) - 1)/2)))
    colnames(alpha_2) = c("Alimento", "Codigo_mercar", "Cantidad_g")

    for (i in 1:nrow(alpha_2)) {
        alpha_2[i,] = alpha_1[c("Alimento",
                                paste0("Cod_mercar_", i),
                                paste0("Unidad_",i))]
    }
    alpha_2[alpha_2 == "NA"] = NA
    alpha_2 = merge(alpha_2, Mercar_mes[c("Codigo_mercar", "Descripcion", "Precio")], by = "Codigo_mercar")
    alpha_2$Precio_minorista_kg = (alpha_2$Precio/as.numeric(gsub("([0-9]+).*$", "\\1", alpha_2$Cantidad_g)))*1000
    Precios_Grupos_SIPSA$Precio_minorista_kg[which(Precios_Grupos_SIPSA$Alimento == levels(as.factor(alpha_2$Alimento)))] = max(alpha_2$Precio_minorista_kg)

    rm(alpha_1, alpha_2)


    #--------                    -------#
    # Mapeo: :Alimentos en unidades     #  Falta generalizar
    #-----                       -------#


    keep = c("Huevo rojo A", "Huevo rojo AA", "Huevo rojo extra")
    Mapeo_Mayorista_Minorista_Subset_3 = Mapeo_Mayorista_Minorista %>% filter(Alimento %in% keep)

    for (j in 1:nrow(Mapeo_Mayorista_Minorista_Subset_3)) {
        alpha_1 = Mapeo_Mayorista_Minorista_Subset_3[j,]
        (levels(as.factor(alpha_1$Alimento)))
        alpha_1 = alpha_1[, colSums(is.na(alpha_1)) != nrow(alpha_1)]

        alpha_2 = data.frame(matrix(ncol=3, nrow = ((ncol(alpha_1) - 1)/2)))

        colnames(alpha_2) = c("Alimento", "Codigo_mercar", "Cantidad_g")


        for (i in 1:nrow(alpha_2)) {
        alpha_2[i,] = alpha_1[c("Alimento",
                                paste0("Cod_mercar_", i),
                                paste0("Unidad_",i))]
        }

        alpha_2[alpha_2 == "NA"] = NA
        alpha_2 = merge(alpha_2, Mercar_mes[c("Codigo_mercar", "Descripcion", "Precio")], by = "Codigo_mercar")
        alpha_2$Precio_minorista_kg = (alpha_2$Precio/as.numeric(gsub("([0-9]+).*$", "\\1", alpha_2$Cantidad_g)))
        Precios_Grupos_SIPSA$Precio_minorista_kg[which(Precios_Grupos_SIPSA$Alimento == levels(as.factor(alpha_2$Alimento)))] = max(alpha_2$Precio_minorista_kg)

        rm(alpha_1, alpha_2)
    }

    #--------                    -------#
    #  Margenes de comercialziación     #
    #-----                       -------#

    grupos_margenes <- levels(as.factor(Precios_Grupos_SIPSA$Grupo));margenes_mes <- data.frame(Grupo = grupos_margenes, margen_estimado = NA,error_estandar = NA)

    for (k in 1:length(grupos_margenes)) {
        grupo_z <- Precios_Grupos_SIPSA %>%
        filter(Grupo %in% grupos_margenes[k]) %>%
        drop_na()
        grupo_z$margen <- (grupo_z$Precio_minorista_kg - grupo_z$Precio_kg) / grupo_z$Precio_kg
        quant <- quantile(grupo_z$margen, probs = c(.25, .75), na.rm = FALSE)
        iqr <- IQR(grupo_z$margen);lower <- quant[1] - 1.5 * iqr;upper <- quant[2] + 1.5 * iqr
        no_outliers <- subset(grupo_z$margen, grupo_z$margen > lower & grupo_z$margen < upper)
        grupo_z <- grupo_z %>% filter(margen %in% no_outliers)
        margenes_mes$margen_estimado[which(margenes_mes$Grupo == grupos_margenes[k])] <- mean(grupo_z$margen) * 100
        margenes_mes$error_estandar[which(margenes_mes$Grupo == grupos_margenes[k])] <- sd(grupo_z$margen)
    }



    #--------                    -------#
    #Margenes de comercialziación hist  #
    #-----                       -------#


    meses <- c("Julio", "Agosto", "Septiembre")
    for (k in meses) {
        file <- paste0("grupos_margenes_",k)
        margen <- get(file, envir = as.environment("package:Foodprice"))
        colnames(margen) <- c("Grupo", paste0("Margen_estimado_", k), paste0("Error_estándar_", k))
        assign(paste0("margen_", k), margen)
        rm(margen)
    }

    # Fusionar los datos en un solo marco de datos
    Margenes_Historicos <- Reduce(function(x, y) merge(x, y, by = "Grupo", all.x = TRUE), mget(paste0("margen_", meses)))

    # Calcular el margen medio
    Margenes_Historicos$margen_medio <- rowMeans(Margenes_Historicos[,c(2,4,6)])


    # -----------------------------------------------------------------#
    #                 Estimación precios minoristas                    #
    #------------------------------------------------------------------#



    if (!is.null(self$Margenes)) {

      Margenes_Historicos$margen_medio=self$Margenes

      precios_mayoristas_grupos_margenes <- merge(Precios_Grupos_SIPSA,
                                                  Margenes_Historicos[c("Grupo", "margen_medio")],
                                                  by = "Grupo", all.x = TRUE)
      precios_mayoristas_grupos_margenes$Precio_minorista_kg <- precios_mayoristas_grupos_margenes$Precio_kg * (1 + precios_mayoristas_grupos_margenes$margen_medio/100)


    } else {

      precios_mayoristas_grupos_margenes <- merge(Precios_Grupos_SIPSA,
                                                  Margenes_Historicos[c("Grupo", "margen_medio")],
                                                  by = "Grupo", all.x = TRUE)

    precios_mayoristas_grupos_margenes$Precio_minorista_kg <- precios_mayoristas_grupos_margenes$Precio_kg * (1 + precios_mayoristas_grupos_margenes$margen_medio/100)

    }


    Estimación_Precios_Minoristas <- precios_mayoristas_grupos_margenes %>%
        filter(Alimento %in% Lista_Alimentos_Definitiva$Alimento) # exclusión criterios 1 y 2


    #--------------------------------------------------- Salida principal 1 ----------------------------- Estimación_Precios_Minoristas ----------------------------------------#


    #--------                    -------  #
    # Mapeo: Precios con contenidos nutri #
    #-----                       -------  #

    precios_kg <- Estimación_Precios_Minoristas[c("Alimento", "Precio_minorista_kg")]
    colnames(Mapeo_Sipsa_TCAC) <- c("Alimento", "Cod_TCAC")

    dataset_sim <- merge(precios_kg, Mapeo_Sipsa_TCAC, by = "Alimento", all.x = TRUE)
    dataset_sim <- merge(dataset_sim, Sipsa_TCAC, by = "Cod_TCAC", all.x = TRUE)

    # Subset de alimentos con unidades de medida no comparables

    # Subset 2: alimentos en unidades de medida distintas a 100g
    dataset_sim_2 <- dataset_sim %>%
        filter(Alimento %in% c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA","Huevo rojo extra"))

    # Subset 1: alimentos en 100g
    dataset_sim_1 <- dataset_sim %>%
        filter(Alimento %in% setdiff(levels(as.factor(dataset_sim$Alimento)), c("Aceite vegetal mezcla","Huevo rojo A", "Huevo rojo AA","Huevo rojo extra")))

    # Subset 1
    dataset_sim_1$Serving <- rep(100, length(precios_kg))
    dataset_sim_1$Precio_100g <- dataset_sim_1$Precio_minorista_kg/10

    # Subset 2

    aux_dataset <- data.frame(matrix(nrow = 4, ncol = 3))
    colnames(aux_dataset) <- c("Alimento", "Serving", "Factor_gramos")

    aux_dataset$Alimento <- c("Aceite vegetal mezcla", "Huevo rojo A", "Huevo rojo AA", "Huevo rojo extra")

    aux_dataset$Serving <- c("1 Litro", "1 Unidad", "1 Unidad", "1 Unidad")

    aux_dataset$Factor_gramos <- c(100/920, 100/50, 100/60, 100/67)

    dataset_sim_2 <- merge(dataset_sim_2, aux_dataset, by = "Alimento", all.x = TRUE)

    dataset_sim_2$Precio_100g <- dataset_sim_2$Precio_minorista_kg * dataset_sim_2$Factor_gramos

    dataset_sim_2 <- dataset_sim_2[colnames(dataset_sim_1)]

    # Base de datos de precios (combinación de ambos subsets)
    dataset_sim <- rbind(dataset_sim_1, dataset_sim_2)
    dataset_sim$Serving <- rep(100, length(precios_kg))

    #--------                    -------  #
    #     Cálculo de precios ajustados    #
    #-----                       -------  #

    # omitir los alimentos sin dato sobre la parte comestible
    dataset_sim$`Lipidos (g)` = as.numeric(dataset_sim$`Lipidos (g)`)

    dataset_sim["Factor de conversión"][dataset_sim["Factor de conversión"] == "SD"] = NA
    dataset_sim = dataset_sim[!is.na(dataset_sim$`Factor de conversión`),]
    dataset_sim$`Factor de conversión` = as.numeric(dataset_sim$`Factor de conversión`)

    # calculo del precio ajustado
    dataset_sim$Precio_100g_ajust = dataset_sim$Precio_100g*dataset_sim$`Factor de conversión`


    #--------                    -------  #
    #     Composición nutricional         #
    #-----                       -------  #

    # reemplazar los valores NA de los macronutrientes por 0
    macro = c("Proteina (g)", "Carbohidratos Totales (g)", "Lipidos (g)")
    for (k in 1:length(macro)) {
        dataset_sim[macro[k]][is.na(dataset_sim[macro[k]])] = "0"
    }

    #informaci?n faltante sobre micronutrientes
    dataset_sim$`Micr sin inf (por alimento)` = rowSums(is.na(dataset_sim))

    dataset_sim = dataset_sim %>% mutate(Int = cut(dataset_sim$`Micr sin inf (por alimento)`, c(c(1,4, 7), Inf),
                                                    right = FALSE))
    dataset_sim$Int = as.character(dataset_sim$Int);dataset_sim["Int"][is.na(dataset_sim["Int"])] = "0";dataset_sim %>% count("Int")

    # por simplicidad, om?tase los alimentos con informaci?n faltante

    dataset_sim = dataset_sim[complete.cases(dataset_sim), ]

    drop = c("Precio_minorista_kg", "Alimento_TCAC", "Precio_100g", "% de parte comestible", "Factor de conversión",
            "Micr sin inf (por alimento)", "Int");dataset_sim = dataset_sim[setdiff(colnames(dataset_sim), drop)]

    dataset_sim = dataset_sim[c(1,2, 20, 21, 3:19)];dataset_sim = dataset_sim[order(dataset_sim$Alimento),];dataset_sim[1,3]="1 Litro";Datos_Insumo_Modelos=dataset_sim

    Datos_Insumo_Modelos[1,3]=100
    #--------------------------------------------------- Salida principal 2 ----------------------------- Datos_Insumo_Modelos ----------------------------------------#


    # -----------------------------------------------------------------#
    #     Salidas de los métodos en en embiente GLOBAL                 #
    #------------------------------------------------------------------#
    if (!is.null(self$Data_Model) && is.data.frame(self$Data_Model) && nrow(self$Data_Model) > 0 && ncol(self$Data_Model) > 0) {
      assign("Datos_Insumo_Modelos",self$Data_Model,envir = globalenv());assign("Estimación_Precios_Minoristas",Estimación_Precios_Minoristas,envir = globalenv())

    } else {
      assign("Datos_Insumo_Modelos",Datos_Insumo_Modelos,envir = globalenv());assign("Estimación_Precios_Minoristas",Estimación_Precios_Minoristas,envir = globalenv())
    }




    # -----------------------------------------------------------------#
    #               Estado de la depuración del módulo                 #
    #------------------------------------------------------------------#

 if(length(warnings())<100) {cat("Depuración del módulo 1 exitosa, la salida principal son las estimaciónes de los alimentos por (Gr); para acceder a esta use «Datos_Insumo_Modelos» en el ambiente global", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

self$Data=Datos_Insumo_Modelos
self$Data3=Datos_Insumo_Modelos


},



    # ------------------------------------------------------------#
    #        MÓDULO 2: CARGA DE DATOS DE REQURIMIENTOS           # -- EN PROGRESO (FALTA GENERALIZAR sleft$ para comunicar con módulos siguientes)
    #-----------------------------------------------------------#

Módulo_2=function(){


  #----------------------------------------#
  # Carga de requerimientos Modelo 1 y 2   #
  #---------------------------------------#

# Modificación de datos del módulo 1
names(self$Data) = c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio")

self$Data <- self$Data %>%
  transform(grupo = substr(Cod_TCAC, start = 1, stop = 1)) %>%
  arrange(Alimento)

self$Data = self$Data[order(self$Data$Alimento),]

self$Data2=self$Data 

#-------------------------------------#
# Carga de requerimientos Modelos   #
#------------------------------------#

# carga de porciones

data(intercambio_gramos, package = "Foodprice",envir=parent.env(environment()))
# carga de requerimientos

data(int_req_m, package = "Foodprice",envir=parent.env(environment()))


data(int_req_f, package = "Foodprice",envir=parent.env(environment()))


data(DRI_M, package = "Foodprice",envir=parent.env(environment()))
data(DRI_F, package = "Foodprice",envir=parent.env(environment()))


# carga de proporción por grupos de alimentos
data(EER_share_M, package = "Foodprice",envir=parent.env(environment()));data(EER_share_F, package = "Foodprice",envir=parent.env(environment()))

#  carga exclusión
data(exclusion_3er_modelo, package = "Foodprice",envir=parent.env(environment()))

# Carga cartidad de alimentos a selecionar
data(cantidad_alimentos_seleccionar, package = "Foodprice",envir=parent.env(environment()))

# Carga requerimentos de energía por grupos de alimentos
data(EER_share_rangos, package = "Foodprice",envir=parent.env(environment()))

# Cambiar nombres de columnas
colnames(EER_share_F)[1] = "Grupo_GABAS"
colnames(EER_share_M)[1] = "Grupo_GABAS"
colnames(cantidad_alimentos_seleccionar)[1] = "Grupo_GABAS"

# Función para la recodificacion de la contribucion de energia segun grupos de alimentos
funcion_EER <- function(x) {
  x$Grupo_GABAS <- recode(x$Grupo_GABAS, "Azúcares" = "Azúcares","Carnes, Huevos, Leguminosas, Frutos secos y Semillas" = "Carnes, huevos y leguminosas","Cereales, Raíces, Tubérculos y Plátanos" = "Cereales y raíces","Frutas y Verduras" = "Frutas y verduras","Grasas" = "Grasas", "Leche y productos lácteos" = "Lácteos","Sin categoría" = "Sin categoría")
  return(x)
}


# Recodificar grupos de alimentos
EER_share_F = funcion_EER(EER_share_F)
EER_share_M = funcion_EER(EER_share_M)
cantidad_alimentos_seleccionar= funcion_EER(cantidad_alimentos_seleccionar)



# Cambiar nombres de columnas en exclusion_3er_modelo
colnames(exclusion_3er_modelo) = c("Alimento", "Cod_TCAC");colnames(EER_share_rangos) = c("Grupo_GABAS", "Min", "Max")

if(length(warnings())<100) {cat("Depuración del módulo 2 exitosa, se cargaron los requerimientos de los tres modelos: DRI_M, DRI_F, EER_share_M, EER_share_F, EER_share_rangos, exclusion_3er_modelo, cantidad_alimentos_seleccionar ", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

data(TCAC, package = "Foodprice",envir=parent.env(environment()))

}, # BASES Resultantes: Data, DRI_M, DRI_F, EER_share_M, EER_share_F, EER_share_rangos, exclusion_3er_modelo, cantidad_alimentos_seleccionar


Módulo_3=function(){

  #--------------------------------------------------------------------------#
  #                   Primer Modelo - Construcción de datos                  #
  #-------------------------------------------------------------------------#


  # excluir azúcar porque no es viable (K003 y K004)
  self$Data = self$Data %>% filter(!Cod_TCAC %in% c("K003", "K004"))

  # vector de precios
  precios = self$Data$Precio_100g_ajust

  # nombre alimentos
  alimentos=self$Data$Alimento

  # Matriz de contenidos energéticos
  A = matrix(as.vector(self$Data$Energia), ncol = length(alimentos))

  #-----------------------------------------------------------------------------------------#
  #                   Primer Modelo Masculino - Solución y verificación                    #
  #--------------------------------------------------------------------------------------#

  #--------- Solución

  modelo_1 = data.frame(alimentos)
  modelo_1 = modelo_1 %>% add_row(alimentos = "Costo")
  colnames(modelo_1) = "Alimentos"
  edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)", "[31, 50)", "[51, 70)", ">70")
  #signos de las restricciones lineales
  constr_signs = c("=")

  #requerimientos de energia por edad
  dri_edad = list()

  for (i in 1:8) {
    b = vector()
    b = DRI_M[i,2]
    dri_edad = append(dri_edad, b)
    names(dri_edad)[i] = paste0("b_", i)
  }
  #solucion del modelo
  for (i in 1:8) {
    df_1 = data.frame()
    df_2 = data.frame()
    b = dri_edad[[i]]
    opt_sol = lp(direction = "min",
                 objective.in = precios,
                 const.mat = A,
                 const.dir = constr_signs,
                 const.rhs = b,
                 compute.sens = TRUE)
    df_1 = cbind(alimentos, opt_sol$solution)
    colnames(df_1) = c("Alimentos", edad[i])
    df_2 = data.frame("Costo", opt_sol$objval)
    colnames(df_2) = colnames(df_1)
    df_1 = rbind(df_1, df_2)
    modelo_1 = merge(modelo_1,df_1, by = "Alimentos")
    rm(b, df_1, df_2)
  }

  # presentacion de la solucion por grupos etarios

  #eliminar cantidades NA
  modelo_1[modelo_1 == 0] = NA
  modelo_1_res = modelo_1[rowSums(is.na(modelo_1[,2:length(colnames(modelo_1))])) != ncol(modelo_1[,2:length(colnames(modelo_1))]),]

  #presentaci?n del resultado en gramos
  modelo_1_res[nrow(modelo_1_res)+1,] = modelo_1_res[1,]

  for (k in 2:ncol(modelo_1_res)) {
    modelo_1_res[3,k] = as.numeric(modelo_1_res[1,k])*100
  }

  modelo_1_res[3,1] = paste0(modelo_1_res[1,1],"(100 g)")
  modelo_1_res = modelo_1_res[c(1,3,2),]

  #--------- Validación

  # vector de energía para el alimento seleccionado
  energia_modelo_1 = self$Data %>% filter(Alimento %in%
                                       modelo_1_res[1,1])

  # valores optimos para cada grupo demografico
  optimo = list()
  desv = list()
  length(optimo) = length(edad)
  length(desv)  = length(optimo)
  n = length(optimo)
  solucion_grupos =  modelo_1_res[1,]

  for (k in 1:n) {
    optimo[[k]] = as.numeric(solucion_grupos[1,k+1])*energia_modelo_1$Energia
    desv[[k]] = optimo[[k]] - dri_edad[[k]]
  }

  # vector de energía para el alimento seleccionado
  energia_modelo_1 = self$Data %>% filter(Alimento %in%
                                       modelo_1_res[1,1])

  # valores optimos para cada grupo demografico
  optimo = list()
  desv = list()
  length(optimo) = length(edad)
  length(desv)  = length(optimo)
  n = length(optimo)
  solucion_grupos =  modelo_1_res[1,]

  for (k in 1:n) {
    optimo[[k]] = as.numeric(solucion_grupos[1,k+1])*energia_modelo_1$Energia
    desv[[k]] = optimo[[k]] - dri_edad[[k]]
  }
  assign(paste("Modelo_1_M", self$Mes, self$Año, sep = "_"),modelo_1_res,envir = globalenv())


 #-----------------------------------------------------------------------------------------#
 #                   Primer Modelo Femenino - Solución y verificación                    #
#--------------------------------------------------------------------------------------#
  # signos de restricciones lineales
  constr_signs = c("=")

  #requerimientos de energia por edad
  dri_edad = list()

  for (i in 1:14) {
    b = vector()
    b = DRI_F[i,2]
    dri_edad = append(dri_edad, b)
    names(dri_edad)[i] = paste0("b_", i)
  }



  # base de datos de resultados
  modelo_1 = data.frame(alimentos)
  modelo_1 = modelo_1 %>% add_row(alimentos = "Costo")
  colnames(modelo_1) = "Alimentos"
  edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)", "[31, 50)",
           "[51, 70)", ">70", "gestantes < 18 años", "gestantes 19 a 30 años",
           "gestantes 31 a 50 años", "lactantes < 18 años", "lactantes 19 a 30 años",
           "lactantes 31 a 50 años")

  #solucion del modelo
  for (i in 1:14) {
    df_1 = data.frame()
    df_2 = data.frame()
    b = dri_edad[[i]]
    opt_sol = lp(direction = "min",
                 objective.in = precios,
                 const.mat = A,
                 const.dir = constr_signs,
                 const.rhs = b,
                 compute.sens = TRUE)
    df_1 = cbind(alimentos, opt_sol$solution)
    colnames(df_1) = c("Alimentos", edad[i])
    df_2 = data.frame("Costo", opt_sol$objval)
    colnames(df_2) = colnames(df_1)
    df_1 = rbind(df_1, df_2)
    modelo_1 = merge(modelo_1,df_1, by = "Alimentos")
    rm(b, df_1, df_2)
  }

  # presentacion de la solucion por grupos etarios

  #eliminar cantidades NA
  modelo_1[modelo_1 == 0] = NA
  modelo_1_res = modelo_1[rowSums(is.na(modelo_1[,2:length(colnames(modelo_1))])) != ncol(modelo_1[,2:length(colnames(modelo_1))]),]

  #presentaci?n del resultado en gramos
  modelo_1_res[nrow(modelo_1_res)+1,] = modelo_1_res[1,]

  for (k in 2:ncol(modelo_1_res)) {
    modelo_1_res[3,k] = as.numeric(modelo_1_res[1,k])*100
  }

  modelo_1_res[3,1] = paste0(modelo_1_res[1,1],"(100 g)")
  modelo_1_res = modelo_1_res[c(1,3,2),]

  assign(paste("Modelo_1_F", self$Mes, self$Año, sep = "_"),modelo_1_res,envir = globalenv())

  if(length(warnings())<100) {cat("Depuración del módulo 3 exitosa, la salida principal son los alimentos del primer modelo por sexo, para acceder a estos use «Modelo_1_F o Modelo_1_M» en el ambiente global", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}


},

Módulo_4=function(){

#--------------------------------------------------------------------------------------#
#                   Segundo Modelo  - Construcción de datos                           #
#------------------------------------------------------------------------------------#
names(self$Data2) = c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio")

self$Data <- self$Data2 %>%
  transform(grupo = substr(Cod_TCAC, start = 1, stop = 1)) %>%
  arrange(Alimento)

  # vector de precios
  precios = self$Data2$Precio_100g_ajust

  # nombre alimentos
  alimentos=self$Data2$Alimento

  # Matriz de contenidos energéticos
# matriz de contenidos nutricionales y energéticos
keep = c("Energia", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC", "Folatos", "VitaminaA",
         "Tiamina", "Riboflavina", "Niacina", "VitaminaB12", "Magnesio", "Fosforo", "Sodio",
         "Calcio", "Hierro", "Zinc")



A = self$Data2[keep] %>% as.matrix() %>% t()
A = rbind(A, A[-1,])

#recodificar la información de requerimeitnos nutricionales
# por simplicidad, se supone que el nivel máximo para los nutrientes sin limite superior es de 999999

f_x = function(a){
  df = data.frame()
  colnames(a) = c("edad", "energia", "proteina_l", "proteina_u", "grasa_l", "grasa_u",
                  "cho_l", "cho_u", "vit_c_l", "vit_c_u", "folato_l", "folato_u",
                  "vit_a_l", "vit_a_u", "tiamina_l", "tiamina_u", "ribo_l", "ribo_u",
                  "niacina_l", "niacina_u", "b12_l", "b12_u", "mg_l", "mg_u", "p_l", "p_u",
                  "na_l", "na_u", "ca_l", "ca_u", "fe_l", "fe_u", "zinc_l", "zinc_u")

  a[is.na(a)] = 999999
  df = a
  return(df)
}

DRI_f = f_x(DRI_F)
DRI_m = f_x(DRI_M)

#vector de limites inferiores
lower = c("proteina_l", "grasa_l","cho_l", "vit_c_l", "folato_l",
          "vit_a_l", "tiamina_l", "ribo_l",
          "niacina_l", "b12_l", "mg_l", "p_l",
          "na_l", "ca_l", "fe_l", "zinc_l")

#vector de limites superiores
upper = setdiff(colnames(DRI_f),lower)
upper = upper[!upper %in% c("edad", "energia")]

#signos de las restricciones
constr_signs = c("=", rep(">=", length(lower)), rep("<=", length(upper)))

#--------------------------------------------------------------------------------------#
#                   Segundo Modelo Masculino  - Construcción de datos                       #
#------------------------------------------------------------------------------------#

# requerimientos de enegia por grupos etarios
dri_edad = list()

for (i in 1:8) {
  b = vector()
  b = as.numeric(DRI_m$energia[i])
  dri_edad = append(dri_edad, b)
  names(dri_edad)[i] = paste0("b_",i)
  rm(b)
}

# requerimientos nutricionales minimos (limites inferiores)
lower_list = list()
for (i in 1:8) {
  df = DRI_m
  df = df[lower]
  l = as.numeric(df[i,])
  lower_list[[i]] = l
  names(lower_list)[i] = paste0("lower_", i)
  rm(l , df)
}

# requerimientos nutricionales máximos (limites superiores)
upper_list = list()
for (i in 1:8) {
  df = DRI_m
  df = df[upper]
  u = as.numeric(df[i,])
  upper_list[[i]] = u
  names(upper_list)[i] = paste0("upper_", i)
  rm(u , df)
}

# vector b segun grupo de edad
b_2 = list()
for (k in 1:8) {
  b_2[[k]] = c(dri_edad[[k]], lower_list[[k]], upper_list[[k]])
}


  #-----------------------------------------------------------------------------------------#
  #                   Segundo Modelo Masculino - Solución y verificación                    #
  #--------------------------------------------------------------------------------------#

b_2[[8]] =  (1 - 0.067)*b_2[[7]]

# base de datos para la presentacion de resultados
modelo_2 = data.frame(alimentos)
modelo_2 = modelo_2 %>% add_row(alimentos = "Costo")
colnames(modelo_2) = "Alimentos"
edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)",
         "[31, 50)", "[51, 70)", ">70")

nutrientes = c("Grupo", "Nutriente", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC",
                    "Folatos" , "VitaminaA" ,    "Tiamina"  ,     "Riboflavina" ,
                    "Niacina" , "VitaminaB12" , "Magnesio" , "Fosforo", "Sodio" ,
                    "Calcio"  , "Hierro", "Zinc")

const_2 = as.data.frame(matrix(ncol = length(nutrientes)))
colnames(const_2) = nutrientes
const_2  = na.omit(const_2)


#solucion del modelo
for (i in 1:8) {
  (edad[i])

  #resolver el modelo

  df_1 = data.frame()
  df_2 = data.frame()
  b = b_2[[i]]
  opt_sol = lp(direction = "min",
               objective.in = precios,
               const.mat = A,
               const.dir = constr_signs,
               const.rhs = b,
               compute.sens = TRUE)
  df_1 = cbind(alimentos, opt_sol$solution)
  colnames(df_1) = c("Alimentos", edad[i])
  df_2 = data.frame("Costo", opt_sol$objval)
  colnames(df_2) = colnames(df_1)
  df_1 = rbind(df_1, df_2)
  modelo_2 = merge(modelo_2,df_1, by = "Alimentos")
  rm(b, df_1, df_2)

  #cumplimiento de las restricciones
  const = A[1:17,]%*%as.matrix(opt_sol$solution)
  const = data.frame(rownames(const), as.numeric(const))
  const = const[-1,]
  const = cbind(const, as.matrix(lower_list[[i]]))
  colnames(const) = c("Nutriente", "Opt", "Rest")
  const$Dif = ((const$Opt - const$Rest)/const$Rest)*100
  const[const<0] <- 0

  #redondear
  for (k in 1:nrow(const)) {
    for (l in 2:ncol(const)) {
      const[k,l] = round(as.numeric(const[k,l]), digits = 2)
    }
  }

  colnames(const)[which(colnames(const) == "Dif")] = paste0("Dif", " (%)")


  const = as.data.frame(t(const))
  colnames(const) = as.character(const[1,])
  const = const[-1,]

  const[,ncol(const)+ 1] = rownames(const)
  const[,ncol(const)+ 1] = c(edad[i], edad[i], edad[i])

  const = const[,c(ncol(const), (ncol(const)-1),1:(ncol(const)-2))]
  rownames(const) = seq(1, nrow(const), by = 1)
  colnames(const)[1] = "Grupo"
  colnames(const)[2] = "Nutriente"

  const_2 = rbind(const_2, const)
  rm(const)
}



#preparacion de resultados en gramos
modelo_2[modelo_2 == 0] = NA
modelo_2_res = modelo_2[rowSums(is.na(modelo_2[,2:length(colnames(modelo_2))])) != ncol(modelo_2[,2:length(colnames(modelo_2))]),]


modelo_2_res = modelo_2_res[-which(modelo_2_res$Alimentos == "Costo"),]


#presentacion de resultados en gramos
for (k in 2:ncol(modelo_2_res)) {
  modelo_2_res[,k] = as.numeric(modelo_2_res[,k])*100
}

modelo_2_res[nrow(modelo_2_res)+1, ] = modelo_2[which(modelo_2$Alimentos == "Costo"),]
modelo_2_res[is.na(modelo_2_res)] = 0

  assign(paste("Modelo_2_M", self$Mes, self$Año, sep = "_"),modelo_2_res,envir = globalenv())
  if(length(warnings())<100) {cat("Depuración del módulo 4 exitosa", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

#------------------------------------------------------------------------------------#
#                   Segundo Modelo Femenino- Construcción de datos                  #
#----------------------------------------------------------------------------------#


#requerimientos de energia segun grupos etarios
dri_edad = list()

for (i in 1:14) {
  b = vector()
  b = as.numeric(DRI_f$energia[i])
  dri_edad = append(dri_edad, b)
  names(dri_edad)[i] = paste0("b_",i)
  rm(b)
}

# requerimientos nutricionales minimos (limites inferiores)
lower_list = list()
for (i in 1:14) {
  df = DRI_f
  df = df[lower]
  l = as.numeric(df[i,])
  lower_list[[i]] = l
  names(lower_list)[i] = paste0("lower_", i)
  rm(l , df)
}


# requerimientos nutricionales maximos (limites superiores)

upper_list = list()
for (i in 1:14) {
  df = DRI_f
  df = df[upper]
  u = as.numeric(df[i,])
  upper_list[[i]] = u
  names(upper_list)[i] = paste0("upper_", i)
  rm(u , df)
}

# vector b segun grupo de edad

b_2 = list()
for (k in 1:14) {
  b_2[[k]] = c(dri_edad[[k]], lower_list[[k]], upper_list[[k]])
}




  #-----------------------------------------------------------------------------------------#
  #                   Segundo Modelo Femenino - Solución y verificación                    #
  #--------------------------------------------------------------------------------------#

# base de datos para la presentacion de resultados
modelo_2 = data.frame(alimentos)
modelo_2 = modelo_2 %>% add_row(alimentos = "Costo")
colnames(modelo_2) = "Alimentos"
edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)", "[31, 50)",
         "[51, 70)", ">70", "gestantes < 18 años", "gestantes 19 a 30 años",
         "gestantes 31 a 50 años", "lactantes < 18 años", "lactantes 19 a 30 años",
         "lactantes 31 a 50 años")

nutrientes = c("Grupo", "Nutriente", "Proteina", "Lipidos", "Carbohidratos", "VitaminaC",
                    "Folatos" , "VitaminaA" ,    "Tiamina"  ,     "Riboflavina" ,
                    "Niacina" , "VitaminaB12" , "Magnesio" , "Fosforo", "Sodio" ,
                    "Calcio"  , "Hierro", "Zinc")

const_2 = as.data.frame(matrix(ncol = length(nutrientes)))
colnames(const_2) = nutrientes
const_2  = na.omit(const_2)


#solucion del modelo
for (i in 1:14) {
  (edad[i])

  #resolver el modelo

  df_1 = data.frame()
  df_2 = data.frame()
  b = b_2[[i]]
  opt_sol = lp(direction = "min",
               objective.in = precios,
               const.mat = A,
               const.dir = constr_signs,
               const.rhs = b,
               compute.sens = TRUE)
  df_1 = cbind(alimentos, opt_sol$solution)
  colnames(df_1) = c("Alimentos", edad[i])
  df_2 = data.frame("Costo", opt_sol$objval)
  colnames(df_2) = colnames(df_1)
  df_1 = rbind(df_1, df_2)
  modelo_2 = merge(modelo_2,df_1, by = "Alimentos")
  rm(b, df_1, df_2)

  #cumplimiento de las restricciones
  const = A[1:17,]%*%as.matrix(opt_sol$solution)
  const = data.frame(rownames(const), as.numeric(const))
  const = const[-1,]
  const = cbind(const, as.matrix(lower_list[[i]]))
  colnames(const) = c("Nutriente", "Opt", "Rest")
  const$Dif = ((const$Opt - const$Rest)/const$Rest)*100
  const[const<0] <- 0

  #redondear
  for (k in 1:nrow(const)) {
    for (l in 2:ncol(const)) {
      const[k,l] = round(as.numeric(const[k,l]), digits = 2)
    }
  }

  colnames(const)[which(colnames(const) == "Dif")] = paste0("Dif", " (%)")


  const = as.data.frame(t(const))
  colnames(const) = as.character(const[1,])
  const = const[-1,]

  const[,ncol(const)+ 1] = rownames(const)
  const[,ncol(const)+ 1] = c(edad[i], edad[i], edad[i])

  const = const[,c(ncol(const), (ncol(const)-1),1:(ncol(const)-2))]
  rownames(const) = seq(1, nrow(const), by = 1)
  colnames(const)[1] = "Grupo"
  colnames(const)[2] = "Nutriente"

  const_2 = rbind(const_2, const)
  rm(const)
}



#preparacion de resultados en gramos
modelo_2[modelo_2 == 0] = NA
modelo_2_res = modelo_2[rowSums(is.na(modelo_2[,2:length(colnames(modelo_2))])) != ncol(modelo_2[,2:length(colnames(modelo_2))]),]


modelo_2_res = modelo_2_res[-which(modelo_2_res$Alimentos == "Costo"),]


#presentacion de resultados en gramos
for (k in 2:ncol(modelo_2_res)) {
  modelo_2_res[,k] = as.numeric(modelo_2_res[,k])*100
}

modelo_2_res[nrow(modelo_2_res)+1, ] = modelo_2[which(modelo_2$Alimentos == "Costo"),]
modelo_2_res[is.na(modelo_2_res)] = 0


  assign(paste("Modelo_2_F", self$Mes, self$Año, sep = "_"),modelo_2_res,envir = globalenv())
  if(length(warnings())<100) {cat("Depuración del módulo 4 exitosa, la salida principal son los alimentos del primer segundo por sexo, para acceder a estos use «Modelo_2_F o Modelo_2_M» en el ambiente global", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

},




Módulo_5=function(){


#---------------------------------------------------------------------------------------#
#                   Tercer Modelo  - Construcción de ecuaciones                        #
#-------------------------------------------------------------------------------------#

colnames(cantidad_alimentos_seleccionar) = c("Grupo_GABAS", "Cantidad")

# función de recodificacion
f_gabas_1 = function(a){
  dataset_0 = data.frame()
  a$Grupo_GABAS[which(a$Grupo_GABAS == "AZUCARES")] = "Azúcares"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CARNES, HUEVOS, LEGUMINOSAS SECAS, FRUTOS SECOS Y SEMILLAS")] = "Carnes, huevos y leguminosas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS")] = "Cereales y raíces"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "FRUTAS Y VERDURAS")] = "Frutas y verduras"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "GRASAS")] = "Grasas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "LECHE Y PRODUCTOS LACTEOS")] = "Lácteos"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "SIN CATEGORIA")] = "Sin categoría"
  dataset_0 = a
  return(dataset_0)
}

f_gabas_2 = function(a){
  dataset_0 = data.frame()
  a$Grupo_GABAS[which(a$Grupo_GABAS == "AZÚCARES")] = "Azúcares"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CARNES, HUEVOS, LEGUMINOSAS, FRUTOS SECOS Y SEMILLAS")] = "Carnes, huevos y leguminosas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CEREALES, RAÍCES, TUBÉRCULOS Y PLÁTANOS")] = "Cereales y raíces"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "VERDURAS Y FRUTAS")] = "Frutas y verduras"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "GRASAS")] = "Grasas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "LECHE Y PRODUCTOS LÁCTEOS")] = "Lácteos"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "SIN CATEGORIA")] = "Sin categoría"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "FRUTAS")] = "Frutas"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "VERDURAS")] = "Verduras"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CEREALES")] = "Cereales"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "RAICES")] = "Raices"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "TUBERCULOS")] = "Tuberculos"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "CARNES")] = "Carnes"
  a$Grupo_GABAS[which(a$Grupo_GABAS == "LEGUMINOSAS")] = "Leguminosas"
  dataset_0 = a
  return(dataset_0)
}


# definición de matrices y vectores de coeficientes por medio de funciones

# primero: un alimento

f_A_1 = function(a){
  A_0 = data.frame()
  A_0 = as.matrix(c(1))
  return(A_0)
}

f_b_1 = function(a){
  b = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
  return(b)
}

# segundo: dos alimentos
  f_A_2 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2])
    vec_y = rep(1,2)
    A_0 = rbind(vec_x, vec_y)
    return(A_0)
  }

  f_b_2 = function(a){
    b1 = 0
    b2 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2)
    return(b)
  }

# tercero: tres alimentos
  f_A_3 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2],0)
    vec_y = c(0, a$Intercambio_g[2], -a$Intercambio_g[3])
    vec_z = rep(1,3)
    A_0 = rbind(vec_x, vec_y, vec_z)
    return(A_0)
  }

  f_b_3 = function(a){
    b1 = 0
    b2 = 0
    b3 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2, b3)
    return(b)
  }

# cuarto: cuatro alimentos

  f_A_4 = function(a){
    a = df_x
    A_0 = data.frame()
    vec_x = c(a$Intercambio_g[1], -a$Intercambio_g[2], 0 , 0)
    vec_y = c(0, a$Intercambio_g[2], -a$Intercambio_g[3], 0)
    vec_z = c(0, 0, a$Intercambio_g[3], -a$Intercambio_g[4])
    vec_w = rep(1,4)
    A_0 = rbind(vec_x, vec_y, vec_z, vec_w)
    return(A_0)
  }

  f_b_4 = function(a){
    b1 = 0
    b2 = 0
    b3 = 0
    b4 = int_req_m_x[which(int_req_m_x$Grupo_GABAS == levels(as.factor(df_x$Grupo_GABAS))),2]
    b = c(b1, b2, b3, b4)
    return(b)
  }


  ##   Preparación de la base de datos de entrada    ##


  # base de datos para el modelo
  dataset_m3 = self$Data3[c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust")]

  dataset_m3 = merge(dataset_m3, intercambio_gramos[c("Cod_TCAC", "Intercambio_g")],
                     by = "Cod_TCAC", all.x = TRUE)

  dataset_m3 = dataset_m3[!duplicated(dataset_m3),]
  dataset_m3$Serving[1]=100
  dataset_m3$Precio_per_int = (dataset_m3$Precio_100g_ajust/as.numeric(dataset_m3$Serving))*dataset_m3$Intercambio_g

  # recuperar grupos y subgrupos GABAS

TCAC = TCAC[c("Cod_TCAC", "Grupo_GABAS", "Subgrupo_GABAS")]

  dataset_m3 = merge(dataset_m3, TCAC, by = "Cod_TCAC")
  dataset_m3 = f_gabas_1(dataset_m3)

  ################
  ##   AD HOC   ##
  ################
  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "FRUTAS") {
      dataset_m3$Grupo_GABAS[k] = "Frutas"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "VERDURAS") {
      dataset_m3$Grupo_GABAS[k] = "Verduras"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "CARNES MAGRAS CRUDAS") {
      dataset_m3$Grupo_GABAS[k] = "Carnes"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "LEGUMINOSAS COCIDAS Y MEZCLAS VEGETALES COCIDAS") {
      dataset_m3$Grupo_GABAS[k] = "Leguminosas"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "TUBÉRCULOS") {
      dataset_m3$Grupo_GABAS[k] = "Tuberculos"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "RAÍCES") {
      dataset_m3$Grupo_GABAS[k] = "Raices"
    }
  }

  for (k in 1:nrow(dataset_m3)) {
    if (dataset_m3$Subgrupo_GABAS[k] == "CEREALES") {
      dataset_m3$Grupo_GABAS[k] = "Cereales"
    }
  }


  ################
  ##   AD HOC   ##
  ################

  # primero: se excluyen los alimentos sin categorías
  dataset_m3 = dataset_m3 %>% filter(!Grupo_GABAS %in% "Sin categoría")

  # segundo: se excluyen los alimentos que no están recomendados por GABAS

colnames(exclusion_3er_modelo)=c("Alimento","Cod_TCAC")
  dataset_m3 = dataset_m3 %>% filter(!Cod_TCAC %in% levels(as.factor(exclusion_3er_modelo$Cod_TCAC)))

exclusion_ad_hoc = c("Carne de cerdo, espinazo", "Yuca ICA", "Papa Betina",
                       "Papa única")

dataset_m3 = dataset_m3 %>% filter(!Alimento %in% exclusion_ad_hoc)

#---------------------------------------------------------------------------------------#
#                   Tercer Modelo  -  Solución y contrucción Femenino                  #
#-------------------------------------------------------------------------------------#

# base de datos de recepción
modelo_3_dieta = data.frame(dataset_m3[c("Grupo_GABAS", "Alimento")])

edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)", "[31, 50)",
         "[51, 70)", ">70", "Gestantes 14-18 años", "Gestantes 19-30 años",
         "Gestantes 31-50 años", "Lactantes 14-18 años", "Lactantes 19-30 años",
         "Lactantes 31-50 años" )

modelo_3_costo = as.data.frame(matrix(ncol = 1))
colnames(modelo_3_costo) = c("Grupo")
modelo_3_costo$Grupo[1] = "Costo"


modelo_3_dieta_g = modelo_3_dieta
modelo_3_dieta_int = modelo_3_dieta


## Solución del    MOD3      ##

colnames(cantidad_alimentos_seleccionar) = c("Grupo_GABAS", "Cantidad")

# recodificar cantidad a seleccionar
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Azúcares")] = "Azúcares"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Carnes, huevos y leguminosas")] = "Carnes, huevos y leguminosas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Cereales, Raíces, Tubérculos y Plátanos")] = "Cereales y raíces"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Frutas y verduras")] = "Frutas y verduras"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Grasas")] = "Grasas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Leche y productos lácteos")] = "Lácteos"

# incluir la cantidad a seleccionar de frutas y verduras
frutas_verduras_cantidad = data.frame(Grupo_GABAS = c("Verduras", "Frutas"),
                                      Cantidad = c(2,2))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, frutas_verduras_cantidad)

# incluir la cantidad a seleccionar de cereales, raíces y tubérculos
cereales_cantidad = data.frame(Grupo_GABAS = c("Tuberculos", "Raices",
                                                      "Cereales"),
                                      Cantidad = c(1,1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, cereales_cantidad)

# incluir la cantidad a seleccionar de carnes y leguminosas
carnes_cantidad = data.frame(Grupo_GABAS = c("Carnes", "Leguminosas"),
                                      Cantidad = c(1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, carnes_cantidad)

# construcción de subsets por grupos de alimentos
gabas = levels(as.factor(dataset_m3$Grupo_GABAS))
datos_grupos = list()
length(datos_grupos) = length(gabas)

gabas = setdiff(gabas, c("Carnes, huevos y leguminosas", "Cereales y raíces"))

for (k in 1:length(gabas)) {
  df = data.frame()
  df = dataset_m3 %>% filter(Grupo_GABAS %in% gabas[k])
  datos_grupos[[k]] = df
  names(datos_grupos)[k] = paste0("Grupo_",gabas[k])
  rm(df)
}

# bucle general para obtener las cantidades de los alimentos
for (i in 1:length(edad)) {
  # restricciones
  int_req_m_x = int_req_f[,c(1,1+i)]
  colnames(int_req_m_x) = c("Grupo_GABAS", "Intercambio")
  int_req_m_x = f_gabas_2(int_req_m_x)

  df_solution = as.data.frame(matrix(ncol = (ncol(dataset_m3)+2)))
  colnames(df_solution) = c(colnames(dataset_m3), "sol_int", "solution_g")
  df_solution = na.omit(df_solution)

  #cereales
  #df = datos_grupos[[3]]
  #q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  #q = as.numeric(q)
  #df = df[order(df$Precio_per_int),]
  #df_x = df[1:q,]

  # A = f_A_3(df_x)
  #b = f_b_3(df_x)
  #df_x$sol_int = solve(A, b)
  #df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  #df_solution = rbind(df_solution, df_x)



  #frutas
  df = datos_grupos[[4]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)

  #verduras
  df = datos_grupos[[10]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)

  #carnes
  #df = datos_grupos[[2]]
  #q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  #q = as.numeric(q)
  #df = df[order(df$Precio_per_int),]
  #df_x = df[1:q,]

  #A = f_A_2(df_x)
  #b = f_b_2(df_x)
  #df_x$sol_int = solve(A, b)
  #df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  #df_solution = rbind(df_solution, df_x)


  # grasas, lacteos y azucares
  for (k in c(1,2,3,5,6,7,8,9)) {
    df = datos_grupos[[k]]
    q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
    q = as.numeric(levels(as.factor(q$Cantidad)))
    df = df[order(df$Precio_per_int),]
    df_x = df[1:q,]

    A = f_A_1(df_x)
    b = f_b_1(df_x)
    df_x$sol_int = solve(A, b)
    df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
    df_solution = rbind(df_solution, df_x)
  }



  costo_df = as.data.frame(matrix(ncol = 2, nrow = 1))
  colnames(costo_df) = c("Grupo", edad[i])
  costo_df$Grupo = "Costo"
  costo_df[,2] = sum(df_solution$Precio_per_int*df_solution$sol_int)

  modelo_3_costo = merge(modelo_3_costo, costo_df, by = "Grupo")



  modelo_3_sol_g = df_solution[c("Alimento", "solution_g")]
  modelo_3_sol_int = df_solution[c("Alimento", "sol_int")]

  colnames(modelo_3_sol_g) = c("Alimento", edad[i])
  colnames(modelo_3_sol_int) = c("Alimento", edad[i])


  modelo_3_dieta_g = merge(modelo_3_dieta_g, modelo_3_sol_g, by = "Alimento")
  modelo_3_dieta_int = merge(modelo_3_dieta_int, modelo_3_sol_int, by = "Alimento")

}


# Organizar los resultados
modelo_3_dieta_g = modelo_3_dieta_g[,c(2,1,3:16)]
modelo_3_dieta_g = modelo_3_dieta_g[order(modelo_3_dieta_g$Grupo_GABAS),]


modelo_3_dieta_int = modelo_3_dieta_int[,c(2,1,3:16)]
modelo_3_dieta_int = modelo_3_dieta_int[order(modelo_3_dieta_int$Grupo_GABAS),]


#----------- Verificación (PENDIENTE)


#assign("Modelo_3_F",modelo_3_dieta_g,envir = globalenv());assign("Modelo_3_F_INT",modelo_3_dieta_int,envir = globalenv());assign("Modelo_3_F_COST",modelo_3_costo,envir = globalenv())
assign(paste("Modelo_3_F_COST", self$Mes, self$Año, sep = "_"),modelo_3_costo,envir = globalenv())


#---------------------------------------------------------------------------------------#
#                   Tercer Modelo  -  Solución y contrucción Masculino                  #
#-------------------------------------------------------------------------------------#

# base de datos de recepción
modelo_3_dieta = data.frame(dataset_m3[c("Grupo_GABAS", "Alimento")])

edad = c("[1, 4)", "[4, 9)", "[9, 14)", "[14, 19)", "[19, 31)", "[31, 50)",
         "[51, 70)", ">70")

modelo_3_costo = as.data.frame(matrix(ncol = 1))
colnames(modelo_3_costo) = c("Grupo")
modelo_3_costo$Grupo[1] = "Costo"


modelo_3_dieta_g = modelo_3_dieta
modelo_3_dieta_int = modelo_3_dieta

###########################
## Solución del          ##
##      modelo tipo 3    ##
###########################

# recodificar cantidad a seleccionar
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Azúcares")] = "Azúcares"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Carnes, huevos y leguminosas")] = "Carnes, huevos y leguminosas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Cereales, Raíces, Tubérculos y Plátanos")] = "Cereales y raíces"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Frutas y verduras")] = "Frutas y verduras"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Grasas")] = "Grasas"
cantidad_alimentos_seleccionar$Grupo_GABAS[which(cantidad_alimentos_seleccionar$Grupo_GABAS == "Leche y productos lácteos")] = "Lácteos"

# incluir la cantidad a seleccionar de frutas y verduras
frutas_verduras_cantidad = data.frame(Grupo_GABAS = c("Verduras", "Frutas"),
                                      Cantidad = c(2,2))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, frutas_verduras_cantidad)

# incluir la cantidad a seleccionar de cereales, raíces y tubérculos
cereales_cantidad = data.frame(Grupo_GABAS = c("Tuberculos", "Raices",
                                                      "Cereales"),
                                      Cantidad = c(1,1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, cereales_cantidad)

# incluir la cantidad a seleccionar de carnes y leguminosas
carnes_cantidad = data.frame(Grupo_GABAS = c("Carnes", "Leguminosas"),
                                      Cantidad = c(1,1))
cantidad_alimentos_seleccionar = rbind(cantidad_alimentos_seleccionar, carnes_cantidad)

# construcción de subsets por grupos de alimentos
gabas = levels(as.factor(dataset_m3$Grupo_GABAS))
datos_grupos = list()
length(datos_grupos) = length(gabas)

gabas = setdiff(gabas, c("Carnes, huevos y leguminosas", "Cereales y raíces"))

for (k in 1:length(gabas)) {
  df = data.frame()
  df = dataset_m3 %>% filter(Grupo_GABAS %in% gabas[k])
  datos_grupos[[k]] = df
  names(datos_grupos)[k] = paste0("Grupo_",gabas[k])
  rm(df)
}

# bucle general para obtener las cantidades de los alimentos
for (i in 1:length(edad)) {
  # restricciones
  int_req_m_x = int_req_m[,c(1,1+i)]
  colnames(int_req_m_x) = c("Grupo_GABAS", "Intercambio")
  int_req_m_x = f_gabas_2(int_req_m_x)

  df_solution = as.data.frame(matrix(ncol = (ncol(dataset_m3)+2)))
  colnames(df_solution) = c(colnames(dataset_m3), "sol_int", "solution_g")
  df_solution = na.omit(df_solution)

  #cereales
  #df = datos_grupos[[3]]
  #q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  #q = as.numeric(q)
  #df = df[order(df$Precio_per_int),]
  #df_x = df[1:q,]

  # A = f_A_3(df_x)
  #b = f_b_3(df_x)
  #df_x$sol_int = solve(A, b)
  #df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  #df_solution = rbind(df_solution, df_x)



  #frutas
  df = datos_grupos[[4]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)

  #verduras
  df = datos_grupos[[10]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)

  #carnes
  #df = datos_grupos[[2]]
  #q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  #q = as.numeric(q)
  #df = df[order(df$Precio_per_int),]
  #df_x = df[1:q,]

  #A = f_A_2(df_x)
  #b = f_b_2(df_x)
  #df_x$sol_int = solve(A, b)
  #df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  #df_solution = rbind(df_solution, df_x)


  # grasas, lacteos y azucares
  for (k in c(1,2,3,5,6,7,8,9)) {
    df = datos_grupos[[k]]
    q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
    q = as.numeric(levels(as.factor(q$Cantidad)))
    df = df[order(df$Precio_per_int),]
    df_x = df[1:q,]

    A = f_A_1(df_x)
    b = f_b_1(df_x)
    df_x$sol_int = solve(A, b)
    df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
    df_solution = rbind(df_solution, df_x)
  }



  costo_df = as.data.frame(matrix(ncol = 2, nrow = 1))
  colnames(costo_df) = c("Grupo", edad[i])
  costo_df$Grupo = "Costo"
  costo_df[,2] = sum(df_solution$Precio_per_int*df_solution$sol_int)

  modelo_3_costo = merge(modelo_3_costo, costo_df, by = "Grupo")



  modelo_3_sol_g = df_solution[c("Alimento", "solution_g")]
  modelo_3_sol_int = df_solution[c("Alimento", "sol_int")]

  colnames(modelo_3_sol_g) = c("Alimento", edad[i])
  colnames(modelo_3_sol_int) = c("Alimento", edad[i])


  modelo_3_dieta_g = merge(modelo_3_dieta_g, modelo_3_sol_g, by = "Alimento")
  modelo_3_dieta_int = merge(modelo_3_dieta_int, modelo_3_sol_int, by = "Alimento")

}


# Organizar los resultados
modelo_3_dieta_g = modelo_3_dieta_g[,c(2,1,3:10)]
modelo_3_dieta_g = modelo_3_dieta_g[order(modelo_3_dieta_g$Grupo_GABAS),]


modelo_3_dieta_int = modelo_3_dieta_int[,c(2,1,3:10)]
modelo_3_dieta_int = modelo_3_dieta_int[order(modelo_3_dieta_int$Grupo_GABAS),]


#assign("Modelo_3_M",modelo_3_dieta_g,envir = globalenv());assign("Modelo_3_M_INT",modelo_3_dieta_int,envir = globalenv());assign("Modelo_3_M_COST",modelo_3_costo,envir = globalenv())
assign(paste("Modelo_3_M_COST", self$Mes, self$Año, sep = "_"),modelo_3_costo,envir = globalenv())

cat("Depuración del módulo 4 exitosa, la salida principal son las tres modelos para cada sexo (Modelo 3); use «Modelo_3_*» para acceder a cada uno")

}


))






