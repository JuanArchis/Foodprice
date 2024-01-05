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
                    "lpSolve","Rglpk","Rsymphony","scatterplot3d","reshape","utils","rio","stringdist"),
    # Parámetros de la clase
    data_list_precios=NULL,
    data_list_abas=NULL,
    Mes=NULL,
    Año=NULL,
    Ciudad=NULL,
    Margenes=NULL,
    Data_Model=NULL,
    Percentil_Abast=NULL,
    Ingreso_Alimentos=NULL,
    Select_Modelos = list(mod1 = FALSE, mod2 = FALSE, mod3 = FALSE),

    # Parámetros privados

    Data=NULL, # Data final del módulo 1
    DRI_m=NULL,
    DRI_f=NULL,
    Data2=NULL,
    Data3=NULL,

    # REQUERIMIENTOS OPCIONALES

#- Primer y segundo modelo
    DRI_m_op=NULL,
    DRI_f_op=NULL,


#- Tercer modelo
    intercambio_gramos_op=NULL,

    int_req_m_op=NULL,
    int_req_f_op=NULL,


    initialize=function(Mes,Año,Ciudad,Margenes=NULL,Data_Model=NULL,Percentil_Abast=NULL,Ingreso_Alimentos=NULL,Select_Modelos=NULL,DRI_m_op=NULL,DRI_f_op=NULL,
        intercambio_gramos_op=NULL,    int_req_m_op=NULL,   int_req_f_op=NULL,data_list_precios=NULL, data_list_abas=NULL
    ){


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

     # opcoinal como Percentil_Abast
    if (!is.null(Percentil_Abast)) {
    if (!is.numeric(Percentil_Abast) || Percentil_Abast < 0 || Percentil_Abast > 1) {
    stop("El parámetro 'Percentil_Abast' debe ser un número entre 0 y 1, consulte la documentación para más información")
    } else {
    }
    self$Percentil_Abast = Percentil_Abast
    } else {
    self$Percentil_Abast =  NULL }


# opcional como Ingreso_Alimentos
if (!is.null(Ingreso_Alimentos)) {
  if (is.vector(Ingreso_Alimentos) && length(Ingreso_Alimentos) == 21) {
    # Si es un vector de tamaño 21
    self$Ingreso_Alimentos = Ingreso_Alimentos
  } else if (is.data.frame(Ingreso_Alimentos) && ncol(Ingreso_Alimentos) == 21) {
    # Si es un data frame con 21 columnas
    self$Ingreso_Alimentos = Ingreso_Alimentos
  } else {
    stop("El parámetro 'Ingreso_Alimentos' debe ser un vector de tamaño 21 o un data frame con 21 columnas. Consulte la documentación para más información.")
  }
} else {
  self$Ingreso_Alimentos = NULL
}


# Select_Modelos como Modelos opcionales
 if (!is.null(Select_Modelos)) {
        if (!is.list(Select_Modelos) || length(Select_Modelos) > 3) {
          stop("El parámetro 'Select_Modelos' debe ser una lista con máximo tres elementos. Consulte la documentación para más información.")
        } else {
          self$Select_Modelos <- list(mod1 = FALSE, mod2 = FALSE, mod3 = FALSE)
          for (model in names(Select_Modelos)) {
            if (model %in% names(self$Select_Modelos)) {
              self$Select_Modelos[[model]] <- Select_Modelos[[model]]
            } else {
              stop("Los modelos proporcionados no son válidos.")
            }
          }
        }
      } else {
       self$Select_Modelos=list(mod1 = TRUE, mod2 = TRUE, mod3 = TRUE)
      }


      # DRI_m_op como parámetro opcional
      if (!is.null(DRI_m_op)) {
      if (!is.data.frame(DRI_m_op)) {
        stop("El parámetro 'DRI_m_op' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$DRI_m_op = DRI_m_op
      }
    } else {
      self$DRI_m_op = NULL
    }




      # DRI_f_op como parámetro opcional
      if (!is.null(DRI_f_op)) {
      if (!is.data.frame(DRI_f_op)) {
        stop("El parámetro 'DRI_f_op' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$DRI_f_op = DRI_f_op
      }
    } else {
      self$DRI_f_op = NULL
    }


      # intercambio_gramos_op como parámetro opcional
      if (!is.null(intercambio_gramos_op)) {
      if (!is.data.frame(intercambio_gramos_op)) {
        stop("El parámetro 'intercambio_gramos_op' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$intercambio_gramos_op = intercambio_gramos_op
      }
    } else {
      self$intercambio_gramos_op = NULL
    }



      # int_req_m_op como parámetro opcional
      if (!is.null(int_req_m_op)) {
      if (!is.data.frame(int_req_m_op)) {
        stop("El parámetro 'int_req_m_op' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$int_req_m_op = int_req_m_op
      }
    } else {
      self$int_req_m_op = NULL
    }



      # int_req_f_op como parámetro opcional
      if (!is.null(int_req_f_op)) {
      if (!is.data.frame(int_req_f_op)) {
        stop("El parámetro 'int_req_f_op' debe ser un data frame, consulte la documentación para más información")
      } else {
        self$int_req_f_op = int_req_f_op
      }
    } else {
      self$int_req_f_op = NULL
    }



    },


    # ---------------------------------------------------------------#
    #          Primer método: Definición de librerias base           # COMPLETO
    #----------------------------------------------------------------#

    Librerias=function(Librerias_base){
    Librerias_base = c("readxl","dplyr","ggplot2","reshape2","knitr","haven","foreign","stringi","labelled","tidyr","plyr","tidyverse",
                        "lpSolve","Rglpk","scatterplot3d","reshape","R6","rio","janitor") # Nombra las librerias necesarias

    if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
    pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes

    print("Se instalaron y cargaron todas la librerias corectamente")

    },

    # ------------------------------------------------------------#
    #   MÓDULO 1: PREPARACIÓN DE DATOS DE INSUMO A LOS MODELOS   # COMPLETO
    #-----------------------------------------------------------#

    Depuración=function(){


if (!is.null(self$Data_Model) && is.data.frame(self$Data_Model) && nrow(self$Data_Model) > 0 && ncol(self$Data_Model) > 0) {
  assign("Datos_Insumo_Modelos",self$Data_Model,envir = globalenv())
  
        self$Data=self$Data_Model
      self$Data3=self$Data_Model
  
  } else {
      


options(rio.column_names = FALSE)

    # ------------------------------------------------------------#
    #   IIMPORTAR DATOS DESDE LA WEB                             # COMPLETO
    #-----------------------------------------------------------#
      options(timeout=1000)
      
    #  -----------------Precios mayoristas------------------------------



temp_dir_P <- tempdir()
archivo_excel_p <- file.path(temp_dir_P, paste0("archivo_P_",self$Año, ".xlsx"))
nombre_data <- paste0("data_list_precios_env", self$Año)


if (!exists("data_list_precios_ev", envir = globalenv())) {
assign("data_list_precios_ev", new.env(parent = emptyenv()), envir = globalenv())
}

if (self$Año>2022) {
  
  # Verificar si el archivo ya existe en el directorio temporal >2022

  if (!file.exists(archivo_excel_p)) {
  url_excel_P <- sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSA-SerieHistoricaMayorista-%d.xlsx", self$Año)
  download.file(url_excel_P, archivo_excel_p, mode = "wb",timeout = 444)
  assign(nombre_data, rio::import_list(archivo_excel_p, setclass = "tbl"), envir = data_list_precios_ev)
    self$data_list_precios =get(nombre_data, envir = data_list_precios_ev)
  } else {
    self$data_list_precios =get(nombre_data, envir = data_list_precios_ev)
  }

}

if (self$Año<2023) {
  # Verificar si el archivo ya existe en el directorio temporal < 2022

  if (!file.exists(archivo_excel_p)) {
  url_excel_P <- sprintf("https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/series-historicas-precios-mayoristas-%d.xlsx", self$Año)
  download.file(url_excel_P, archivo_excel_p, mode = "wb",timeout = 444)
  assign(nombre_data, rio::import_list(archivo_excel_p, setclass = "tbl"), envir = data_list_precios_ev)
    self$data_list_precios =get(nombre_data, envir = data_list_precios_ev)
  } else {
    self$data_list_precios =get(nombre_data, envir = data_list_precios_ev)
  }
    } 
      
    #  -----------------Abastecimiento-----------------------------


if (!is.null(self$Percentil_Abast)){ # hiperparámetro

if (self$Año>2022) {

if (!exists("data_list_abast_ev", envir = globalenv())) {
assign("data_list_abast_ev", new.env(parent = emptyenv()), envir = globalenv())

temp_dir_A <- tempdir()
archivo_excel_A <- file.path(temp_dir_A, paste0("archivo_A_",self$Año, ".xlsx"))

if (!exists("data_list_abast_ev")) {data_list_abast_ev <- new.env(parent = emptyenv())}
nombre_data_abast <- paste0("data_list_abast_ev", self$Año)}

  # Verificar si el archivo ya existe en el directorio temporal >2022


  if (!file.exists(archivo_excel_A)) {
  url_excel_A <- sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSAbastecimiento-Microdatos-%d.xlsx", self$Año)
  download.file(url_excel_A, archivo_excel_A, mode = "wb",timeout = 444)
  assign(nombre_data_abast, rio::import_list(archivo_excel_A, setclass = "tbl"), envir = data_list_abast_ev)
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  } else {
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  }


 }

if (self$Año<2023) {


if (!exists("data_list_abast_ev", envir = globalenv())) {
assign("data_list_abast_ev", new.env(parent = emptyenv()), envir = globalenv())}

temp_dir_A <- tempdir()
archivo_excel_A <- file.path(temp_dir_A, paste0("archivo_A_",self$Año, ".xlsx"))
nombre_data_abast <- paste0("data_list_abast_ev", self$Año)
  
  # Verificar si el archivo ya existe en el directorio temporal < 2023

  if (!file.exists(archivo_excel_A)) {
  url_excel_A <- sprintf("https://www.dane.gov.co/files/investigaciones/agropecuario/sipsa/series-historicas/microdato-abastecimiento-%d.xlsx", self$Año)
  download.file(url_excel_A, archivo_excel_A, mode = "wb",timeout = 444)
  assign(nombre_data_abast, rio::import_list(archivo_excel_A, setclass = "tbl"), envir = data_list_abast_ev)
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  } else {
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  }
      }
    } else{
      self$data_list_abas=NULL
      }

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

    #--  asgina el més con base en la posición de la hoja y depura un poco las columnas combinadas y texto inecesario de excel

    Meses=Nombres_Meses[1:length(self$data_list_precios)-1];Data_Sipsa_Precios=(self$data_list_precios[[which(Meses %in% self$Mes)+1]]) # Se extraen los meses disponibles con base en la data dada

    Data_Sipsa_Precios=Data_Sipsa_Precios[-c(1:4,nrow(Data_Sipsa_Precios)),-c(6,7)];Data_Sipsa_Precios=na.omit(Data_Sipsa_Precios) # Un poco de depuración

    colnames(Data_Sipsa_Precios) = c("Fecha", "Grupo", "Alimento", "Mercado", "Precio_kg");Data_Sipsa_Precios$Precio_kg=as.numeric(Data_Sipsa_Precios$Precio_kg);Data_Sipsa_Precios$Fecha=as.Date(paste(self$Año,which(Meses %in% self$Mes),"1", sep = "-"),format = "%Y-%m-%d") # Cambia los nombres y asigna fechas

    # -- Establece la ciudad de interés


  ciudades_colombia <- c("Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Cúcuta", "Bucaramanga", 
"Pereira", "Santa Marta", "Ibagué", "Pasto", "Manizales", "Neiva", "Soledad", "Villavicencio", "Valledupar", 
"Armenia", "Soacha", "Itagüí", "Sincelejo", "Popayán", "Floridablanca", "Palmira", "Buenaventura", 
"Barrancabermeja", "Dosquebradas", "Tuluá", "Envigado", "Cartago", "Maicao", "Florencia", "Girardot", 
"Sogamoso", "Buga", "Tunja", "Girón", "Mocoa", "Ipiales", "Facatativá", "Yopal", "Tumaco", "Riohacha", 
"Quibdó", "Turbo", "Magangué", "Apartadó", "Montería", "Arauca", "Mitu", "Puerto Carreño", "San Andrés")

asociar_ciudad_mercado <- function(ciudad, df) {
  opciones_mercado <- unique(df$Mercado[grep(ciudad, df$Mercado, ignore.case = TRUE)])
  
  if (length(opciones_mercado) == 0) {
    stop("Error:No se encontraron opciones de mercado para la ciudad especificada. Por favor, verifique su ortografía o escriba una ciudad disponible.")
    opciones_mercado=NULL
  } else {
    return(opciones_mercado)
  }
}


asociar_ciudad_entrada_usuario <- function(entrada_usuario, lista_ciudades, df) {

  similitudes <- sapply(lista_ciudades, function(ciudad) stringdist::stringdist(entrada_usuario, ciudad, method = "jw"))
  # Encontramos la ciudad más cercana en términos de texto a la entrada del usuario
  ciudad_mas_cercana <- lista_ciudades[which.min(similitudes)]
  # Llamamos a la función asociar_ciudad_mercado con la ciudad encontrada
  opciones_ciudad <- asociar_ciudad_mercado(ciudad_mas_cercana, df)
  return(opciones_ciudad)
}


Mercados_ciudad=asociar_ciudad_entrada_usuario(self$Ciudad,ciudades_colombia,Data_Sipsa_Precios)





    if(!is.null(Mercados_ciudad)) {
        Data_Sipsa_Precios = Data_Sipsa_Precios %>% filter(Mercado %in% Mercados_ciudad)
    } else {cat("Error,",self$Ciudad," aún no está en los datos públicos de precios SIPSA",sep="")}


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
    # -- Carga y limpia los datos de abastecimiento SIPSA

  if (!is.null(self$Percentil_Abast)){

    Data_Sipsa_Abas=(self$data_list_abas[[as.integer(which(sapply(Lista_Semestres, function(x) self$Mes %in% x)))+2]]) # Se extraen los meses disponibles con base en la data dada
    colnames(Data_Sipsa_Abas) = c("Ciudad_Mercado", "Fecha","Cod_Dep", "Cod_Mun", "Dep_Proc", "Mun_Proc","Grupo", "Alimento", "Cantidad_KG")
    Data_Sipsa_Abas <- janitor::remove_empty(Data_Sipsa_Abas, which = "rows");Data_Sipsa_Abas=Data_Sipsa_Abas[-c(1:2),]
    Data_Sipsa_Abas$Fecha=as.numeric(Data_Sipsa_Abas$Fecha)
    Data_Sipsa_Abas$Fecha=as.Date(Data_Sipsa_Abas$Fecha,origin = "1899-12-30")
    Data_Sipsa_Abas <- janitor::remove_empty(Data_Sipsa_Abas, which = "cols") # Elimina las columnas con total NA
    Data_Sipsa_Abas$Cantidad_KG=as.numeric(Data_Sipsa_Abas$Cantidad_KG)

    # -- Seleciona la ciudad de interés- ABSATECIMIENTO


asociar_ciudad_mercado_Abast <- function(ciudad, df) {
  opciones_mercado <- unique(df$Ciudad_Mercado[grep(ciudad, df$Ciudad_Mercado, ignore.case = TRUE)])
  
  if (length(opciones_mercado) == 0) {
    print("Error: No se encontraron opciones de mercado para la ciudad especificada.")
    opciones_mercado=NULL
  } else {
    return(opciones_mercado)
  }
}


asociar_ciudad_entrada_usuario_Abas <- function(entrada_usuario, lista_ciudades, df) {

  similitudes <- sapply(lista_ciudades, function(ciudad) stringdist::stringdist(entrada_usuario, ciudad, method = "jw"))
  # Encontramos la ciudad más cercana en términos de texto a la entrada del usuario
  ciudad_mas_cercana <- lista_ciudades[which.min(similitudes)]
  # Llamamos a la función asociar_ciudad_mercado con la ciudad encontrada
  opciones_ciudad <- asociar_ciudad_mercado_Abast(ciudad_mas_cercana, df)
  return(opciones_ciudad)
}


Mercados_ciudad_Abas=asociar_ciudad_entrada_usuario_Abas(self$Ciudad,ciudades_colombia,Data_Sipsa_Abas)



    if(!is.null(Mercados_ciudad)) {

        Data_Sipsa_Abas = Data_Sipsa_Abas %>% filter(Ciudad_Mercado %in% Mercados_ciudad_Abas) } else 
        {cat("Error,",self$Ciudad," aún no está en los datos públicos de abastecimiento SIPSA",sep="")}


# ---------------------------cargar ciudades atuomáticamente

# Crear una lista para almacenar los resultados de cada mercado
resultados_mercados <- list()

# Iterar sobre cada mercado en Mercados_ciudad_Abas
for (mercado in Mercados_ciudad_Abas) {
  data_mercado <- subset(Data_Sipsa_Abas, Ciudad_Mercado == mercado)
  Alimentos_Mercado_Abas <- levels(as.factor(data_mercado$Alimento))
  
  Data_Mercado_Unico <- data.frame(Alimentos_Mercado_Abas, Total_Mercado = NA)
  colnames(Data_Mercado_Unico) <- c("Alimento_abs", paste0("Total_", gsub(", ", "_", mercado)))
  
  for (i in 1:length(Alimentos_Mercado_Abas)) {
    Datos_Alimento <- subset(data_mercado, Alimento == Alimentos_Mercado_Abas[i])
    Datos_Alimento$Cantidad_KG <- as.numeric(Datos_Alimento$Cantidad_KG)  # Convertir a tipo numérico
    
    Data_Mercado_Unico[i, paste0("Total_", gsub(", ", "_", mercado))] <- sum(Datos_Alimento$Cantidad_KG, na.rm = TRUE)
  }
  resultados_mercados[[mercado]] <- Data_Mercado_Unico
}


Data_Sipsa_Abas_Unicos <- Reduce(function(x, y) merge(x, y, by = "Alimento_abs", all = TRUE), resultados_mercados);Data_Sipsa_Abas_Unicos[is.na(Data_Sipsa_Abas_Unicos)] <- 0


# Obtener las columnas numéricas para calcular la suma por fila
columnas_numericas <- sapply(Data_Sipsa_Abas_Unicos, is.numeric)

# Calcular la suma por fila en las columnas numéricas
Data_Sipsa_Abas_Unicos$Total <- rowSums(Data_Sipsa_Abas_Unicos[columnas_numericas], na.rm = TRUE)

Data_Sipsa_Abas_Unicos=Data_Sipsa_Abas_Unicos[,c("Alimento_abs","Total")]

#----# Salida: Data_Sipsa_Abas_Unicos #----#

  } else {Data_Sipsa_Abas_Unicos=NULL}

    # ---------------------------------------------------------------#
    #                                Mapeos                          # REVISAR GENERALIZACIÓN
    #----------------------------------------------------------------#

    # SIPSA (precios mayoristas-abastecimiento)
    data(Mapeo_Precios_Abs, package = "Foodprice",envir=parent.env(environment()))



    # SIPSA-TCAC (Códigos de sipsa a  Composición de Alimentos Colombianos)

    data(Mapeo_Sipsa_TCAC, package = "Foodprice",envir=parent.env(environment()));colnames(Mapeo_Sipsa_TCAC) = c("Alimento", "Codigo_TCAC")
    Mapeo_Sipsa_TCAC1=Mapeo_Sipsa_TCAC
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
    Data_abs_precios_Sipsa=Data_Sipsa_Precios_Unicos

    # Asignación de un alimento con abastecimiento a cada producto de la base de datos de precios
    #Data_abs_precios_Sipsa = merge(Data_Sipsa_Precios_Unicos, Mapeo_Precios_Abs, by = "Alimento", all.x = TRUE)


 if (!is.null(self$Percentil_Abast)){
    # Asignación del valor de abastecimiento en cada caso
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Precios_Unicos, Mapeo_Precios_Abs, by = "Alimento", all.x = TRUE)
    Data_abs_precios_Sipsa = merge(Data_Sipsa_Abas_Unicos, Data_abs_precios_Sipsa,by = "Alimento_abs", all.x = TRUE)
    # Selección de las variables de interés
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[c("Alimento", "Precio_kg", "Total")]
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[order(Data_abs_precios_Sipsa$Alimento),]
    colnames(Data_abs_precios_Sipsa) = c("Alimento",paste0("Precio_kg_", self$Mes), paste0("Total_Cali_", self$Mes))
 }
 else {
    # Selección de las variables de interés
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[c("Alimento", "Precio_kg")]
    Data_abs_precios_Sipsa = Data_abs_precios_Sipsa[order(Data_abs_precios_Sipsa$Alimento),]
    colnames(Data_abs_precios_Sipsa) = c("Alimento",paste0("Precio_kg_", self$Mes))
 }




    # -----------------------------------------------------------------#
    #                       Criterios de Exc                           # REVISAR GENERALIZACIÓN Y PERFECCIÓN DEL MISMO
    #------------------------------------------------------------------#
 if (!is.null(self$Percentil_Abast)){

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
    quant = quantile(criterio_2[,2],probs = self$Percentil_Abast, na.rm = TRUE)

    # Eliminar los alimentos cuyo flujo de carga está abajo del percentil 25
    criterio_2 = criterio_2[criterio_2[,2] < quant,]

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

 }
 else {


    #--------                               -------#
    #  Primer  criterio de exclusión: Nutrición    #
    #-----                                  -------#

    Alimentos_Exclu_Criterio_1 = Primer_Criterio_Lista_Alimentos[Primer_Criterio_Lista_Alimentos$`COD. TCAC` == "EX000","Alimento"]

    #--------                               -------#
    # Lista depurada con base en los dos criterios #
    #-----                                  -------#



    # Construir el vector con la totalidad de alimentos excluidos
    # (criterio 1, criterio 2, flujo de carga nulo y exclusiones ad hoc)

    Alimentos_Excluidos_Criterio_1 = Alimentos_Exclu_Criterio_1["Alimento"]
    Alimentos_Excluidos = c(Alimentos_Excluidos_Criterio_1,"Queso Caquetá")

    # Exclusión de los alimentos y construcción de la lista definitiva
    Lista_Alimentos_Definitiva = Data_abs_precios_Sipsa %>% filter(Alimento %in% setdiff(levels(as.factor(Data_abs_precios_Sipsa$Alimento)), Alimentos_Excluidos))


 }

 
    # -----------------------------------------------------------------#
    #                        Marginalización                           #  REVISAR GENERALZIACIÓN
    #------------------------------------------------------------------#


    #-- Construcción data por grupos (SIPSA)

    Grupos_Alimentos_Sipsa = Data_Sipsa_Precios[c("Alimento", "Grupo")];Grupos_Alimentos_Sipsa = Grupos_Alimentos_Sipsa[!duplicated(Grupos_Alimentos_Sipsa), ]
    Precios_Grupos_SIPSA = merge(Data_Sipsa_Precios_Unicos, Grupos_Alimentos_Sipsa,by = "Alimento", all.x = TRUE, all.y = FALSE, no.dups = TRUE)
    

    

    #--------                    -------#
    #  Margenes de comercialziación     #
    #-----                       -------#

    grupos_margenes <- levels(as.factor(Precios_Grupos_SIPSA$Grupo));Margenes_Historicos <- data.frame(Grupo = grupos_margenes, margen_medio=NA)

    # margen medio
    Margenes_Historicos$margen_medio <- c(4.925515,32.154734,21.770773,26.226295,17.150887,6.884347,76.380988,54.096494)


    # -----------------------------------------------------------------#
    #                 Estimación precios minoristas                    #
    #------------------------------------------------------------------#


    if (!is.null(self$Margenes)) {

      Margenes_Historicos$margen_medio=self$Margenes
      Precios_Grupos_SIPSA$Grupo <- toupper(Precios_Grupos_SIPSA$Grupo)

      precios_mayoristas_grupos_margenes <- merge(Precios_Grupos_SIPSA,
                                                  Margenes_Historicos[c("Grupo", "margen_medio")],
                                                  by = "Grupo", all.x = TRUE)
      precios_mayoristas_grupos_margenes$Precio_minorista_kg <- precios_mayoristas_grupos_margenes$Precio_kg * (1 + precios_mayoristas_grupos_margenes$margen_medio/100)


    } else {
      Precios_Grupos_SIPSA$Grupo <- toupper(Precios_Grupos_SIPSA$Grupo)
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
    dataset_sim_1$Serving <- rep(100, nrow(dataset_sim_1))
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
    dataset_sim$Serving <- rep(100, nrow(dataset_sim))

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
    #                         Alimentos faltantes                      #
    #------------------------------------------------------------------#


if (!is.null(self$Ingreso_Alimentos)) {
  alimentos_faltantes <- Alimentos_Sipsa_Precios[!(Alimentos_Sipsa_Precios %in% Mapeo_Sipsa_TCAC1$Alimento)]

  if (is.data.frame(self$Ingreso_Alimentos)) {
    # Si es un data frame, buscar los alimentos en la columna 'Alimento'
    alimentos_encontrados <- self$Ingreso_Alimentos$Alimento[self$Ingreso_Alimentos$Alimento %in% alimentos_faltantes]
  } else {
    # Si es un vector, buscar los alimentos directamente
    alimentos_encontrados <- self$Ingreso_Alimentos[self$Ingreso_Alimentos %in% alimentos_faltantes]
  }

  alimentos_faltantes <- alimentos_faltantes[!(alimentos_faltantes %in% alimentos_encontrados)]
  
  Datos_Insumo_Modelos <- rbind(self$Ingreso_Alimentos, Datos_Insumo_Modelos)
} else {
  alimentos_faltantes <- Alimentos_Sipsa_Precios[!(Alimentos_Sipsa_Precios %in% Mapeo_Sipsa_TCAC1$Alimento)]
}


    
  assign(paste0("Datos_Insumo_Modelos_",self$Año,"_",self$Mes),Datos_Insumo_Modelos,envir = globalenv());assign(paste0("Estimación_Precios_Minoristas_",self$Año,"_",self$Mes),Estimación_Precios_Minoristas,envir = globalenv())

      self$Data=Datos_Insumo_Modelos
      self$Data3=Datos_Insumo_Modelos
mensaje <- paste("En la ciudad de", self$Ciudad, "del año", self$Año, "y mes", self$Mes, ", se omitieron los siguientes alimentos por falta de información nutricional " , length(alimentos_faltantes) ," :", paste(alimentos_faltantes, collapse = ", "), ". Si conoce la información de estos, utilice el parámetro opcional llamado 'Ingreso_Alimentos' para ingresarlos")
print(mensaje)

}



    # -----------------------------------------------------------------#
    #               Estado de la depuración del módulo                 #
    #------------------------------------------------------------------#

 if(length(warnings())<100) {cat("Depuración del módulo 1 exitosa", "\n")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}


},



    # ------------------------------------------------------------#
    #        MÓDULO 2: CARGA DE DATOS DE REQURIMIENTOS           # -- EN PROGRESO (FALTA GENERALIZAR sleft$ para comunicar con módulos siguientes)
    #-----------------------------------------------------------#

Módulo_2=function(){




# Depuración modelo 1
#

# Modificación de datos del módulo 1
names(self$Data) = c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio")

self$Data <- self$Data %>%
  transform(grupo = substr(Cod_TCAC, start = 1, stop = 1)) %>%
  arrange(Alimento)

self$Data = self$Data[order(self$Data$Alimento),]

self$Data2=self$Data 
self$Data = self$Data %>% filter(!Cod_TCAC %in% c("K003", "K004", "K033","D013"))



#
# Depuración modelo 2
#


names(self$Data2) = c("Cod_TCAC", "Alimento", "Serving", "Precio_100g_ajust",  "Energia","Proteina","Carbohidratos","Lipidos",  "Calcio",  "Zinc", "Hierro", "Magnesio","Fosforo","VitaminaC", "Tiamina", "Riboflavina","Niacina", "Folatos", "VitaminaB12", "VitaminaA","Sodio")

self$Data2 <- self$Data2 %>%
  transform(grupo = substr(Cod_TCAC, start = 1, stop = 1)) %>%
  arrange(Alimento)
 

#-------------------------------------#
# Carga de requerimientos Modelo 3   #
#------------------------------------#

# Intercambio en gramos de alimentos

if (!is.null(self$intercambio_gramos_op)){
intercambio_gramos=self$intercambio_gramos_op
} else {
data(intercambio_gramos, package = "Foodprice",envir=parent.env(environment()))
  
}



#int_req_m_op

if (!is.null(self$int_req_m_op)){
int_req_m=self$int_req_m_op
} else {
data(int_req_m, package = "Foodprice",envir=parent.env(environment()))
  
}


#int_req_f_op

if (!is.null(self$int_req_f_op)){
int_req_f=self$int_req_f_op
} else {
data(int_req_f, package = "Foodprice",envir=parent.env(environment()))
  
}


#-------------------------------------#
# Carga de requerimientos Modelos 1 y 2 #
#------------------------------------#

#DRI M

if (!is.null(self$DRI_m_op)){
DRI_M=self$DRI_m_op
} else {
data(DRI_M, package = "Foodprice",envir=parent.env(environment()))
  
}

#DRI F

if (!is.null(self$DRI_f_op)){
DRI_F=self$DRI_f_op
} else {
data(DRI_F, package = "Foodprice",envir=parent.env(environment()))
  
}


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

if(length(warnings())<100) {cat("Carga de requerimientos exitosa")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

data(TCAC, package = "Foodprice",envir=parent.env(environment()))




}, # BASES Resultantes: Data, DRI_M, DRI_F, EER_share_M, EER_share_F, EER_share_rangos, exclusion_3er_modelo, cantidad_alimentos_seleccionar


Módulo_3=function(){

  #--------------------------------------------------------------------------#
  #                   Primer Modelo - Construcción de datos                  #
  #-------------------------------------------------------------------------#


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

  if(length(warnings())<100) {cat ("Ejecución del modelo 1 correcta") } else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

},

Módulo_4=function(){

#--------------------------------------------------------------------------------------#
#                   Segundo Modelo  - Femenino                        #
#------------------------------------------------------------------------------------#





# vector de precios
precios = self$Data2$Precio_100g_ajust

# nombre alimentos
alimentos=self$Data2$Alimento

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


"-------------------------------------------------------------------"
"-------------------------------------------------------------------"
"-------------------------------------------------------------------"

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



"-------------------------------------------------------------------"
"-------------------------------------------------------------------"
"-------------------------------------------------------------------"




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

#------------------------------------------------------------------------------------#
#                   Segundo Modelo Masculino- Construcción de datos                  #
#----------------------------------------------------------------------------------#


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

  if(length(warnings())<100) {cat("Ejecución del modelo 2 correcta")} else {cat("Cantidad de errores encontrados:",length(warnings()), "\n")}

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





  #frutas
  df = datos_grupos[["Grupo_Frutas"]]
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
  df = datos_grupos[["Grupo_Verduras"]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)



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


  #frutas
  df = datos_grupos[["Grupo_Frutas"]]
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
  df = datos_grupos[["Grupo_Verduras"]]
  q = cantidad_alimentos_seleccionar[which(cantidad_alimentos_seleccionar$Grupo_GABAS == levels(as.factor(df$Grupo_GABAS))),2]
  q = as.numeric(levels(as.factor(q$Cantidad)))
  df = df[order(df$Precio_per_int),]
  df_x = df[1:q,]

  A = f_A_2(df_x)
  b = f_b_2(df_x)
  df_x$sol_int = solve(A, b)
  df_x$solution_g = df_x$sol_int*df_x$Intercambio_g
  df_solution = rbind(df_solution, df_x)





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

names(modelo_3_costo)[names(modelo_3_costo) == "Gupo"][1] <- "Alimentos"
#assign("Modelo_3_M",modelo_3_dieta_g,envir = globalenv());assign("Modelo_3_M_INT",modelo_3_dieta_int,envir = globalenv());assign("Modelo_3_M_COST",modelo_3_costo,envir = globalenv())
assign(paste("Modelo_3_M_COST", self$Mes, self$Año, sep = "_"),modelo_3_costo,envir = globalenv())

cat("Ejecución del modelo 3 correcta")

},

Modelos = function() {

      self$Módulo_2()
      print("")

      if (self$Select_Modelos$mod1) {
        self$Módulo_3()
        print("")
      }
      if (self$Select_Modelos$mod2) {
        self$Módulo_4()
        print("")
      }
      if (self$Select_Modelos$mod3) {
        self$Módulo_5()
        print("")
      }
    }


))






