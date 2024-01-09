#------------------------------------------------------------------------------------------#
#                 D O C U M E N T A C I Ó N: FUNCIÓN-Datos_dane_col                        #
#-----------------------------------------------------------------------------------------#

\name{Datos_dane_col}
\alias{Datos_dane_col}
\title{Función para procesar datos del DANE en Colombia}
\usage{
Datos_dane_col(
  Mes,
  Año,
  Ciudad,
  Percentil_Abast = NULL,
  Ingreso_Alimentos = NULL,
  data_list_precios = NULL,
  data_list_abas = NULL,
  Margenes = NULL
)
}
\description{
La función Datos_dane_col permite procesar datos del Departamento Administrativo Nacional de Estadística (DANE) en Colombia para un mes, año y ciudad específicos. Realiza la estimación de los precios minorístas además del mapeo de los alimentos con sus nutrientes, considerando diferentes parámetros opcionales para obtener información detallada sobre el abastecimiento y los ingresos por alimentos en la ciudad seleccionada.
Esta permite descargar y depurar los datos desde la web o localmente.
}
\examples{
# Ejemplo de uso:
# Obtener datos para Medellín, enero de 2023
Datos_dane_col(Mes = "Enero", Año = 2023, Ciudad = "Medellín")
}

\arguments{
  \item{Parámetros necesarios:}{
    \itemize{
      \item{Mes}{Un texto representando el mes del año (por ejemplo, "Enero").}
      \item{Año}{Un valor numérico entre 2014 y 2023, indicando el año de interés.}
      \item{Ciudad}{Un texto que especifica la ciudad de interés en Colombia.}
    }
  }

  \item{Parámetros opcionales:}{
    \itemize{
      \item{Percentil\_Abast}{Opcional. Un valor numérico entre 0 y 1 que indica el percentil para el análisis de abastecimiento.}
      \item{Ingreso\_Alimentos}{Opcional. Un vector o data frame con 21 columnas o tamaño 21, representando ingresos por alimentos.}

      \item{data\_list\_precios}{Opcional. Una lista que contiene datos de precios.
      Para el funcionamiento corecto el paquete supone los datos directamente descargados desde la página, es decir, NO debe depurar los datos; Además, como se supone es un excel con varias hojas, debe cargar este como una lista con la librería RIO, por ejemplo:
      rio::import_list(Archivo_excel, setclass = "tbl") }

      \item{data\_list\_abas}{Opcional. Una lista de datos relacionados con el abastecimiento. Aplica las mismas condiciones de carga que data_list_precios.}
      \item{Margenes}{Opcional. Un vector de tamaño 8 relacionado con márgenes a cada grupo alimenticio, el orden de este es:}
    }
  }
}


\details{
La función realiza una validación exhaustiva de los parámetros ingresados para asegurar su coherencia y luego procesa los datos correspondientes del DANE para ofrecer análisis detallados sobre el abastecimiento y los ingresos por alimentos en la ciudad especificada.
}

\value{
La función retorna análisis y estadísticas detalladas relacionadas con el abastecimiento y los ingresos por alimentos en la ciudad y periodo de tiempo seleccionados.
}

\note{
Es importante revisar la documentación para asegurar que los parámetros proporcionados sean correctos y coherentes con los requisitos establecidos.
}
