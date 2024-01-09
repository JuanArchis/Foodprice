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
La función Datos_dane_col permite procesar datos del Departamento Administrativo Nacional de Estadística (DANE) en Colombia para un mes, año y ciudad específicos. Realiza la estimación de los precios minoristas además del mapeo de los alimentos con sus nutrientes, considerando diferentes parámetros opcionales para obtener información detallada sobre el abastecimiento y los ingresos por alimentos en la ciudad seleccionada. También permite descargar y depurar los datos desde la web o localmente.
}
\examples{
# Ejemplo de uso:
# Obtener datos para Medellín, enero de 2023
Datos_dane_col(Mes = "Enero", Año = 2023, Ciudad = "Medellín")
}

\arguments{
  \item{Parámetros necesarios:}{
    \item{\code{Mes}}{Un texto representando el mes del año (por ejemplo, "Enero").}
    \item{\code{Año}}{Un valor numérico entre 2014 y 2023, indicando el año de interés.}
    \item{\code{Ciudad}}{Un texto que especifica la ciudad de interés en Colombia.}
  }

  \item{Parámetros opcionales:}{
    \item{\code{Percentil\_Abast}}{Opcional. Un valor numérico entre 0 y 1 que indica el percentil para el análisis de abastecimiento.}
    \item{\code{Ingreso\_Alimentos}}{Opcional. Un vector o data frame con 21 columnas o tamaño 21, representando ingresos por alimentos.}
    \item{\code{data\_list\_precios}}{Opcional. Una lista que contiene datos de precios. Para el funcionamiento correcto, el paquete asume que los datos son directamente descargados desde la página, es decir, NO se debe depurar los datos. Además, como se supone que es un archivo Excel con varias hojas, debe cargarse como una lista con la librería RIO, por ejemplo: rio::import\_list(Archivo\_excel, setclass = "tbl")}
    \item{\code{data\_list\_abas}}{Opcional. Una lista de datos relacionados con el abastecimiento. Aplica las mismas condiciones de carga que data\_list\_precios.}
    \item{\code{Margenes}}{Opcional. Un vector de tamaño 8 relacionado con márgenes a cada grupo alimenticio, en el orden correspondiente: carnes, frutas, granos y cereales, lacteos y huevos, pescados, procesados, tuberculos, raices y platanos, y verduras y hortalizas.}
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
