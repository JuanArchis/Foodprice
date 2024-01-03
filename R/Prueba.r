remove.packages("Foodprice")
devtools::install_github("JuanArchis/Foodprice");library(Foodprice)
Prueba1=Foodprice::Paquete_Foodprice_ver_0_0_1$new(Mes="Septiembre",Año=2022,Ciudad="Cali")
Prueba1$Librerias()
Prueba1$Módulo_1()
Prueba1$Módulo_2()
Prueba1$Módulo_3()
Prueba1$Módulo_4()
Prueba1$Módulo_5()



temp_dir_A <- tempdir()
archivo_excel_A <- file.path(temp_dir_A, paste0("archivo_A_",self$Año, ".xlsx"))
if (!exists("data_list_abast_ev")) {data_list_abast_ev <- new.env(parent = emptyenv())}
nombre_data_abast <- paste0("data_list_abast_ev", self$Año)

  # Verificar si el archivo ya existe en el directorio temporal

  if (!file.exists(archivo_excel_A)) {
  archivo_excel_A <- sprintf("https://www.dane.gov.co/files/operaciones/SIPSA/anex-SIPSAbastecimiento-Microdatos-%d.xlsx", self$Año)
  download.file(archivo_excel_A, archivo_excel_A, mode = "wb",timeout = 444)
  assign(nombre_data_abast, rio::import_list(archivo_excel_A, setclass = "tbl"), envir = data_list_abast_ev)
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  } else {
    self$data_list_abas =get(nombre_data_abast, envir = data_list_abast_ev)
  }


