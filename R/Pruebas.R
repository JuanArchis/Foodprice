cantidad_alimentos_seleccionar  = read_excel("C:/Users/kmili/OneDrive/Escritorio/ /Metodologia_CIAT-main/datos_primarios/requerimientos_fuente/requerimientos_energia_nutrientes.xlsx",
                   sheet = 6)

ruta_archivo <- file.path("C:/Users/kmili/OneDrive/Escritorio/Foodprice/Foodprice/data", "cantidad_alimentos_seleccionar.rda")
save(cantidad_alimentos_seleccionar  , file = ruta_archivo)


remove.packages("Foodprice")

#---------------------#
#       ENSAYOS       #
#--------------------#

library("rio")
x <- rio::import_list("C:/Users/kmili/Downloads/series-historicas-precios-mayoristas-2022.xlsx", setclass = "tbl")
y <- rio::import_list("C:/Users/kmili/Downloads/microdato-abastecimiento-2022.xlsx", setclass = "tbl")

Prueba1=Foodprice::Paquete_Foodprice_ver_0_0_1$new(data_list_precios=x,data_list_abas=y,Mes="Julio",Año=2022,Ciudad="Cali")

Prueba1$Librerias()
Prueba1$Módulo_1()
Prueba1$Módulo_2()
Prueba1$Módulo_3()


