remove.packages("Foodprice")
devtools::install_github("JuanArchis/Foodprice");library(Foodprice)



library(rio)
x <- rio::import_list("/home/juan-c/Descargas/PreciosM-SIPSA.xlsx", setclass = "tbl")
y <- rio::import_list("/home/juan-c/Descargas/Abastecimiento.xlsx", setclass = "tbl")


Prueba1=Foodprice::Paquete_Foodprice_ver_0_0_1$new(data_list_precios=x,data_list_abas=y,Mes="Septiembre",Año=2022,Ciudad="Cali",
Margenes =c(4.925515,32.154734,21.770773,26.226295,17.150887,6.884347,76.380988,54.096494))
Prueba1$Librerias()
Prueba1$Módulo_1()
Prueba1$Módulo_2()
Prueba1$Módulo_3()
Prueba1$Módulo_4()
Prueba1$Módulo_5()


