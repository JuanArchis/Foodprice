# Foodprice

<p align="center">
<a name="top" href="#"> <img src="https://media2.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif" alt="mf-dots" height="40%" width="60%"/> </a>

# 

### :computer: **Introducción:**

En el presente repositorio encontrará la versión 0.0.1 del paquete Foodprice, el cúal alberga el módulo uno, encargado de la depuración y estimación de los precios minoristas que servirán como insumo a los modelos.

#

### :warning: **Dependencias:**

Antes de instalar y cargar es paquete es menester que instale el siguientes paquetes: rio. Esto lo puede hacer ejecutando el código:

```
install.packages("rio");library(rio)

```
El paquete cuenta con una función para la carga de los datos, de tal forma que se puedan cargar todas las hojas disponibles en el excel.
#

### :wrench: **Instrucciones de instalación y uso:**



**1. Instalación**
            
Instale y cargue el paquete alojado en el presente repositorio ejecutando en R (debe tener acceso al repositorio, dado que este es privado):            

```
devtools::install_github("JuanArchis/Foodprice");library(Foodprice)
```

**2. Preparación de los datos de entrada**

Como se nombró en el apartado de dependencias, la idea es que el usuario no repare en limpiar o selecionar hojas de la data de precios y/o abastecimiento de SIPSA; dado lo anterior, como
entrada se solicita cargar los datos con la libreria "rio". Por ejemplo:


```
x <- rio::import_list("path de los datos de precios SIPSA", setclass = "tbl") # mayoristas sipsa
y <- rio::import_list("path de los datos de abastecimiento"  SIPSA"", setclass = "tbl")
```
El objeto o función a llamar es : Paquete_Foodprice_ver_0_0_1.

**3. Argumentos del objeto**

- **data_list_precios:** Ingresar los datos de precios de SIPSA tal y cual como se descargaron.
- **data_list_abas:** Ingresar los datos de abastecimiento de SIPSA tal y cual como se descargaron.
- **Mes:** Mes o muestra de estudio, debe estar en formato texto, por ejemplo: "Julio"
- **Año:** Año de estudio,su formato debe ser númerico; por ejemplo, 2022.
- **Ciudad:** Ciudad de estudio, debe estar en formato texto, por ejemplo: "Cali".
            

            
**4. Instrucciones de uso**

El paquete es una clase, por lo cual se debe crear una asginación que cuente con la función "Paquete_Foodprice_ver_0_0_1" y sus argumentos de entrada. Lo anterior permite crear varias asignaciones indicando diferente ciudades, año y mes de estudio.

**4.1 Crear asignación**
            
Creando una asginación que contendrá la depuración del objeto. En ella se llama la clase y se indican los argumentos de esta; por ejemplo, sea la asginación "Prueba1":  
            
```
Prueba1=Foodprice::Paquete_Foodprice_ver_0_0_1$new(data_list_precios=x,data_list_abas=y,Mes="Julio",Año=2022,Ciudad="Cali")
```
            
**4.2 Cargar el módulo 0**  
            
Ejecutar el módulo 0, el cual contiene las librerias necesarias para la clase; este se encarga de instalar en caso de ser necesario todos los paquetes.

            
```
Prueba1$Librerias()
```
            
**4.3 Cargar el módulo 1**
            
Ejecutar el módulo 1,se encarga de la depuración y mostrará dos salidas: 
- Estimación_Precios_Minoristas: Datos de la estimación de los precios minoristas.
- Datos_Insumo_Modelos: Datos depurados que servirán como insumo a los modelos.
```        
Prueba1$Módulo_1()
```
  
## :bangbang: Aspectos a tener en cuenta
  
**1. En la presente versión se omiten los precios de cavasa, sólo se trabajan con los datos públicos.**
  
**2. Se encontraron dos POSIBLES errores en el código original que son pertinentes de revisar:**
  
- Linea 55 del módulo 2 (código 2100): La suma omite la columna 1.
  
```
abs_cavasa_mes$Total_alimento = rowSums(abs_cavasa_mes[,2:length(fechas)], na.rm =TRUE)

```
  
- Linea 17 del módulo 3 (código 3300): El promedio de los márgenes de comercialización aparentemente no es coherente.
  
```
for (k in 1:nrow(margenes_historicos)) {
  margenes_historicos$margen_medio[k] = mean(as.numeric(margenes_historicos[k, 2:4]))
} 
```
  
**4. Se añadieron los módulos 2 y 3**
            
- El módulo 2 se encarga de lso datos pertinentes para los modelos de requerimientos
- El módulo 3 se encarga del primer modelo.

Para usarlos sólo ejecute:
            
```  
Prueba1$Módulo_2()
Prueba1$Módulo_3()
```            
El módulo 3 mostrará en el environment global dos bases: Modelo_1_F y Modelo_1_M            
#
            
En la versión 0.1.1 (actual) sólo se encuentra disponible la ciudad  de Cali y el Año 2022.
