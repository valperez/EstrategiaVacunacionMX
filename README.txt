Autores: 
Rodrigo Zepeda            
Valeria Perez Chavez      

ESTRATEGIA DE VACUNACIÓN MX

-- data --
codigo_conversion_datos : es el codigo de R que utiliza y convierte las bases de datos anteriores en su versión final.
	Sus resultados son:
	datos_1.rds : información del número de habitantes por entidad y municipio
	Notas: el municipio SAN JUAN MIXTEPEC de la entidad de Oaxaca está repetido. Se cree que esto se debe 
		a un error en las fuentes de las bases de datos. No debe afectar en los resultado finales.
	datos_2.rds : información SIR por municipio y entidad
descarga_covid.R : contiene el codigo para descargar la información utilizada para la base de datos_2 así como los 
		   catalogos utilizados
diccionario_covid.rds y dats_covid.rds son el resultado de la descarga anterior. 
conversion_SIR : código para generar los datos SIR de cada entidad de la república dividido en grupos de edad junto con 
		 la población en cada entidad. Sus resultados son :
	datos_SIR_i donde i es el estado al que se refiere la información.

201128 Catalogos : contiene información sobre la key usada en la base de datos 2.

Base_datos_Poblacion : contiene la cantidad de población en México divido por entidad, municipio y grupo de edad

Catalogo_Entidad : contiene el nombre de cada entidad junto con el número clave que le corresponde.

Catalogo_Municipio : contiene el nombre de cada municipio con el número clave que le corresponde. 
		De igual forma contiene la entidad a la que pertenece.



-- model --
fits_julia : contiene los codigos para hacer el ajuste de vacunacion usando Julia
Modelo_Poisson : es el modelo que estamos ajustando para predecir el efecto de las vacunas en México 
		utilizando información de países

-- Otros Paises Datos ---
Carpeta destinada a guardar la información de vacunación de Israel, Estados Unidos, Chile, Reino Unido, 
	Brasil, Costa Rica, Colombia.

-- plots ---
Carpeta destinada a guardar las graficas con los resultados finales de los modelos. 
