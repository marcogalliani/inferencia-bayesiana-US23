########################################################################
#                                                                      #
#                 AMPLIACIÓN DE INFERENCIA ESTADÍSTICA                 #
#                                                                      #
#                  TERCERO DEL GRADO DE ESTADÍSTICA                    #
#                                                                      #
#             DPTO. ESTADÍSTICA E INVESTIGACIÓN OPERATIVA              # 
#                                                                      #
#                   PROF. JOSÉ MARÍA FERNÁNDEZ PONCE                   #
#                                                                      #
########################################################################

########################################################################
#                                                                      #
#        TEMA 6: INFERENCIA BAYESIANA EN MODELOS MULTIPARAMÉTRICOS     #
#                                                                      #
#              SCRIPT 15: TABLAS DE CONTINGENCIA                       #
#                                                                      #
#                                                                      # 
########################################################################

#######################################################################






###########################################################################################################
#
#  UNA APLICACIÓN A DATOS ARQUEOLÓGICOS DE LA EDAD DEL COBRE DEL ASENTAMIENTO DE VALENCINA-GUZMÁN
#
#############################################################################################################
#
# A seis kilómetros de Sevilla en dirección Oeste se encuentran dos pequeños pueblos:
# Valencina de la Concepción y Castilleja de Guzmán. Entre ambos no superan en total una población 
# de 9.000 habitantes. Hasta aquí no pasarían por ser de los pueblos más importantes de la provincia sino 
# llega a ser porque en su subsuelo se encuentra uno de los sitios arqueológicos más importantes del 
# Sur de Europa de la Edad del Cobre. Dicha comunidad prehistórica vivía en un entorno físico muy diferente 
# al actual. A lo largo de los años de estudio del sitio arqueológico de Valencina-Guzmán se han recogido 
# gran cantidad de huesos humanos llegándose a contrastar un total de 135 individuos localizados en 
# diferentes enterramientos y de distintos sexos y edades. Así se obtuvo la siguiente tabla:

#        Megalíticos	   No Megalíticos	Totales
#   Varón	  13              	 9         	22
#   Hembra	10              	11          21
#           23                21
#
# Observando esta tabla surge una pregunta lógica: 
# ¿existía una intencionalidad según sexo a la forma de ser enterrado? Dicho en otras palabras, 
# ¿se usaban más las construcciones megalíticas para enterrar a los varones que las mujeres?
#
##############################################################################################################



###########################################################################################################
#
#
#   TABLA DE CONTINGENCIA
#
#########################################################################################################
library(LearnBayes)
data=matrix(c(13,10,9,11),c(2,2))
data
sum(data)

## Test Chi Cuadrado de Independencia
ll=chisq.test(data,correct=FALSE)
ll$expected
ll$p.value
ll$method

ll$statistic

qchisq(0.95,1)
qchisq(0.90,1)


## Test Chi Cuadrado de Independencia
ll=chisq.test(data,correct=TRUE)
ll
ll$expected
ll$p.value
ll$statistic
ll$method
ll$observed
ll$residuals


## Test Exacto de Fisher
# Obliga a marginales y totales fijos antes del muestreo

fisher.test(data,simulate.p.value=FALSE)

## Test de Barnard
# No hay marginales fijas ni totales

install.packages("Barnard")
library(Barnard)

barnard.test(13,10,9,11,dp=0.001)



# Factor de Bayes tabla de contingencia modelo multinomial para la independencia en H1

a=matrix(rep(1,4),c(2,2))
a
ctable(data,a)


### Factor de Bayes con el paquete BayesFactor
install.packages("BayesFactor")
library(BayesFactor)


data=matrix(c(13,10,9,11),c(2,2))
data


contingencyTableBF(data, sampleType="jointMulti",priorConcentration = 1) # En este caso se considera fijo el total N
contingencyTableBF(data, sampleType="hypergeom",priorConcentration = 1) # Se consideran fijos filas y columnas
contingencyTableBF(data, sampleType="indepMulti",fixedMargin="rows",priorConcentration = 1) # Se consideran fijas filas o columnas

# Este comando calcula BF_{10} donde en la hipótesis nula pone la independencia



## Estudio metalurgia vs Domestico-Funerario

library(LearnBayes)
data=cbind(c(30,4,0,3),c(7,20,10,2))
data
sum(data)

## Test Chi Cuadrado de Independencia
ll=chisq.test(data,correct=FALSE)  # la corrección de Yates sólo se usa para las tablas 2x2
ll$expected
ll$p.value
ll$statistic



## Test Exacto de Fisher
# Oblica a marginales y totales fijos antes del muestreo

fisher.test(data)


#Factor de Bayes
contingencyTableBF(data, sampleType="jointMulti",priorConcentration = 1)



# Factor de Bayes tabla de contingencia modelo multinomial
# H0 es modelo de dependencia, H1 modelo de independencia
a=matrix(rep(1,10),c(5,2))
a
ctable(data,a)


### Herramientas vs enterramiento

data=matrix(c(2,5,8,12),c(2,2))
data
sum(data)

## Test Chi Cuadrado de Bondad de ajuste
ll=chisq.test(data,correct=FALSE)
ll$expected
ll$p.value
ll$statistic

## Test Exacto de Fisher
# Oblica a marginales y totales fijos antes del muestreo

fisher.test(data)




# Factor de Bayes tabla de contingencia modelo multinomial

a=matrix(rep(1,4),c(2,2))
a
ctable(data,a)



