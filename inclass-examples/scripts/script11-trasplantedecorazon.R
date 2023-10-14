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
#             TEMA 3: INFERENCIA BAYESIANA EN EL MODELO DE POISSON     #
#                                                                      #
#                     SCRIPT 11: TRASPLANTES DE CORAZÓN                #
#                                                                      #
#                                                                      # 
########################################################################


########################################################################
#
# Supongamos que deseamos estudiar la razón de éxito de operación de trasplantes
# de corazón en un determinado hospital. En particular,observamos que que el 
# número de operaciones n y el número de fallecimientos en 30 días desde la operación
# se denotará por y. En definitiva, nos interesa la probabilidad de fallecer de un paciente.
# Esta predicción se basa en un modelo que usa una información como la condición del paciente
# antes de la operación como el sexo y la raza. Basados en estas probabilidades de predicción
# se puede obtener el número esperado de muertes que será denotado por e. Un modelo estandar
# se supone que el número de fallecimientos y sigue una Poisson de media (n*lambda), y el objetivo
# es estimar la razón de fallecimientos lambda.
#
#    La estimación usual de lambda es el estimador de máxima verosimilitud: lambda_est=y/n. 
# Desafortunadamente, este estimador puede ser "pobre" cuando el número de fallecimientos y es muy
# próximo a cero. En este caso, cuando se recuentan pocos fallecimientos, es preferible usar una
# estimación Bayesiana que usa un conocimiento a priori acerca del tamaño de la razón de mortalidad. Una 
# selección conveniente para la distribución a priori es un miembro de la gamma(alpha, beta).
#
# En concreto, supongamos que el número de fallecimientos observados es zj y el número de operaciones es oj para
# cada diez hospitales (j=1,...,10) donde zj es una Poisson con media (oj*lambda). Si asignamos a lambda una
# a priori no informativa g(lambda):= 1/lambda. Por tanto, la distribución a posteriori es una gamma de parámetros
# alpha=sum(zj) y beta=sum(oj). Los datos arrojan un valor de 16 fallecimientos y un total de 15174 operaciones
# entre los diez hospitales.
#
# Ahora bien, consideremos que para el hospital A se encuentra un solo fallecimiento entre 66 operaciones
# efectuadas. Es decir este hospital tiene una razón de fallecimientos de 1/66. Vamos a construir un conjunto
# de comandos en R para los cálculos Bayesianos: en primer lugar la distribución predictiva y luego una región HPDI 
#
####################################################

alpha=16; beta=15174
yobs=1;ex=66
ncero=10
y=0:10


py=(gamma(alpha+yobs+1)/ gamma(alpha+yobs))*((1/gamma(y+1))*((beta+ex)/(ncero+beta+ex))**(17))*
  ((ncero)/(ncero+beta+ex))**(y)
pp=sum(py)

popi=dnbinom(y,alpha+yobs,(beta+ex)/(ncero+beta+ex))

cbind(y,round(popi,3),round(py/pp,3))
####################################################
#
# La densidad a posteriori de lambda se puede resumir mediante 1000 valores simulados de la gamma
#
###################################################

lambdaA=rgamma(1000, shape=alpha+yobs,rate=beta+ex)
hist(lambdaA,freq=FALSE,main="Histograma de densidad")
curve(dgamma(x,alpha+yobs,rate=beta+ex),add=TRUE,col="red")

library(HDInterval)
dens2 <- density(lambdaA)
hdi(dens2, credMass=0.90)




