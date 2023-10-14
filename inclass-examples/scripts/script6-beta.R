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
#             TEMA 3: INFERENCIA BAYESIANA EN MODELOS PARAMÉTRICOS     #
#                                                                      #
#                     SCRIPT 6: LA DISTRIBUCIÓN BETA                   #
#                                                                      #
#                                                                      # 
########################################################################


# ESTUDIO DE LA DISTRIBUCIÓN BETA
#
# La distribución Beta tiene dos parámetros de forma y juega un papel muy importante en la estimación
# Bayesiana de una proporción.
#
# Caso 1: Distribuciones betas simétricas para parámetros mayores a la unidad

par(mfrow=c(1,1))
curve(dbeta(x, 1,1),xlim=range(0,1),ylim=range(0,3.2),xlab="Beta simétrica",ylab="")
curve(dbeta(x,2,2),xlim=range(0,1),ylim=range(0,3.2), col="blue",add=T)
curve(dbeta(x,3,3),xlim=range(0,1),ylim=range(0,3.2), col="red",add=T)
curve(dbeta(x,4,4),xlim=range(0,1),ylim=range(0,3.2), col="violet",add=T)
curve(dbeta(x,7,7),xlim=range(0,1),ylim=range(0,3.2), col="green",add=T)
curve(dbeta(x,7.9,7.9),xlim=range(0,1),ylim=range(0,3.2), col="yellow",add=T)

# Caso 2: Distribuciones betas simétricas para parámetros menores a la unidad

curve(dbeta(x, 0,0),xlim=range(0,1),ylim=range(0,3.2),xlab="Beta simétrica parámetros menores a la unidad",ylab="")
curve(dbeta(x,0.2,0.2),xlim=range(0,1),ylim=range(0,3.2), col="blue",add=T)
curve(dbeta(x,0.3,0.3),xlim=range(0,1),ylim=range(0,3.2), col="red",add=T)
curve(dbeta(x,0.4,0.4),xlim=range(0,1),ylim=range(0,3.2), col="violet",add=T)
curve(dbeta(x,0.7,0.7),xlim=range(0,1),ylim=range(0,3.2), col="green",add=T)
curve(dbeta(x,.89,.89),xlim=range(0,1),ylim=range(0,3.2), col="yellow",add=T)
curve(dbeta(x,0.9,0.9),xlim=range(0,1),ylim=range(0,3.2), col="black",add=T)
curve(dbeta(x,0.99,0.99),xlim=range(0,1),ylim=range(0,3.2), col="blue",add=T)

# Caso 3: Distribuciones betas sesgadas a la izquierda para parámetros mayores a la unidad

curve(dbeta(x, 2,1),xlim=range(0,1),ylim=range(0,7),xlab="Beta sesgada izq. b=1",ylab="")
curve(dbeta(x,3,1),xlim=range(0,1),ylim=range(0,7), col="blue",add=T)
curve(dbeta(x,4,1),xlim=range(0,1),ylim=range(0,7), col="red",add=T)
curve(dbeta(x,5,1),xlim=range(0,1),ylim=range(0,7), col="violet",add=T)
curve(dbeta(x,6,1),xlim=range(0,1),ylim=range(0,7), col="green",add=T)
curve(dbeta(x,7.9,1),xlim=range(0,1),ylim=range(0,7), col="yellow",add=T)


curve(dbeta(x, 3,2),xlim=range(0,1),ylim=range(0,3.5),xlab="Beta sesgada izq. b=2",ylab="")
curve(dbeta(x,4,2),xlim=range(0,1),ylim=range(0,3.5), col="blue",add=T)
curve(dbeta(x,5,2),xlim=range(0,1),ylim=range(0,3.5), col="red",add=T)
curve(dbeta(x,6,2),xlim=range(0,1),ylim=range(0,3.5), col="violet",add=T)
curve(dbeta(x,7,2),xlim=range(0,1),ylim=range(0,3.5), col="green",add=T)
curve(dbeta(x,8,2),xlim=range(0,1),ylim=range(0,3.5), col="yellow",add=T)

# Caso 4: Distribuciones betas sesgadas a la derecha para parámetros mayores a la unidad


curve(dbeta(x, 2,3),xlim=range(0,1),ylim=range(0,3.5),xlab="Beta sesgada izq. b=2",ylab="")
curve(dbeta(x,2,4),xlim=range(0,1),ylim=range(0,3.5), col="blue",add=T)
curve(dbeta(x,2,5),xlim=range(0,1),ylim=range(0,3.5), col="red",add=T)
curve(dbeta(x,2,6),xlim=range(0,1),ylim=range(0,3.5), col="violet",add=T)
curve(dbeta(x,2,7),xlim=range(0,1),ylim=range(0,3.5), col="green",add=T)
curve(dbeta(x,2,8),xlim=range(0,1),ylim=range(0,3.5), col="yellow",add=T)

# Ejemplo numérico

x<-rbeta(1000, 3,3)  # generamos una muestra de tamaño 1000 de una beta a=3, b=3
hist(x,freq=FALSE)  # Representamos el histograma de densidad de la muestra
curve(dbeta(x,2,2),add=T) # vemos el ajuste a una beta(2,2)
aa<-function(a){mean(x)-a/(a+3)} # A partir de los datos estimamos el primer parámetro suponiendo b=3
xx<-uniroot(aa,lower=0,upper=100) # Resuelve la ecuación del primer parámetro en términos de la media
curve(dbeta(x,xx$root,3),col="red",add=T)  # Ajusta la curva estimada

