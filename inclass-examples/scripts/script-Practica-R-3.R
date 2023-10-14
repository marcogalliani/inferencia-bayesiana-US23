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
#             TEMA 3: INFERENCIA BAYESIANA EN BETA-BINOMIAL            #
#                                                                      #
########################################################################





## ESTUDIO DEL CASO DISCRETO

# CONSTRUCCION Y REPRESENTACION DE LA APRIORI Y APOSTERIORI

par(mfrow=c(1,2)) # Para que salgan dos gráficos en la misma altura

p=seq(0.05,0.95, by=0.1) 

prior=c(2,4,8,8,4,2,1,1,1,1) 
prior=prior/sum(prior) 
plot(p,prior,type="h",ylab="Distribution a priori")


library("LearnBayes") 
data=c(11,16) 
post=pdisc(p,prior,data) # pdisc es un comando para calcular la distribución
# a posteriori de una discreta 
cbind(p,prior,post) # cbind es un comando para unir vectores o matrices
plot(p,post, type="h", ylab="Distribución a posteriori",col="red") 

# Estimador de Bayes

# para la función de pérdida cuadrática
pbayes=sum(p*post)
pbayes

#para la función de pérdida L1
install.packages("spatstat")
library(spatstat)
weighted.mean(p,post)
weighted.median(p, post)


# Región de Credibilidad al 95%

library(tidyverse)
lo=cbind(p,prior,post)
df=data.frame(lo)

a=df%>%arrange(desc(post)) #crea una nueva ordenada según post manteniendo los casos

a$postcum=cumsum(a$post)

sort(a$p[which(a$postcum<0.95)])

######################################################
#
#   CASO CONTINUO
#
######################################################

s=11;f=16 #se supone que la a priori tiene de parámetros a y b, debido a la información existente.
q1=list(p=.5,x=0.3) #mediana 0.3
q2=list(p=.9,x=0.5) #percentil 90
beta.select(q1,q2)

a=beta.select(q1,q2)[1]
b=beta.select(q1,q2)[2]

# A continuación veamos ahora el efecto gráfico de transformación de la apriori en la 
# aposteriori

a=beta.select(q1,q2)[1]
b=beta.select(q1,q2)[2]

curve(dbeta(x,a,b),col="black",xlim=range(0,1),ylim=range(0,5.4)) #a priori
curve(dbeta(x,s+1,f+1),col="red",xlim=range(0,1),add=T) #verosimilitud
curve(dbeta(x,a+s,f+s),col="blue",xlim=range(0,1),add=T) #a posteriori

# Los comandos siguientes hacen lo mismo, del paquete LearnBayes
par(mfrow=c(1,1))
par=c(a,b)
triplot(par,c(s,f))


#Vamos a calcular el factor de Bayes para el contraste $H_0: p<0.5$ frente a $H_1:p>=0.5$
  
  
pp1<-1-pbeta(0.5,a+s,f+b)
pp0=pbeta(0.5,a+s,f+b)
p1=1-pbeta(0.5,a,f)
p0=pbeta(0.5,a,f)
FB01= (pp0/pp1)/(p1/p0)
print(FB01)


#Podemos obtener también la versión de los intervalos de confianza, que en el caso Bayesiano se llaman regiones creíbles
#si queremos una zona creíble al 95% calcularemos los cuantiles a posteriores al nivel del $5\%$ y $95\%$
  
  
rc<-qbeta(c(0.05,0.95),a+s,b+f)
print(rc)


#Para calcular el intervalo de máxima densidad (HPDI) tenemos que recurrir a simular una muestra de la densidad a posteriori y luego usar el paquete \texttt{HDInterval}. En este caso tendríamos



library(HDInterval)
muestra=rbeta(10000,a+s,b+f)
dens <- density(muestra)
hdi(dens, credMass=0.90)


#Vemos ahora como se usaría la distribución predicitiva en el modelo beta-binomial.


ab=c(11+a,16+b) # ojo que en realidad esta es la aposteriori, que es la apriori en el 
# modelo predictivo
m=20; ys=0:20
pred<-pbetap(ab,m,ys)
pred=round(pred,3)
pp=cbind(ys,pred)
pp
predplot(ab,20,7)

