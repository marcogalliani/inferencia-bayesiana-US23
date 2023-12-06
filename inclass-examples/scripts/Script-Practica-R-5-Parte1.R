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
#             TEMA 4: INFERENCIA BAYESIANA EN LA DISTRIBUCION NORMAL   #
#                                PARTE 1.                              #
#                                                                      #
########################################################################


#  PROBLEMA 1


mu=100
tau=12.16
sigma=15
n=4
se=sigma/sqrt(4)
ybar=c(110,125,140)
tau1=1/sqrt(1/se^2+1/tau^2)
mu1=(ybar/se^2+mu/tau^2)*tau1^2
summ1=cbind(ybar,mu1,tau1)
summ1


#Veamos el efecto en gráficas

#Caso 1


curve(dnorm(x,mean=mu,sd=tau),xlim=c(mu-3*tau,mu+3*tau),ylim=c(0,0.08),col="red",
      ylab="Comparativa de densidades")
curve(dnorm(x,mean=ybar[1],sd=se),xlim=c(mu-3*tau,mu+3*tau),ylim=c(0,0.05),col="blue",add=T)
curve(dnorm(x,mean=mu1[1],sd=tau1),xlim=c(mu-3*tau,mu+3*tau),ylim=c(0,0.05),col="green",add=T)


#Caso 2


curve(dnorm(x,mean=mu,sd=tau),xlim=c(mu-3*tau,150),ylim=c(0,0.08),col="red",
      ylab="Comparativa de densidades")
curve(dnorm(x,mean=ybar[2],sd=se),xlim=c(mu-3*tau,150),ylim=c(0,0.05),col="blue",add=T)
curve(dnorm(x,mean=mu1[2],sd=tau1),xlim=c(mu-3*tau,150),ylim=c(0,0.05),col="green",add=T)


#Caso 3


curve(dnorm(x,mean=mu,sd=tau),xlim=c(mu-3*tau,180),ylim=c(0,0.08),col="red",
      ylab="Comparativa de densidades")
curve(dnorm(x,mean=ybar[3],sd=se),xlim=c(mu-3*tau,180),ylim=c(0,0.05),col="blue",add=T)
curve(dnorm(x,mean=mu1[3],sd=tau1),xlim=c(mu-3*tau,150),ylim=c(0,0.05),col="green",add=T)


# Estudiemos ahora la influencia de elegir una t de student como apriori


tscale=20/qt(0.95,2)
tscale


#Representemos ahora en un mismo gráfico las dos posibles a priori

curve(dnorm(x,mean=mu,sd=tau),xlim=c(mu-3*tau,mu+3*tau),ylim=c(0,0.06),col="red",
      ylab="Dos posibles a priori")
curve(1/tscale*dt((x-mu)/tscale,df=2),xlim=c(mu-3*tau,mu+3*tau),ylim=c(0,0.08),col="blue",add=T)

#Ahora vamos a hacer los cálculos de la a posteriori usando una a priori $t$-Student. 
summ2=c()
for (i in 1:3){
  theta=seq(60,180,length=500)
  like= dnorm((theta-ybar[i])/7.5)
  prior=dt((theta-mu)/tscale,2)
  post=like*prior
  post=post/sum(post)
  m= sum(theta*post)
  s=sqrt(sum(theta^2*post)-m^2)
  summ2=rbind(summ2,c(ybar[i],m,s))
}
summ2


#Ahora vamos a comparar las dos a posterioris, normal frente a $t$-student.

cbind(summ1,summ2)


# PROBLEMA 2. LA MARATHON DE NUEVA YORK


library(LearnBayes)
data("marathontimes")
View(marathontimes)
attach(marathontimes)
hist(time,ylab="Frecuencia absoluta",xlab="Tiempos",main="Histograma de los tiempos de la marathon")
boxplot(time,main="Diagrama de Cajas")

shapiro.test(time)
qqnorm(time)

library(ggplot2)


  

library(qqplotr)
datos=data.frame(marathontimes)

gg <- ggplot(data = datos, mapping = aes(sample = time)) +
  geom_qq_band(bandType = "boot",  alpha = 0.25) +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Cuantiles Teóricos", y = "Cuantiles Muestrales")
gg
  
  
  

#A continuación seleccionamos una normal a priori con la información de los percentiles que se dispone.


n=length(time)
media=mean(time)
q1=list(p=0.1,x=190)
q2=list(p=0.9,x=350)
nn=normal.select(q1,q2)
mu0=nn$mu
s0=nn$sigma


#Supongamos que la desviación típica de la verosimilitud es conocida y vale 


s=75
muposte= (n*s0^2*mean(time)+s^2*mu0)/(n*s0^2+s^2)

sposte=sqrt((s0^2*s^2)/(n*s0^2+s^2))

mm=cbind(muposte,sposte)
print(mm)


curve(dnorm(x,mean=mu0,sd=s0),xlim=c(mu0-3*s0,mu0+3*s0),ylim=c(0,0.05),col="red",
      ylab="Comparativa de densidades")

curve(dnorm(x,mean=muposte,sd=sposte),xlim=c(muposte-4*sposte,muposte+4*sposte),
      ylim=c(0,0.05),add=TRUE,col="blue")


#Cálculo de la Región de Credibilidad


inferior=muposte-qnorm(0.975,0,1)*sposte
superior=muposte+qnorm(0.975,0,1)*sposte
rc=cbind(inferior,superior)
print(rc)

#Factor Bayes para  H_0: mu <= 310  frente a  H_1: mu > 310
f0=pnorm(310,mu0,s0)
f1=1-f0

f0/f1

f0poste=pnorm(310,muposte,sposte)
f0poste
f1poste=1-f0poste

f0poste/f1poste

BF01=(f0poste*f1)/(f1poste*f0)

print(BF01)

#Por tanto, según los valores de la tabla de Jeffrey, hay una evidencia fuerte hacia que la hipótesis mu <= 310.
  
  
  
