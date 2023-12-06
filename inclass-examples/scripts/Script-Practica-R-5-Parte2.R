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
#                                PARTE 2.                              #
#                                                                      #
########################################################################

#La Normal-GammaI



library("plot3Drgl")
library(plot3D)
require(rgl)

m=10 # valor de la media
c= 1/10 # valor de la constante
a=1# primer parámetro de la gamma invertida
b=4 # segundo valor de la gamma invertida
x <- seq(m-2, m+2, length.out = 50)  
y <- seq(0.11, 5, length.out = 25) 
ngai <- function(x,y) {
  b^a/(gamma(a)) *(1/y^(a+1))*exp(-b/y)*(1/(sqrt(2*pi)))*exp(-(x-m)^2/(2*y*c))
}

z <- outer(x, y, ngai)  
persp(x, y, z,main="Perspectiva Cónica",
      zlab = "Height",
      theta = 30, phi = 15,
      col = "springgreen", shade = 0.5)

persp3D(x,y,z,theta=30, phi=50, axes=TRUE,scale=2, box=TRUE, nticks=5, 
        
        ticktype="detailed", xlab="X-value", ylab="Y-value", zlab="Z-value", 
        
        main="Normal Gamma Invertida")
        
#Problema 1.

#LA MARATHON DE NUEVA YORK




library(LearnBayes)
data("marathontimes")
View(marathontimes)
attach(marathontimes)
hist(time,freq=F)
curve(dnorm(x,mean=mean(time),sd=sd(time)),add=T,col="blue")
shapiro.test(time)
t.test(time)


#Efecto gráfico sobre distintas curvas según posibles valores de las medias del IC.


hist(time,freq=F)
curve(dnorm(x,mean=255,sd=sd(time)),add=T,col="red")
curve(dnorm(x,mean=265,sd=sd(time)),add=T,col="green")
curve(dnorm(x,mean=285,sd=sd(time)),add=T,col="purple")
curve(dnorm(x,mean=300,sd=sd(time)),add=T,col="blue")
curve(dnorm(x,mean=400,sd=sd(time)),add=T,col="blue")


#Hagamos lo mismo para la varianza


n=length(time)
uppert=(n-1)*sd(time)^2/qchisq(0.025,df=n-1);sqrt(uppert)
lowert=(n-1)*sd(time)^2/qchisq(0.975,df=n-1);sqrt(lowert)

#Representemos curvas normales para diferentes desviaciones típicas.


hist(time,freq=F)
curve(dnorm(x,mean=255,sd=39),add=T,col="red")
curve(dnorm(x,mean=265,sd=45),add=T,col="green")
curve(dnorm(x,mean=285,sd=37.5),add=T,col="purple")
curve(dnorm(x,mean=300,sd=70),add=T,col="blue")
curve(dnorm(x,mean=270,sd=100),add=T,col="blue")

#A continuación representamos el contorno de la normal-gamma-invertida

d=mycontour(normchi2post,c(220,330,500,10000),time)
title(xlab="Media",ylab="Varianza")

Su=sum((time-mean(time))^2)
n=length(time)
S=Su/(n) # Se usará más adelante
sigma2=(Su)/rchisq(1000,df=n)
mu=rnorm(1000,mean=mean(time),sd=sqrt(sigma2)/sqrt(n))

#A continuación se muestran todos estos datos simulados en un contorno.


d=mycontour(normchi2post,c(220,330,500,10000),time)
title(xlab="Media",ylab="Varianza")
points(mu,sigma2)


# A continuación se calculan los HPDI para las marginales correspondientes

library(HDInterval)
dens2 <- density(sigma2)
sqrt(hdi(dens2, credMass=0.90))

tsup=qt(0.975,n)
ss=mean(time)+tsup*sqrt(S/n)
ii=mean(time)-tsup*sqrt(S/n)
print(c(ii,ss))

