
#------------------------------------------------------#
# Diana Carolina Arias Cod. 1528008
# Cesar Andres Saavedra Cod. 1628466
# Simulacion Estadistica
# Tarea No. 2
#------------------------------------------------------#
########################################################
########################################################
#------------------------Punto1------------------------#
#---Situacion A---#
count_bueno<-function (data,n){
  y<-0
  x<-sample(data,n,replace=F)
  for(i in 1:n){
    if(x[i]=="Buen_Estado"){
      y[i]<-1
    }else{
      y[i]<-0
    }
  }
  all_sum<-sum(y,na.rm=T)
  if(all_sum==n){
    return(1)
  }else{return(0)}
}

aprox_x<-function(n_simul){
  x<-0
  for(i in 1:n_simul){
    x[i]<-count_bueno(chips_memoria,5)
  }
  y<-sum(x)/length(x)
  return(y)
}

chips_memoria<-c(rep('Buen_Estado',90),rep('Danados',10))
aprox_x(100)


#---Situacion B---#
dhyper(5,90,10,5,log=F)


#------------------------Punto2------------------------#
#---Situacion A---#
prob <- matrix(200)
nsim=25
total=1
for (i in 1:nsim){
  total=total*((366-i)/365)
  prob[i] <- 1-total
  plot(prob, xlab="Numero de Personas en la Habitacion", ylab="Probabilidad", 
       main="Paradoja del Cumpleanos", pch=18, type="b", col="royalblue4")
}
print(prob)

#---Situacion B---#
cuartos = 100000
personas = 1
asfd <- matrix(NA)

for(i in 1:nsim){
  personas  = personas*((100001-i)/cuartos)
  asfd[i]<- personas
}
cuartos<-100
x<-0
matrix<- matrix(NA)
for(i in 1:25){
  matrix[i] <- sample(cuartos,1)
}
matrix
print(asfd)

#------------------------Punto3------------------------#
#---Situacion A---#
sample(1:100,5)
choose(100,5)

#---Situacion B---#
dhyper(2,90,10,5)
(choose(90,2)*choose(10,3))/choose(100,5)

#---Situacion C---#
chips.memoria<-rhyper(100,90,10,5)
mean(chips.memoria)
var(chips.memoria)

#---Situacion D---#
sum((0:5)*dhyper(0:5,90,10,5))

#------------------------Punto4------------------------#
#---Situacion A---#
a=c(5,6,7,6,8,7)
length(a)
unique(a)
length(unique(a))
length(a)-length(unique(a))
duplicated(a)
length(duplicated(a))
sum(duplicated(a))

#---Situacion B---#

#------------------------Punto5------------------------#
#---Situacion A---#
dado <- c(1:6)
sample(dado,3,replace=T)

sim1 = 10
sim2 = 100
sim3 = 1000
conteo1 <- 0
conteo2 <- 0
conteo3 <- 0

# Para 10 simulaciones
for (j in 1:sim1){
  lanzamiento1 <- sample(dado,3,replace=T)
  total = 0
  for(i in 1:3){
    if(lanzamiento1[i]== "6"){
      total=total+1}
    else{total=total}
  }
  conteo1[j]=total
}

# Para 100 simulaciones 
for (j in 1:sim2){
  lanzamiento2 <- sample(dado,3,replace=T)
  total = 0
  for(i in 1:3){
    if(lanzamiento2[i]== "6"){
      total=total+1}
    else{total=total}
  }
  conteo2[j]=total
}

# Para 1000 simulaciones 
for (j in 1:sim3){
  lanzamiento3 <- sample(dado,300,replace=T)
  total = 0
  for(i in 1:3){
    if(lanzamiento3[i]== "6"){
      total=total+1}
    else{total=total}
  }
  conteo3[j]=total
}

#par(mfrow=c(3,1))
plot(conteo1, pch=10, col="royalblue4" ,main="Lanzamiento de 3 Dados para 10 Simulaciones", 
     xlab="Numero de Lanzamientos", ylab="Cantindad de '6' obtenidos", bty="n", cex=2)
plot(conteo2, pch=10, col="royalblue4", main="Lanzamiento de 3 Dados para 100 Simulaciones", 
     xlab="Numero de Lanzamientos", ylab="Cantindad de '6' obtenidos", bty="n", cex=2)
plot(conteo3, pch=10, col="royalblue4", main="Lanzamiento de 3 Dados para 1000 Simulaciones", 
     xlab="Numero de Lanzamientos", ylab="Cantindad de '6' obtenidos", bty="n", cex=2)

#------------------------Punto6------------------------#
#---Binomial---#
Nsim1=100
matriz1=matrix(200)
for(i in 1:Nsim1){
  dbinom=dbinom(i,100,0.1)
  matriz1[i]=dbinom
  plot(matriz1, main="Binomial (100, 0.1)", xlab="Numero de Simulaciones", ylab="", pch=18, col="royalblue4", bty="n", cex=2, type="b")}
for(i in 1:Nsim1){
  dbinom=dbinom(i,100,0.9)
  matriz1[i]=dbinom
  plot(matriz1, main="Binomial (100, 0.9)", xlab="Numero de Simulaciones", ylab="", pch=18, col="royalblue4", bty="n", cex=2, type="s")}

#---Poisson---#
Nsim2=100
matriz2=matrix(200)
for(j in 1:Nsim2){
  dpois1=dpois(j,lambda=10)
  matriz2[j]=dpois1
  plot(matriz2, main="Poisson para Lambda=10", xlab="Numero de Simulaciones", ylab="", pch=18, col="royalblue4", bty="n", cex=2, type="b")}
for(j in 1:Nsim2){
  dpois3=dpois(j,lambda=30)
  matriz2[j]=dpois3
  plot(matriz2, main="Poisson para Lambda=30", xlab="Numero de Simulaciones", ylab="", pch=18, col="tomato4", bty="n", cex=2, type="b")}

#---Hipergeometrica---#
Nsim3=100
matriz3=matrix(200)
for(i in 1:Nsim3){
  dhypher=dhyper(i,90,10,5)
  matriz3[i]=dhyper
  plot(matriz3)}
for(i in 1:Nsim3){
  dhypher=dhyper(i,70,30,5)
  matriz3[i]=dhyper
  plot(matriz3)}

#---Binomial Negativa---#
Nsim4=100
matriz4=matrix(200)

for(i in 1:Nsim4){
  dnbinom=dnbinom(i,100,0.2)
  matriz4[i]=dnbinom
  plot(matriz4)}
for(i in 1:Nsim4){
  dnbinom=dnbinom(i,100,0.8)
  matriz4[i]=dnbinom
  plot(matriz4)}

#---Normal---#
Nsim5=100
matriz5=matrix(200)
for(i in 1:Nsim5){
  dnorm=dnorm(i,50,5)
  matriz5[i]=dnorm
  plot(matriz5)}
for(i in 1:Nsim5){
  dnorm=dnorm(i,100,5)
  matriz5[i]=dnorm
  plot(matriz5)}

#---Gamma---#

#---Weibull---#

#------------------------Punto7------------------------#
#---Situacion A---#
Combinaciones <- choose(52,5)
SinAs <- choose(48,5)
pro <- SinAs/Combinaciones

#---Situacion B---#
baraja <- c('AC','AD','AP','AT','2C','2D','2P','2T','3C','3D','3P','3T','4C','4D','4P','4T',
            '5C','5D','5P','5T','6C','6D','6P','6T','7C','7D','7P','7T','8C','8D','8P','8T',
            '9C','9D','9P','9T','10C','10D','10P','10T','JC','JD','JP','JT','KC','KD','KP','KT','QC','QD','QP','QT')
sample(baraja,5,F)



#------------------------Punto8------------------------#
b = pi/4                                
a = 0      
integral = (b-a)*mean(log(1+(tan(runif(1000000, min=a, max=b)))^2)) 
integral


