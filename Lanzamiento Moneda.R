
#Lanzamiento de una moneda

##-----------------------------Moneda legal P(cara)=P(sello)=0.5-------------------------##


L.M <- function(N){                                   					#N representa el numero de lanzamientos
  
  Moneda<-c("s","c")									                          #Conformaci?n de la moneda
  res<-0											                                  #Se inicia el vector que almacenara los resultados de la moneda
  prob<-0										                                   	#Se inicia el vector que almacena las probabilidades calculadas
  
  for (i in 1:N){
    res[i]<-sample(Moneda,1)
    prob[i]<-sum(res=="c")/i
    plot(1:i,prob,type="l",lty=2,ylim=c(0,1),
    ylab="Probabilidad",xlab="#Lanzamientos", 
    main ="Probabilidad Lanzamiento de una Moneda")
  }
  abline(h=0.5,col="red")									                      #Se agrega al grafico final la linea de prob esperada.
  return(prob[N])
}

L.M (100)										                                  #Se ejecuta la funci?n para 1000 lanzamientos



##-----------------------------Moneda cargada P(cara)=P, P(sello)=1-P-------------------------##



prob <- function(p,N){                                                                       #p y N representan la probabilidad de cara en la moneda y el numero de lanzamientos
  
  
  moneda <- c("cara","sello")                                                                 #conformaci?n de la moneda
  resultado <- character()                                                                    #vector que almacenara los resultados de los lanzamientos
  prob<-numeric()                                                                             #vector que almacena los porcentajes sucesivos
  for (i in 1:N){                                                                             #ciclo de lanzamiento
    resultado [i] <- sample(moneda,1,prob=c(p,1-p))                                           #resultado aleatorio, segun probabilidad
    prob [i]<- sum(resultado=="cara")/length(resultado)                                       #Calculo de la frecuencia en el lanzamiento i
    plot(prob,type="l", ylim=c(0,1), 
    main= paste("Simulacion Frecuencia de cara en el lanzamiento de la moneda, N=",i))        #grafico de la tendencia en la frecuencia
    abline(h=p,col="red")                                                                     #Adici?n de linea  de referencia en p
  }
  
}


prob(0.4, 1000)

