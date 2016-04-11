
setwd("C:/Users/pedro/Desktop/tarea3/clustering")


install.packages("devtools")
library(devtools)
install.packages("rgl")
library(rgl)



# metodo codo de jambu
codo_jambu= function(data){
  data_s <- data
  t <- (nrow(data_s)-1)*sum(apply(data_s,2,var))
  for (i in 2:15) t[i] <- sum(kmeans(data_s, centers=i)$withinss)
  plot(1:15, t, type="b", xlab="# Clusters", ylab="Within groups sum of squares")
}

#matriz de confusion
confusion_matrix = function(cl, clust)
{
  return(table(True = cl, Predicted = clust))
}

#diagonal de matriz
sum_diagonal = function(matrix)
{
  suma = 0;
  
  for (i in 1:ncol(matrix))
  {
  suma = suma + matrix[i,i]
  }
  
  return(suma)
}


#evaluar matriz de confusion

evaluation = function(Matrix, Rows)
{
  aciertos = sum_diagonal(Matrix)
  Tasa_aciertos = (aciertos/Rows) * 100
  Tasa_fallos = 100 - Tasa_aciertos
  
 print( paste("Tasa de aciertos: ", Tasa_aciertos))
 print( paste("Tasa de fallos:  ",Tasa_fallos) )
}

##################################################################
##################################################################

#a.csv

datos_a = read.csv("a.csv",header = F)

#analisis exploratorio
summary(datos_a)


#redefinir las clases, para que esten en un rango de [1:3] , en vez de [0:2]
datos_a$V3 = as.numeric(datos_a$V3)
datos_a$V3 = datos_a$V3 + 1


# visualizar datos: 3 cluster con forma de esfera
plot(datos_a[,1:2], col = datos_a$V3,
     xlab = "X", ylab = "Y",
     main = "a.csv")


a_kmean = kmeans(x = datos_a[, c("V1", "V2")],
                   centers = 3)


plot(x = datos_a$V1,
     y = datos_a$V2,
     col = a_kmean$cluster)



points(x = a_kmean$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)



#matriz de confusion para k means y evaluacion

matrix_kmeans_a = confusion_matrix(datos_a$V3, a_kmean$cluster)

evaluation(matrix_kmeans_a, nrow(datos_a))




##############HCLUST#######################

#matriz de distancias

a = datos_a
a$V3 = NULL
a = as.matrix(a)
distance_a = dist(a)



#modelo single

modelo_single_a = hclust(distance_a, method="single")
corte_a1= cutree(modelo_single_a, k = 3)

plot(x = a[,1], y = a[,2], main = paste(c("Data_set", "a.csv") , collapse = " "),
    sub = paste(c("single", "h-clush"), collapse= " "),
    xlab = "x", ylab = "y",
    col = corte_a1)


#matriz de confusion para k single y evaluacion

matrix_single_a = confusion_matrix(datos_a$V3, corte_a1)

evaluation(matrix_single_a, nrow(datos_a))




#modelo Complete

modelo_complete_a = hclust(distance_a, method="complete")
corte_a2= cutree(modelo_complete_a, k = 3)

plot(x = a[,1], y = a[,2], main = paste(c("Data_set", "a.csv") , collapse = " "),
     sub = paste(c("complete", "h-clush"), collapse= " "),
     xlab = "x", ylab = "y",
     col = corte_a2)



#matriz de confusion para complete y evaluacion
matrix_complete_a = confusion_matrix(datos_a$V3, corte_a2)

evaluation(matrix_complete_a , nrow(datos_a))




#modelo average

modelo_average_a = hclust(distance_a, method="average")
corte_a3= cutree(modelo_average_a, k = 3)

plot(x = a[,1], y = a[,2], main = paste(c("Data_set", "a.csv") , collapse = " "),
     sub = paste(c("average", "h-clush"), collapse= " "),
     xlab = "x", ylab = "y",
     col = corte_a3)


#matriz de confusion para average y evaluacion
matrix_single_a = confusion_matrix(datos_a$V3, corte_a3)

evaluation(matrix_single_a, nrow(datos_a))


#################################################
##############################################

########## Moon.csv ##############


datos_moon = read.csv("moon.csv",header = F)

#analisis exploratorio
summary(datos_moon)


#redefinir las clases, para que esten en un rango de [1:2] , en vez de [0:2]
datos_moon$V3 = as.numeric(datos_moon$V3)
datos_moon$V3 = datos_moon$V3 + 1




# visualizar datos: 
plot(datos_moon[,1:2], col = datos_moon$V3,
     xlab = "X", ylab = "Y",
     main = "moon.csv")


#Se aprecian dos cluster con forma de parabola cada uno d igual dimension

moon_kmean = kmeans(x = datos_moon[, c("V1", "V2")],
                 centers = 2)


plot(x = datos_moon$V1,
     y = datos_moon$V2,
     col = moon_kmean$cluster)




points(x = moon_kmean$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 2)



#matriz de confusion para kmeans y evaluacion

matrix_kmeans_moon = confusion_matrix(datos_moon$V3, moon_kmean$cluster)

evaluation(matrix_kmeans_moon, nrow(datos_moon))




##############HCLUST#######################

#matriz de distancias

moon = datos_moon
moon$V3 = NULL
moon = as.matrix(moon)
distance_moon = dist(moon)



#modelo single

modelo_single_moon = hclust(distance_moon, method="single")
corte_moon1= cutree(modelo_single_moon, k = 2)

plot(x = moon[,1], y = moon[,2], main = paste(c("Data_set", "moon.csv") , collapse = " "),
     sub = paste(c("single", "h-clush"), collapse= " "),
     xlab = "x", ylab = "y",
     col = corte_moon1)

#matriz de confusion para single y evaluacion

matrix_single_moon = confusion_matrix(datos_moon$V3, corte_moon1)

evaluation(matrix_single_moon, nrow(datos_moon))




#modelo Complete

modelo_complete_moon = hclust(distance_moon, method="complete")
corte_moon2= cutree(modelo_complete_moon, k = 2)

plot(x = moon[,1], y = moon[,2], main = paste(c("Data_set", "moon.csv") , collapse = " "),
     sub = paste(c("complete", "h-clush"), collapse= " "),
     xlab = "x", ylab = "y",
     col = corte_moon2)


#matriz de confusion para complete y evaluacion

matrix_complete_moon = confusion_matrix(datos_moon$V3, corte_moon2)

evaluation(matrix_complete_moon, nrow(datos_moon))



#modelo average

modelo_average_moon = hclust(distance_moon, method="average")
corte_moon3= cutree(modelo_average_moon, k = 2)

plot(x = moon[,1], y = moon[,2], main = paste(c("Data_set", "moon.csv") , collapse = " "),
     sub = paste(c("average", "h-clush"), collapse= " "),
     xlab = "x", ylab = "y",
     col = corte_moon3)


#matriz de confusion para average y evaluacion

matrix_average_moon = confusion_matrix(datos_moon$V3, corte_moon3)

evaluation(matrix_average_moon, nrow(datos_moon))



#####################################################################
#####################################################################
#################################################################


#good_luck.csv

datos_good = read.csv("good_luck.csv",header = F)

#analisis exploratorio
summary(datos_moon)


#redefinir las clases, para que esten en un rango de [1:2], en vez de  [0:2]
datos_good$V11 = as.numeric(datos_good$V11)
datos_good$V11 = datos_good$V11 + 1


# visualizar datos: 
plot(datos_good[,1:10], col = datos_good$V11,
     main = "goodluck.csv")


good_kmean = kmeans(x = datos_good[, 1:10],
                    centers = 2)

plot(datos_good[,1:10], col= good_kmean$cluster,main="k-mean good_luck")



#matriz de confusion para kmean y evaluacion

matrix_kmeans_good = confusion_matrix(datos_good$V11, good_kmean$cluster)

evaluation(matrix_kmeans_good, nrow(datos_good))




##############  HCLUST  #######################

#matriz de distancias

good = datos_good
good$V11 = NULL
good = as.matrix(good)
distance_good = dist(good)



#modelo single

modelo_single_good = hclust(distance_good, method="single")
corte_good1= cutree(modelo_single_good , k = 2)
good= as.data.frame(good)

plot(good[,1:10],
     main = paste(c("Single", "goodluck.csv"), collapse = " "),
     col = corte_good1)



#matriz de confusion para single y evaluacion

matrix_single_good = confusion_matrix(datos_good$V11, corte_good1)

evaluation(matrix_single_good, nrow(datos_good))



#modelo Complete

modelo_complete_good = hclust(distance_good, method="complete")
corte_good2= cutree(modelo_complete_good, k = 2)
good= as.data.frame(good)

plot(good[,1:10],
     main = paste(c("Complete", "goodluck.csv"), collapse = " "),
     col = corte_good2)


#matriz de confusion para complete y evaluacion

matrix_complete_good = confusion_matrix(datos_good$V11, corte_good2)

evaluation(matrix_complete_good, nrow(datos_good))





#modelo average

modelo_average_good = hclust(distance_good, method="average")
corte_good3= cutree(modelo_average_good, k = 2)
good= as.data.frame(good)

plot(good[,1:10],
     main = paste(c("Average", "goodluck.csv"), collapse = " "),
     col = corte_good3)


#matriz de confusion para average y evaluacion

matrix_average_good = confusion_matrix(datos_good$V11, corte_good3)

evaluation(matrix_average_good, nrow(datos_good))




########################################################################
########################################################################


# H.CSv

datos_h = read.csv("h.csv",header = F)

#analisis exploratorio

summary (datos_h)

#llevar el rango a [1,11]

datos_h$V4 = floor(datos_h$V4) - 3

plot3d(datos_h$V1, datos_h$V2, datos_h$V3, col = datos_h$V4, main = "h.csv")



h_kmean= kmeans(x = datos_h[, 1:3], centers = 11)
plot3d(datos_h$V1, datos_h$V2, datos_h$V3, col = h_kmean$cluster, main = "K-mean h.csv")



# matriz confusion para kmeans

matrix_kmeans_h = confusion_matrix(datos_h$V4, h_kmean$cluster)

evaluation(matrix_kmeans_h, nrow(datos_h))



##############  HCLUST  #######################

#matriz de distancias

h = datos_h
h$V4 = NULL
h = as.matrix(h)
distance_h = dist(h)

 
#single h

modelo_single_h = hclust(distance_h, method="single")
corte_h1= cutree(modelo_single_h, k = 11)

plot3d(datos_h$V1, datos_h$V2, datos_h$V3, col = corte_h1, main = " Single  h.csv")



#matriz confusion single h

table_single_h = confusion_matrix(datos_h$V4, corte_h1)

evaluation(table_single_h, nrow(datos_h))





#complete h


modelo_h2 = hclust(distance_h, method="complete")
corte_h2= cutree(modelo_h2, k = 11)

plot3d(datos_h$V1, datos_h$V2, datos_h$V3, col = corte_h2, main = "h.csv")




#matriz confusion complete h

matrix_complete_h = confusion_matrix(datos_h$V4, corte_h2)


evaluation(matrix_complete_h, nrow(datos_h))







#average h


modelo_h3 = hclust(distance_h, method="average")
corte_h3= cutree(modelo_h3, k = 11)

plot3d(datos_h$V1, datos_h$V2, datos_h$V3, col = corte_h3, main = "h.csv")



#matriz confusion average h

matrix_average_h = confusion_matrix(datos_h$V4, corte_h3)

evaluation(matrix_average_h, nrow(datos_h))





##############################################################################
#S.cvs



datos_s = read.csv("s.csv",header = F)

#analisis exploratorio
summary(datos_s$V4)

# fijamos las clases que estan en un rango de [-5,5]  a [1,6]
for(i in 1:length(datos_s$V4)){
 
  if (datos_s$V4[i]< -4.0)
    datos_s$V4[i]=as.numeric(1)
  
  else if(datos_s$V4[i]< -2.0)
    datos_s$V4[i]=as.numeric(2)
  
  else if(datos_s$V4[i]< -0.0)
    datos_s$V4[i]=as.numeric(3)
  
  else if(datos_s$V4[i]< 2.0)
    datos_s$V4[i]=as.numeric(4)
  
  else if(datos_s$V4[i]< 4.0)
    datos_s$V4[i]=as.numeric(5)
  
  else 
    datos_s$V4[i]=as.numeric(6)
    
  
}

#visualizar datos

plot3d(datos_s$V1, datos_s$V2, datos_s$V3, col = datos_s$V4, main = "s.csv")


#aplicando k medias

s_kmean= kmeans(x = datos_s[, 1:3], centers = 6)

plot3d(datos_s$V1, datos_s$V2, datos_s$V3, col = s_kmean$cluster, main = "s.csv")


# matriz confusion para kmeans y evaluacion


matrix_kmeans_s = confusion_matrix(datos_s$V4, s_kmean$cluster)

evaluation(matrix_kmeans_s, nrow(datos_s))




##############  HCLUST  #######################

#matriz de distancias


s = datos_s
s$V4 = NULL
s = as.matrix(s)
distance_s = dist(s)


#single s

modelo_single_s = hclust(distance_s, method="single")
corte_s1= cutree(modelo_single_s, k = 6)

plot3d(datos_s$V1, datos_s$V2, datos_s$V3, col = corte_s1, main = "Single s.csv")



#matriz confusion single s y evaluacion

matrix_single_s = confusion_matrix(datos_s$V4, corte_s1)

evaluation(matrix_single_s, nrow(datos_s))




#complete s

modelo_complete_s = hclust(distance_s, method="complete")
corte_s2= cutree(modelo_complete_s, k = 6)

plot3d(datos_s$V1, datos_s$V2, datos_s$V3, col = corte_s2, main = "s.csv")



#matriz confusion complete s y evaluacion

matrix_complete_s = confusion_matrix(datos_s$V4, corte_s2)

evaluation(matrix_complete_s, nrow(datos_s))



#average s


modelo_average_s = hclust(distance_s, method="average")
corte_s3= cutree(modelo_average_s, k = 6)

plot3d(datos_s$V1, datos_s$V2, datos_s$V3, col = corte_s3, main = "s.csv")



#matriz confusion average s  y evaluacion

matrix_average_s = confusion_matrix(datos_s$V4, corte_s3)

evaluation(matrix_average_s, nrow(datos_s))




####################################################
################################
#HELP.csv



datos_help = read.csv("help.csv",header = F)

#analisis exploratorio

summary(datos_help)

#establecer el rango a partir de 1

#rango de [1,20]

datos_help$V4 = floor(datos_help$V4) + 6

summary(datos_help$V4)



plot3d(datos_help$V1, datos_help$V2, datos_help$V3, col = datos_help$V4)
#se observan 3 cluster

#al asignar las clases se observa una distribucion, que  forma algo parecido a las letras 'S' y 'O'.
#Esta distribución es parecida a los datasets h.csv y s.csv. Donde los métodos no tuvieron un rendimiento óptimo




#Podemos definir 3 cluster, basandonos en los valores del eje X, que delimitan cada letra según el gráfico observado

for(i in 1:length(datos_help$V4)){
  
  if (datos_help$V1[i]< 10.0)
    datos_help$V4[i]=as.numeric(1)
  
  else if(datos_help$V1[i]< 45.0)
    datos_help$V4[i]=as.numeric(2)
  
  else 
    datos_help$V4[i]=as.numeric(3)
  
  
}



#aplicando k medias para help

help_kmean= kmeans(x = datos_help[, 1:3], centers = 3)

plot3d(datos_help$V1, datos_help$V2, datos_help$V3, col = help_kmean$cluster, main="help.csv")




# matriz confusion help


matrix_kmeans_help = confusion_matrix(datos_help$V4, help_kmean$cluster)

evaluation(matrix_kmeans_help, nrow(datos_help))




##############  HCLUST  #######################
#matriz de distancias


help = datos_help
help$V4 = NULL
help = as.matrix(help)
distance_help = dist(help)


#single help


modelo_single_help = hclust(distance_help, method="single")
corte_help1= cutree(modelo_single_help, k = 3)

plot3d(datos_help$V1, datos_help$V2, datos_help$V3, col = corte_help1, main = "single help.csv")




#matriz confusion single help  y evaluacion

matrix_single_help = confusion_matrix(datos_help$V4, corte_help1)

evaluation(matrix_single_help, nrow(datos_help))



#complete help


modelo_complete_help = hclust(distance_help, method="complete")
corte_help2= cutree(modelo_help2, k = 3)

plot3d(datos_help$V1, datos_help$V2, datos_help$V3, col = corte_help2, main = "complete help.csv")



#matriz confusion complete help y evaluacion

matrix_complete_help = confusion_matrix(datos_help$V4, corte_help2)

evaluation(matrix_complete_help, nrow(datos_help))



#average help


modelo_average_help = hclust(distance_help, method="average")
corte_help3= cutree(modelo_average_help, k = 3)

plot3d(datos_help$V1, datos_help$V2, datos_help$V3, col = corte_help3, main = "average help.csv")




#matriz confusion average h

matrix_average_help = confusion_matrix(datos_help$V4, corte_help3)

evaluation(matrix_average_help, nrow(datos_help))




###############################################################
###########################################
#guess.csv

datos_guess = read.csv("guess.csv",header = F)


#analisis exploratorio

summary(datos_guess)

plot(datos_guess, xlab = "X", ylab = "Y",
     main = "guess.csv")


codo_jambu(datos_guess)


#k means para guest.csv

guess_kmean = kmeans(x = datos_guess[, c("V1", "V2")],
                    centers = 5)


plot(x = datos_guess$V1,
     y = datos_guess$V2,
     col = guess_kmean$cluster)



points(x = guess_kmean$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 2)







##############  HCLUST  #######################

#matriz de distancias

guess = datos_guess
guess = as.matrix(guess)
distance_guess = dist(guess)



#modelo single para guest.csv

modelo_single_guess = hclust(modelo_single_guess, method="single")
corte_guess1= cutree(modelo_guess1, k = 5)


plot(x = guess[,1], y = guess[,2], main = paste(c("Single", "guess.csv") , collapse = " "),
     xlab = "x", ylab = "y",
     col = corte_guess1)







#modelo Complete

modelo_complete_guess = hclust(distance_guess, method="complete")
corte_guess2= cutree(modelo_complete_guess, k = 5)

plot(x = guess[,1], y = guess[,2], main = paste(c("complete", "guess.csv") , collapse = " "),
     xlab = "x", ylab = "y",
     col = corte_guess2)




#modelo average

modelo_average_guess = hclust(distance_guess, method="average")
corte_guess3= cutree(modelo_average_guess, k = 5)

plot(x = guess[,1], y = guess[,2], main = paste(c("average", "guess.csv") , collapse = " "),
     xlab = "x", ylab = "y",
     col = corte_guess3)








