##################################################################################
# Trabajo 2
# Aprendizaje Automático
# Grado en Ingeniería Informática
# Granada, 2 de Mayo de 2015.
##################################################################################
# Datos del estudiante:
# Nombre: Fernández Bosch, Pedro
# NIF: 76422233-H
##################################################################################
# Ejecutar el script en R: source("ruta/fichero.R")

##################################################################################
## EJERCICIO 1
##################################################################################

# Cargar las librerias:
library(ISLR)
library(MASS)
library(class)
library(boot)

# Cargar Weekly:
data(Weekly)
attach(Weekly)

## EJERCICIO 1.1

# Información de Weekly:
?Weekly

readline(prompt="Presione [enter] para continuar")

psummary<-summary(Weekly)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# Pares de gráficos de dispersión por variables de Weekly:
pairs(Weekly)

readline(prompt="Presione [enter] para continuar")

# Matriz de dispersión por variables de Weekly:
pcor<-cor(Weekly[,-9])
print(pcor)

readline(prompt="Presione [enter] para continuar")

# Gráfica de variable predictora Volume
plot(Volume)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.2

# Ajuste de un modelo de regresión logística para predecir Direction usando las variables Volume y Lag1 - Lag5 como predictores.
glm.fit=glm(Direction~Volume+Lag1+Lag2+Lag3+Lag4+Lag5 ,data=Weekly ,family =binomial )
psummary<-summary (glm.fit )
print(psummary)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.3

# Calculo de la matriz de confusión y el porcentaje total de predicciones correctas

glm.pred=rep ("Down", 1089)
glm.probs =predict (glm.fit ,type ="response")
glm.pred[glm.probs >.5]="Up"
ptable<-table(glm.pred,Direction)
print(ptable)

readline(prompt="Presione [enter] para continuar")

# Fracción general de observaciones correctas:
pmean<-(557+54)/1089
print(pmean)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(glm.pred==Direction)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.4

# Ajuste de un modelo de regresión logística a los datos entre 1990 y 2008 usando Lag2 como el único predictor y 
# calculo de la matriz de confusión y la fracción global de predicciones correctas para el periodo 2009 y 2010:
train=(Year<=2008)
train.X=Weekly[train,1:8]
train.Y=Weekly[train,9]
test.X=Weekly[!train,1:8]
test.Y=Weekly[!train,9]

glm.fit=glm(Direction~Lag2, data = Weekly[train,], family = binomial)
psummary<-summary(glm.fit)
print(psummary)

readline(prompt="Presione [enter] para continuar")

glm.probs=predict(glm.fit, Weekly[!train,], type = "response")
glm.pred=rep("Down",length(test.Y))
glm.pred[glm.probs>0.5]="Up"
ptable<-table(glm.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(glm.pred==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.5

# Repetir el apartado anterior (1.4) usando LDA, QDA y KNN con K=1

# Método LDA:

lda.fit =lda(Direction~Lag2 ,data=Weekly ,subset =train)
pldafit<-lda.fit
print(pldafit)

readline(prompt="Presione [enter] para continuar")

lda.pred=predict(lda.fit, Weekly[!train,])
lda.class=lda.pred$class
ptable<-table(lda.class, test.Y)
print(ptable)

pmean<-readline(prompt="Presione [enter] para continuar")

pmean<-mean(lda.class==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método  QDA:

qda.fit =qda(Direction~Lag2 ,data=Weekly ,subset =train)
pqdafit<-qda.fit
print(pqdafit)

readline(prompt="Presione [enter] para continuar")

qda.pred=predict(qda.fit, Weekly[!train,])
qda.class=qda.pred$class
ptable<-table(qda.class, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

# Método KNN con K=1:

train.Lag2.X = as.matrix(Lag2[train])
test.Lag2.X = as.matrix(Lag2[!train])

set.seed(1)
knn.pred=knn(train.Lag2.X,test.Lag2.X,train.Y,k=1)
ptable<-table(knn.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(knn.pred==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.6

# No es posible utilizar Today como predictor:

Today.Up=Today > 0
ptodayup<-Direction[Today.Up]
print(ptodayup)

readline(prompt="Presione [enter] para continuar")

Today.Down=Today < 0
ptodaydown<-Direction[Today.Down]
print(ptodaydown)

readline(prompt="Presione [enter] para continuar")

# Utilizando Lag4, Lag5 y Volume como predictores:

# Método RLG:
glm.fit=glm(Direction~Lag4+Lag5+Volume, data = Weekly[train,], family = binomial)
glm.probs=predict(glm.fit, Weekly[!train,], type = "response")
glm.pred=rep("Down",length(test.Y))
glm.pred[glm.probs>0.5]="Up"
ptable<-table(glm.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(glm.pred==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método LDA:

lda.fit =lda(Direction~Lag4+Lag5+Volume ,data=Weekly ,subset =train)
lda.pred=predict(lda.fit, Weekly[!train,])
lda.class=lda.pred$class
ptable<-table(lda.class, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(lda.class==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método QDA:

qda.fit =qda(Direction~Lag4+Lag5+Volume,data=Weekly ,subset =train)
qda.pred=predict(qda.fit, Weekly[!train,])
qda.class=qda.pred$class
ptable<-table(qda.class, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(qda.class==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método KNN:

train.L4L5V.X=cbind(Lag4 ,Lag5, Volume)[train ,]
test.L4L5V.X=cbind(Lag4 ,Lag5, Volume)[!train ,]

set.seed(1)
knn.pred=knn(train.L4L5V.X,test.L4L5V.X,train.Y,k=1)
ptable<-table(knn.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(knn.pred==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

knn.pred=knn(train.L4L5V.X,test.L4L5V.X,train.Y,k=10)
ptable<-table(knn.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(knn.pred==test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 1.7

# Repetir el punto anterior usando un modelo de ajuste de validación cruzada con 10 particiones:

nr<-dim(Weekly)[1]
random.weekly<-Weekly[sample.int(nr),]

pdim<-dim(random.weekly)
print(pdim)

readline(prompt="Presione [enter] para continuar")

# Método RLG:

set.seed (17)
cv.error= rep (0 ,10)
for (i in 1:10) {
    glm.fit=glm(Direction~poly(Lag4+Lag5+Volume ,i),data=Weekly, family=binomial)
    cv.error[i]=cv.glm (Weekly ,glm.fit ,K=10) $delta[1]
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método LDA:

cv.error=rep (0,10)
a=1
b=108

for (i in 1:10){
    test= random.weekly [a:b,]
    train= random.weekly [-(a:b),]   
    
    lda.fit =lda(Direction~Lag4+Lag5+Volume ,data=train)
    lda.pred=predict(lda.fit, test)
    lda.class=lda.pred$class    
         
    cv.error[i]= mean(lda.class!= test[,9])
         
    a=a+108
         
    if(i < 10){ 
        b=b+108 
    } else{ 
        b=b+117
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método QDA:

cv.error=rep (0,10)
a=1
b=108
 
for (i in 1:10){
    test= random.weekly [a:b,]
    train= random.weekly [-(a:b),]
     
    qda.fit =qda(Direction~Lag4+Lag5+Volume ,data=train)
    qda.pred=predict(qda.fit, test)
    qda.class=qda.pred$class    
         
    mean(qda.class!= test[,9])
    cv.error[i]= mean(qda.class!= test[,9])
       
    a=a+108
         
    if(i < 10){ 
        b=b+108 
    } else{ 
        b=b+117
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método KNN:

set.seed(1)
cv.error=rep (0,10)
a=1
b=108

for (i in 1:10){
    test= random.weekly[a:b,]
    test.X= test[,c(5,6,7)]
    test.Y= test[,c(9)]
    train= random.weekly[-(a:b),]
    train.X= train[,c(5,6,7)]
    train.Y= train[,c(9)] 
         
    knn.pred=knn(train.X,test.X, train.Y,k=10)
    cv.error[i]= mean(knn.pred!= test.Y)   
       
    a=a+108
         
    if(i < 10){ 
        b=b+108 
    } else{ 
        b=b+117
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
## EJERCICIO 2
##################################################################################

# Cargar las librerias:
library (MASS)
library (ISLR)

# Cargar Auto
data(Auto)
attach(Auto)

## EJERCICIO 2.a

# Creación de una variable binaria llamada mpg01:

Auto$mpg01=0
Auto$mpg01[Auto$mpg>median(Auto$mpg)]=1
pnames<-names(Auto)
print(pnames)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.b

# Pares de gráficos de dispersión por variables de Weekly:
pairs(Auto)

readline(prompt="Presione [enter] para continuar")

ptable<-table(Auto$mpg01)
print(ptable)

readline(prompt="Presione [enter] para continuar")

# Representación gráfica mediante la función boxplots():

# cylinder~mpg01:

boxplot(cylinders ~ mpg01, data = Auto, xlab="mpg01",ylab ="cylinders")

readline(prompt="Presione [enter] para continuar")

# displacement~mpg01:
  
boxplot(displacement ~ mpg01, data = Auto, xlab="mpg01",ylab ="displacement")

readline(prompt="Presione [enter] para continuar")

# horsepower~mpg01:

readline(prompt="Presione [enter] para continuar")

boxplot(weight ~ mpg01, data = Auto, xlab="mpg01",ylab ="horsepower")

# weight:~mpg01:

readline(prompt="Presione [enter] para continuar")

boxplot(weight ~ mpg01, data = Auto, xlab="mpg01",ylab ="weight")

# acceleration~mpg01:

readline(prompt="Presione [enter] para continuar")

boxplot(acceleration ~ mpg01, data = Auto, xlab="mpg01",ylab ="acceleration")

# year~mpg01:

readline(prompt="Presione [enter] para continuar")

boxplot(year ~ mpg01, data = Auto, xlab="mpg01",ylab ="year")

# origin~mpg01:

readline(prompt="Presione [enter] para continuar")

boxplot(origin ~ mpg01, data = Auto, xlab="mpg01",ylab ="origin")

## EJERCICIO 2.c

# Definir un conjunto de validación dividiendo los datos en un conjunto de entrenamiento (70%) y otro de test (30%).

train=1:275
test=276:392
pnames<-names(Auto)
print(pnames)

readline(prompt="Presione [enter] para continuar")

train.X=Auto[train,c(-1,-6,-9,-10)]
pnames<-names(train.X)
print(pnames)

readline(prompt="Presione [enter] para continuar")

train.Y=Auto[train,c(10)]
test.X=Auto[test,c(-1,-6,-9,-10)]
test.Y=Auto[test,c(10)]
ptable<-table(test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

# Modelo LDA:

lda.fit =lda(mpg01~cylinders+ displacement+horsepower+weight+year+origin ,data= Auto,subset =train)
lda.pred=predict(lda.fit, test.X)
lda.class=lda.pred$class
ptable<-table(lda.class, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(lda.class!=test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Modelo QDA:

qda.fit =qda(mpg01~cylinders+ displacement+horsepower+weight+year+origin ,data= Auto,subset =train)
qda.pred=predict(qda.fit, test.X)
qda.class=qda.pred$class
ptable<-table(qda.class, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(qda.class!=test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Modelo Regresión Logística:

glm.fit=glm(mpg01~cylinders+displacement+horsepower+weight+year+origin, data = Auto[train,], family = binomial)
glm.probs=predict(glm.fit, test.X, type = "response")
glm.pred=rep(0,length(test.Y))
glm.pred[glm.probs>0.5]=1
ptable<-table(glm.pred, test.Y) 
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(glm.pred!=test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Modelo KNN:

set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=22)
ptable<-table(knn.pred, test.Y)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(knn.pred!=test.Y)
print(pmean)

readline(prompt="Presione [enter] para continuar")

## EJERCICIO 2.d

# Repetir los experimentos a-d del punto anterior pero usando Validación Cruzada de 5-particiones

set.seed (1)

nr<-dim(Auto)[1]
random.auto<-Auto[sample.int(nr),]
pdim<-dim(random.auto)
print(pdim)

readline(prompt="Presione [enter] para continuar")

# Método LDA:

cv.error=rep (0,5)
a=1
b=78

for (i in 1:5){
    test= random.auto [a:b,]
    train= random.auto [-(a:b),]
     
    lda.fit =lda(mpg01~cylinders+displacement+horsepower+weight+year+origin ,data=train)
    lda.pred=predict(lda.fit, test)
    lda.class=lda.pred$class    
     
    cv.error[i]= mean(lda.class!= test[,10])
         
    a=a+78
         
    if(i < 5){ 
        b=b+78 
    } else{ 
        b=b+80
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método QDA:

cv.error=rep (0,5)
a=1
b=78

for (i in 1:5){
    test= random.auto [a:b,]
    train= random.auto [-(a:b),]
         
    qda.fit =qda(mpg01~cylinders+displacement+horsepower+weight+year+origin ,data=train)
    qda.pred=predict(qda.fit, test)
    qda.class=qda.pred$class    
         
    mean(qda.class!= test[,10])
    cv.error[i]= mean(qda.class!= test[,10])
         
    a=a+78
         
    if(i < 5){ 
        b=b+78 
    } else{ 
        b=b+80
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método RGL:

set.seed (17)
cv.error= rep (0 ,5)

for (i in 1:5) {
    glm.fit=glm(mpg01~poly(cylinders+displacement+horsepower+weight+year+origin,i),data=Auto)
    cv.error[i]=cv.glm (Auto ,glm.fit ,K=5) $delta[1]
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Método KNN:

set.seed(1)
cv.error=rep (0,5)
a=1
b=78
 
for (i in 1:5){
    test= random.auto[a:b,]
    test.X= test[,c(-1,-6,-9,-10)]
    test.Y= test[,c(10)]
    train= random.auto[-(a:b),]
    train.X= train[,c(-1,-6,-9,-10)]
    train.Y= train[,c(10)] 
    
    knn.pred=knn(train.X,test.X, train.Y,k=22)
    cv.error[i]= mean(knn.pred!= test.Y)   
    
    a=a+78
    
    if(i < 5){ 
        b=b+78 
    } else{ 
        b=b+80
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
## EJERCICIO 3
##################################################################################

# Cargar las librerias:

library (ISLR)

# Cargar Auto:

data(Boston)
attach(Boston)

pnames<-names(Boston)
print(pnames)

readline(prompt="Presione [enter] para continuar")

# Creación de una variable binaria llamada crim01:

Boston$crim01=0
Boston$crim01[crim>median(crim)]=1

# Pares de gráficos de dispersión por variables de Weekly:

pairs(Boston)

readline(prompt="Presione [enter] para continuar")

pnames<-names(Boston)
print(pnames)

readline(prompt="Presione [enter] para continuar")

# Ajuste del modelo de Regresión Logística usando crim01 como variable respuesta y el resto de variables como predictores:

glm.fit=glm(crim01~.,data=Boston)
psummary<-summary (glm.fit)
print(psummary)

readline(prompt="Presione [enter] para continuar")

# Validación Cruzada - Método RGL:

set.seed (17)
cv.error= rep (0 ,5)
for (i in 1:5) {
    glm.fit=glm(crim01~poly(nox+age+rad+medv,i),data=Boston)
    cv.error[i]=cv.glm (Boston ,glm.fit ,K=5) $delta[1]
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Calculo del modelo - Método RGL:

glm.fit=glm(crim01~nox+age+rad+medv, data=Boston)
glm.probs=predict(glm.fit, Boston, type = "response")
glm.pred=rep(0,length(Boston$crim01))
glm.pred[glm.probs>0.5]=1
ptable<-table(glm.pred,Boston$crim01)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(glm.pred== Boston$crim01)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Validación Cruzada - Método LDA:

nr<-dim(Boston)[1]
random.boston<-Boston[sample.int(nr),]
pdim<-dim(random.boston)
print(pdim)

readline(prompt="Presione [enter] para continuar")

cv.error=rep (0,5)
a=1
b=101
 
for (i in 1:5){
    test= random.boston [a:b,]
    train= random.boston [-(a:b),]
         
    lda.fit =lda(crim01~nox+age+rad+medv ,data=train)
    lda.pred=predict(lda.fit, test)
    lda.class=lda.pred$class    
    cv.error[i]= mean(lda.class!= test[,15])
         
    a=a+101
    
    if(i < 5){ 
        b=b+101 
    } else{ 
        b=b+102
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Calculo del modelo - Método LDA:

lda.fit =lda(crim01~nox+age+rad+medv ,data= Boston)
lda.pred=predict(lda.fit, Boston)
lda.class=lda.pred$class 
ptable<-table(lda.class, Boston$crim01)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(lda.class==Boston$crim01)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Validación Cruzada - Método QDA:

cv.error=rep (0,5)
a=1
b=101
 
for (i in 1:5){
    test= random.boston [a:b,]
    train= random.boston [-(a:b),]
         
    qda.fit =qda(crim01~nox+age+rad+medv ,data=train)
    qda.pred=predict(qda.fit, test)
    qda.class=qda.pred$class         
    
    cv.error[i]= mean(qda.class!= test[,15])
     
    a=a+101
         
    if(i < 5){ 
        b=b+101 
    } else{ 
        b=b+102
    }
}
pcv<-cv.error
print(pcv)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(cv.error)
print(pmean)

readline(prompt="Presione [enter] para continuar")

# Calculo del modelo - Método QDA:

qda.fit =qda(crim01~nox+age+rad+medv ,data=Boston)
qda.pred=predict(qda.fit, Boston)
qda.class=qda.pred$class 
ptable<-table(qda.class, Boston$crim01)
print(ptable)

readline(prompt="Presione [enter] para continuar")

pmean<-mean(qda.class == Boston$crim01)
print(pmean)

readline(prompt="Presione [enter] para continuar")

##################################################################################
print("FIN DEL PROGRAMA")
##################################################################################

##################################################################################
## FIN DEL PROGRAMA
##################################################################################