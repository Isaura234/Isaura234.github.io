setwd("C:/Users/Toshiba/OneDrive/UCR/2023/Multivariado/Trabajo final")
load("ECE.Rdata")

library(ROCR)
library(caret)
library(cluster)
library(labelled)
library(rpart)
library(rattle)
library(modeest)
library(adabag)
library(DT)
library(randomForest)
library(dplyr)
library(RColorBrewer)
library(colorRamps)
library(extrafont)
library(gridExtra)

base17=base17[,-c(11)]
base22=base22[,-c(11)]
baseQ17=baseQ17[,-c(11)]
baseQ22=baseQ22[,-c(11)]
base22=base22[base22$Pais_nacimiento!=99,]

#Descriptivos -------
table(base17$condicion_actividad, base17$Sexo)
table(base17$condicion_actividad, base17$Estado_conyugal) 
table(base17$condicion_actividad, base17$Educacion_asiste)
table(base17$condicion_actividad, base17$Educacion_nivel_grado)
table(base17$condicion_actividad, base17$EducacionNoregular_asiste)
table(base17$condicion_actividad, base17$Idioma)
table(base17$condicion_actividad, base17$Region)
table(base17$condicion_actividad, base17$Zona)
table(base17$condicion_actividad, base17$Pais_nacimiento)
table(base17$condicion_actividad, base17$Permanencia_pais)

table(base22$condicion_actividad, base22$Sexo)
table(base22$condicion_actividad, base22$Estado_conyugal) 
table(base22$condicion_actividad, base22$Educacion_asiste)
table(base22$condicion_actividad, base22$Educacion_nivel_grado)
table(base22$condicion_actividad, base22$EducacionNoregular_asiste)
table(base22$condicion_actividad, base22$Idioma)
table(base22$condicion_actividad, base22$Region)
table(base22$condicion_actividad, base22$Zona)
table(base22$condicion_actividad, base22$Pais_nacimiento)
table(base22$condicion_actividad, base22$Permanencia_pais)

#graficos
t1=table(base17$condicion_actividad, base17$Sexo)
t2=table(base17$condicion_actividad, base17$Estado_conyugal)
t3=table(base17$condicion_actividad, base17$Educacion_nivel_grado)
t4=table(base22$condicion_actividad, base22$Sexo)
t5=table(base22$condicion_actividad, base22$Estado_conyugal)
t6=table(base22$condicion_actividad, base22$Educacion_nivel_grado)

#para 2017
names=c("Categoria", "Caracteristica", "Cantidad", "Condicion", "Anio")
categ=c(rep("Sexo", each=4), rep(c("Estado conyugal", "Nivel de educacion"), each=6), rep("Sexo", each=4), rep(c("Estado conyugal", "Nivel de educacion"), each=6))
carac=rep(c("Hombre", "Mujer", "Casado", "Soltero", "Union libre", "Ninguno", "Secundaria o menos", "Estudios superior"), each=2,2)
cant=round(c(t1[1,1]/rowSums(t1)[1], t1[2,1]/rowSums(t1)[2], t1[1,2]/rowSums(t1)[1], t1[2,2]/rowSums(t1)[2], 
             t2[1,1]/rowSums(t2)[1], t2[2,1]/rowSums(t2)[2], t2[1,2]/rowSums(t2)[1], t2[2,2]/rowSums(t2)[2],
             t2[1,3]/rowSums(t2)[1], t2[2,3]/rowSums(t2)[2], t3[1,1]/rowSums(t3)[1], t3[2,1]/rowSums(t3)[2], 
             t3[1,2]/rowSums(t3)[1], t3[2,2]/rowSums(t3)[2], t3[1,3]/rowSums(t3)[1], t3[2,3]/rowSums(t3)[2], 
             t4[1,1]/rowSums(t4)[1], t4[2,1]/rowSums(t4)[2], t4[1,2]/rowSums(t4)[1], t4[2,2]/rowSums(t4)[2], 
             t5[1,1]/rowSums(t5)[1], t5[2,1]/rowSums(t5)[2], t5[1,2]/rowSums(t5)[1], t5[2,2]/rowSums(t5)[2],
             t5[1,3]/rowSums(t5)[1], t5[2,3]/rowSums(t5)[2], t6[1,1]/rowSums(t6)[1], t6[2,1]/rowSums(t6)[2], 
             t6[1,2]/rowSums(t6)[1], t6[2,2]/rowSums(t6)[2], t6[1,3]/rowSums(t6)[1], t6[2,3]/rowSums(t6)[2]),3)*100
cond=rep(c("Desempleado", "Ocupado"), 16)
anio=c(rep(2017, 16), rep(2022, 16))

tabla1=data.frame(categ, carac, cant, cond, anio)
names(tabla1)=names
tabla1$Categoria=factor(tabla1$Categoria)
tabla1$Caracteristica=factor(tabla1$Caracteristica)
tabla1$Condicion=factor(tabla1$Condicion)
levels(tabla1$Caracteristica)=c("Casado", "Estudios \nsuperior", "Hombre", "Mujer", "Ninguno", "Secundaria \no menos", "Soltero", "Union \nlibre")
tabla1$Anio=factor(tabla1$Anio)

g1=ggplot(tabla1, aes(fill=Condicion,y=Cantidad, x=reorder(Caracteristica, -Cantidad))) + 
  facet_grid(Anio~factor(tabla1$Categoria, levels = c("Sexo", "Estado conyugal", "Nivel de educacion")),
             scales = "free", space ="free", switch = "x")+
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual("Condicion", values=c("#82E0AA", "#7FB3D5"))+
  labs(x="", y="")+
  scale_y_continuous(labels = NULL)+
  theme(strip.text = element_text(size= 13), strip.placement="outside",axis.ticks.y=element_blank(),
        axis.line.x=element_line(colour = "black"), 
        panel.background = element_blank(), 
        strip.background = element_rect(colour="white", fill="white"), axis.text = element_text(size = 13))+
  geom_text(aes(label=paste(Cantidad, "%")), position=position_dodge(width=0.9), vjust=-0.25)

#Separacion de la base----------
RNGkind(sample.kind="Rounding")
set.seed(10)
n=nrow(base17)
m=sample(1:n, round(0.8*n))
train17=base17[m,]
test17=base17[-m,]

set.seed(10)
n=nrow(base22)
m=sample(1:n, round(0.8*n))
train22=base22[m,]
test22=base22[-m,]

#Modelo logistico ---------
mod1=glm(factor(condicion_actividad)~., data=train17, family=binomial(link = "logit"))
pred=predict(mod1, test17, type="response")>0.5
table(test17$condicion_actividad, pred)
#no logra clasificar los desempleados

mod2=glm(factor(condicion_actividad)~., data=train22, family=binomial(link = "logit"))
pred2=predict(mod2, test22, type="response")>0.5
table(test22$condicion_actividad, pred2)
#no logra clasificar los desempleados 

#ajustando el peso para la base 2017
table(train17$condicion_actividad)
pond1=1/table(train17$condicion_actividad)
ponderador1=ifelse(train17$condicion_actividad=="Desempleado", pond1[1], pond1[2])

mod3=glm(factor(condicion_actividad)~., data=train17, family=binomial, weights = ponderador1)
pred3=predict(mod3, test17, type="response")>0.5
table(test17$condicion_actividad, pred3)
#mejora la clasificacion de desempelado

#ajustando el peso para la base 2022
table(train22$condicion_actividad)
pond2=1/table(train22$condicion_actividad)
ponderador2=ifelse(train22$condicion_actividad=="Desempleado", pond2[1], pond2[2])

mod4=glm(factor(condicion_actividad)~., data=train22, family=binomial, weights = ponderador2)
pred4=predict(mod4, test22, type="response")>0.5
table(test22$condicion_actividad, pred4)
#mejora la clasificacion de desempelado

#seleccion de variables con modelo logistico 2017 --------
matriz1=matrix(nrow=4, ncol=ncol(train17)-1)
row.names(matriz1)=c("e", "FP", "FN", "AUC")
vars=names(train17[,-12])
colnames(matriz1)=paste("Sin", vars)
for(j in 1:length(vars)){
  mod1=glm(factor(condicion_actividad)~., data=train17[,-j], family=binomial, weights = ponderador1)
  pred1=predict(mod1, test17[,-j], type="response")
  clas1=ifelse(pred1>0.5, "Ocupado", "Desempleado")
  confu=table(test17$condicion_actividad, clas1)
  e=(1-sum(diag(confu))/sum(confu))*100
  falsos=(1-diag(confu)/apply(confu, 1, sum))*100
  FP=falsos[1]
  FN=falsos[2]
  predict1=prediction(as.numeric(pred1), test17$condicion_actividad)
  auc=attributes(performance(predict1, "auc"))$y.values[[1]]*100
  matriz1[,j]=c(e, FP, FN, auc)
  print(j)
}
matriz1

#seleccion de variables con base 2022 -------
matriz2=matrix(nrow=4, ncol=ncol(train22)-1)
row.names(matriz2)=c("e", "FP", "FN", "AUC")
vars=names(train22[,-12])
colnames(matriz2)=paste("Sin", vars)
for(j in 1:length(vars)){
  mod1=glm(factor(condicion_actividad)~., data=train22[,-j], family=binomial, weights = ponderador2)
  pred1=predict(mod1, test22[,-j], type="response")
  clas1=ifelse(pred1>0.5, "Ocupado", "Desempleado")
  confu=table(test22$condicion_actividad, clas1)
  e=(1-sum(diag(confu))/sum(confu))*100
  falsos=(1-diag(confu)/apply(confu, 1, sum))*100
  FP=falsos[1]
  FN=falsos[2]
  predict1=prediction(as.numeric(pred1), test22$condicion_actividad)
  auc=attributes(performance(predict1, "auc"))$y.values[[1]]*100
  matriz2[,j]=c(e, FP, FN, auc)
  print(j)
}
matriz2

#hacer bagging con modelo 2017 --
m=5
n=nrow(train17)
q=ncol(train17)-1
tabla1=matrix(nrow=nrow(test17), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T)  
  train2=train17[f, ]
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=glm(factor(condicion_actividad)~., data=train2, family=binomial, weights = ponderador)
  tabla1[,i]=predict(mod, newdata=test17, "response")>0.5
  print(i)
}
pred5=apply(tabla1,1,mlv)
table(test17$condicion_actividad, pred5)

#hacer bagging con base 2022
m=5
n=nrow(train22)
tabla2=matrix(nrow=nrow(test22), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T)  
  train2=train22[f, ]
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=glm(factor(condicion_actividad)~., data=train2, family=binomial, weights = ponderador)
  tabla2[,i]=predict(mod, newdata=test22, "response")>0.5
  print(i)
}
pred6=apply(tabla2,1,mlv)
table(test22$condicion_actividad, pred6)

#modelo arbol de desicion 2017 -------
mod7=rpart(train17$condicion_actividad~.,weights = ponderador1, method="class", data=train17, control = rpart.control(cp = 0.0025, maxdepth = 5))
fancyRpartPlot(mod7, palettes=c("Greens", "Reds"), sub = "", cex=0.5)
library(rpart.plot)
rpart.plot(mod7, type = 1, extra = 104)
pred7=predict(mod7, newdata=test17, type="class")
table(test17$condicion_actividad, pred7)

#modelo arbol de desicion 2022 -------
mod8=rpart(train22$condicion_actividad~.,weights = ponderador2, method="class", data=train22, control = rpart.control(cp = 0.0025, maxdepth = 5))
fancyRpartPlot(mod8, palettes=c("Greens", "Reds"), cex=0.5)
rpart.plot(mod8, type = 1, extra = 104)
pred8=predict(mod8, newdata=test22, type="class")
table(test22$condicion_actividad, pred8)

#Bagging a pie con arboles 2017 --------
m=5 
n=nrow(train17)
q=ncol(train17)-1  
tabla3=matrix(nrow=nrow(test17), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T)  
  train2=train17[f, ] 
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador, control = rpart.control(cp = 0.0025, maxdepth = 5))  
  tabla3[,i]=predict(mod, newdata=test17, "class") 
  print(i)
}
pred9=apply(tabla3,1,mlv) 
table(test17$condicion_actividad, pred9)

#Bagging a pie con arboles 2022 -------
m=5 
n=nrow(train22)
q=ncol(train22)-1  
tabla4=matrix(nrow=nrow(test22), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T)  
  train2=train22[f, ] 
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador, control = rpart.control(cp = 0.0025, maxdepth = 5))  
  tabla4[,i]=predict(mod, newdata=test22, "class") 
  print(i)
}
pred10=apply(tabla4,1,mlv) 
table(test22$condicion_actividad, pred10)

#bosques aleatorios 2017 ------
n=nrow(train17)
q=ncol(train17)-1
m=5
tabla5=matrix(nrow=nrow(test17), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T) 
  v=sample(1:11, 8)
  train2=train17[f, c(12,v)]
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
  tabla5[,i]=predict(mod, newdata=test17, type="class")
  print(i)
}
pred11=apply(tabla5, 1, mlv)
table(test17$condicion_actividad, pred11)

#bosques aleatorios 2022 --------
n=nrow(train22)
q=ncol(train22)-1
m=5
tabla6=matrix(nrow=nrow(test22), ncol=m)
for (i in 1:m){
  f=sample(1:n, n, replace=T) 
  v=sample(1:11, 9)
  train2=train22[f, c(12,v)]
  pond=1/table(train2$condicion_actividad)
  ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
  mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
  tabla6[,i]=predict(mod, newdata=test22, type="class")
  print(i)
}
pred12=apply(tabla6, 1, mlv)
table(test22$condicion_actividad, pred12)

#evaluacion de los modelos ------
eval=function(y, pred){
  confu=table(y, pred)
  e=1-sum(diag(confu))/sum(confu)
  falsos=1-diag(confu)/apply(confu,1,sum)
  error=c(e,falsos)*100
  predict1=prediction(as.numeric(pred), y)
  auc=attributes(performance(predict1, "auc"))$y.values[[1]]*100
  des=performance(predict1, "tpr", "fpr")
  ks=max(attributes(des)$y.values[[1]]*100-attributes(des)$x.values[[1]]*100)
  indicadores=c(error, auc, ks)
  names(indicadores)=c("e", "FP", "FN", "AUC", "KS")
  return(indicadores=indicadores)
}

#calibracion del logistico -------
p=seq(from=0.3, to=0.8, by=0.05)
res1=matrix(nrow=5, ncol=length(p))
res2=matrix(nrow=5, ncol=length(p))
for(i in 1:length(p)){
  mod=glm(factor(condicion_actividad)~., data=train17, family=binomial, weights = ponderador1)
  pred=predict(mod, test17, type="response")>p[i]
  f1=eval(test17$condicion_actividad, as.numeric(pred))
  res1[1:5,i]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  
  mod.c=glm(factor(condicion_actividad)~., data=train22, family=binomial, weights = ponderador2)
  pred.c=predict(mod.c, test22, type="response")>p[i]
  f2=eval(test22$condicion_actividad, as.numeric(pred.c))
  res2[1:5,i]=as.numeric(c(f2[1],f2[2], f2[3], f2[4], f2[5]))
  print(i)
}

df1=data.frame(prob=rep(p, 10), v=c(res1[1,], res1[2,], res1[3,], res1[4,], res1[5,], 
                                    res2[1,], res2[2,], res2[3,], res2[4,], res2[5,]), 
               ind=rep(c("Error","FP", "FN", "AUC", "KS"),each=11, 2), 
               anio=c(rep(2017,55), rep(2022,55)))
df1$cat=paste(df1$anio, df1$ind)
df1$ind=factor(df1$ind)
df1$anio=factor(df1$anio)
df1$cat=factor(df1$cat)
facet_names <- c(
  `2017` = "Año 2017",
  `2022` = "Año 2022"
)

g2=ggplot(df1, aes(prob, v, group=cat)) +
  geom_line(aes(color=ind), size=0.8)+
  geom_point(aes(color=ind))+
  geom_vline(xintercept = 0.5,
             linetype = 2,
             color = 2)+
  facet_grid(~anio, scales = "free", labeller=as_labeller(facet_names))+
  labs(title = "",x = "Probabilidad", y = "Porcentaje", color=c('Indicador'))+
  theme(axis.line.y=element_line(colour = "black"),
        axis.line.x=element_line(colour = "black"), panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), 
        legend.key.width=unit(1,"cm"), panel.spacing = unit(1, "cm"),
        axis.text = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.2)),
        axis.title.x = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.2)),
        strip.text.y = element_text(size = rel(1.2)))

#calibrando CP con modelo de arboles --------
cp=seq(0.0001, 0.009, by=0.0002)
res3=matrix(nrow=5, ncol=length(cp))
res4=matrix(nrow=5, ncol=length(cp))
for(i in 1:length(cp)){
  mod.c=rpart(train17$condicion_actividad~.,weights = ponderador1, method="class", data=train17, control = rpart.control(cp = cp[i], maxdepth = 7))
  pred.c=predict(mod.c, newdata=test17, type="class")
  f1=eval(test17$condicion_actividad, as.numeric(pred.c))
  res3[1:5,i]=as.numeric(c(f1[1],f1[2],f1[3], f1[4], f1[5]))
  
  mod.c2=rpart(train22$condicion_actividad~.,weights = ponderador2, method="class", data=train22, control = rpart.control(cp = cp[i], maxdepth = 5))
  pred.c2=predict(mod.c2, newdata=test22, type="class")
  f2=eval(test22$condicion_actividad, as.numeric(pred.c2))
  res4[1:5,i]=as.numeric(c(f2[1],f2[2], f2[3], f2[4], f2[5]))
  print(i)
}

df2=data.frame(cp=rep(cp, 6), v=c(res3[1,], res3[4,], res3[5,], res4[1,], res4[4,], res4[5,]), ind=rep(c("Error", "AUC", "KS"),each=45, 2), 
               anio=c(rep(2017,135), rep(2022,135)))
df2$cat=paste(df2$anio, df2$ind)
df2$ind=factor(df2$ind)
df2$anio=factor(df2$anio)
df2$cat=factor(df2$cat)
facet_names <- c(
  `2017` = "Año 2017",
  `2022` = "Año 2022"
)

g3=ggplot(df2, aes(cp, v, group=cat)) +
  geom_line(aes(color=ind), size=0.8)+
  geom_point(aes(color=ind))+
  geom_vline(xintercept = 0.0025,
             linetype = 2,
             color = 2)+
  facet_grid(~anio, scales = "free", labeller=as_labeller(facet_names))+
  labs(title = "",x = "CP", y = "Porcentaje", color=c('Indicador'))+
  theme(axis.line.y=element_line(colour = "black"),
        axis.line.x=element_line(colour = "black"), panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), 
        legend.key.width=unit(1,"cm"), panel.spacing = unit(1, "cm"),
        axis.text = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.2)),
        axis.title.x = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.2)),
        strip.text.y = element_text(size = rel(1.2)))

#Calibracion de profundidad del arbol -------
prof=c(1:8)
res5=matrix(nrow=5, ncol=length(prof))
res6=matrix(nrow=5, ncol=length(prof))
for(i in 1:length(prof)){
  mod.c=rpart(train17$condicion_actividad~.,weights = ponderador1, method="class", data=train17, control = rpart.control(cp = 0.0025, maxdepth = prof[i]))
  pred.c=predict(mod.c, newdata=test17, type="class")
  f1=eval(test17$condicion_actividad, as.numeric(pred.c))
  res5[1:5,i]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  
  mod.c2=rpart(train22$condicion_actividad~.,weights = ponderador2, method="class", data=train22, control = rpart.control(cp = 0.0025, maxdepth = prof[i]))
  pred.c2=predict(mod.c2, newdata=test22, type="class")
  f2=eval(test22$condicion_actividad, as.numeric(pred.c2))
  res6[1:5,i]=as.numeric(c(f2[1], f2[2], f2[3], f2[4], f2[5]))
  print(i)
}

df3=data.frame(profundidad=rep(prof, 6), v=c(res5[1,], res5[4,], res5[5,], res6[1,], res6[4,], res6[5,]), 
               ind=rep(c("Error", "AUC", "KS"),each=8, 2), anio=c(rep(2017,24), rep(2022,24)))
df3$cat=paste(df3$anio, df3$ind)
df3$ind=factor(df3$ind)
df3$anio=factor(df3$anio)
df3$cat=factor(df3$cat)
facet_names <- c(
  `2017` = "Año 2017",
  `2022` = "Año 2022"
)

g4=ggplot(df3, aes(profundidad, v, group=cat)) +
  geom_line(aes(color=ind), size=0.8)+
  geom_point(aes(color=ind))+
  geom_vline(xintercept = 5,
             linetype = 2,
             color = 2)+
  facet_grid(~anio, scales = "free", labeller=as_labeller(facet_names))+
  labs(title = "",x = "Profundidad", y = "Porcentaje", color=c('Indicador'))+
  theme(axis.line.y=element_line(colour = "black"),
        axis.line.x=element_line(colour = "black"), panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), 
        legend.key.width=unit(1,"cm"), panel.spacing = unit(1, "cm"),
        axis.text = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.2)),
        axis.title.x = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.2)),
        strip.text.y = element_text(size = rel(1.2)))

#calibracion randomforest-------
va=c(2:11)
res17=matrix(nrow=5, ncol=10)
res18=matrix(nrow=5, ncol=10)

for (j in 1:length(va)){
  n=nrow(train17)
  q=ncol(train17)-1
  m=5
  tabla5=matrix(nrow=nrow(test17), ncol=m)
  for (i in 1:m){
    f=sample(1:n, n, replace=T) 
    v=sample(1:11, va[j])
    train2=train17[f, c(12,v)]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
    tabla5[,i]=predict(mod, newdata=test17, type="class")
    print(paste(j,i))
  }
  pred.c=apply(tabla5, 1, mlv)
  f1=eval(test17$condicion_actividad, pred.c)
  
  n=nrow(train22)
  q=ncol(train22)-1
  m=5
  tabla6=matrix(nrow=nrow(test22), ncol=m)
  for (l in 1:m){
    f=sample(1:n, n, replace=T) 
    v=sample(1:11, va[j])
    train2=train22[f, c(12,v)]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
    tabla6[,l]=predict(mod, newdata=test22, type="class")
    print(paste(j,l))
  }
  pred.c2=apply(tabla6, 1, mlv)
  f2=eval(test22$condicion_actividad, pred.c2)
  res18[1:5,j]=as.numeric(c(f2[1], f2[2], f2[3], f2[4], f2[5]))
  res17[1:5,j]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  print(j)
}

df5=data.frame(n=rep(2:11, 6), v=c(res17[1,], res17[4,], res17[5,], res18[1,], res18[4,], res18[5,]), ind=rep(c("Error", "AUC", "KS"),each=10, 2), 
               anio=c(rep(2017,30), rep(2022,30)))
df5$cat=paste(df5$anio, df5$ind)
df5$ind=factor(df5$ind)
df5$anio=factor(df5$anio)
df5$cat=factor(df5$cat)
facet_names <- c(
  `2017` = "Año 2017",
  `2022` = "Año 2022"
)

g5=ggplot(df5, aes(n, v, group=cat)) +
  geom_line(aes(color=ind), size=0.8)+
  geom_point(aes(color=ind))+
  geom_vline(data=filter(df5, anio=="2017"), aes(xintercept=8), colour="red", linetype=2)+
  geom_vline(data=filter(df5, anio=="2022"), aes(xintercept=9), colour="red", linetype=2)+
  facet_grid(~anio, scales = "free", labeller=as_labeller(facet_names))+
  labs(title = "",x = "Número de variables", y = "Porcentaje", color=c('Indicador'))+
  theme(axis.line.y=element_line(colour = "black"),
        axis.line.x=element_line(colour = "black"), panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), 
        legend.key.width=unit(1,"cm"), panel.spacing = unit(1, "cm"),
        axis.text = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.2)),
        axis.title.x = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.2)),
        strip.text.y = element_text(size = rel(1.2)))

#validacion cruzada para modelo logistico -------
cortes=createFolds(1:nrow(base17), k=10)
cortes2=createFolds(1:nrow(base22), k=10)
res7=matrix(nrow=5, ncol=10) #almacen logistico 2017
res8=matrix(nrow=5, ncol=10) #almacen logistico 2022
res9=matrix(nrow=5, ncol=10) #almacen bagging 2017
res10=matrix(nrow=5, ncol=10)
for(i in 1:10){
  train3=base17[-cortes[[i]],]
  test3=base17[cortes[[i]],]
  
  train4=base22[-cortes2[[i]],]
  test4=base22[cortes2[[i]],]
  
  pond1=1/table(train3$condicion_actividad)
  ponderador1=ifelse(train3$condicion_actividad=="Desempleado", pond1[1], pond1[2])
  mod.c=glm(factor(condicion_actividad)~., data=train3, family=binomial, weights = ponderador1)
  pred.c=predict(mod.c, test3, type="response")>0.5
  f1=eval(test3$condicion_actividad, as.numeric(pred.c))
  
  pond2=1/table(train4$condicion_actividad)
  ponderador2=ifelse(train4$condicion_actividad=="Desempleado", pond2[1], pond2[2])
  mod.c2=glm(factor(condicion_actividad)~., data=train4, family=binomial, weights = ponderador2)
  pred.c2=predict(mod.c2, test4, type="response")>0.5
  f2=eval(test4$condicion_actividad, as.numeric(pred.c2))
  
  m=5
  n=nrow(train17)
  q=ncol(train17)-1
  tabla1=matrix(nrow=nrow(test3), ncol=m)
  for (j in 1:m){
    f=sample(1:n, n, replace=T)  
    train2=train17[f, ]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=glm(factor(condicion_actividad)~., data=train2, family=binomial, weights = ponderador)
    tabla1[,j]=predict(mod, newdata=test3, "response")>0.5
    print(paste(i,j))
  }
  pred.c3=apply(tabla1,1,mlv)
  f3=eval(test3$condicion_actividad, as.numeric(pred.c3))
  
  m=5
  n=nrow(train22)
  q=ncol(train22)-1
  tabla2=matrix(nrow=nrow(test4), ncol=m)
  for (l in 1:m){
    f=sample(1:n, n, replace=T)  
    train2=train22[f, ]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=glm(factor(condicion_actividad)~., data=train2, family=binomial, weights = ponderador)
    tabla2[,l]=predict(mod, newdata=test4, "response")>0.5
    print(paste(i,l))
  }
  pred.c4=apply(tabla2,1,mlv)
  f4=eval(test4$condicion_actividad, as.numeric(pred.c4))
  
  res7[1:5,i]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  res8[1:5,i]=as.numeric(c(f2[1], f2[2], f2[3], f2[4], f2[5]))
  res9[1:5,i]=as.numeric(c(f3[1], f3[2], f3[3], f3[4], f3[5]))
  res10[1:5,i]=as.numeric(c(f4[1], f4[2], f4[3], f4[4], f4[5]))
  print(i)
}

#validacion cruzada arboles de desicion -------
cortes=createFolds(1:nrow(base17), k=10)
cortes2=createFolds(1:nrow(base22), k=10)
res11=matrix(nrow=5, ncol=10) #almacen arbol 2017
res12=matrix(nrow=5, ncol=10) #almacen arbol 2022
res13=matrix(nrow=5, ncol=10) #almacen bagging 2017
res14=matrix(nrow=5, ncol=10) #almacen bagging 2022
for(i in 1:10){
  train3=base17[-cortes[[i]],]
  test3=base17[cortes[[i]],]
  
  train4=base22[-cortes2[[i]],]
  test4=base22[cortes2[[i]],]
  
  pond1=1/table(train3$condicion_actividad)
  ponderador1=ifelse(train3$condicion_actividad=="Desempleado", pond1[1], pond1[2])
  mod.c=rpart(train3$condicion_actividad~.,weights = ponderador1, method="class", data=train3, control = rpart.control(cp = 0.0025, maxdepth = 5))
  pred.c=predict(mod.c, newdata=test3, type="class")
  f1=eval(test3$condicion_actividad, as.numeric(pred.c))
  
  pond2=1/table(train4$condicion_actividad)
  ponderador2=ifelse(train4$condicion_actividad=="Desempleado", pond2[1], pond2[2])
  mod.c2=rpart(train4$condicion_actividad~.,weights = ponderador2, method="class", data=train4, control = rpart.control(cp = 0.0025, maxdepth = 5))
  pred.c2=predict(mod.c2, newdata=test4, type="class")
  f2=eval(test4$condicion_actividad, as.numeric(pred.c2))
  
  m=5 
  n=nrow(train17)
  q=ncol(train17)-1  
  tabla3=matrix(nrow=nrow(test3), ncol=m)
  for (j in 1:m){
    f=sample(1:n, n, replace=T)  
    train2=train17[f, ] 
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador, control = rpart.control(cp = 0.0025, maxdepth = 5))  
    tabla3[,j]=predict(mod, newdata=test3, "class") 
    print(paste(i, j))
  }
  pred.c3=apply(tabla3,1,mlv) 
  f3=eval(test3$condicion_actividad, as.numeric(pred.c3))
  
  m=5 
  n=nrow(train22)
  q=ncol(train22)-1  
  tabla4=matrix(nrow=nrow(test4), ncol=m)
  for (l in 1:m){
    f=sample(1:n, n, replace=T)  
    train2=train22[f, ] 
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador, control = rpart.control(cp = 0.0025, maxdepth = 5))  
    tabla4[,l]=predict(mod, newdata=test4, "class") 
    print(paste(i,l))
  }
  pred.c4=apply(tabla4,1,mlv) 
  f4=eval(test4$condicion_actividad, as.numeric(pred.c4))
  
  res11[1:5,i]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  res12[1:5,i]=as.numeric(c(f2[1], f2[2], f2[3], f2[4], f2[5]))
  res13[1:5,i]=as.numeric(c(f3[1], f3[2], f3[3], f3[4], f3[5]))
  res14[1:5,i]=as.numeric(c(f4[1], f4[2], f4[3], f4[4], f4[5]))
  print(i)
}

#validacion cruzada de random forest --------
cortes=createFolds(1:nrow(base17), k=10)
cortes2=createFolds(1:nrow(base22), k=10)
res15=matrix(nrow=5, ncol=10) #almacen randomforest 2017
res16=matrix(nrow=5, ncol=10) #almacen randomforest 2022
for(i in 1:10){
  train3=base17[-cortes[[i]],]
  test3=base17[cortes[[i]],]
  
  train4=base22[-cortes2[[i]],]
  test4=base22[cortes2[[i]],]
  
  n=nrow(train17)
  q=ncol(train17)-1
  m=5
  tabla5=matrix(nrow=nrow(test3), ncol=m)
  for (j in 1:m){
    f=sample(1:n, n, replace=T) 
    v=sample(1:11, 8)
    train2=train17[f, c(12,v)]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
    tabla5[,j]=predict(mod, newdata=test3, type="class")
    print(paste(i,j))
  }
  pred.c=apply(tabla5, 1, mlv)
  f1=eval(test3$condicion_actividad, as.numeric(pred.c))
  
  n=nrow(train22)
  q=ncol(train22)-1
  m=5
  tabla6=matrix(nrow=nrow(test4), ncol=m)
  for (l in 1:m){
    f=sample(1:n, n, replace=T) 
    v=sample(1:11, 9)
    train2=train22[f, c(12,v)]
    pond=1/table(train2$condicion_actividad)
    ponderador=ifelse(train2$condicion_actividad=="Desempleado", pond[1], pond[2])
    mod=rpart(condicion_actividad~., method = "class", data=train2, weights = ponderador) 
    tabla6[,l]=predict(mod, newdata=test4, type="class")
    print(paste(i,l))
  }
  pred.c2=apply(tabla6, 1, mlv)
  f2=eval(test4$condicion_actividad, as.numeric(pred.c2))
  
  res15[1:5,i]=as.numeric(c(f1[1], f1[2], f1[3], f1[4], f1[5]))
  res16[1:5,i]=as.numeric(c(f2[1], f2[2], f2[3], f2[4], f2[5]))
  print(i)
}

#grafico AUC 
df4=data.frame(n=rep(1:10, 10), v=c(res7[4,], res9[4,], res11[4,], res13[4,], res15[4,], res8[4,],
                                    res10[4,], res12[4,], res14[4,], res16[4,]), 
               modelo=c(rep("Logistico", 10), rep("Bagging Logistico", 10), rep("Arbol", 10), rep("Bagging Arbol",10),
                        rep("Random Forest", 10), rep("Logistico", 10), rep("Bagging Logistico", 10), rep("Arbol", 10),
                        rep("Bagging Arbol", 10), rep("Random Forest", 10)), anio=c(rep(2017,50), rep(2022,50)))
df4$cat=paste(df4$anio, df4$modelo)
df4$modelo=factor(df4$modelo)
df4$anio=factor(df4$anio)
df4$cat=factor(df4$cat)

ggplot(df4, aes(n, v, group=cat)) +
  geom_line(aes(color=modelo), size=0.8)+
  geom_point(aes(color=modelo))+
  facet_grid(~anio, scales = "free", labeller=as_labeller(facet_names))+
  labs(title = "",x = "Cortes", y = "Porcentaje", color=c('Modelo'))+
  theme(axis.line.y=element_line(colour = "black"),
        axis.line.x=element_line(colour = "black"), panel.background = element_blank(),
        strip.background = element_rect(colour="white", fill="white"), 
        legend.key.width=unit(1,"cm"), panel.spacing = unit(1, "cm"),
        axis.text = element_text(size=rel(1)),
        axis.title.y = element_text(size=rel(1.2)),
        axis.title.x = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1)),
        legend.title=element_text(size=rel(1.2)),
        strip.text.y = element_text(size = rel(1.2)))+
  coord_cartesian(ylim=c(60,75))

#Grafico knn ----
