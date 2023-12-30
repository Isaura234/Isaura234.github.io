setwd("C:/Users/Toshiba/OneDrive/UCR/2022/Experimentos II/Analisis/Base calificacion")
library(readxl)
library(car)
library(gridExtra)
library(lattice)
base=read_excel("base_experimentos.xlsx", sheet = "datos_ordenados")
base=subset(base,Contador==7)
options(contrasts=c("contr.sum","contr.poly"))
base$id=factor(base$id)
base$tratamiento=factor(base$tratamiento)
base$sexo=factor(base$sexo)
base$colegio=factor(base$colegio)
base$conexion=factor(base$conexion)
base$beca=factor(base$beca)

levels(base$colegio)=c("Publico", "Privado")
levels(base$tratamiento)=c("Sin", "Con")
levels(base$sexo)=c("Hombre", "Mujer")
levels(base$conexion)=c("Inalambrico", "Fibra optica")
#levels de beca

#descriptivos 
prop.table(table(base$tratamiento, base$colegio))*100
prop.table(table(base$tratamiento, base$sexo))*100
prop.table(table(base$tratamiento, base$beca))*100
prop.table(table(base$tratamiento, base$GAM))*100
prop.table(table(base$tratamiento, base$conexion))*100
summary(base$Calificacion[1:105])
summary(base$Calificacion[106:210])
summary(base$Calificacion[211:315])
summary(base$Calificacion[316:420])
summary(base$Calificacion[421:525])
summary(base$Calificacion[526:630])
summary(base$Calificacion[631:735])

#Analisis de linealidad 
xyplot(Calificacion ~ id_test|id, type=c("r", "p"), data=base[base$tratamiento=="Sin",], col="#008B8B", pch = 19, xlab = "Examen", ylab = "CalificaciÃ³n")
xyplot(Calificacion ~ id_test|id, type=c("r", "p"), data=base[base$tratamiento=="Con",], col="#008B8B", pch = 19, xlab = "Examen", ylab = "CalificaciÃ³n")

#graficanco solo algunos de los estudiantes
base1=subset(base, id=="3" | id=="12" | id=="35" | id=="74" | id=="81" | id=="101"| id=="115"| id=="118"| id=="120" | id=="128") #sin incentivo 
base2=subset(base, id=="22" | id=="27" | id=="41" | id=="44" | id=="62" | id=="77"| id=="87"| id=="93"| id=="105" | id=="114") #con incentivo 

my.settings <- list(
  strip.background=list(col="#FFA88B"),
  strip.border=list(col="Black"))

g1=xyplot(Calificacion ~ id_test|id, type=c("r", "p"), data=base1, col="#EF3B2C", pch = 19, xlab = "N° Examen", ylab = "Calificación",
          key=list(lines=list(col="#EF3B2C", lyt=1, lwd=2), text=list(c("Sin incentivo"))),
          par.settings = my.settings,
          par.strip.text=list(col="BLACK"))

my.settings2 <- list(
  strip.background=list(col="#ABD9E9"),
  strip.border=list(col="Black"))
g2=xyplot(Calificacion ~ id_test|id, type=c("r", "p"), data=base2, col="#4575B4", pch = 19, xlab = "N° Examen", ylab = "Calificación",
          key=list(lines=list(col="#4575B4", lyt=1, lwd=2), text=list(c("Con incentivo"))),
          par.settings = my.settings2,
          par.strip.text=list(col="BLACK"))

grid.arrange(g1,g2, ncol=2)

#Modelo planteado
library(lme4)
mod1=lmer(Calificacion ~ id_test + tratamiento + sexo + colegio + beca + conexion + Promediomatricula + 
            id_test:tratamiento + id_test:sexo + id_test:colegio + id_test:beca + id_test:conexion +
            tratamiento:sexo + tratamiento:colegio + tratamiento:beca + tratamiento:conexion +
            (1+id_test|id), data=base)

#verificacion de supuestos 
#supuesto de normalidad de errores 
res=residuals(mod1)
shapiro.test(res)
qqPlot(res, ylab = "Residuales", xlab = "Cuantiles") #hay normalidad

#supueesto de esfericidad prueba de Mauchly
library(rstatix)
anova_test(data = base, dv = Calificacion, wid = id, within = id_test)
#no se rechaza H0, se cumple el supuesto 

#Prueba de correlacion entre interceptos y pendientes 
mod1=lmer(Calificacion ~ id_test + tratamiento + sexo + colegio + beca + conexion + Promediomatricula + 
            id_test:tratamiento + id_test:sexo + id_test:colegio + id_test:beca + id_test:conexion +
            tratamiento:sexo + tratamiento:colegio + tratamiento:beca + tratamiento:conexion +
            (1+id_test|id), data=base)

mod2=lmer(Calificacion ~ id_test + tratamiento + sexo + colegio + beca + conexion + Promediomatricula + 
            id_test:tratamiento + id_test:sexo + id_test:colegio + id_test:beca + id_test:conexion +
            tratamiento:sexo + tratamiento:colegio + tratamiento:beca + tratamiento:conexion +
            (1|id) + (0+id_test|id), data=base)
anova(mod1, mod2)

beta0=beta1=c()
l=length(table(base$id))
suj=as.numeric(base$id)
for(i in 1:l){
  mod=lm(Calificacion~id_test, base[suj==i,])
  beta0[i]=mod$coef[1]
  beta1[i]=mod$coef[2]
}

plot(beta1, beta0, pch = 18, col = "#4575B4", xlab="Pendiente", ylab = "Intercepto")
abline(lm(beta0~beta1), col = "#FFA88B", lwd = 2)


#Prueba de pendientes iguales 
my.settings3 <- list(
  strip.background=list(col="#ABD9E9"),
  strip.border=list(col="Black"))
xyplot(Calificacion ~ id_test|factor(tratamiento, labels=c("Sin incentivo", "Con incentivo")), group=id, pch=19, data=base, type="r",
       par.settings = my.settings3, xlab="N� Examen")

mod3=lmer(Calificacion ~ id_test + tratamiento + sexo + colegio + beca + conexion + Promediomatricula + 
            id_test:tratamiento + id_test:sexo + id_test:colegio + id_test:beca + id_test:conexion +
            tratamiento:sexo + tratamiento:colegio + tratamiento:beca + tratamiento:conexion +
            (1|id), data=base)
anova(mod2, mod3)

#Prueba de interaccion 
drop1(mod2, test="Chisq")
mod2=update(mod2, .~. -id_test:colegio)
mod2=update(mod2, .~. -tratamiento:conexion)
mod2=update(mod2, .~. -id_test:beca)
mod2=update(mod2, .~. -id_test:sexo)
mod2=update(mod2, .~. -id_test:conexion)
mod2=update(mod2, .~. -tratamiento:colegio)
mod2=update(mod2, .~. -tratamiento:sexo)

#interaccion entre id_test y tratamiento 
xyplot(Calificacion~id_test, group=tratamiento, col=c("#FF9578", "#5D84BC"), type="r", lwd=2, xlab="N° Examen", ylab="Calificaci�n",
       key=list(space="top",columns=2,lines=list(col=c("#FF9578", "#5D84BC"), lty=1, lwd=2), text=list(c("Sin incentivo", "Con incentivo"))), data=base, ylim=c(6,8),
       scales = list(x="free", y="free"))

#interaccion entre beca y tratamiento
xyplot(Calificacion~factor(tratamiento, labels=c("Sin incentivo","Con incentivo")), group=beca, col=c("#FF5757", "#89C1F4"), type="a", lwd=2, xlab="", ylab="Calificaci�n",
       key=list(space="top",columns=2,lines=list(col=c("#FF5757", "#89C1F4"), lty=1, lwd=2), text=list(c("Sin Beca","Con beca"))), data=base, ylim=c(6,8),
       scales = list(x="free", y="free"))

#Pendiente para el tratamiento sin incentivo
summary(mod2)$coef
contrasts(base$tratamiento)
pendiente1=summary(mod2)$coef[2,1]+summary(mod2)$coef[9,1]
pendiente1

#Pendiente para el tratamiento con incentivo
summary(mod2)$coef
contrasts(base$tratamiento)
pendiente2=summary(mod2)$coef[2,1]-summary(mod2)$coef[9,1]
pendiente2

#interaccion entre tratamiento y beca
#se va a comparar el tratamiento en cada nivel de beca 
contrasts(base$tratamiento)
contrasts(base$beca)

#En beca 0 
v11=c(1,0,1,0,0,1,0,0,0,1) #sin incentivo 
v21=c(1,0,-1,0,0,1,0,0,0,-1) #con incentivo 

#en beca 1
v12=c(1,0,1,0,0,-1,0,0,0,-1) #sin incentivo 
v22=c(1,0,-1,0,0,-1,0,0,0,1)

c1=v11-v21
c2=v12-v22
cont=cbind(c1,c2)
L=t(cont)%*%summary(mod2)$coef[,1]
ee=sqrt(diag(t(cont)%*%vcov(mod2)%*%cont))
qt=L/ee
p=pt(qt, 105-10, lower.tail=FALSE)
p<0.05/2

#cota inferior 
d=1
qt=qt(1-0.05/d, 105-10)
lim=L[1]-qt*ee[1]
