library(readxl)

data_frame = read.csv2('~/Documentos/data_frame.csv', sep=";")
age = data_frame$idade

print(age)



# Função que monta os grÃ¡ficos da idade
mat <- matrix(c(1,2))
print(mat)

# Definindo o layout do meu histograma
layout(mat, c(1,1), c(2.5,1))

# Identificando o maior da lista "a"
topox=ceiling(max(age))
print(topox)

# Aqui definimos os parametros de nosso grafico, por exemplo, distancia.
par(mar=c(0,5,2,0))

# Aqui criamos nosso primeiro histograma
b <- hist(age, breaks=c(0, seq(1,topox,1)),include.lowest = TRUE, right = FALSE, plot=FALSE)

#Aqui identificamos o valor maximo dentro dentro do nosso primeiro esquema de histograma
topoy = max(c(b$counts))

# Calculando a porcentagem de frequenncia
porcent=round((c(b$counts/lenght(age))*100), 2)

# As barras laranjas e o nome do grafico definido abaixo
hist(
   age,
   breaks=c(0, seq(1,topox,1)),
   include.lowest = TRUE,
   right = FALSE,
   xlim = c(0, topox),
   ylim = c(0, topoy),
   xlab = "Frequencia",
   ylab = "Frequencia",
   col = "orange",
   main = "Idade",
   axes = FALSE,
   density = 20
)

# Definindo o padrao de preenchimento dos eixos no graficos
axis(1, at=seq(0,topox,by=1))
axis(2, at=seq(0,topoy,by=4))

j <- 0.6
k <- 6

# Informando as porcentagens
for (i in 1:22){
  if(porcent[i] != 0 & !is.na(porcent[i]))
    text(j, k, paste(porcent[i], "%"))
    j<-j+1
}

par(mar=c(1,4,3,0))

# Definindo nosso bloxspot e suas configuracoes
c <-boxplot (
          age,
          horizontal = TRUE,
          outline = FALSE,
          xlim=c(0,2),
          ylim=c(0,22),
          col = "orange",
          axes=FALSE
      )











