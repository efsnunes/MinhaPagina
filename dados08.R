install.packages("tidyverse")
install.packages("dplyr")
#carregar os dados

mostardas<-read.csv("mostardas.csv", header=TRUE, sep=";", dec=",")
str(mostardas)

library("tidyverse")
library("dplyr")
#renomeando as colunas
nomecoluna<-c("Data","PrecTot","tempMed")
names(mostardas)<-nomecoluna

mostardas<-mostardas[,-4]

view(mostardas)
mostardas$Data<- as.Date(mostardas$Data, "%Y-%m-%d")

#transforma uma data em dia, mes e ano ##
mostardas$Dia<-as.numeric(format(mostardas$Data, format = "%d"))
mostardas$Mes<-as.numeric(format(mostardas$Data, format = "%m"))
mostardas$Ano<-as.numeric(format(mostardas$Data, format = "%Y"))
 
view(mostardas)

#fazendo a media mensal retirada do volume total do mes

mostardas$PrecTot<-as.numeric(mostardas$PrecTot)

mostardas<-transform(mostardas,PrecMed=PrecTot/Dia)

mostardas<-mostardas[,-2]

view(mostardas)

#separando os dados por meses

janeiro<-mostardas%>%filter(Mes=="1")
#plot(janeiro$Ano,janeiro$PrecMed)
fevereiro<-mostardas%>%filter(Mes=="2")
março<-mostardas%>%filter(Mes=="3")
abril<-mostardas%>%filter(Mes=="4")
maio<-mostardas%>%filter(Mes=="5")
junho<-mostardas%>%filter(Mes=="6")
julho<-mostardas%>%filter(Mes=="7")
agosto<-mostardas%>%filter(Mes=="8")
setembro<-mostardas%>%filter(Mes=="9")
outubro<-mostardas%>%filter(Mes=="10")
novembro<-mostardas%>%filter(Mes=="11")
dezembro<-mostardas%>%filter(Mes=="12")

#DADOS ESTAÇÕES DO ANO

verão<-rbind(janeiro,fevereiro,dezembro)
verão$Estacao <- "Verão"
outono<-rbind(março,abril,maio)
outono$Estacao <- "Outono"
inverno<-rbind(junho,julho,agosto)
inverno$Estacao <- "Inverno"
primavera<-rbind(setembro,outubro,novembro)
primavera$Estacao <- "Primavera"


mediaanualverão<-aggregate(PrecMed~Ano,data=verão,FUN=mean)
mdverão<-mean(mediaanualverão$PrecMed)
plot(mediaanualverão$Ano,mediaanualverão$PrecMed,type='l')
abline(h=mdverão, col="red")

mediaanualoutono<-aggregate(PrecMed~Ano,data=outono,FUN=mean)
mdoutono<-mean(mediaanualoutono$PrecMed)
plot(mediaanualoutono$Ano,mediaanualoutono$PrecMed,type='l')
abline(h=mdoutono, col="red")

mediaanualinverno<-aggregate(PrecMed~Ano,data=inverno,FUN=mean)
mdinverno<-mean(mediaanualinverno$PrecMed)
plot(mediaanualinverno$Ano,mediaanualinverno$PrecMed,type='l')
abline(h=mdinverno, col="red")

mediaanualprimavera<-aggregate(PrecMed~Ano,data=primavera,FUN=mean)
mdprimavera<-mean(mediaanualprimavera$PrecMed)
plot(mediaanualprimavera$Ano,mediaanualprimavera$PrecMed,type='l')
abline(h=mdprimavera, col="red")


summary(mostardas)


estações<-rbind(verão,outono,primavera,inverno)
boxplot(verão$PrecMed~verão$Ano)
boxplot(outono$PrecMed~outono$Ano)
boxplot(primavera$PrecMed~primavera$Ano)
boxplot(inverno$PrecMed~inverno$Ano,main="Precipitação Média no Inverno")


# Carregar o pacote ggplot2
library(ggplot2)

# Criar o boxplot combinado
ggplot(estações, aes(x = factor(Ano), y = estações$PrecMed, fill = Estacao)) +
  geom_boxplot() +
  facet_wrap(~ Estacao, scales = "free_y") +  # Dividir os gráficos por estação
  labs(title = "Precipitação Média por Estação do Ano (2014-2024)",
       x = "Ano", y = "Precipitação Média") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

install.packages("ggpubr")
library(ggpubr)
ggdensity(mostardas$PrecMed, fill="lightgray",na.rm=TRUE)

ggqqplot(mostardas$PrecMed,na.rm=TRUE)


#resume uma média anual
#anos<-jag %>% group_by(Ano) %>% summarise(TempMed=round(mean(TempMax,na.rm=TRUE),1),
#                                                          PrecMedia=round(mean(Prec,na.rm=TRUE),1),
#                                                                         UmidadeMedia=round(mean(Umidade,na.rm=TRUE),1))
hist(mostardas$PrecMed)
main="Dados Precipitação Média"
xlab="Precipitação(x$3.6)"
xlim=c(0,30)
hist(mostardas$PrecMed,xlab="Precipitação(x$3.6)",xlim=c(0,30),main="Dados Precipitação Média")
probability=TRUE
hist(mostardas$PrecMed,xlab="Precipitação(x$3.6)",xlim=c(0,30),main="Dados Precipitação",probability=TRUE)
lines(mostardas$PrecMed)
as.numeric(mostardas$PrecMed)
#density(lines(mostardas$PrecMed)

table(mostardas$PrecMed)
barplot(table(mostardas$PrecMed))
ylim=c(0,150)
xlab="Type"
ylab="Frequency"
axis.lty="solid"
space=.05
barplot(table(mostardas$PrecMed),ylim=c(0,150),xlab="Type",ylab="Frequency",axis.lty="solid",space=.05)
pie(table(mostardas$PrecMed))
#dotchart(x,labels(11))
type.frame<-data.frame(table(mostardas$PrecMed))
type.frame
dotchart(type.frame$Freq,type.frame$Var1)
dotchart(type.frame[,2],type.frame[,1])
#rajada.subset<-subset(dadoss,select=c(Data,DirVento,RajVento))
head(rajada.subset)
pairs(rajada.subset)
boxplot(dadoss$RajVento~dadoss$Data,xlab="DirVento",ylab="Hora.UTC")
ggplot(dadoss,aes(x=RajVento))
ggplot(dadoss,aes(x=RajVento))+geom_histogram()
geom_histogram(binwidth = 3,color="black",fill="white")
ggpairs(rajada.subset)
type.frame<-data.frame(table(dadoss$RajVento.Type))
colnames(type.frame<-c("Type","Frequency"))
type.frame

= "%Y"))


###############################################################################################







# Cálculo dos limites do box plot
LIC = quantile(dadoss$Temp, probs = 0.25,na.rm=TRUE) - 1.5*(IQR(dadoss$Temp,na.rm=TRUE))
LSC = quantile(dadoss$Temp, probs = 0.75,na.rm=TRUE) + 1.5*(IQR(dadoss$Temp,na.rm=TRUE))


##  Boxplot incluindo valores
valores <- as.integer(c(boxplot.stats(dadoss$Temp)$out, boxplot.stats(dadoss$Temp)$stats, LSC, LIC))

boxplot(dadoss$Temp,range=2 ,main = "Boxplot (Temperatura 2023)", 
        ylim=c(0,39), ylab = "ºC");text(y = valores, labels = valores, x = 1.25)

#install.packages("ggplot2")
library(ggplot2)


# Gráfico de dispersão de Temp versus RajVento
plot(dadoss.subset$Temp, dadoss.subset$RajVento,
     xlab = "Temperatura", ylab = "Rajada de Vento",
     main = "Rajada de Vento versus Temperatura")


dados_agrupados <- dadoss %>%
  group_by(estacao) %>%
  summarise(RajVento = mean(RajVento), DirVento = mean(DirVento), Temp = mean(Temp), estacao = mean(estacao))


# Remover linhas com valores NA
dados_agrupados_sem_na <- na.omit(dados_agrupados)



# Calculando a correlação entre a direção do vento e a velocidade do vento
correlacao_direcao_velocidade <- cor(dadoss$DirVento, dadoss$RajVento, use = "complete.obs")

# Exibindo o resultado
correlacao_direcao_velocidade

correlacao_Mes_temperatura <- cor(dadoss$Mes, dadoss$Temp, use = "complete.obs")

# Exibindo o resultado
correlacao_Mes_temperatura

save.image("AreadeTrabalho_Capao.RData")
##############################################################################

# Selecionar as colunas de chuva, dia e vento

chuvamed<-dplyr::select(inundacao,Dia,Chuva..mm.,Vel..Vento..m.s.)


# Calcular a média geral da chuva
media_geral <- mean(chuvamed$Chuva..mm., na.rm = TRUE)

# Filtrar as linhas onde a chuva está acima da média geral
chuva_acima_media <- chuvamed[chuvamed$Chuva..mm. > media_geral, ]

# Contar o número de dias que a temperatura esteve acima da média geral
dias_acima_media <- nrow(chuva_acima_media)

# Exibir o número de dias
print(dias_acima_media)


# Instalar e carregar os pacotes necessários
install.packages("lubridate")
library(lubridate)
library("dplyr")

# Supondo que o dataframe se chame 'df' e a coluna com data e hora se chame 'DateTime'
inundacao <- data.frame(
  Data = seq(from = as.POSIXct("2023-01-01 00:00"), to = as.POSIXct("2023-01-23 23:00"), by = "hour"),
  Chuva = sample(0:10, 552, replace = TRUE)  # Dados de chuva exemplo
)

# Visualizando as primeiras linhas do dataframe original
head(inundacao)

# Convertendo a coluna 'DateTime' para apenas data
inundacao <- inundacao %>%
  mutate(Date = as.Date(Data))

# Agrupando por dia e somando a precipitação diária (exemplo de análise)
inundacao_daily <- inundacao %>%
  group_by(Date) %>%
  summarise(Total_Chuva = sum(Chuva))

# Visualizando as primeiras linhas do dataframe agrupado por dia
head(inundacao_daily)

# Análise: contagem de dias com chuva
dias_com_chuva <- nrow(filter(inundacao_daily, Total_Chuva > 0))
print(dias_com_chuva)