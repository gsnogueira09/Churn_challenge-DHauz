        # HIPÓTESE 1

## Visualização

library (ggplot2)
library(reshape2)

### Carregamento do arquivo - modificado manualmente
setwd("C:/Users/Gui/Desktop/Nova Pasta")
data<-read.table("churn_by_region.csv",sep = ",",h=T)
data

### Gerando a visualização
graph <- ggplot(data, aes(region, cust, group = class, fill = class)) +
  geom_col(position = "dodge", colour = "black")+
  scale_fill_manual(values=c('#999999','#E69F00'), name="Classe",
                    labels=c("Total","Churns"))+
  xlab("Regiões")+ylab("Número de clientes")+
  ylim(0,50000)
graph

### Salvar gráfico
ggsave("churn_by_region.pdf", graph, dpi=600, units="cm", height = 20,
       width = 30)


        # HIPÓTESE 2 - Não foi possível testar a normalidade

## Teste de hipótese

### Carregamento do arquivo
setwd("C:/Users/Gui/Desktop/Nova Pasta")
data<-read.table("churns_by_month.csv",sep = ",",h=T)
data

### Pré-processamento de dados
data1=data[,c(1,25)]
data1

### Teste de normalidade

install.packages("dgof")
library(dgof)

data2<-data1[data1$churn == 0, ]
data2
data3<-data1[data1$churn == 1, ]
data3

data_test=data2[,2]
data_test

ks.test(data_test,"pnorm",mean(data_test),sd(data_test))
hist(data_test)

### Teste-t para comparação de médias
t.test(data1$months~data1$churn)

### Embora o teste-t tenha sido significativo, não há como afirmar
### que as amostras apresentam médias diferentes. Os gráficos feitos
### com os dados fornecidos refutam essa ideia. É necessário uma investigação
### mais detalhada para entender o que houve.

### Visualização

graph2<-ggplot() + 
  geom_boxplot(data=data1, aes(x=as.factor(churn), y=months), 
               fill="dodgerblue3", color = "blue4", alpha=0.9) + 
  xlab("Tipo de usuário") + ylab("Total de meses") +
  theme(text = element_text(size = 14), 
        axis.line = element_line(colour = "grey"), 
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"))+
  scale_x_discrete(labels = c("Clientes", "Ex-clientes"))
graph2

### Salvar gráfico
ggsave("churn_by_months.pdf", graph2, dpi=600, units="cm", height = 20,
       width = 30)

      # HIPÓTESE 3

## Visualização

### Carregamento do arquivo
setwd("C:/Users/Gui/Desktop/Nova Pasta")
data<-read.table("change_mou_x_change_rev.csv",sep = ",",h=T)
data

graph3<-ggplot(data, aes(x=change_mou, y=change_rev, color=churn))+
  geom_point()+xlab("Porcentagem de mudança em minutos")+
  ylab("Porcentagem de mudança na fatura")
graph3

### Salvar gráfico
ggsave("minutes_money_corr.pdf", graph3, dpi=600, units="cm", height = 20,
       width = 30)