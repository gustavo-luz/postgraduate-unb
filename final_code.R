#An�lise inicial do arquivo gerado pelo e-Lattes

#Bibliotecas usadas
{
library(tidyverse)
library(tm)
library(jsonlite)
library(listviewer)
library(RColorBrewer)
library(igraph)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(qdap)
library(wordcloud2)
}

#Defini��o da pasta e leitura de arquivos
{
  #Pasta com c�digos e arquivos
  setwd("~/Documentos/UnB/Engenharia de Produ��o/Data Science 4 ALL/elattes") #Pasta contendo os arquivos
  #upload de arquivo com fun��es para transformar listas em Data Frames
  #source serve pra pegar as fun��es do programa j� feito, como se fosse um library
  source("elattes.ls2df.R")  
perfil <- fromJSON("prof.profile.json")
public <- fromJSON("publication.json")
orient <- fromJSON("835.advise.json")
graphl <- fromJSON("gra.graph.json")
}

## 3.1 Prepara��o dos dados
{
#N�mero de Pessoas que foram encontradas: 15
length(perfil)

#examinar o perfil alternativo
jsonedit(perfil)


# an�lise dos perfis no formato DF
#Arquivo Profile por Curr�culo
# extrai perfis dos professores 
perfil.df.professores <- extrai.perfis(perfil)

# extrai producao bibliografica de todos os professores 
perfil.df.publicacoes <- extrai.producoes(perfil)

#extrai orientacoes 
perfil.df.orientacoes <- extrai.orientacoes(perfil)

#extrai areas de atuacao 
perfil.df.areas.de.atuacao <- extrai.areas.atuacao(perfil)

jsonedit(perfil.df.publicacoes)
}

##3.2 docentes por grande �rea e �rea
{
perfil.df.areas.de.atuacao %>%
  group_by(grande_area) %>%
  summarise(count = n_distinct(idLattes)) %>%
  ggplot(aes(x=2, y=count, fill=grande_area)) +
  geom_col() +
  coord_polar(theta='x') +
  theme_void()
##2
perfil.df.areas.de.atuacao %>%
  group_by(area) %>%
  summarise(count = n_distinct(idLattes)) %>%
  ggplot(aes(x=2, y=count, fill=area)) +
  geom_col() +
  coord_polar(theta='y') +
  theme_void()
}

#### 3.3 docentes por tipo de produ��o
{
table(unlist(sapply(perfil, function(x) names(x$producao_bibiografica))))
  }

## 3.4 docentes por tipo de orienta��o
{
table(unlist(sapply(perfil, function(x) names(x$orientacoes_academicas))))
}

#### 3.5periodicos por ano
{
##Análise dos dados no formato DF
public.periodico.df <- pub.ls2df(public, 1) #artigos
public.livros.df <- pub.ls2df(public, 2) #livros
public.eventos.df <- pub.ls2df(public, 5) #eventos

public.periodico.df %>%
  group_by(ano) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano, y = Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "#CC79A7")+
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal()
}

#### 3.6 livros por ano
{
public.livros.df %>%
  group_by(ano) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano, y = Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "#CC79A7")+
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal()
  }

## 3.7 n�mero de eventos realizados
{
jsonedit(public.eventos.df)

public.eventos.df %>%
  group_by(ano_do_trabalho) %>%
  summarise(Quantidade = n()) %>%
  ggplot(aes(x = ano_do_trabalho, y = Quantidade)) +
  geom_bar(position = "stack",stat = "identity", fill = "#CC79A7")+
  geom_text(aes(label=Quantidade), vjust=-0.3, size=2.5)+
  theme_minimal()
  }

#3.8 Eventos por pa�s
{
public.eventos.df %>%
  filter(pais_do_evento %in% 
           c(names(head(sort(table(public.eventos.df$pais_do_evento)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,pais_do_evento) %>%
  ggplot(aes(x=ano_do_trabalho,y=pais_do_evento, color= pais_do_evento)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()
  }

#3.9 Eventos por classifica��es
{
public.eventos.df %>%
  filter(classificacao %in% 
           c(names(head(sort(table(public.eventos.df$classificacao)
                             , decreasing = TRUE), 10)))) %>%
  group_by(ano_do_trabalho,classificacao) %>%
  ggplot(aes(x=ano_do_trabalho,y=classificacao, color= classificacao)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()
  }

# 3.10 Livros por pa�s
{
public.livros.df %>%
  filter(pais_de_publicacao %in% c("Brasil", "Estados Unidos", "Holanda",
                                   "Grã-Bretanha", "Alemanha", "Suiça")) %>%
  group_by(ano,pais_de_publicacao) %>%
  ggplot(aes(x=ano,y=pais_de_publicacao, color= pais_de_publicacao)) +
  xlab("Ano") + ylab("Pais") + geom_point() + geom_jitter()
  }

###ORIENTA��ES - SEM LISTA, S� DATAFRAME

#3.11 Natureza das Orienta��es completas
{
#Orientação
##Ap�s analisar os dados no formato DF
orient.posdoutorado.df <- ori.ls2df(orient, 6) #pos-Doutorado conclu�???do
orient.doutorado.df <- ori.ls2df(orient, 7) #Doutorado conclu�???do
orient.mestrado.df <- ori.ls2df(orient, 8) #Mestrado conclu�???do
orient.df <- rbind(rbind(orient.posdoutorado.df, orient.doutorado.df), orient.mestrado.df)
df_orienta��esporlattes <- as.data.frame(sort(table(unlist(
  orient.df$idLattes1, recursive = TRUE)),decreasing=TRUE))

  ### duas formas de an�lise

ggplot(orient.df,aes(ano,fill=natureza)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orienta��es Completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")


ggplot(orient.df,aes(natureza,fill=ano)) +
  geom_bar(stat = "count", position="dodge") +
  ggtitle("Natureza das Orienta��es completas Por Ano") +
  theme(legend.position="right",legend.text=element_text(size=7)) +
  guides(fill=guide_legend(nrow=5, byrow=TRUE, title.position = "top")) +
  labs(x="Ano",y="Quantidade")

  }

##### 3.12 ANALISE POR IDLATTES
{
  ggplot(df_orienta��esporlattes, aes(x=Var1, y=Freq)) +
  geom_point(size=3) +
  geom_segment(aes(x=Var1,
                   xend=Var1,
                   y=0,
                   yend=Freq)) +
  labs(title="N�mero de Orienta��es por Professor", x="Lattes do Professor",
       y="N�mero de Orienta��es") +
  theme(axis.text.x = element_text(angle=75, vjust=0.6))
  }

### 3.13 PARTE AN�LISE DE REDES
{
df.prog <- read.table("PesqPosCapes.csv", sep = "\t", header = TRUE, fill=TRUE, colClasses = "character")
df.unique <- df.prog[!duplicated(df.prog[,1]),c(1,2,4,7)]

#Grafo
g <- g.ls2ig(graphl)
df <- as.data.frame(V(g)$name); colnames(df) <- "idLattes"
df <- left_join(df, df.unique, by = "idLattes")
g1 <- g.ls2ig(graphl)


V(g)$degree <- degree(g)
V(g)$betweenness <- betweenness(g,weights = NA, normalized = TRUE)
V(g)$closeness <- closeness(g,normalized = TRUE)
V(g)$eigen <- eigen_centrality(g)$vector
V(g)$cluster <- cluster_leading_eigen(g)$membership
V(g)$nomes <- df$Docente

df <- as.data.frame(V(g)$name)

par(mfrow=c(2,3))
#grafo sem nomes
plot(g,vertex.label=NA)

plot(V(g)$degree,
     xlab = "",
     ylab = "Valor", 
     main = "N�mero de Conex�es",
     col="blue",
     type = "p")

plot(V(g)$closeness,
     xlab = "",
     ylab = "Valor", 
     main = "Closeness",
     col="blue",
     type = "p")

plot(V(g)$betweenness,
     xlab = "",
     ylab = "Valor", 
     main = "Betweenness",
     col="blue",
     type = "p")

plot(V(g)$cluster,
     xlab = "",
     ylab = "Valor", 
     main = "Clusters",
     col="blue",
     type = "p")

plot(V(g)$eigen,
     xlab = "",
     ylab = "Valor", 
     main = "Eigen",
     col="blue",
     type = "p")
par(mfrow=c(1,1))


{
  #cria arquivo com dados quantitativos para análise
  perfil.df <- data.frame()
  perfil.dftudo <- perfil.df.professores %>% 
    select(idLattes, nome, resumo_cv, senioridade) %>% 
    left_join(
      perfil.df.orientacoes %>% 
        select(orientacao, idLattes) %>% 
        filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
        group_by(idLattes) %>% 
        count(orientacao) %>% 
        spread(key = orientacao, value = n), 
      by = "idLattes") %>% 
    left_join(
      perfil.df.publicacoes %>% 
        select(tipo_producao, idLattes) %>% 
        filter(!grepl("ARTIGO_ACEITO", tipo_producao)) %>% 
        group_by(idLattes) %>% 
        count(tipo_producao) %>% 
        spread(key = tipo_producao, value = n), 
      by = "idLattes") %>% 
    left_join(
      perfil.df.areas.de.atuacao %>% 
        select(area, idLattes) %>% 
        group_by(idLattes) %>% 
        summarise(n_distinct(area)), 
      by = "idLattes")
  
  
  #
  perfil.df <- perfil.df.professores %>% 
    select(idLattes, nome, resumo_cv, senioridade) %>% 
    left_join(
      perfil.df.orientacoes %>% 
        select(orientacao, idLattes) %>% 
        #      filter(!grepl("EM_ANDAMENTO", orientacao)) %>% 
        group_by(idLattes) %>% 
        count(orientacao) %>% 
        spread(key = orientacao, value = n), 
      by = "idLattes") %>% 
    left_join(
      perfil.df.publicacoes %>% 
        select(tipo_producao, idLattes) %>% 
        group_by(idLattes) %>% 
        count(tipo_producao) %>% 
        spread(key = tipo_producao, value = n), 
      by = "idLattes") %>% 
    left_join(
      perfil.df.areas.de.atuacao %>% 
        select(area, idLattes) %>% 
        group_by(idLattes) %>% 
        summarise(n_distinct(area)), 
      by = "idLattes")
  
  
}

V(g)$Area <- df$AreaPos
V(g)$orient_dout <- perfil.df$ORIENTACAO_CONCLUIDA_DOUTORADO
V(g)$orient_mest <- perfil.df$ORIENTACAO_CONCLUIDA_MESTRADO
V(g)$publicacao <- perfil.df$PERIODICO
V(g)$eventos <- perfil.df$EVENTO

minhapaleta <- brewer.pal(7,"Reds")



#HEAD: ORIENTA��ES E PUBLICA��ES
par(mfrow=c(1,2))

#Grafo por orientações de mestrado
plot(g,vertex.size = V(g)$orient_mest,
     vertex.label=NA,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="Orienta��es de Mestrado",
     
)

plot(g,vertex.size = V(g)$publicacao,
     vertex.label=NA,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="Publica��es"
)

par(mfrow=c(1,1))
#outro visualizador pra aumentar o zoom
dev.new(width=7, height=3, unit="in")

#Grafo por orientações de mestrado
plot(g,vertex.size = V(g)$orient_mest,
     vertex.label.cex = 0.8,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="Orienta��es de Mestrado"
)
#publica��o
plot(g,vertex.size = V(g)$publicacao,
     vertex.label.cex = 0.7,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="Publica��es"
)


dev.off(which = dev.cur())


par(mfrow=c(1,2))
#Grafo por número de conexões
plot(g,vertex.size = V(g)$degree,
     vertex.label=NA,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="N�mero de Conex�es")

#area: Marcelo �nico que n�o �
plot(g,vertex.size = V(g)$area,
     vertex.label= NA,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="�rea"
)

dev.new(width=7, height=3, unit="in")

par(mfrow=c(1,1))
plot(g,vertex.size = V(g)$degree,
     vertex.label.cex = 0.7,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="N�mero de Conex�es")

#area: Marcelo �nico que n�o �
plot(g,vertex.size = V(g)$area,
     vertex.label.cex = 0.7,
     layout=layout.circle,
     vertex.color = minhapaleta,
     main="�rea"
)

dev.off(which = dev.cur())
plot(g)
}

###### 3.14 PARTE TEXT MINING
{
#peri�dicos
{
  tit_per <- public$PERIODICO %>% 
    sapply(function(x) (x$titulo)) %>% unlist()
  tit_source <- VectorSource(tit_per)
  tit_corpus <- VCorpus(tit_source)
  tit_corpus <- tm_map(tit_corpus,removePunctuation)
  tit_corpus <- tm_map(tit_corpus,removeNumbers)
  tit_corpus <- tm_map(tit_corpus,tolower)
  tit_corpus <- tm_map(tit_corpus,stripWhitespace)
  
  stopwords("en"); stopwords("pt")
  #adicionar palavras a serem tiradas ap�s stopwords("pt"): num '2013' , texto "texto"
  myStopWords <- c(stopwords("en"),stopwords("pt"),"brazil","brasil","brazilian","federal","distrito","district","es","rn","new","fortalezace","list","sisvan",'clist',"marco","listlanguage","en","clist",'-2014',"the", '2013',"dos","sobre","erica:","among","DOS","erica","THE","mais","MAIS")
  
  tit_clean <- tm_map(tit_corpus, removeWords, myStopWords)
  
  corpus <- (VectorSource(tit_clean))
  corpus <- Corpus(corpus)
  
  freqteste <- freq_terms(corpus,top = 50)
  freqteste
  
  freqgraph <- freq_terms(corpus,top=20)
  
  barplot(freqgraph$FREQ, las = 2, names.arg = freqgraph$WORD,
          col ="purple", main ="20 palavras mais frequentes - Peri�dicos",
          ylab = "Frequ�ncia das Palavras")
  
  
  wordcloud2(freqteste, size=0.6, color=brewer.pal(8,"Spectral"))
  
}


#eventos
{
  tit_ev <- public$EVENTO %>% 
    sapply(function(x) (x$titulo)) %>% unlist()
  tit_source <- VectorSource(tit_ev)
  tit_corpus <- VCorpus(tit_source)
  tit_corpus <- tm_map(tit_corpus,removePunctuation)
  tit_corpus <- tm_map(tit_corpus,removeNumbers)
  tit_corpus <- tm_map(tit_corpus,tolower)
  tit_corpus <- tm_map(tit_corpus,stripWhitespace)
  
  stopwords("en"); stopwords("pt")
  #adicionar palavras a serem tiradas ap�s stopwords("pt"): num '2013' , texto "texto"
  myStopWords <- c(stopwords("en"),stopwords("pt"),"brazil","brasil","brazilian","federal","distrito","district","es","rn","new","fortalezace","list","sisvan",'clist',"marco","listlanguage","en","clist",'-2014',"the", '2013',"dos","sobre","erica:","among","DOS","erica","THE","mais","MAIS")
  
  tit_clean <- tm_map(tit_corpus, removeWords, myStopWords)
  
  corpus <- (VectorSource(tit_clean))
  corpus <- Corpus(corpus)
  
  freqteste <- freq_terms(corpus,top = 50)
  freqteste
  
  freqgraph <- freq_terms(corpus,top=10)
  
  plot(freqgraph)
  
  wordcloud2(freqteste, size=0.6, color=brewer.pal(8,"Spectral"))
  
}


#livro
{
  tit_livro <- public$LIVRO %>% 
    sapply(function(x) (x$titulo)) %>% unlist()
  tit_source <- VectorSource(tit_livro)
  tit_corpus <- VCorpus(tit_source)
  tit_corpus <- tm_map(tit_corpus,removePunctuation)
  tit_corpus <- tm_map(tit_corpus,removeNumbers)
  tit_corpus <- tm_map(tit_corpus,tolower)
  tit_corpus <- tm_map(tit_corpus,stripWhitespace)
  
  stopwords("en"); stopwords("pt")
  #adicionar palavras a serem tiradas ap�s stopwords("pt"): num '2013' , texto "texto"
  myStopWords <- c(stopwords("en"),stopwords("pt"),"en",'en',"brazil","brasil","brazilian","federal","distrito","district","es","rn","new","fortalezace","list","sisvan",'clist',"marco","listlanguage","en","clist",'-2014',"the", '2013',"dos","sobre","erica:","among","DOS","erica","THE","mais","MAIS")
  
  tit_clean <- tm_map(tit_corpus, removeWords, myStopWords)
  
  corpus <- (VectorSource(tit_clean))
  corpus <- Corpus(corpus)
  
  freqteste <- freq_terms(corpus,top = 10)
    freqteste
  
  barplot(freqteste$FREQ, las = 2, names.arg = freqteste$WORD,
          col ="purple", main ="Palavras mais frequentes - Livros",
          ylab = "Frequ�ncia das Palavras")
  
  freqnuvemlivro <- freq_terms(corpus,top=50)
  wordcloud2(freqnuvemlivro, size=0.6, color=brewer.pal(8,"Spectral"))
}

}










