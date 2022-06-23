### Algoritmo de Cluster Hierárquico - Não supervisionado - ###

# Instalando e instanciando os pacotes necessários

pacotes <- c("tidyverse","cluster","dendextend","factoextra","fpc","gridExtra",
             "readxl","magrittr","dplyr","ggplot2")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

## Breve definição: Algoritmos de Clusters são utilizados para tarefas não supervisionadas,
#isso significa que não temos uma variável alvo definida para prever e então precisaremos
#agrupar as variaveis através de suas distâncias Euclidianas e a partir dai definir grupos.

#Para este exemplo, vamos utilizar uma base nativa da linguagem R que contém diversos
#modelos de carros e suas respectivas características.

#Carregando base de dados
df_carros <- mtcars
#Para saber mais sobre a base de dados escolhida
?mtcars

#Visualizando os dados
View(df_carros)

#Podemos observar que algumas variavéis possuem valores numericamente desproporcionais as demais,
#portando, vamos normalizar/padronizar esses valores para que o nosso algoritmo seja mais eficaz.
#Importante: se essa etapa não for realizada, poderá alterar consideravelmente a distância
#Euclidiana calculada entre as variáveis e destoar o resultados dos grupos.

df_carros_normalizada <- scale(df_carros)
df_carros_normalizada
#Observação: Os dados gerados pela normalização não precisam ser interpretados ok?
#Depois de termos o nosso algoritmo treinado, retornaremos eles a sua forma normal e
#seremos capazes de avaliar o resultado tranquilamente.

#Agora vamos calcular a distância Euclidiana entre nossas variáveis.
df_carros_distancia <- dist(df_carros_normalizada, method = "euclidian")
df_carros_distancia

#Chegou a hora mais esperada, treinar o algoritmo de cluster hierarquico.
df_carros_cluster_hierarquico <- hclust(df_carros_distancia, method = "single") 
df_carros_cluster_hierarquico 
#Conhecido popularmente como técnica do vizinho mais próximo.

#Avaliando o dendohrama com o resultado dos nossos grupos
plot(df_carros_cluster_hierarquico, cex = 0.7, hang = -1)
#O Dendograma demonstra de forma visual a formação dos grupos, vamos traçar algumas linhas
#sugerindo a quantidade de grupos.
rect.hclust(df_carros_cluster_hierarquico, k = 5)
#Me parece que com 5 grupos podemos ter uma boa distribuição, sendo 2 grupos com alto volume
#de dados e baixa variabilidade e 3 com menor volume e alta variabilidade, no entanto,
#essa ainda não é a melhor forma de avaliar, vamos para a estatística!

#O método ELBOW nos auxilia na definição da quantidade "ideal" de grupos, pois ele calcula
#a soma da variabilidade entre os eles, portanto, a contar do passo em que a nossa linha 
#ficar plana ou próxima disso temos uma boa ideia da quantidade de grupos.
fviz_nbclust(df_carros_normalizada, FUN = hcut, method = "wss")
#O método sugere que a gente "corte" o número de grupos na curva do cotovelo(Elbow),
#portanto, uma quantidade interessante me parece 4 ou no máximo 5 grupos. Optei seguir com 4.

#Aqui estamos criando os nossos 4 grupos
df_carros_grupos <- cutree(df_carros_cluster_hierarquico, k = 4)
table(df_carros_grupos)
df_carros_grupos
#Observem que agora temos os nossos carros divididos em 4 grupos.

#Vamos transformar a saída do nosso algoritmo em uma data frame para podermos posteriormente
#unificar com o data frame original
df_carros_resultado_cluster <- data.frame(df_carros_grupos) 
df_carros_resultado_cluster
#Pronto, já temos um data frame contento o resultado do nosso trabalho

#Juntando o resultado do algoritmo com a base original 
df_carros_final <- cbind(df_carros, df_carros_resultado_cluster)
df_carros_final
#Uouuuu!! Observem a última coluna, prontinho, temos um data frame contento o nosso
#agrupamento gerado através de um algoritmo de machine learning não supervisionado!!!

#Agora vamos gerar algumas estatísticas descritivas para entender melhor o conteúdo de cada
#um dos nossos grupos
media_df_carros_final <- df_carros_final %>%
  group_by(df_carros_grupos) %>%
  summarise(n = n(), 
           mpg = mean(mpg),
           cyl = mean(cyl),
           disp = mean(disp),
           hp = mean(hp),
           drat = mean(drat),
           wt = mean(wt),
           qsec = mean(qsec),
           vs = mean(vs),
           am = mean(am),
           gear = mean(gear),
           carb = mean(carb))

media_df_carros_final

#Pronto, agora sabemos exatamente a média das variáveis dos nossos grupos e podemos concluir
#algumas coisas:
# 1) Nossos dois primeiros grupos são os maiores em quantidade
# 2) Carros com poucos cilindros consomem menos combustível
# 3) Carros com muitos cavalos de potência não são econômicos
# 4) Os carros mais econômicos são os mais leves
# 5) Os carros com menos cavalos de potência são os mais leves
#Algumas conclusões podem parecer óbvias se você entende de carros, o que não é o meu caso rsrsrs,
#São inúmeras possibilidades de análises, é hora de colocar a criatividade em prática!


##### DESCRIÇÃO TRADUZIDA DAS VARIÁVEIS ########################################
#mpg	Miles/(US) gallon -- Milhas por galão
#cyl	Number of cylinders -- número de cilindros
#disp	Displacement (cu.in.) -- deslocamento
#hp	Gross horsepower -- cavalos de potência
#drat	Rear axle ratio -- relação do eixo traseiro
#wt	Weight (1000 lbs) -- peso por mil libras (1lbs = 0,43kg)
#qsec	1/4 mile time
#vs	Engine (0 = V-shaped, 1 = straight)
#am	Transmission (0 = automatic, 1 = manual) -- Câmbio, 0 = automático e 1 = manual 
#gear	Number of forward gears -- número de marchas
#carb	Number of carburetors -- número de carburadores 
################################################################################
