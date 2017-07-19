---
layout: post
title: "Análise com uso de PCA - Falas femininas no cinema"
author: "Leticia Wanderley"
date: 2017-07-19 10:02:04
published: true
tags: [htmlwidgets, r]
---



## PCA

Os dados utilizados são uma junção de um conjunto de filmes retirado do [IMDb](http://www.imdb.com/) com o conjunto de personagens e suas respectivas falas retirados dos scripts desses filmes. Os dados podem ser encontrados [neste repositório](https://github.com/matthewfdaniels/scripts).

Para melhor analisar os dados foram criadas três novas colunas, representando a porcentagem de falas de personagens femininas comparadas com o total falas de personagens do filme, a porcentagem de personagens femininas no filme e um índice que representa a presença relativa de falas de personagens femininas nos filmes, a razão entre a porcentagem de falas de personagens femininas e a porcentagem de personagens femininas. 



Ao todo existem seis atributos numéricos para cada filme, são eles: renda, ano de lançamento, idade mediana dos personagens, porcentagem de personagens femininas, porcentagem de falas de personagens femininas e índice relativo de presença e falas de personagens femininas do filme. Seria muito complicado analisar o conjunto de dados em seis dimensões distintas. O cerébro humano consegue distinguir relativamente bem, no máximo, três dimensões. Por esse motivo, se fará uso de uma técnica de redução de dimensionalidade. 

Redução de dimensionalidade consiste no processo de redução do número de variáveis em um conjunto de dados. Esse processo pode ser divido em _seleção de atributos_, processo no qual se seleciona os atributos mais significativos para representar o conjunto de dados, e _extração de atiíbutos_, processo no qual se transforma um conjunto grande de varíaveis em um conjunto menor sem perder muito da significância do conjunto.

Para esta análise será utilizada uma técnica de extração de atributos chamada __Análise de Componentes Principais__ ou Principal Components Analysis (PCA). Nessa técnica as n colunas/atributos/dimensões de um conjunto de dados são transformadas em um número menor de variáveis não relacionadas, os componentes principais. No gráfico abaixo se pode observar os dois componentes principais do conjunto de dados de filmes analisado.


{% highlight r %}
pr.out <- prcomp(select(data, -title), scale=TRUE)

autoplot(pr.out, data = data, size = 2,  
         loadings = FALSE)
{% endhighlight %}

![plot of chunk pca](/ad1-blog/figure/source/pca-falas-femininas-em-hollywood/2017-07-19-pca-falas-femininas-em -hollywood/pca-1.png)

O procedimento do PCA tenta reduzir a dimensionalidade agrupando varáveis correlacionadas em um único componente principal. Para isso ele analisa a variância das dimensões dentro do conjunto e agrupa aquelas que tem variância e tendência de crescimento ou decrescimento semelhantes. 


{% highlight r %}
autoplot(pr.out, data = data, size = 2, 
         colour = "grey",
         loadings = TRUE, loadings.colour = 'deeppink3',
         loadings.label = TRUE, 
         loadings.label.size = 3.5,
         loadings.label.colour='navy')
{% endhighlight %}

![plot of chunk dimvectors](/ad1-blog/figure/source/pca-falas-femininas-em-hollywood/2017-07-19-pca-falas-femininas-em -hollywood/dimvectors-1.png)

No exemplo analisado, para calcular o componente principal 1 (PC1) as dimensões com maior relevância foram character_words, fem_words, fem_characters e median_age. As três primeiras apresentam comportamento semelhante, a medida que um ponto (filme) se move para a esquerda no eixo x o valor das três aumenta, já com relação a median_age o comportamente é inverso, a idade mediana das personagens do filme aumenta a medida que se anda para a direita no eixo x. As dimensões de gross e year tem pouca influência no valor do PC1, isso pode ser observado pela inclinação pequena dos os vetores das duas em relação ao eixo x. Para calcular o componente principal 2 (PC2) as dimensões com maior relavância foram gross e year, a renda e o ano de lançamento do filme, respectivamente. A medida que um ponto (filme) sobe do eixo y o valor de gross aumenta e o valor de year diminui. As duas dimensões são quase inversamente proporcionais. Entre as dimensões restantes, a que parece ter maior influência no valor de PC2 é character_words, o valor dessa coluna aumenta a medida que se sobe no eixo y.

Na tabela abaixo se pode ver as funções de cada dimensão no cálculo dos componentes principais.


{% highlight r %}
tidy(pr.out, "variables") %>% 
    filter(PC <= 2) %>% 
    spread(column, value)
{% endhighlight %}



{% highlight text %}
##   PC character_words fem_characters  fem_words     gross median_age
## 1  1      -0.4080117    -0.55838868 -0.6654867 0.1705135  0.1828414
## 2  2       0.3454570    -0.09512827  0.1279713 0.6367854 -0.1103073
##         year
## 1 -0.1278678
## 2 -0.6614908
{% endhighlight %}

### Relembrando a distribuição dos grupos:

{% highlight r %}
data.scaled = data %>% 
  mutate_each(funs(as.vector(scale(.))), 2:7)
set.seed(12)
n_clusters = 4
km = data.scaled %>% 
    select(-title) %>%  
    kmeans(centers = n_clusters, nstart = 10) #Aplicando KMeans

df = data.frame(pr.out$x, title=data$title) #Aplicando resultados do PCA

p = km %>% augment(df) %>% 
    ggplot(aes(x = PC1, y = PC2, colour = .cluster)) +
        geom_point(aes(text=paste('Filme:', title)), size=1, alpha=0.8)

ggplotly(p)
{% endhighlight %}

![plot of chunk interactive](/ad1-blog/figure/source/pca-falas-femininas-em-hollywood/2017-07-19-pca-falas-femininas-em -hollywood/interactive-1.png)
O grupo 1, **#GirlPower**, tem mais personagens femininas e mais falas de personagens femininas que a média geral e isso faz com que a relação entre essas duas variáveis também fique mais positiva/maior. O grupo 2, **Mulher de enfeite** tem alta presença de personagens femininas mas estas personagens tem poucas falas se comparadas as falas masculinas. O grupo 3, **It's a Man's World** tem poucas personagens femininas e poucos diálogos vindos dessas personagens, como os dois valores são baixos a relação entre eles se torna positiva também. O grupo 4, **Populares**, difere principalmente na renda de seus filmes, é uma renda maior que a tendência do conjunto, já para valores de presença e diálogos femininos ele apresenta valores dentro da média.

### O quanto de informação é representada pelos componentes principais?

{% highlight r %}
tidy(pr.out, "pcs") %>% 
    ggplot(aes(x = PC, y = cumulative)) + 
    geom_line() + 
    geom_point() + 
    geom_text(aes(label=cumulative),hjust=0, vjust=0) +
    labs(x = "Componentes principais utilizados", 
         y = "Prop. cumulativa da variância original \n
              que esses PCs representam")
{% endhighlight %}

![plot of chunk unnamed-chunk-1](/ad1-blog/figure/source/pca-falas-femininas-em-hollywood/2017-07-19-pca-falas-femininas-em -hollywood/unnamed-chunk-1-1.png)

O gráfico acima mostra que 2 componentes principais representam 56.115% da variância do conjunto de dados. Isto significa que após a aplicação da técnica de redução de dimensionalidade PCA ao escolher dois componentes principais os mesmos terão capacidade de atribuir um pouco mais da metade da significância dos dados originais.
