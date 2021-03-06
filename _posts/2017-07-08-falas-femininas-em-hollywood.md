---
layout: post
title: "Falas femininas no cinema - Uma análise"
author: "Leticia Wanderley"
date: 2017-07-08 19:34:14
published: true
tags: [htmlwidgets, r]
---



## Agrupamento de mais dados em mais dimensões 

Neste checkpoint se tratará de dados relativos a presença feminina, na forma de falas, dentro dos scripts de longa metragens. Para isso os dados utilizados sao uma junção de um conjunto de filmes retirado do [IMDb](http://www.imdb.com/) com o conjunto de personagens e suas respectivas falas retirados dos scripts desses filmes. Os dados podem ser encontrados [neste repositorio](https://github.com/matthewfdaniels/scripts).


{% highlight r %}
roles <- read_csv("character_list5.csv")
films <- read_csv("meta_data7.csv")
merged <- left_join(films, roles, by="script_id")
merged <- select(merged, -lines_data)
merged <- na.omit(merged)
merged$age <- as.numeric(as.character(merged$age))
{% endhighlight %}

### Tratando dados

Mais especificamente as variáveis utilizadas serão: o **ano** de estreia do longa, o **índice de personagens femininas sobre a quantidade total de personagens do filme** (esse valor é retirado dos personagens presentes no script do filme), o **índice de palavras faladas por personagens femininas sobre o números total de palavras faladas no script**, a **idade mediana** dos personagens e a **renda** doméstica e ajustada (de acordo com a inflação) do filme.

Outra variável interessante que vai fazer parte da análise é a relação entre o índice de participação de personagens femininas nos diálogos dos filmes e o índice de personagens femininas. Esse valor serve para analisar se as personagens femininas tem, proporcionalmente, a mesma participação nos diálogos que as personagens masculinas. Por exemplo, se um filme tem um índice de 0.3 de personagens femininas e 0.4 de participação feminina nos diálogos, a relação entre esses valores é maior que 1, o que significa que apesar de haver poucas personagens femininas elas falam mais que as personagens masculinas, proporcionalmente.

Calculando índices de personagens femininas, de palavras faladas por personagens femininas, relação entre quantidade e palavras faladas por personagens femininas e mediana da idade.


{% highlight r %}
data <-  merged %>%
  group_by(title, year, gross) %>%
  summarise(median_age=median(age,  na.rm=TRUE),
            fem_characters=length(gender[gender=="f"])/length(gender),
            fem_words=sum(words[gender=="f"])/sum(words),
            character_words=ifelse(fem_characters!=0.0, log2(fem_words/fem_characters), -1)) %>% ungroup()

data <- na.omit(data)
{% endhighlight %}

Outra particularidade dos dados é que eles pertencem a intervalos distintos em tamanho e distribuição, a renda tem valores muitos maiores que a idade mediana, que tem valores maiores que a proporção de personagens femininas, esta por sua vez um índice entre 0 e 1. Por esse motivo é interessante normalizar os dados e colocá-los dentro de um intervalo de valores mais parecido.


### K-Means

K-Means clustering é um método de quantização vetorial que separa as _n_ instâncias de um dataset em _k_ clusters diferentes. Uma instância pertence a um cluster se, entre todos os cluster disponíveis, é aquele cluster que tem o centro com média mais próxima à média da instância. O algoritmo funciona iniciando k clusters em pontos randômicos e calculando a média das instâncias do dataset para decidir que instância tem a média mais próxima, pertence, a que centro de cluster. Depois que todas as instâncias foram alocadas se recalcula o centro dos clusters e se realiza outra iteração de análise das instâncias e sua participação nos clusters. O algoritmo pára quando os clusters não mudam ou se atinge um determinado número de iterações.
O K-Means é muito sensível a variações nos intervalos de distribuição de valores de dados, por isso é muito importante que os dados sejam normalizados. 


{% highlight r %}
set.seed(12)
n_clusters = 4
km = data.scaled %>% 
    select(-title) %>%  
    kmeans(centers = n_clusters, nstart = 10)
autoplot(km, data=data.scaled)
{% endhighlight %}

![plot of chunk unnamed-chunk-2](/ad1-blog/figure/source/falas-femininas-em-hollywood/2017-07-08-falas-femininas-em-hollywood/unnamed-chunk-2-1.png)

No caso dessa análise foi decidido se separar as instâncias em 4 clusters distintos. Essa decisão não foi completamente desinformada. Ela foi baseada na exploração de vários valores de número de clusters. O valor de betweeness/toss gráfico mostra que a distância média entre todos os pontos de um cluster para o seu centro diminui à medida que se aumenta o número de clusters, até eventualmente atingir o valor máximo, quando cada instância é o centro de um cluster. O valor escolhido, o foi porque representa a posição onde a diminuição de distância até o centro do cluster começa a crescer mais devagar.


{% highlight r %}
set.seed(12)
exploring_k = tibble(k = 1:15) %>% 
    group_by(k) %>% 
    do(
        kmeans(select(data.scaled, -title), 
               centers = .$k, 
               nstart = 20) %>% glance()
    )

exploring_k %>% 
    ggplot(aes(x = k, y = betweenss / totss)) + 
    geom_line() + 
    geom_point()
{% endhighlight %}

![plot of chunk unnamed-chunk-3](/ad1-blog/figure/source/falas-femininas-em-hollywood/2017-07-08-falas-femininas-em-hollywood/unnamed-chunk-3-1.png)

Se se plota os grupos utilizando somente as variáveis renda e a relação entre o índice de personagens femininas e índice de diálogos femininos se observa 4 grupos distintos. Um deles, o grupo 1, tem uma renda mediana dentro do conjunto e um valor geralmente positivo de relação entre presença de personagens femininas nos diálogos e presença personagens femininas no filme. O grupo 2 também apresenta uma renda mediana, mas ao contrário do grupo 1, tem uma baixa presença proporcional feminina nos diálogos de seus filmes. O grupo 3 tem alta presença de personagens femininas nos diálogos e renda, também, mediana. Já o grupo 4 apresenta maior variação, tanto de renda, quanto de presença proporcional feminina nos diálogos.


{% highlight r %}
km %>% augment(data.scaled) %>%
    ggplot(aes(x = gross, y = character_words, label = title, colour = .cluster)) +
    geom_line(alpha = .5) +
    facet_wrap(~ .cluster) +
    labs(title="Analisando renda e relação falas/personagens", x="Renda", y="Presença nos diálogos/Presença")
{% endhighlight %}

![plot of chunk unnamed-chunk-4](/ad1-blog/figure/source/falas-femininas-em-hollywood/2017-07-08-falas-femininas-em-hollywood/unnamed-chunk-4-1.png)

Os quatros grupos formados apresentam características distintas para a maioria das variáveis apresentadas. A variável mais constante entre os grupos, e a que parece menos influenciar na separação é a idade mediana. Ao se observar os filmes como linhas contínuas com pontos para cada variável vê se que o intervalo da idade mediana é semelhante em quase todos os grupos, diferindo um pouco mais no grupo 4. O ano de lançamento também parece não ter muita influência na separação dos grupos, isso pode se dever ao fato que existem filmes com temáticas e visões muito diferentes sendo lançados ao mesmo tempo.
O grupo 1, **#GirlPower**, tem mais personagens femininas e mais falas de personagens femininas que a média geral e isso faz com que a relação entre essas duas variáveis também fique mais positiva/maior. O grupo 2, **Mulher de enfeite** tem alta presença de personagens femininas mas estas personagens tem poucas falas se comparadas as falas masculinas. O grupo 3, **It's a Man's World** tem poucas personagens femininas e poucos diálogos vindos dessas personagens, como os dois valores são baixos a relação entre eles se torna positiva também. O grupo 4, **Populares**, difere principalmente na renda de seus filmes, é uma renda maior que a tendência do conjunto, já para valores de presença e diálogos femininos ele apresenta valores dentro da média.


{% highlight r %}
lines = km %>% augment(data.scaled) %>% gather(key = "variável", 
           value = "valor", 
           -title, -.cluster) 
lines %>% 
    ggplot(aes(x = `variável`, y = valor, group = title, colour = .cluster)) + 
    geom_line(alpha = .5) + 
    facet_wrap(~ .cluster) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x="", y="")
{% endhighlight %}

![plot of chunk unnamed-chunk-5](/ad1-blog/figure/source/falas-femininas-em-hollywood/2017-07-08-falas-femininas-em-hollywood/unnamed-chunk-5-1.png)
