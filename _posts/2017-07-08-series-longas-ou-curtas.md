---
layout: post
title: "Séries longas ou curtas?"
author: "Leticia Wanderley"
date: 2017-07-08 18:57:49
published: true
tags: [htmlwidgets, r]
---



## Problema 1 - Checkpoint 4

Nesta análise serão observados dados retirados do [imdb](http://www.imdb.com/), estes dados contém várias informações sobre as séries em questão, entre elas: nome, temporada, episódio, nota dos usuários e porcentagem de valores de notas atribuídas. A nota é atribuída pelos usuários do imdb e a porcentagem de valores de notas é quantidade de votos de 1 a 10 em relação a quantidade de votos total. 



### Existe algum ponto de virada entre as temporadas de uma série depois do qual as notas começam a diminuir/aumentar?


Ao analisar as séries escolhidas no checkpoint 1 foi possível perceber que as séries com mais temporadas tinham avaliações piores que as séries com menos temporadas. E que depois de uma certa temporada as notas das avaliações começavam a cair perceptivelmente. Dessa observação surgiu a ideia de checar se isso o mesmo acontecia com todas as séries do universo e se existia um ponto (temporada) depois do qual as notas tendiam a ser menores.


{% highlight r %}
series_median %>%
  ggplot(aes(x=reorder(season, season, FUN=median), y=mediana)) + theme_bw() +
  geom_jitter(width = .1, alpha = .6) +
  geom_boxplot(outlier.colour = NA, alpha = .6) +
  labs(title="Notas por temporada (todas as séries)", x="Temporada", y="Mediana da nota")
{% endhighlight %}

![plot of chunk boxplot](/ad1-blog/figure/source/series-longas-ou-curtas/2017-07-08-series-longas-ou-curtas/boxplot-1.png)

O boxplot acima indica um leve declínio na mediana a medida que as temporadas avançam. Também se pode ver que são as temporadas iniciais que apresentam valores muito abaixo ou muito acima da mediana.


{% highlight r %}
seasons_median %>%
  ggplot(aes(x=season, y=mediana)) + theme_bw() +
  geom_line() +
  labs(title="Mediana da nota atribuída à uma temporada", x="Temporada", y="Nota") +
  scale_x_continuous(breaks = seq(1, 9, 1))
{% endhighlight %}

![plot of chunk time](/ad1-blog/figure/source/series-longas-ou-curtas/2017-07-08-series-longas-ou-curtas/time-1.png)

É mais fácil ver o comportamento das notas se se observa um time series das nota da temporada. Aparentemente existe um pico global de notas na segunda temporada das séries e um pico local na marca da quinta temporada. Depois da quinta temporada as notas começam a diminuir, atingindo o ponto mais baixo na oitava temporada.


{% highlight r %}
moreThanFour <-
  dados %>%
  group_by(series_name) %>%
  filter(max(season) > 4)
{% endhighlight %}


Se retirarmos as séries com menos de 5 temporadas do subset podemos analisar o comportamento das medianas das temporadas sem o viés das temporadas iniciais, que tendem a ter as notas mais altas.


{% highlight r %}
mtf_series_median %>%
  ggplot(aes(x=reorder(season, season, FUN=median), y=mediana)) + theme_bw() +
  geom_jitter(width = .1, alpha = .6) +
  geom_boxplot(outlier.colour = NA, alpha = .6) +
  labs(title="Notas por temporada (séries com mais de 4 temporadas)", x="Temporada", y="Mediana da nota")
{% endhighlight %}

![plot of chunk moreThanFiveSeriesBoxplot](/ad1-blog/figure/source/series-longas-ou-curtas/2017-07-08-series-longas-ou-curtas/moreThanFiveSeriesBoxplot-1.png)

 Como esperado, a diferença entre as temporadas iniciais e as finais diminui quando se remove as séries com poucas temporadas (<5) do conjunto de dados. Isso indica que as séries com muitas temporadas recebem notas mais constantes durante suas temporadas, se observa no boxplot que a variação entre a mediana das notas das 4 primeiras temporadas é pequena, de no máximo 0.2 ponto. Ainda assim, a diminuição das notas depois da quinta temporada continua visível.


{% highlight r %}
mtf_seasons_median %>%
  ggplot(aes(x=season, y=mediana)) + theme_bw() +
  geom_line() +
  labs(title="Mediana da nota atribuída à uma temporada (séries com mais de 4 temporadas)", 
       x="Temporada", y="Nota") +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  theme(plot.title=element_text(size=12))
{% endhighlight %}

![plot of chunk moreThanFiveSeriesTimeSeries](/ad1-blog/figure/source/series-longas-ou-curtas/2017-07-08-series-longas-ou-curtas/moreThanFiveSeriesTimeSeries-1.png)

Ao observar o gráfico acima, vê se que existem dois picos de mesmo valor na mediana de cada temporada, um na segunda e outro na quinta temporada. Depois do pico da quinta temporada a mediana começa a diminuir, atingindo seu pior valor na oitava temporada e melhorando levemente na nona temporada. A remoção das séries com menos de 5 temporadas não afetou muito o resultado obtido. Apenas mostrou que as séries do conjunto que tinha poucas temporadas são mais bem avaliadas que as séries mais longas.

> Dentro do conjunto de séries analisado, existe uma tendência de que as séries recebam notas menores de seus avaliadores a partir da sexta temporada. As séries mantêm uma avaliação relativamente constante até a quinta temporada e partir daí começam a decair, recebendo notas mais baixas.

### O episódio piloto tem mais notas mínimas que o episódio final da série?

{% highlight r %}
series <- 
  dados %>%
    group_by(series_name) %>%
    filter(series_ep %in% c(min(series_ep), max(series_ep))) %>%
    arrange(series_name) %>%
    mutate(diff = ((r1 - lag(r1, default=first(r1)))*100)) %>%
    filter(diff != 0.0)
{% endhighlight %}

Esta pergunta surgiu da hipótese que os usuários avaliadores tendem a ser menos rígidos com episódios pilotos do que com episódios finais. O tempo investido na série influencia na escolha da nota quando o usuário não gosta do episódio? Os usuários ficariam com "raiva" depois de investir tempo em uma série e não gostar de seu episódio final a ponto de atribuir-lhes notas mais baixas? Os usuários são menos críticos a episódios piloto pois entendem que estes são introdutórios?


{% highlight r %}
series %>%
  ggplot(mapping = aes(x = diff, y=series_name, color=series_name)) +
  geom_point() +
  labs(title="Diferença entre a porcentagem de notas 1 do último e primeiro episódios", 
       y="", x="Diferença") +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  theme(plot.title=element_text(size=11))
{% endhighlight %}

![plot of chunk r1Diff](/ad1-blog/figure/source/series-longas-ou-curtas/2017-07-08-series-longas-ou-curtas/r1Diff-1.png)

O gráfico acima mostra o quão maior foi a porcentagem de notas 1 atribuídas ao ultima episódio de uma série comparado com a porcentagem de notas 1 atribuídas ao episódio piloto da série.


{% highlight r %}
sum(series$diff>0)
{% endhighlight %}



{% highlight text %}
## [1] 17
{% endhighlight %}



{% highlight r %}
sum(series$diff<0)
{% endhighlight %}



{% highlight text %}
## [1] 14
{% endhighlight %}

Ao analisar o gráfico se pode ver que a grande maioria das séries tem a porcentagem de notas mínimas para os episódios piloto e final muito parecidas. Porém, quando a porcentagem de notas mínimas atribuídas ao episódio final é maior ela é bem maior que a porcentagem atribuída ao episódio piloto, e no caso contrário a diferença não é tão grande. No total, 17 séries tiveram maior porcentagem de notas mínimas no último episódio, comparado ao piloto e 14 tiveram maior porcentagem de notas mínimas atribuídas ao piloto em comparação ao último episódio.

É importante notar que não existem garantia que os usuários que atribuem as notas são os mesmos para todos os episódios, por isso, a análise sendo feita se refere a posição geral dos usuários.

> No conjunto de séries analisado *não* é possível afirmar que os usuários são mais rígidos com o último episódio da série em relação ao piloto. A diferença entre a atribuição de notas mínimas foi alta em alguns casos. Porém, na grande maioria as atribuições de notas 1 foram muito semelhantes.
