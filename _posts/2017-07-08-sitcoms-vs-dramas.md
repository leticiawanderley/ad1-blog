---
layout: post
title: "Sitcoms vs. Dramas"
author: "Leticia Wanderley"
date: 2017-07-08 18:57:52
published: true
tags: [htmlwidgets, r]
---



## Problema 1 - Checkpoint 1


As séries escolhidas foram [Modern Family](http://www.imdb.com/title/tt1442437/), [Sherlock](http://www.imdb.com/title/tt1475582/), [House of Cards](http://www.imdb.com/title/tt1856010/) e [How I Met Your Mother](http://www.imdb.com/title/tt0460649/). Modern Family e How I Met Your Mother são séries americanas no estilo sitcom, produzidas respectivamente pelos canais ABC e CBS. Ambas tem uma quantidade relativamente grande de temporadas, 7 ou mais, com aproximadamente 20 episódios de 20 minutos de duração por temporada.
Já Sherlock e House of Cards são séries de drama com quatro temporadas cada. Sherlock é produzida pelo canal britânico BBC tem 4 episódios de aproximadamente 1 hora por temporada e House of Cards é produzida, nos EUA, pelo serviço de streaming Netflix e conta com 13 episódios de aproximadamente 1 hora por temporada.

Nesta análise serão observados dados retirados do [imdb](http://www.imdb.com/), estes dados contém várias informações sobre as séries em questão, entre elas: nome, temporada, episódio e nota dos usuários. A nota é atribuída pelos usuários do imdb e será usada na análise como métrica de qualidade do episódio da série. 

### Respondendo às perguntas

#### a. Qual das séries que você escolheu é mais bem avaliada no IMDB? A diferença é grande? Pequena?


{% highlight r %}
series %>% 
  ggplot(aes(x = reorder(series_name, -UserRating, FUN=median), y = UserRating)) + theme_bw() +
  geom_boxplot(outlier.alpha = .2) +
  geom_point(data=(group_by(series, series_name) %>% summarise(UserRating=max(UserRating))), 
             aes(x=series_name, y=UserRating), colour="darkgreen", alpha=.8) +
  geom_point(data=(group_by(series, series_name) %>% summarise(UserRating=min(UserRating))), 
             aes(x=series_name, y=UserRating), colour="darkred", alpha=.8) +
  labs(x="Nome da série", y="Mediana das notas", title="Série mais bem avaliada")
{% endhighlight %}

![plot of chunk boxplot](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/boxplot-1.png)

Nos boxplots acima se pode ver que a mediana e a nota máxima da série Sherlock são as maiores do conjunto analisado, seguidas por House of Cards e depois por How I Met Your Mother e Modern Family. Quando se observa a nota mínima tem se um resultado um pouco diferente, a série com a maior nota mínima é House of Cards, é essa, também, a série que tem a menor variação entre as notas atribuídas por episódio. Já How I Met Your Mother tem a menor nota mínima do conjunto e também a maior variação de notas.

Analisando o IQR* é possível observar que as notas da série Modern Family se concentram em volta de sua mediana, 8. As notas da série House of Cards se concentram em volta de um intervalo de valores relativamente pequeno, entre 8.4 e 8.9. No caso de Sherlock e How I Met Your Mother as notas estão menos concentradas, o intervalo de valores onde se encontram 50% da notas é maior, entre 8.4 e 9.3 no caso de Sherlock e entre 7.9 e 8.6 no caso de How I Met Your Mother.

> De acordo com a maior parte das métricas analisadas __Sherlock__ é a série mais bem avaliada do subconjunto escolhido. A diferença entre as métricas, no entanto, não varia drasticamente entre as séries. Com as medianas e as médias variando dentro de um intervalo de 1 ponto, as notas máximas variando num intervalo de 0.2 ponto e as notas mínimas variando por volta de 2 pontos. Sendo assim, a diferença entre as séries pode ser considerada pequena. O que se observa nesse subconjunto específico é que as séries de drama são melhor avaliadas quando comparadas com as de comédia. Outro fator que pode contribuir para esse resultado é o fato que as séries melhor avaliadas têm uma menor quantidade de episódios por temporada, é possível que, com menos episódios com os quais se preocupar, os produtores foquem mais na qualidade. 

__IQR__ (Interquartile Range), diferença entre o terceiro e primeiro quartil das observações. é uma medida de dispersão que representam onde estão concentrados os 50% centrais dos dados. 

##### Valores


{% highlight r %}
series %>%
  group_by(Série=series_name) %>% 
  summarize(Mediana=median(UserRating),Máximo=max(UserRating),Mínimo=min(UserRating),
            IQR=IQR(UserRating),Média=mean(UserRating)) %>%
  arrange(desc(Mediana))
{% endhighlight %}



{% highlight text %}
## # A tibble: 4 x 6
##                   Série Mediana Máximo Mínimo   IQR    Média
##                   <chr>   <dbl>  <dbl>  <dbl> <dbl>    <dbl>
## 1              Sherlock     9.0    9.7    7.8  0.80 8.866667
## 2        House of Cards     8.6    9.6    7.9  0.45 8.705769
## 3 How I Met Your Mother     8.3    9.5    5.7  0.70 8.229327
## 4         Modern Family     8.0    9.5    5.9  0.40 7.989305
{% endhighlight %}

#### b. Qual das séries que você escolheu tem episódios de qualidade mais irregular segundo o IMDB? A diferença é grande? Pequena?


{% highlight r %}
series %>%
  ggplot(aes(x=series_ep, y=UserRating)) + 
  theme_bw() +
  geom_line() +
  facet_wrap(~series_name, scales="free") +
  labs(title="Variação das notas dos usuários", x="Episódio", y="Nota")
{% endhighlight %}

![plot of chunk ratings](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/ratings-1.png)

Como se pode ver nos gráficos acima todas as séries apresentam irregularidade nas notas dos episódios, os graficos sao formados por constantes vales e picos. É possível ver que os episódios iniciais das séries não tem as melhores nem as piores notas e que as notas dos episódios finais de Sherlock e How I Met Your Mother caíram com relação aos episódios anteriores. Com base no intervalo de variação a série cujas notas menos variaram foi House of Cards, variando 1.7 pontos, a série cujas notas mais variam eh How I Met Your Mother, variando 3.8 pontos.

Com base no intervalo entre a maior e a menor nota se pode dizer que How I Met Your Mother foi a série mais irregular. Porém, o gráfico dessa série mostra que não havia tanta variação até o episódio 150, a partir deste episódio a série começou a ter picos e vales bem mais proeminentes. O mesmo aconteceu com Modern Family por volta do episódio 175. Por essa razão é necessária a análise individual de cada série, por temporada.



##### Sherlock


{% highlight r %}
draw.season.ratings(sherlock, "Sherlock")
{% endhighlight %}

![plot of chunk sherlock](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/sherlock-1.png)

Com exceção da quarta temporada, o último episódio da temporada de Sherlock sempre recebe a maior nota da temporada. Sempre existe um vale no ponto central da temporada e, como observado anteriormente, o episódio inicial não é nem o melhor nem o pior da temporada. A última temporada parece ser um pouco mais distinta das três temporada anteriores, tem o comportamento mais anormal, não sendo o último episódio o melhor, e a mediana mais baixa das quatros temporadas. Se pode concluir que existe uma certa irregularidade nas notas dos episódios de todas as temporadas e a última temporada aumentou o intervalo entre as notas máxima e mínima por conter o episódio com a avaliação mais baixa.

##### House of Cards


{% highlight r %}
draw.season.ratings(hoc, "House of Cards")
{% endhighlight %}

![plot of chunk hoc](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/hoc-1.png)

A série House of Cards parece apresentar uma tendência positiva, as notas dos episódios parecem, em geral, aumentar durante a temporada. Em 50% dos caso a nota do primeiro episódio é a menor da temporada e em outros 50% a nota do último episódio é a maior da temporada. Em todas as temporadas a nota do último episódio é maior que a do penúltimo. House of Cards aparentar ser uma série com notas regulares, apresentando apenas alguns pouco picos e vales durante suas temporadas.

##### Modern Family


{% highlight r %}
draw.season.ratings(mf, "Modern Family")
{% endhighlight %}

![plot of chunk mf](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/mf-1.png)

Na série Modern Family é possível notar uma leve regularidade entre os episódios de uma temporada. Existem alguns outliers com notas muito altas e outros poucos com notas muito baixas. Porém, em geral, a série parece ser regular com notas variando ao redor de 8 pontos. Em quase todas as temporadas o último episódio tem uma nota maior que o seu antecessor. Em todas as temporadas existe um pico na metade central das notas, ou seja, sempre há um episódio muito bem avaliado no meio da temporada, o que pode ser explicado pelo fato que é comum haver um hiato separando a metade inicial da metade final da série e o episódio anterior ao hiato deve deixar a audiência investida na série para que continue a assisti-la depois da pausa.

##### How I Met You Mother


{% highlight r %}
draw.season.ratings(himym, "How I Met Your Mother")
{% endhighlight %}

![plot of chunk himym](/ad1-blog/figure/source/sitcoms-vs-dramas/2017-07-08-sitcoms-vs-dramas/himym-1.png)

Ao observar as 9 temporadas da série How I Met Your Mother é possível perceber a irregularidade dos episódios dentro de uma temporada. Apesar de existirem algumas cadeias regulares de episódios na oitava e nona temporadas, existe uma grande variação de notas, com gráficos formando picos e vales acentuados frequentemente, em todas as temporadas. Se se prestar atenção no intervale de notas das temporadas é perceptível que houve uma diminuição significativa nas notas das últimas três temporadas. Outra prova da irregularidade da série é o fato que a nona temporada contém tanto a nota mais alta quanto a mais baixa de toda a série.

> Quando se analisa o desvio padrão* das notas de usuário das séries se tem um resultado semelhante ao discutido acima. As séries mais irregulares são Sherlock e How I Met Your Mother. Sherlock tem um alto desvio em detrimento de seu pequeno intervalo entre as notas limite porque suas temporadas são compostas por pouco episódios, o que faz com que um único vale durante a temporada tenha grande peso na sua variância. How I Met Your Mother, por outro lado, tem muitos episódios por temporada mas, muito comumente, existe grande diferença entre as notas de dois episódios subsequentes. O motivo pelo qual o intervalo entre a maior e menor nota de How I Met Your Mother é grande é a baixa de qualidade que a série sofreu a partir de sua sétima temporada.

__Desvio padrão__ é uma medida de dispersão que calcula o quão longe estão os valores das observações do valor médio do conjunto. Ele indica quão espalhadas estão as observações, um baixo desvio padrão indica que os valores das observações estão próximos e um alto valor de desvio indica que as observações estão muito dispersas.

##### Valores


{% highlight r %}
group_by(series, Série=series_name) %>% 
  summarize(Desvio=sd(UserRating), 
            Intervalo=(max(UserRating)-min(UserRating))) %>%
  arrange(-Desvio)
{% endhighlight %}



{% highlight text %}
## # A tibble: 4 x 3
##                   Série    Desvio Intervalo
##                   <chr>     <dbl>     <dbl>
## 1              Sherlock 0.5665266       1.9
## 2 How I Met Your Mother 0.5597235       3.8
## 3         Modern Family 0.4028034       3.6
## 4        House of Cards 0.3907828       1.7
{% endhighlight %}
