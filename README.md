# Estatistica-Descritiva
# ##### An?lise Estat?stica do Banco mtcars do R4.0 ############################### ######################################
# ##### Objetivo: An?lise do Consumo M?dio de Combus?vel de Carros Modelos 1973/1974
# ##### Descrição: o conjunto de dados, denominado de mtcars, foi obtido a partir das edições de março, abril, junho e julho de 1974 da revista Motor Trend para um estudo realizado por Hocking (1976) e posteriormente, reportado por Henderson e Velleman (1981)

# ###### INTRODUÇÃO ########################################### ############################################
# Na crise do petróleo fera de 1973, membros da Organização dos Países Árabes Exportadores de Petróleo (OPAEP) aplicaram sanções em protesto ao apoio dos Estados Unidos e outras nações ? Israel durante a Guerra do Yom Kippur.
# O conflito resultou no aumento do preço do petróleo de tr?sd?lares por barril para cerca de 12 dólares no mundo inteiro, sendo que os preços fixados para os Estados Unidos ainda eram maiores.
# Como uma alternativa ? alta do preço do petróleo no mercado mundial, os Estados Unidos iniciaram um programa de eficiência energética, conhecido como Corporate Average Fuel Economy (CAFE), com o propósito de reduzir o consumo de combustível de carros, pick-ups, minivans e SUVs (Almeida Filho, 2018)
# O conjunto de dados, denominado de mtcars,est? disponivel na biblioteca datasets do software R para consulta.

# ################################################## ################################################### ######


# ##### Carregar os Pacotes ########################################## ###################################
biblioteca ( arrumado )
biblioteca ( GGally )
biblioteca( caboExtra )
biblioteca ( ggplot2 )
biblioteca ( enredo )
biblioteca ( gridExtra )
biblioteca( ggcorrplot )
biblioteca ( gsummary )
biblioteca ( tabela x )
biblioteca ( PerformanceAnalytics )
biblioteca ( readxl )
biblioteca ( corrplot )
biblioteca ( Hmisc )
biblioteca ( PerformanceAnalytics )
# ################################################## ################################################### ###


# ##### ANALISE EXAPLORATORIA DE DADOS #################################### #############################
# ##### As vari?veis observadas no conjunto de dados s?o definidos como:
# Variável Resposta (Y):
#   1) mpg: eficiencia (milhas por gal?o de combustivel).
# Variáveis ​​Explicativas(X):
#   1) cyl: nômero de cilindros.
#   2) disp: cilindradas (polegada c?bica).
#   3) hp: potência bruta (HP).
#   4) drat: relação de eixo traseiro.
#   5) peso: peso (1000 libras).
#   6) qsec: tempo no quarto de milha (segundos).
#   7) vs: formato do motor (0 = V e 1 = linha).
#   8) am: tipo de transmissão (0 = automático e 1 = manual).
#   9) marcha: nômero de marchas para frente.
# 10) carb: nômero de carburadores.
# ################################################## ################################################### ##


# ########### Estatística Descritiva ##################################### ##############################
cabeça ( mtcars )
resumo( mtcars )

# ##### Resumo tabular ############################################ ######################################
resumo  <-  mtcars % > %
  select( - am , - vs ) % > %
  pivot_longer(everything()) % > %
  group_by( nome ) % > %
  resumir_at( " valor " ,
               lista ( Faltando  = ~ sum(is.na( . )), media = ~ mean( . ),
                    desvPad = ~ sd( . ), minimo = ~ min( . ),
                    Q1 = ~ quantil( . , 0,25 ), med = ~ mediana( . ),
                    Q3 = ~ quantil( . , 0,75 ), maxi = ~ max( . ))) % > %
  mutate_if( is.numeric , format , digits = 3 , nsmall  =  2 )

colnames( resumo ) <- c( ' Vari?vel ' , ' Faltando ' , ' M?dia ' ,
                      ' Desvio padro ' , ' M?nimo ' , ' Q1 ' ,
                      ' Mediana ' , ' Q3 ' , ' Máximo ' )
kbl( resumo , booktabs  =  T , caption  =  ' Estat?sticas descritivas daS Caracter?sticas Quantitativas ' , longtable  =  T ) % > %
  kable_styling( position  =  ' center ' , latex_options  = c( " listrado " , " hold_position " ))
# ################################################## ##################################################


# ##### Correlograma das variáveis ​​explicativas ######################################

# Matriz de Correlação
cor( mtcars )

# Gr?fico da Matriz de Correla??o
correl  = cor( mtcars )
ggcorrplot( correl )


# Grafico Customizado
# method = "quadrado" (padrão)
# method = estilos (círculo)

# hac.order = agrupamento hierqrquico
# type = esconder espelhamento
  # tipo = superior, inferior
# lab = adicionar o valor da correlação
# lab_size = ajustar a fonte ao tamanho
# p.mat = excluindo o coeiciente não significativo
# insig = "blank" deixar em branco coef não significativo


ggcorrplot( correl ,
           método  =  " quadrado " ,
           hc.order  =  TRUE ,
           tipo  =  " inferior " ,
           laboratório  =  VERDADEIRO ,
           lab_size  =  3.0 ,
           cores = c( " tijolo refratário " , " branco " , " dodgerblue4 " ),
           contorno.cor  =  " branco " ,
           title  =  " Matriz de Correlação " ,
           ggtheme  = theme_gray()
)



# Teste t para Corelação
# P-valor das correlações
cor_pmat( mtcars )

# Gráfico da Matriz de P-valor
ggcorrplot( correl ,
           método  =  " quadrado " ,
           hc.order  =  TRUE ,
           tipo =  " inferior " ,
           laboratório  =  VERDADEIRO ,
           lab_size  =  3.0 ,
           p.mat  = cor_pmat( mtcars ),
           cores = c( " tijolo refratário " , " branco " , " dodgerblue4 " ),
           contorno.cor  =  " branco " ,
           title  =  " Matriz de Correlação " ,
           ggtheme  =  theme_gray
)




mtcars % > %
  select( - vs , - am , - mpg ) % > %
  ggpairs()
# ################################################## ##################################



# ##### Gráfico de Dispers?o ####################################### ###
# ##### Gráfico 1 ########################################### ###########
fig1  <-  mtcars % > %
  ggplot(aes( x = cil , y = mpg )) +
  geom_point() +
  labs( x  =  ' N?mero de cilindros ' , y  =  ' Efici?ncia (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
Figura 1
# ################################################## ####################

# ##### Grafico 2 ######################################## #############
fig2  <-  mtcars % > %
  ggplot(aes( x = disp , y = mpg )) +
  geom_point() +
  labs( x  =  ' Cilindradas (in^3) ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
Figura 2
# ################################################## ####################

# ##### Grafico 3 ######################################### #############
fig3  <-  mtcars % > %
  ggplot(aes( x = hp , y = mpg )) +
  geom_point() +
  labs( x  =  ' Potência (HP) ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig3
# ################################################## ####################


# ##### Grafico 4 ######################################## #############
fig4  <-  mtcars % > %
  ggplot(aes( x = drat , y = mpg )) +
  geom_point() +
  labs( x  =  ' Relação de Eixo T ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig4
# ################################################## ####################

# ##### Grafico 5 ######################################### #############
fig5  <-  mtcars % > %
  ggplot(aes( x = wt , y = mpg )) +
  geom_point() +
  labs( x  =  ' Peso (1000 lb) ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig5
# ################################################## ####################


# ##### Grafico 6 ######################################## #############
fig6  <-  mtcars % > %
  ggplot(aes( x = qsec , y = mpg )) +
  geom_point() +
  labs( x  =  ' Tempo(s) ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig6
# ################################################## ####################

# ##### Grafico 7 ######################################## #############
fig7  <-  mtcars % > %
  ggplot(aes( x = engrenagem , y = mpg )) +
  geom_point() +
  labs( x  =  ' N?mero de Marchas ' , y  =  ' Efici?ncia (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig7
# ################################################## ####################

# ##### Grafico 8 ######################################## #############
fig8  <-  mtcars % > %
  ggplot(aes( x = carb , y = mpg )) +
  geom_point() +
  labs( x  =  ' Número de Carburadores ' , y  =  ' Eficiência (mpg) ' ) + 
  geom_smooth( método  =  lm , se  =  FALSE )
fig8
# ################################################## ####################

# ##### Gr?fico Agrupado ######################################### #############

grid.arrange( fig2 , fig3 , fig4 , fig5 , fig6 , ncol  =  3 , nrow  =  2 )
grid.arrange( fig1 , fig2 , fig3 , fig4 , fig5 , fig6 , fig7 , fig8 , ncol  =  3 , nrow  =  3 )
# ################################################## ####################



# ##### Gr?fico de Dispers?o com Pontos Estratificados ################################
# ##### Gr?fico 2 ######################################## #############
ffig2  <-  mtcars % > %
  ggplot(aes( x = disp , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Cilindradas (in^3) ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )
fig2
# ################################################## ####################


# ##### Gr?fico 3 ######################################### #############
ffig3  <-  mtcars % > %
  ggplot(aes( x = hp , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Potência (HP) ' , y  =  ' Eficiência (mpg) ' ) +
  tema( lenda.posição  =  ' nenhum ' )
fig3
# ################################################## ####################


# ##### Gr?fico 4 ######################################## #############
ffig4  <-  mtcars % > %
  ggplot(aes( x = drat , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Relação do eixo traseiro ' , y  =  ' Eficiência (mpg) ' ) +
  tema( lenda.posição  =  ' nenhum ' )
fig4
# ################################################## ####################


# ##### Gr?fico 5 ######################################### #############
ffig5  <-  mtcars % > %
  ggplot(aes( x = wt , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Peso (1000 lb) ' , y  =  ' Eficiência (mpg) ' ) +
  tema( lenda.posição  =  ' nenhum ' )
fig5
# ################################################## ####################

# ##### Gr?fico 6 ######################################## #############
ffig6  <-  mtcars % > %
  ggplot(aes( x = qsec , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  # geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Tempo(s) ' , y  =  ' Eficiência (mpg) ' ) +
  tema( lenda.posição  =  ' nenhum ' )
# ################################################## ####################

# ##### Gr?fico 7 ######################################## #############
ffig7  <-  mtcars % > %
  ggplot(aes( x = engrenagem , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' N?mero de marchas ' , y  =  ' Efici?ncia (mpg) ' ) +
  tema( lenda.posição  =  ' nenhum ' )
fig7
# ################################################## ####################


# ##### Gr?fico 8 ######################################## #############
ffig8  <-  mtcars % > %
  ggplot(aes( x = carb , y = mpg )) +
  geom_point(aes( cor  =  fator ( cil ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( cyl )), se  =  FALSE ) +
  labs( x  =  ' Número de carburadores ' , y  =  ' Eficiência (mpg) ' )
fig8
# ################################################## ####################
# ###### Gr?fico de dispers?o com pontos estratificados pelo (N?mero de Cilindros)

grid.arrange( ffig2 , ffig3 , ffig4 , ffig5 , ffig6 , ffig7 , ffig8 ,
             ncol  =  2 , nrow  =  4 )
# ################################################## ################################

# ##### Gr?fico de Dispers?o com pontos Estratificados ############################
ffig1  <-  mtcars % > %
  ggplot(aes( x = cil , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' N?mero de cilindros ' , y  =  ' Efici?ncia (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig2  <-  mtcars % > %
  ggplot(aes( x = disp , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Cilindradas (in^3) ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig3  <-  mtcars % > %
  ggplot(aes( x = hp , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Potência (HP) ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig4  <-  mtcars % > %
  ggplot(aes( x = drat , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Relação do eixo traseiro ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig5  <-  mtcars % > %
  ggplot(aes( x = wt , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Peso (1000 lb) ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig6  <-  mtcars % > %
  ggplot(aes( x = qsec , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  # geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Tempo(s) ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig7  <-  mtcars % > %
  ggplot(aes( x = engrenagem , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' N?mero de marchas ' , y  =  ' Efici?ncia (mpg) ' ) + 
  tema( lenda.posição  =  ' nenhum ' )

ffig8  <-  mtcars % > %
  ggplot(aes( x = carb , y = mpg )) +
  geom_point(aes( cor  =  fator ( vs ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( vs )), se  =  FALSE ) +
  labs( x  =  ' Número de carburadores ' , y  =  ' Eficiência (mpg) ' ) + 
  tema( legenda.posição  =  ' topo ' )

# ################################################## ################################
# Gr?fico de dispers?o com pontos estratificados pelo (Formato do Motor)
grid.arrange( ffig1 , ffig2 , ffig3 , ffig4 , ffig5 , ffig6 , ffig7 , ffig8 ,
             ncol  =  2 , nrow  =  4 )
# ################################################## ##################################


# ##### Gr?fico de dispers?o com pontos estratificados ################################
# Gr?fico de dispers?o com pontos estratificados pelo (Tipo de Transmiss?o)

ffig1  <-  mtcars % > %
  ggplot(aes( x = cil , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' N?mero de cilindros ' , y  =  ' Efici?ncia (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig2  <-  mtcars % > %
  ggplot(aes( x = disp , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Cilindradas (in^3) ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig3  <-  mtcars % > %
  ggplot(aes( x = hp , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Potência (HP) ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig4  <-  mtcars % > %
  ggplot(aes( x = drat , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Relação do eixo traseiro ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig5  <-  mtcars % > %
  ggplot(aes( x = wt , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Peso (1000 lb) ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig6  <-  mtcars % > %
  ggplot(aes( x = qsec , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  # geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Tempo(s) ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig7  <-  mtcars % > %
  ggplot(aes( x = engrenagem , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' N?mero de marchas ' , y  =  ' Efici?ncia (mpg) ' ) +  
  tema( lenda.posição  =  ' nenhum ' )

ffig8  <-  mtcars % > %
  ggplot(aes( x = carb , y = mpg )) +
  geom_point(aes( cor  =  fator ( am ))) +
  geom_smooth( method  =  lm , aes( color  =  factor ( am )), se  =  FALSE ) +
  labs( x  =  ' Número de carburadores ' , y  =  ' Eficiência (mpg) ' ) +  
  tema( legenda.posição  =  ' topo ' )

# Gr?fico de dispers?o com pontos estratificados pelo tipo de transmiss?o
grid.arrange( ffig1 , ffig2 , ffig3 , ffig4 , ffig5 , ffig6 , ffig7 , ffig8 ,
             ncol  =  2 , nrow  =  4 )
# ################################################## ###############################


# ##### Boxplot ############################################ #######################
fig9  <-  mtcars % > %
  ggplot(aes( x = as.factor( cyl ), y = mpg )) +
  geom_boxplot() +
  labs( x  =  ' N?mero de cilindros ' , y  =  ' Efici?ncia (mpg) ' )
fig9
# ################################################## ##############################

fig10  <-  mtcars % > %
  ggplot(aes( x = as.factor( vs ), y = mpg )) +
  geom_boxplot() +
  labs( x  =  ' Formato do motor ' , y  =  ' Eficiência (mpg) ' )

fig11  <-  mtcars % > %
  ggplot(aes( x = as.factor( am ), y = mpg )) +
  geom_boxplot() +
  labs( x  =  ' Tipo de transmissão ' , y  =  ' Eficiência (mpg) ' )

fig12  <-  mtcars % > %
  ggplot(aes( x = as.factor( engrenagem ), y = mpg )) +
  geom_boxplot() +
  labs( x  =  ' N?mero de marchas ' , y  =  ' Efici?ncia (mpg) ' )

fig13  <-  mtcars % > %
  mutate( carb_novo = ifelse( carb < = 2 , 0 , 1 )) % > %
  ggplot(aes( x = as.factor( carb_novo ), y = mpg )) +
  geom_boxplot() +
  labs( x  =  ' Número de carburadores ' , y  =  ' Eficiência (mpg) ' )

grid.arrange( fig10 , fig11 , ncol  =  2 , nrow  =  1 )
# ################################################## ##########################

# ################################################## ##################
# ######## Uma diversão??o select() ? utilizado para selecionar as variáveis ​​de interesse
novo = select( mtcars , mpg , cyl )

# No exemplo s?o selecionados todas as vari?veis mpg excluídos:
novo = select( mtcars , - c( mpg ))

# ? possvel selecionar uma sequncia de variveis a partir de seus nomes
novo = select( mtcars , cyl : drat )


# ####### A Fun??o filter() seleciona as vari?veis da base de dados
novo = filtro( mtcars , hp > 146 )

# mais de um critrio de filtrao de
novo = filtro( mtcars , hp > 146  &  am == 1 )

# ###### Uma diversão??o mutate() ? utilizado para incluir informações ou variáveis ​​na base de dados
novo = mutate( mtcars , novacol = ( mpg * 100 ))

# ###### Uma diversão??o summarise() ? uma ferramenta poderosa para agregar sumariza??es unindo diversos c?lculos ao longo de uma base de dados
resumo( mtcars ,
          media.hp = média( hp ),
          qtd.hp = comprimento( hp ),
          qtdunico.hp = length(unique( hp )))

# ##### Ainda, ? possível agrupar as informações com a diversão group_by() ao mesmo tempo em que só desejamos c?lculos adjacentes
resumir(grupo_por( mtcars , cyl.agrup = cyl ),
          hp.medio = média( hp ),
          wt.medio = média( wt ),
          qtd = n())

# ##### Uma diversão??o count() ? utilizado para resumir a contagem de determinados objetos dentro de uma variável do banco de dados
contagem( mtcars , cyl )

# ##### A fun??o provide() ordena a base de dados de acordo com o ordenamento da vari?vel escolhida
novo = arranjar( mtcars , cyl )

# ##### Ainda ? possível indicar mais de uma variável para este ordenamento, bem como utilizar a função desc() para organizar em ordem decrescente:
novo = arranjar( mtcars , mpg , desc( disp ))
cabeça
# ################################################## #################

# ##### O operador pipe (s?mbolos %>%) #############################
# Abaixo segue um exemplo, onde o objetivo ? filtrar os
# veículos com transmissão manual (am == 1), agrupando-os pela voz
# de cilindros ("cyl") e em seguida retomando am?dia das vari?veis "drat" e
# "hp" para cada agrupamento:

novo  =  mtcars % > %
  filtro( am  ==  1 ) % > %
  group_by( cil ) % > %
  resumo( disp.drat = média( drat ),
            hp.media = média( hp ))
novo

# ################################################## #############################


# ########## Pacote ggplot2 + plotly ################################### ###########
# Banco de Dados, Motor Trend Car Road Tests (mtcars)

# A diversão aes() descreve como as variáveis ​​são mapeadas em aspectos visuais de formas geométricas definidas por geoms
ggplot( dados  =  mtcars , aes( x  =  disp , y  =  mpg )) + 
  geom_point()

# Agora, a vari?vel am (tipo de transmiss?o) foi mapeada ? cor dos pontos, sendo que pontos vermelhos correspondem ? transmissão automática (valor 0) e pontos azuis ? transmissão manual (valor 1)
ggplot( data  =  mtcars , aes( x  =  disp , y  =  mpg , color  = as.factor( am ))) + 
  geom_point()

# No entanto, também podemos mapear uma vari?vel cont?nua ? cor dos pontos
ggplot( mtcars , aes( x  =  disp , y  =  mpg , color  =  cyl )) + 
  geom_point()


# ###### Tamb?m podemos mapear o tamanho dos pontos ? uma variável de nível de interesse:
ggplot( mtcars , aes( x  =  disp , y  =  mpg , color  =  cyl , size  =  wt )) +
  geom_point()

# ##### Facetas ############################################ ####
fig1 <- ggplot( mtcars , aes( x  =  mpg , y  =  disp , color  = as.factor( cyl ))) + 
  geom_point() + 
  facet_grid( . ~ am )
ggplotly()
# ################################################## ###########
