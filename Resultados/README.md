Pacotes utilizados neste RMD.

```{r}
library(rmdformats)
library(pscl)
library(MASS)
library(ggplot2)
library(GGally)
```

## Simulação dos Dados

O dataset simulado possui as seguintes variáveis:

- **fish_count**: contagem de peixes no local (numérica);
- **depth**: profundidade do local (numérica);
- **water_temp**: temperatura média da água (numérica);
- **veg_density**: densidade da vegetação aquática (numérica, entre 0 e 1);
- **pollution**: nível de poluição do local (categórica: "baixo", "médio" ou "alto");
- **predators**: presença de predadores no local (binária: 1 para presença, 0 para ausência).

### Simulação da Variável Resposta

A variável **fish_count** foi simulada de forma a incluir 500 zeros (representando valores inflacionados) e 700 valores gerados a partir de uma distribuição Poisson com parâmetro igual a 4. Essa escolha reflete um cenário de contagens inflacionadas de zeros, com a parte não nula do modelo aproximando uma média em torno de 4. 

Os modelos ajustados devem capturar essa característica, mesmo considerando o ruído adicionado pelos zeros estruturais.

### Simulação das Covariáveis

As covariáveis foram criadas considerando a separação entre locais com contagem nula (`fish_count = 0`) e não nula (`fish_count > 0`). A lógica subjacente é que essas variáveis apresentam relação com a resposta **fish_count**, mas com um componente aleatório para adicionar variabilidade.

#### Profundidade (depth)

- Para locais com contagem nula (`fish_count = 0`): a profundidade foi simulada como uma variável normal com média 5 e desvio padrão 1 ($N(5, 1)$).
- Para locais com contagem não nula (`fish_count > 0`): a profundidade foi gerada como $5 + 0.5 \ln(\text{fish_count}+1) + N(0, 0.5)$, refletindo uma relação direta, mas não perfeita, com a contagem de peixes.

#### Temperatura da Água (water_temp)

- Para locais com contagem nula (`fish_count = 0`): a temperatura foi simulada como $N(15, 2)$, representando uma temperatura média com maior variabilidade.
- Para locais com contagem não nula (`fish_count > 0`): a temperatura segue $15 + 2 \ln(\text{fish_count}+1) + N(0, 1)$, indicando um aumento na temperatura à medida que a contagem de peixes cresce.

#### Densidade da Vegetação (veg_density)

- Para locais com contagem nula (`fish_count = 0`): a densidade da vegetação foi gerada a partir de uma distribuição uniforme entre 0 e 0.4.
- Para locais com contagem não nula (`fish_count > 0`): a densidade segue uma distribuição uniforme entre 0.5 e 1, indicando maior vegetação onde há mais peixes.

#### Nível de Poluição (pollution)

- Para locais com contagem nula (`fish_count = 0`): a variável categórica foi gerada com maior probabilidade de "alto" nível de poluição ($50\%$), seguido de "médio" ($30\%$) e "baixo" ($20\%$).
- Para locais com contagem não nula (`fish_count > 0`): os níveis de poluição foram gerados com maior probabilidade de "baixo" ($90\%$), seguido de "médio" ($4.5\%$) e "alto" ($0.5\%$).

#### Presença de Predadores (predators)

- Para locais com contagem nula (`fish_count = 0`): a presença de predadores foi simulada como uma variável binária com probabilidade $30\%$.
- Para locais com contagem não nula (`fish_count > 0`): a probabilidade de predadores aumenta para $70\%$, refletindo um ambiente mais atrativo para predadores onde há maior densidade de peixes.

```{r}
set.seed(123)  # Para reprodutibilidade

# Simulando a contagem de peixes com zeros inflacionados e valores Poisson
fish <- data.frame(
  "fish_count" = c(rep(0,500), rpois(700, 4))
)

# Variáveis contínuas relacionadas a fish_count
fish$depth <- ifelse(
  fish$fish_count == 0,
  rnorm(nrow(fish), mean = 5, sd = 1),  # Profundidade média para contagens zero
  5 + 0.5 * log1p(fish$fish_count) + rnorm(nrow(fish), mean = 0, sd = 0.5) # Relacionado à contagem de peixes
)

fish$water_temp <- ifelse(
  fish$fish_count == 0,
  rnorm(nrow(fish), mean = 15, sd = 2),  # Temperatura média para contagens zero
  15 + 2 * log1p(fish$fish_count) + rnorm(nrow(fish), mean = 0, sd = 1) # Relacionado à contagem de peixes
)

# Variáveis adicionais

# Densidade de vegetação
fish$veg_density <- ifelse(
  fish$fish_count == 0,
  runif(nrow(fish), min = 0, max = 0.4),  # Menor densidade de vegetação para contagens zero
  runif(nrow(fish), min = 0.5, max = 1)   # Maior densidade de vegetação para contagens maiores
)

# Nível de poluição
fish$pollution <- ifelse(
  fish$fish_count == 0,
  sample(c("high", "medium", "low"), nrow(fish), replace = TRUE, prob = c(0.5, 0.3, 0.2)),  # Mais chance de alta poluição
  sample(c("low", "medium", "high"), nrow(fish), replace = TRUE, prob = c(0.9, 0.045, 0.005))   # Mais chance de baixa poluição
)

# Presença de predadores
fish$predators <- ifelse(
  fish$fish_count == 0,
  rbinom(nrow(fish), 1, prob = 0.3),  # Menor probabilidade de predadores onde há menos peixes
  rbinom(nrow(fish), 1, prob = 0.7)   # Maior probabilidade de predadores onde há mais peixes
)

fish$pollution <- as.factor(fish$pollution)
fish$predators <- as.factor(fish$predators)

# Visualizar os primeiros dados do dataset
head(fish)
```

## Análise Inicial dos Dados

### Distribuição da Variável Resposta

O histograma abaixo apresenta a distribuição de **fish_count**:

```{r}
hist(fish$fish_count)
```

- A variável possui uma alta concentração de zeros, sugerindo a presença de excessos de zeros nos dados.
- Esse padrão pode gerar **superdispersão**, em que a variância dos dados é significativamente maior que a média.

Podemos calcular a média e variância para confirmar nossa tese:

```{r}
mean(fish$fish_count)
var(fish$fish_count)
```

Porém, quando não consideramos os valores de 0, chegamos numa aproximação melhor.

```{r}
mean(subset(fish, fish_count != 0)$fish_count)
var(subset(fish, fish_count != 0)$fish_count)
```

Removendo os zeros, a variância e a média se tornam mais consistentes, indicando que o excesso de zeros é o principal fator gerador da superdispersão.

Essas observações sugerem o uso de modelos inflacionados de zero, como o **ZIP**.

### Matriz de Dispersão

A matriz de dispersão apresenta as relações entre **fish_count** e as covariáveis. A análise gráfica inicial indica:

```{r}
# Selecionar apenas as variáveis de interesse
fish_data <- fish[, c("fish_count", "depth", "water_temp", "veg_density", "pollution", "predators")]

# Criar os gráficos de dispersão com ggpairs
ggpairs(fish_data, 
        aes(color = as.factor(predators)),  # Colorir por uma variável binária (exemplo: `predators`)
        upper = list(continuous = wrap("points", alpha = 0.6)),  # Configurações para gráficos de dispersão
        lower = list(continuous = "smooth"))  # Adicionar linha de tendência

```

- Relações positivas de **fish_count** com **depth**, **water_temp** e **veg_density**.
- **pollution** e **predators** parecem influenciar indiretamente a contagem de peixe

Podemos observar separadamente cada covariável com a variável resposta também:

```{r}
# Gráfico de dispersão de fish_count vs. depth
ggplot(fish, aes(x = depth, y = fish_count)) +
  geom_point(alpha = 0.6) +
  labs(title = "Fish Count vs Depth")

# Gráfico de dispersão de fish_count vs. water_temp
ggplot(fish, aes(x = water_temp, y = fish_count)) +
  geom_point(alpha = 0.6) +
  labs(title = "Fish Count vs Water Temperature")

# Gráfico de dispersão de fish_count vs. veg_density
ggplot(fish, aes(x = veg_density, y = fish_count)) +
  geom_point(alpha = 0.6) +
  labs(title = "Fish Count vs Vegetation Density")

# Gráfico de dispersão de fish_count vs. pollution
ggplot(fish, aes(x = pollution, y = fish_count)) +
  geom_boxplot() +
  labs(title = "Fish Count by Pollution Level")

# Gráfico de dispersão de fish_count vs. predators
ggplot(fish, aes(x = as.factor(predators), y = fish_count)) +
  geom_boxplot() +
  labs(title = "Fish Count by Presence of Predators", x = "Predators (0 = No, 1 = Yes)")
```

## Ajuste dos Modelos

### Modelo Poisson

Foi ajustado um modelo Poisson inicial com as covariáveis disponíveis.

```{r}
# Ajustando um modelo Poisson com as variáveis depth e water_temp
poisson_model <- glm(fish_count ~ depth + water_temp + veg_density + pollution + predators, family = poisson, data = fish)

# Resumo do modelo
summary(poisson_model)
```

Podemos observar os coeficientes com a inversa da função de ligação, para saber quanto o aumento de cada covariável afeta na variável resposta. Isso será feito para cada modelo.

```{r}
exp(poisson_model$coefficients)
```

#### Modelo Poisson sem Covariáveis

Um modelo sem covariáveis também foi ajustado para avaliar o ajuste básico, servindo como comparação inicial.

```{r}
# Ajustando um modelo Poisson sem nenhuma variável
poisson_model_sem <- glm(fish_count ~ 1, family = poisson, data = fish)

# Resumo do modelo
summary(poisson_model_sem)
```



```{r}
exp(poisson_model_sem$coefficients)
```



### Modelo ZIP (Zero-Inflated Poisson)

O modelo ZIP foi ajustado utilizando todas as variáveis para a modelagem não nula e apenas duas para a modelagem nula.

```{r}
# Ajustando o modelo ZIP
zip_model <- zeroinfl(fish_count ~ depth + water_temp + veg_density + pollution + predators | depth + water_temp, 
                      data = fish, dist = "poisson")

# Resumo do modelo
summary(zip_model)
```

```{r}
exp(zip_model$coefficients$count)
```

```{r}
exp(zip_model$coefficients$zero)/(1+exp(zip_model$coefficients$zero))
```
#### Modelo ZIP sem Covariáveis

Assim como no caso Poisson, o modelo ZIP foi ajustado sem covariáveis para fins de comparação.

```{r}
# Ajustando o modelo ZIP sem covariáveis
zip_model_sem <- zeroinfl(fish_count ~ 1 | 1, 
                      data = fish, dist = "poisson")

# Resumo do modelo
summary(zip_model_sem)
```

```{r}
exp(zip_model_sem$coefficients$count)
```

```{r}
exp(zip_model_sem$coefficients$zero)/(1+exp(zip_model_sem$coefficients$zero))
```

Podemos observar os valores ajustados da média $mu$ para valores não-nulos, que sabemos que deve ser próximo de 4, e do parâmetro $p$ de cada modelo inflacionado também.

```{r}
# Obter os parâmetros estimados
fitted_mu_pois <- predict(poisson_model, type = "response")  # Média da parte Poisson 
# Média ajustada para as contagens positivas
mean(fitted_mu_pois[fish$fish_count > 0])
```

```{r}
# Obter os parâmetros estimados
fitted_mu_pois_sem <- predict(poisson_model_sem, type = "response")  # Média da parte Poisson 
# Média ajustada para as contagens positivas
mean(fitted_mu_pois_sem[fish$fish_count > 0])
```

```{r}
# Obter os parâmetros estimados
fitted_mu_zip <- predict(zip_model, type = "response")  # Média da parte Poisson (mu)
fitted_p_zip <- predict(zip_model, type = "zero")       # Probabilidade de zeros estruturais (p)
# Média ajustada para as contagens positivas
mean(fitted_mu_zip[fish$fish_count > 0])
# Média ajustada de p
mean(fitted_p_zip)
```

```{r}
# Obter os parâmetros estimados
fitted_mu_zip_sem <- predict(zip_model_sem, type = "response")  # Média da parte Poisson (mu)
fitted_p_zip_sem <- predict(zip_model_sem, type = "zero")       # Probabilidade de zeros estruturais (p)
# Média ajustada para as contagens positivas
mean(fitted_mu_zip_sem[fish$fish_count > 0])
# Média ajustada de p
mean(fitted_p_zip_sem)
```


<!-- ```{r} -->
<!-- # Calcular a probabilidade de Y = 0 -->
<!-- prob_y0 <- fitted_p + (1 - fitted_p) * exp(-fitted_mu) -->
<!-- ``` -->


<!-- ```{r} -->
<!-- # Ver probabilidades -->
<!-- head(prob_y0) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- obs_prob_y0 <- mean(fish$fish_count == 0) -->
<!-- est_prob_y0 <- mean(prob_y0) -->
<!-- c(Observed = obs_prob_y0, Estimated = est_prob_y0) -->
<!-- ``` -->

<!-- Se tiramos o 0, a média de mu prevista pelo modelo Poisson é feita corretamente! -->

<!-- ```{r} -->
<!-- # Obter os parâmetros estimados -->
<!-- fitted_mu <- predict(zip_model, type = "response")  # Média da parte Poisson (mu) -->
<!-- # Média ajustada para as contagens positivas -->
<!-- mean(fitted_mu[fish$fish_count > 0]) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- prob_y0 -->
<!-- ``` -->





<!-- ```{r} -->
<!-- fitted_mu -->
<!-- ``` -->


## Comparação de Modelos

### Critérios de Informação (AIC)

Os modelos ajustados foram comparados utilizando o AIC. O menor valor de AIC indica o melhor modelo em termos de ajuste aos dados.



```{r}
# Comparando o AIC dos modelos
AIC(zip_model, poisson_model, zip_model_sem, poisson_model_sem)
```

Dentre os modelos avaliados, os dois melhores são o ZIP e o de Poisson ambos com covariáveis, mas o ZIP ainda teve um desempenho melhor de acordo com o AIC.

Podemos confirmar isso com o teste de Vuong.

### Teste de Vuong

O teste de Vuong foi aplicado para comparar diretamente os modelos Poisson e ZIP com covariáveis.

```{r}
vuong_test <- vuong(zip_model, poisson_model)
```

Os resultados indicaram que o modelo ZIP é significativamente superior ao Poisson para os dados simulados, ao nível de confiança de $95\%$.

## Extensão: Modelos Binomial Negativa

Além dos modelos Poisson e ZIP, foram ajustados:

- **Modelo Binomial Negativa (NB)**:
  Ajusta a superdispersão adicionando um parâmetro de dispersão extra.

- **Modelo Zero-Inflated Binomial Negativa (ZINB)**:
  Combina a inflacionação de zeros com a modelagem de superdispersão.
  
Os modelos foram aplicados sem entrar muito em detalhes.

```{r}
# Ajustando um modelo Zero-Inflated Binomial Negativa (ZINB)
zinb_model <- zeroinfl(fish_count ~ depth + water_temp + veg_density + pollution + predators | depth + water_temp, 
                       data = fish, 
                       dist = "negbin")

# Resumo do modelo
summary(zinb_model)
```


```{r}
# Ajustando o modelo Binomial Negativa
nb_model <- glm.nb(fish_count ~ depth + water_temp + veg_density + pollution + predators, data = fish)

# Resumo do modelo
summary(nb_model)
```

Dentre os modelos com covariáveis, ambos os modelos inflacionados, ZIP e ZINB, tiveram melhor desempenho do que suas versões padrões. Não há grande diferença entre o modelo ZIP e o ZINB pelo AIC.

```{r}
AIC(zip_model, poisson_model, zinb_model, nb_model)
```

Podemos confirmar que o ZINB foi melhor ajustado que o modelo da Binomial Negativa pelo teste de Vuong, ao nível de confiança de $95\%$.:

```{r}
vuong_test <- vuong(zinb_model, nb_model)
```

Por fim, fazemos o teste de Vuong entre o ZIP e o ZINB:

```{r}
vuong_test <- vuong(zip_model, zinb_model)
```

Confirmamos que não há diferença significativa entre esses dois modelos, ao nível de confiança de $95\%$. Então, por critério de facilidade, escolhemos o modelo ZIP.

## Considerações Finais

- Modelos inflacionados de zeros, como ZIP e ZINB, são ferramentas poderosas para lidar com excessos de zeros nos dados.
- A escolha do modelo ideal deve levar em conta os critérios de informação (AIC, BIC), possiveis testes de hipóteses, como o teste de Vuong, seu desempenho preditivo e simplicidade do modelo.

No fim, tanto o modelo ZIP quanto o modelo ZINB poderiam ser escolhidos pelo seu desempenho. Como critério de desempate, escolhemos o modelo ZIP pela simplicidade maior deste modelo em relação ao ZINB.





