# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos


#1 # Quantos dos produtos do cadastro nunca foram comprados?

# Obtenho a lista distinta de product_ids comprados
insta_products %>%
  distinct(product_id) -> comprados

#filtro da lista de produtos aqueles que nunca foram comprados
products %>% 
  filter(!product_id %in% as_vector(comprados)) -> naocomprados

naocomprados %>%
  distinct() %>%
  count()


select(product_name, product_id)
#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 

products %>%
  left_join(aisles, by="aisle_id") %>%
  left_join(departments, by="department_id") -> combined

#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.

combined %>%
  group_by(aisle, department) %>%
  count() %>% 
  arrange(desc(n)) %>%
  head(10) -> top_dez_comprados


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

combined %>%
  group_by(aisle, department) %>%
  count() %>% 
  arrange(desc(n)) %>%
  mutate(top_ten=ifelse(n >= 874, "yes", "no")) -> t

combined%>%
  group_by(aisle_id,department_id)%>%
  count()%>%
  arrange(desc(n))%>%
  head(10) -> top_ten

products%>%  
  right_join(top_ten, by = ("aisle_id")) %>%
  inner_join(insta_products, by = ("product_id"))%>%
  distinct(order_id)%>%
  count() -> order_count

total_orders <- nrow(insta_orders)
percentage <- ((order_count/total_orders) * 100)
print(percentage)

#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

insta_products %>%
  left_join(products, by="product_id") %>%
  left_join(aisles, by="aisle_id") %>%
  left_join(departments, by="department_id") %>%
  filter(department != "missing" | aisle != "missing") -> orders_filtered_joined


#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4

insta_orders %>%
  left_join(orders_filtered_joined, by="order_id") -> order_complete

   # Transforme as variáveis user_id, department e aisle em factor
order_complete$user_id = as.factor(order_complete$user_id)
   
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)
order_complete$order_hour_of_day = factor(order_complete$order_hour_of_day, ordered=T)
   
# Este dataframe deverá ser utilizado em todas as atividades seguintes
summary(order_complete)

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
library(ggplot2)

order_complete  %>%
  select(user_id, order_id, order_hour_of_day) %>%
  group_by(order_hour_of_day) %>%
  distinct(user_id, order_id, order_hour_of_day) -> t

ggplot(t, aes(x=order_hour_of_day)) +
  geom_histogram(stat = "count" )

t %>%
  group_by(order_hour_of_day) %>%
  count(sum = n()) %>%
  arrange(desc(sum)) %>%
  head(5) -> top_hours_orders

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)

order_complete %>%
  filter(order_hour_of_day %in% as.factor(top_hours_orders$order_hour_of_day)) %>%
  group_by(product_id, product_name) %>%
  count(n = n()) %>%
  arrange(desc(n)) %>%
  head(15)

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,

order_complete %>%
  filter(order_hour_of_day %in% as.factor(top_hours_orders$order_hour_of_day))  %>%
  group_by(order_hour_of_day, product_name) %>%
  summarise(Mean = mean(product_id)) -> t2

ggplot(t2, aes(x=order_hour_of_day, y=Mean)) +
  geom_line(aes(color=product_name))


   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 


#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda


#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.


#13 # Identifique, por usuário, o tempo médio entre pedidos


#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado


#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.


#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

