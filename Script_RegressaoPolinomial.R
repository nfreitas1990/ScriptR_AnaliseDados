# Regressão Polinomial

# Regressões
# Polinomio quadrático
lm_poly <- lm(exerg_total ~ poly (dist_conf,2), data= tabela_lm) 
summary(lm_poly)

lm_poly <- lm(exerg_total ~  dist_conf + I(dist_conf^2), data= tabela_lm) 
summary(lm_poly)

curve(6825.9637 -207.0904*x + 2.3779*x^2)

plot_ly(tabela_lm, x= ~ dist_conf,
        y= ~ exerg_total,
        type="scatter")#|> 
# add_lines(x =  ~ dist_conf, y = fitted(lm_poly))

# https://anotherecoblog.wordpress.com/2020/07/02/interpretando-os-resultados-de-um-glm/
# I(dia juliano^2) foi significativo (p<2*10-16) – de modo que a relação é de fato quadrática, e não linear. Segundo que o coeficiente é negativo. Em uma equação quadrática, quando o elemento quadrático tem sinal negativo, isso significa que a curva sobe e depois desce (visualize mentalmente os valores de -x2 perto de zero para entender melhor). Se o sinal for positivo, a curva desce e depois sobe. (E confesso que precisei perguntar isso para meu amigo Marcos, que mantém este blog bemmm dahora sobre coisas matemáticas legais). Como o sinal é negativo, concluímos que a abundância de baleias é menor no começo do ano, depois sobre, atinge um pico, e desce. O que está de acordo com o que já sabemos sobre esses serzinhos lindos.