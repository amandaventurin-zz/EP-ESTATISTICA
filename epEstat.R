# Escolhendo e carregando o arquivo em .csv
data = read.csv(file.choose(), header = T, sep = ",")

# Carregar apenas a variavel de interesse 
x = data$comprimento_petala

# Criando uma funcao que calcula a diferenca entre duas fdas
decdf = function(y, baseline, treatment) ecdf(baseline)(y) - ecdf(treatment)(y)

# Criando um iterador para o while
iterador = 0

# O boolean determina se a distribuicao em questao se adequa ou nao ao conjunto de dados
deuCerto = TRUE

# Conta quantos testes foram bem-sucedidos para a distribuicao em questao
contagemCertos = 0

# METODO DE MONTE CARLO: Os testes sao repetidos diversas vezes. O numero escolhido foi 100

#DISTRIBUICAO NORMAL

# A primeira dsitribuicao testada eh a distribuicao normal
while(iterador < 100) {
  # A cada iteracao, eh criado um novo vetor que segue a dist. normal com a media e desvio padrao de X
  funcaoTeste = rnorm(x, mean(x), sd(x))
  
  # 
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  # Percorrendo todos os valores de cada vetor
  for(i in length(diferencas)) {
    # Aplicando a diferen?a de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a dist?ncia entre os dois valores excede um epsilon (o escolido foi 0.05)
      deuCerto = FALSE # o teste falha
      break;
    }
  }
  
  # Se o teste foi bem sucedido
  if(deuCerto) {
    # Aumenta a contagem de certos
    contagemCertos = contagemCertos + 1
  }
  
  # Incrementando o iterador do while
  iterador = iterador + 1
  
  # Resetando as vari?veis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos ? inferior a 90%, consideramos que a distr. n?o ? adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribui??o normal n?o se adequa ao conjunto de dados")
} else {
  print("A distribui??o normal se adequa ao conjunto de dados")
}

# Informamos ao usu?rio a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as vari?veis utilizadas em cada distribui??o
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUI??O DE POISSON

while(iterador < 100) {
  # A cada itera??o, ? criado um novo vetor que segue a dist. de poisson com lambda = Var(x)
  funcaoTeste = rpois(length(x), var(x))
  
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  # Percorrendo todos os valores de cada vetor
  for(i in length(diferencas)) {
    # Aplicando a diferen?a de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a dist?ncia entre os dois valores excede um epsilon (o escolido foi 0.05)
      deuCerto = FALSE # o teste falha
      break;
    }
  }
  
  # Se o teste foi bem sucedido
  if(deuCerto) {
    # Aumenta a contagem de certos
    contagemCertos = contagemCertos + 1
  }
  
  # Incrementando o iterador do while
  iterador = iterador + 1
  
  # Resetando as vari?veis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos ? inferior a 90%, consideramos que a distr. n?o ? adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribui??o de poisson n?o se adequa ao conjunto de dados")
} else {
  print("A distribui??o de poisson se adequa ao conjunto de dados")
}

# Informamos ao usu?rio a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as vari?veis utilizadas em cada distribui??o
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUI??O UNIFORME

while(iterador < 100) {
  
  funcaoTeste  = runif(length(x), min(x), max(x))
  
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  for(i in length(diferencas)) {
    print(i)
    print(diferencas[i])
    # Aplicando a diferen?a de Kullback Liebler
    if(abs(diferencas[i]) != 0) {
      # Se a dist?ncia entre os dois valores excede um epsilon (o escolido foi 0.05)
      deuCerto = FALSE # o teste falha
      break;
    }
  }
  
  # Se o teste foi bem sucedido
  if(deuCerto) {
    # Aumenta a contagem de certos
    contagemCertos = contagemCertos + 1
  }
  
  # Incrementando o iterador do while
  iterador = iterador + 1
  
  # Resetando as vari?veis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos ? inferior a 90%, consideramos que a distr. n?o ? adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribui??o uniforme n?o se adequa ao conjunto de dados")
} else {
  print("A distribui??o uniforme se adequa ao conjunto de dados")
}

# Informamos ao usu?rio a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as vari?veis utilizadas em cada distribui??o
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUI??O EXPONENCIAL
while(iterador < 100) {
  funcaoTeste = rexp(length(x), rate=1/mean(x))
 
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  for(i in length(diferencas)) {
    # Aplicando a diferen?a de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a dist?ncia entre os dois valores excede um epsilon (o escolido foi 0.05)
      deuCerto = FALSE # o teste falha
      break;
    }
  }
  
  # Se o teste foi bem sucedido
  if(deuCerto) {
    # Aumenta a contagem de certos
    contagemCertos = contagemCertos + 1
  }
  
  # Incrementando o iterador do while
  iterador = iterador + 1
  
  # Resetando as vari?veis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos ? inferior a 90%, consideramos que a distr. n?o ? adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribui??o exponencial n?o se adequa ao conjunto de dados")
} else {
  print("A distribui??o exponencial se adequa ao conjunto de dados")
}

# Informamos ao usu?rio a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as vari?veis utilizadas em cada distribui??o
iterador = 0
contagemCertos = 0
deuCerto = TRUE

