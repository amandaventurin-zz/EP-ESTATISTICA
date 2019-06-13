# Escolhendo e carregando o arquivo em .csv
data = read.csv(file.choose(), header = T, sep = ",")

# Carregar apenas a variavel de interesse 
x = data$comprimento_petala

decdf = function(y, baseline, treatment) ecdf(baseline)(y) - ecdf(treatment)(y)

# Criando um iterador para o while
iterador = 0

# O boolean servirá para determinar se a distribuição em questão se adequa ou não ao conjunto de dados
deuCerto = TRUE

# Conta quantos testes foram bem-sucedidos para a distribuição em questão
contagemCertos = 0

# METODO DE MONTE CARLO: Os testes são repetidos diversas vezes. O numero escolhido foi 100

#DISTRIBUIÇÃO NORMAL

# A primeira dsitribuição testada é a distribuição normal
while(iterador < 100) {
  # A cada iteração, é criado um novo vetor que segue a dist. normal com a media e desvio padrao de X
  funcaoTeste = rnorm(x, mean(x), sd(x))
  
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  # Percorrendo todos os valores de cada vetor
  for(i in length(diferencas)) {
    # Aplicando a diferença de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a distância entre os dois valores excede um epsilon (o escolido foi 0.05)
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
  
  # Resetando as variáveis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos é inferior a 90%, consideramos que a distr. não é adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribuição normal não se adequa ao conjunto de dados")
} else {
  print("A distribuição normal se adequa ao conjunto de dados")
}

# Informamos ao usuário a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as variáveis utilizadas em cada distribuição
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUIÇÃO DE POISSON

while(iterador < 100) {
  # A cada iteração, é criado um novo vetor que segue a dist. de poisson com lambda = Var(x)
  funcaoTeste = rpois(length(x), var(x))
  
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  # Percorrendo todos os valores de cada vetor
  for(i in length(diferencas)) {
    # Aplicando a diferença de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a distância entre os dois valores excede um epsilon (o escolido foi 0.05)
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
  
  # Resetando as variáveis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos é inferior a 90%, consideramos que a distr. não é adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribuição de poisson não se adequa ao conjunto de dados")
} else {
  print("A distribuição de poisson se adequa ao conjunto de dados")
}

# Informamos ao usuário a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as variáveis utilizadas em cada distribuição
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUIÇÃO UNIFORME

while(iterador < 100) {
  
  funcaoTeste  = runif(length(x), min = min(x,na.rm=FALSE), max = max(x, na.rm = FALSE))
  
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  for(i in length(diferencas)) {
    # Aplicando a diferença de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a distância entre os dois valores excede um epsilon (o escolido foi 0.05)
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
  
  # Resetando as variáveis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos é inferior a 90%, consideramos que a distr. não é adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribuição uniforme não se adequa ao conjunto de dados")
} else {
  print("A distribuição uniforme se adequa ao conjunto de dados")
}

# Informamos ao usuário a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as variáveis utilizadas em cada distribuição
iterador = 0
contagemCertos = 0
deuCerto = TRUE

#DISTRIBUIÇÃO EXPONENCIAL

while(iterador < 100) {
  funcaoTeste = rexp(length(x), 1/mean(x))
 
  pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
  diferencas = decdf(pontos, x, funcaoTeste)
  
  for(i in length(diferencas)) {
    # Aplicando a diferença de Kullback Liebler
    if(abs(diferencas[i]) > 0.05) {
      # Se a distância entre os dois valores excede um epsilon (o escolido foi 0.05)
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
  
  # Resetando as variáveis utilizadas no escopo do while (em cada teste)
  funcaoTeste = NULL
  i = 0
  deuCerto = TRUE
}

# Se a porcentagem de testes bem-sucedidos é inferior a 90%, consideramos que a distr. não é adequada ao conjunto de dados
if(contagemCertos <= 90) {
  print("A distribuição exponencial não se adequa ao conjunto de dados")
} else {
  print("A distribuição exponencial se adequa ao conjunto de dados")
}

# Informamos ao usuário a porcentagem de testes bem-sucedidos
print(c(contagemCertos, "%"))

# Resetando as variáveis utilizadas em cada distribuição
iterador = 0
contagemCertos = 0
deuCerto = TRUE

