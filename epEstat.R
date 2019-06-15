# Escolhendo e carregando o arquivo em .csv
data = read.csv2(file.choose(), header = T, sep = ",")

# Carregar apenas a variavel de interesse 
x = data[,21]

# Essa funcao determina se o valor eh discreto ou nao, retornando um boolean
discreto = function(valor) {
  if(round(valor) != valor) {
      return(FALSE)
  } else {
    return(TRUE)
  }
}

# Essa funcao aplica discreto() para todos os valores de um vetor
ehDiscreto = function(x) {
  i = 1
  while(i <= length(x)) {
    if(!discreto(x[i])) {
      return(FALSE)
    }
    i = i + 1
  }
  return(TRUE)
}
 
# Criando uma funcao que calcula a diferenca entre duas fdas, e retorna um double
decdf = function(y, baseline, treatment) ecdf(baseline)(y) - ecdf(treatment)(y)

# Essa funcao serve para determinar a melhor distribuicao se o vetor for continuo, e nao retorna nenhum valor
melhorDistContinua = function(x) {
  iterador = 0 # Criando um iterador para o while
  deuCerto = TRUE # Determina se um teste foi bem sucedido ou nao
  
  # METODO DE MONTE CARLO: Os testes sao repetidos diversas vezes. O numero escolhido foi 100
  
  #DISTRIBUICAO NORMAL
  contagemCertosNormal = 0 # Determina quantos testes foram bem sucedidos na dist. normal
  while(iterador < 100) {
    # A cada iteracao, eh criado um novo vetor que segue a dist. normal com a media e desvio padrao de X
    funcaoTeste = rnorm(x, mean(x), sd(x))
    
    # Eh definido um vetor de valores continuos para percorrer todos os pontos da fda
    pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
    
    # Calcula a diferenca entre todos os pontos e armazena num vetor
    diferencas = decdf(pontos, x, funcaoTeste)
    
    # Percorrendo todos os valores do vetor
    for(i in length(diferencas)) {
      # Aplicando a diferenca de Kullback Liebler
      if(abs(diferencas[i]) > 0.05) {
        # Se a distancia entre os dois valores excede um epsilon (o escolido foi 0.05)
        deuCerto = FALSE # o teste falha
        break;
      }
    }
    
    # Se o teste foi bem sucedido
    if(deuCerto) {
      # Aumenta a contagem de certos
      contagemCertosNormal = contagemCertosNormal + 1
    }
    
    # Incrementando o iterador do while
    iterador = iterador + 1
    
    # Resetando as variaveis utilizadas no escopo do while (em cada teste)
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  # Resetando as variaveis utilizadas em cada distribuicao
  iterador = 0
  deuCerto = TRUE
  
  #DISTRIBUICAO UNIFORME
  contagemCertosUniforme = 0
  while(iterador < 100) {
    funcaoTeste  = runif(length(x), min(x), max(x))
    pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
    diferencas = decdf(pontos, x, funcaoTeste)
    
    for(i in length(diferencas)) {
      if(abs(diferencas[i]) > 0.05) {
        deuCerto = FALSE
        break;
      }
    }
  
    if(deuCerto) {
      contagemCertosUniforme = contagemCertosUniforme + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  iterador = 0
  deuCerto = TRUE
  
  #DISTRIBUI??O EXPONENCIAL
  contagemCertosExponencial = 0
  while(iterador < 100) {
    funcaoTeste = rexp(length(x), rate=1/mean(x))
    pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
    diferencas = decdf(pontos, x, funcaoTeste)
    
    for(i in length(diferencas)) {
      if(abs(diferencas[i]) > 0.05) {
        deuCerto = FALSE
        break;
      }
    }
    
    if(deuCerto) {
      contagemCertosExponencial = contagemCertosExponencial + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  iterador = 0
  deuCerto = TRUE
  
  # Agora, criaremos um ranking das distribuicoes mais provaveis
  ranking = sort(c(contagemCertosNormal, contagemCertosUniforme, contagemCertosExponencial), decreasing = TRUE)
  
  indice = 1
  while(indice <= length(ranking)) {
    if(ranking[indice] == contagemCertosNormal) {
      print(c(indice,"- Normal:",contagemCertosNormal,"% de chances"), quote = F)
    } else if(ranking[indice] == contagemCertosUniforme) {
      print(c(indice,"- Uniforme:",contagemCertosUniforme,"% de chances"), quote = F)
    } else if(ranking[indice] == contagemCertosExponencial) {
      print(c(indice,"- Exponencial:",contagemCertosExponencial,"% de chances"), quote = F)
    }
    indice = indice + 1
  }
  rm(indice)
}

# Essa funcao determina a melhor distribuicao para vetores discretos, tambem nao retorna nenhum valor
melhorDistDiscreta = function(x) {
  iterador = 0 # Criando um iterador para o while
  deuCerto = TRUE # Determina se um teste foi bem sucedido ou nao
  
  #DISTRIBUICAO DE POISSON
  contagemCertosPoisson = 0
  while(iterador < 100) {
    # A cada iteracao, eh criado um novo vetor que segue a dist. de poisson com lambda = Var(x)
    funcaoTeste = rpois(length(x), var(x))
    pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)))
    diferencas = decdf(pontos, x, funcaoTeste)
    
    for(i in length(diferencas)) {
      if(abs(diferencas[i]) > 0.05) {
        deuCerto = FALSE
        break;
      }
    }
    
    if(deuCerto) {
      contagemCertosPoisson = contagemCertosPoisson + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  iterador = 0
  deuCerto = TRUE
  
  # Agora, criaremos um ranking das distribuicoes mais provaveis
  ranking = sort(c(contagemCertosPoisson), decreasing = TRUE)
  
  indice = 1
  while(indice <= length(ranking)) {
    if(ranking[indice] == contagemCertosPoisson) {
      print(c(indice,"- Poisson:",contagemCertosPoisson,"% de chances"), quote = F)
    }
    indice = indice + 1
  }
  rm(indice)
}

# Essa funcao chama as funcoes certas para cada tipo de dados do vetor x
ep = function(x) {
  hist(x, probability = T)
  lines(density(x), col="red")
  if(ehDiscreto(x)) {
    melhorDistDiscreta(x)
  } else {
    melhorDistContinua(x)
  }
  summary(x)
}
ep(x)

