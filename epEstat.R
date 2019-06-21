# Escolhendo e carregando o arquivo em .csv
data = read.csv(file.choose(), header = T, sep = ",")

# Carregar apenas a variavel de interesse 
x = data[,3]

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
  
  # METODO DE MONTE CARLO: Os testes sao repetidos diversas vezes. O numero escolhido foi 1000
  
  #DISTRIBUICAO NORMAL
  contagemCertosNormal = 0 # Determina quantos testes foram bem sucedidos na dist. normal
  while(iterador < 1000) {
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
  chancesDeUniforme = 0
  # Para a distribuicao uniforme, dividimos os dados (ordenados) em 10 intervalos de mesmo tamanho.
  # Para isso, utilizamos uma tabela de frequências.
  # O tamanho do intervalo é length(x)/10.
  # Contabilizamos quantos dados se encontram em cada intervalo.
  
  freq = table(x)
  a = 0
  contagens = c()
  while(a <= length(x)) {
    i = a + 1
    contagem = 0
    while(i <= a + round(length(freq)/10)) {
      contagem = contagem + freq[i]
      i = i + 1
    }
    contagens = c(contagens, contagem)
    rm(contagem)
    a = a + round(length(freq)/10)
  }
  
  # Maior dispersao indica a maior diferenca entre o numero de valores entre dois intervalos
  maiorDispersao = 0
  i = 1
  for(i in contagens) {
    j = 1
    for(j in contagens) {
      if(is.na(i) | is.na(j)) {
        next
      }
      if(abs(i - j) > maiorDispersao) {
        maiorDispersao = abs(i - j)
      }
    }
  }
  rm(i)
  rm(j)
  
  # Porcentagem da maior dispersao em relacao a quantidade de dados (vai de 0 a 100)
  porcentagemMaiorDispersao = (maiorDispersao*100)/length(x)
  
  # Consideramos que a chance de ser uma uniforme eh o complementar da porcentadem de maior dispersao
  chancesDeUniforme = 100 - porcentagemMaiorDispersao
  
  #DISTRIBUICAO EXPONENCIAL
  contagemCertosExponencial = 0
  while(iterador < 1000) {
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
  
  # DISTRIBUICAO GAMMA
  contagemCertosGamma = 0
  while(iterador < 1000) {
    funcaoTeste = rgamma(length(x), shape = mean(x)**2/var(x), scale = var(x)/mean(x))
    
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
    
    if(deuCerto) {
      contagemCertosGamma = contagemCertosGamma + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  iterador = 0
  deuCerto = TRUE
  
  #DISTRIBUIÇÃO DE WEIBULL
  contagemCertosWeibull = 0
  # Aplicamos fitdistr para descobrir os parametros
  library(MASS)
  
  parametroShape = as.double(fitdistr(x, "weibull")$estimate[1])
  parametroScale = as.double(fitdistr(x, "weibull")$estimate[2])
  
  while(iterador < 1000) {
    funcaoTeste = rweibull(x, parametroShape, parametroScale)
    
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
      contagemCertosWeibull = contagemCertosWeibull + 1
    }
    
    # Incrementando o iterador do while
    iterador = iterador + 1
    
    # Resetando as variáveis utilizadas no escopo do while (em cada teste)
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  #DISTRIBUIÇÃO POWERLAW
  
  y = sort(x)
  z = 1 - ecdf(x)(sort(x))
  
  y = y[1:length(y)-1]
  z = z[1:length(z)-1]
  
  a = log(y,10)
  b = log(z,10)
  
  # a chance de a dist ser uma power law eh a correlacao linear entre a e b
  chancesDePowerlaw = abs(cor(a, b))*100
  
  #DISTRIBUICAO LOG-NORMAL
  contagemCertosLogNormal = 0 # Determina quantos testes foram bem sucedidos na dist. normal
  varlog = 0
  meanlog = 0
  iterador = 0
  deuCerto = TRUE
  
  varlog = log(((var(x)/(mean(x)**2))+1), exp(1))
  meanlog = log((mean(x)), exp(1)) - varlog/2
  
  while(iterador < 1000) {
    # A cada iteracao, eh criado um novo vetor que segue a dist. normal com a media e desvio padrao de X
    funcaoTeste = rlnorm(x, meanlog, sqrt(varlog))
    
    # Eh definido um vetor de valores continuos para percorrer todos os pontos da fda
    pontos = seq(from=max(min(funcaoTeste), min(x)), to=min(max(x), max(funcaoTeste)), by=0.01)
    
    # Calcula a diferenca entre todos os pontos e armazena num vetor
    diferencas = decdf(pontos, rlnorm(x, meanlog, sqrt(abs(varlog))), funcaoTeste)
    
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
      contagemCertosLogNormal = contagemCertosLogNormal + 1
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
  
  # Agora, criaremos um ranking das distribuicoes mais provaveis
  ranking = sort(c(contagemCertosNormal, chancesDeUniforme*10, contagemCertosExponencial, contagemCertosGamma, contagemCertosWeibull, chancesDePowerlaw*10, contagemCertosLogNormal), decreasing = TRUE)
  
  normal = FALSE
  uniforme = FALSE
  exponencial = FALSE
  gamma = FALSE
  weibull = FALSE
  powerLaw = FALSE
  lognormal = FALSE
  
  indice = 1
  while(indice <= length(ranking)) {
    if(ranking[indice] == contagemCertosNormal & !normal) {
      print(c(indice,"- Normal:",format(contagemCertosNormal/10, digits =2, nsmall = 0, decimal.mark = "."),"% de chances"), quote = F)
      normal = TRUE
    } else if(ranking[indice] == chancesDeUniforme*10 & !uniforme) {
      print(c(indice,"- Uniforme:",format(chancesDeUniforme, digits =2, nsmall = 0, decimal.mark = "."),"% de chances"), quote = F)
      uniforme = TRUE
    } else if(ranking[indice] == contagemCertosExponencial & !exponencial) {
      print(c(indice,"- Exponencial:",format(contagemCertosExponencial/10, digits =2, nsmall = 0, decimal.mark = "."),"% de chances"), quote = F)
      exponencial = TRUE
    } else if(ranking[indice] == contagemCertosGamma & !gamma) {
      print(c(indice, "- Gamma:", format(contagemCertosGamma/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      gamma = TRUE
    } else if(ranking[indice] == contagemCertosWeibull & !weibull) {
      print(c(indice, "- Weibull:", format(contagemCertosWeibull/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      weibull = TRUE
    } else if(ranking[indice] == chancesDePowerlaw*10 & !powerLaw) {
      print(c(indice, "- PowerLaw:", format(chancesDePowerlaw, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      powerLaw = TRUE
    } else if(ranking[indice] == contagemCertosLogNormal & !lognormal) {
      print(c(indice, "- Lognormal:", format(contagemCertosLogNormal/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      lognormal = TRUE
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
  while(iterador < 1000) {
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
  
  #DISTRIBUICAO GEOMETRICA
  contagemCertosGeometrica = 0
  while(iterador < 1000) {
    funcaoTeste = rgeom(length(x), 1/mean(x))
    
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
    
    if(deuCerto) {
      contagemCertosGeometrica = contagemCertosGeometrica + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  iterador = 0
  deuCerto = TRUE
  
  #DISTRIBUICAO BINOMIAL
  contagemCertosBinomial = 0
  while(iterador < 1000) {
    funcaoTeste = rbinom(length(x), round((mean(x)*mean(x))/(mean(x) - var(x))), (mean(x) - var(x))/mean(x))
    
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
    
    if(deuCerto) {
      contagemCertosBinomial = contagemCertosBinomial + 1
    }
    
    iterador = iterador + 1
    
    funcaoTeste = NULL
    i = 0
    deuCerto = TRUE
  }
  
  # Agora, criaremos um ranking das distribuicoes mais provaveis
  ranking = sort(c(contagemCertosPoisson, contagemCertosGeometrica, contagemCertosBinomial), decreasing = TRUE)
  
  poisson = FALSE
  geometrica = FALSE
  binomial = FALSE
  
  indice = 1
  while(indice <= length(ranking)) {
    if(ranking[indice] == contagemCertosPoisson & !poisson) {
      print(c(indice,"- Poisson:",format(contagemCertosPoisson/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      poisson = TRUE
    } else if(ranking[indice] == contagemCertosGeometrica & !geometrica) {
      print(c(indice, "- Geometrica:", format(contagemCertosGeometrica/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      geometrica = TRUE
    } else if(ranking[indice] == contagemCertosBinomial & !binomial) {
      print(c(indice, "- Binomial:", format(contagemCertosBinomial/10, digits =2, nsmall = 0, decimal.mark = "."), "% de chances"), quote = F)
      binomial = TRUE
    }
    indice = indice + 1
  }
  rm(indice)
}

# Essa funcao chama as funcoes certas para cada tipo de dados do vetor x
ep = function(x) {
  plot.ecdf(x)
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
