# Escolhendo e carregando o arquivo em .csv
data = read.csv(file.choose(), header = T, sep = ",")

# Carregar apenas a variavel de interesse 
x = data$largura_petala


#DISTRIBUICAO LOG-NORMAL
contagemCertosLogNormal = 0# Determina quantos testes foram bem sucedidos na dist. normal
varlog = 0
meanlog = 0
iterador = 0
deuCerto = TRUE

varlog = log(((var(x)/(mean(x)**2))+1), exp(1))
meanlog = log((mean(x)), exp(1)) - varlog/2

decdf = function(y, baseline, treatment) ecdf(baseline)(y) - ecdf(treatment)(y)

while(iterador < 100) {
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

