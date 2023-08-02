#faz a soma de 2 mais dois
#o colchete, represent O vetor
#pra rodar um script de R, aperta ctrl + enter
2 + 2 
#operaçao de subtraçao
2 -2
#operaçao de divisao
2/2
#operaçao de multiplicaçao
2 *2 
#potencialciaçao
2**2
2^2
#primeira funçao do r
#a base por padrao em R, eh o logaritmo neperiano, na base de e-euler
log(x=10)
#logaritmo em uma base especificada
log(10,2)
#isto eh um objeto no r
#atribui o resultado a um objeto ou variavel
# a notaçao <- atribui um valor a uma varaiavel, pode-se o sinal de =
#alt + _ pode fazer a seta <-
#c(), cria vetor-concatenou as variaveis
x <- 2 + 2
y <- x + 2
z=2
#compraçoes
0==2
0!=2
2 >0
2 <0
2 >=2
2<=2
idades <- c(20,22,33,20,18)
idades[3]
idades[3] <- 18
cidades <- c("blumenal","itajai","navegantes")
cidades[1]
nome_e_idade <- c("jenifer",26)
#em vetores de strings e numeros, o numero eh transformadom em string
#pode-se realizar operaçoes de vetores com tamanhos multiplos
vetor01 <- c(2,2,4,5)
vetor02 <- c(3,4,5)
soma <- vetor01 + vetor02
#matrizes
mat <- matrix(vetor01,nrow = 2)
mat
#dataframe, permite mais de um tipo de valores 
df <- data.frame(
  aluno= c("jenifer","michele","cristina"),
  cidade=c("itajai","curitiba","mandirituba"),
  idade=c(26,45,45)
)
df
