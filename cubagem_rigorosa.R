
# Codigo para receber dados de árvores cubados e calcular o volume de cada
# seguindo a fórmula de Smalian.
# O arquivo de entrada de ser .csv e possuir as colunas "arv", "hi", "di", "DAP", "HT".


if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

library(gridExtra)


if (!file.exists("arvores_cubadas.csv")) {
  stop("O arquivo 'arvores_cubadas.csv' não foi encontrado no diretório atual.")
}

dados_cubagem <- read.csv("arvores_cubadas.csv", header = TRUE, sep = ",")

# Verificar se o arquivo possui as colunas necessárias
required_columns <- c("arv", "hi", "di", "DAP", "HT")

if (!all(required_columns %in% colnames(dados_cubagem))) {
  stop("O arquivo não possui todas as colunas necessárias: 'arv', 'hi', 'di', 'DAP', 'HT'")
}

# Converter colunas para numeric
dados_cubagem[required_columns] <- lapply(dados_cubagem[required_columns], as.numeric)

rm(required_columns)

# Área de cada seção

dados_cubagem["gi"] <- (pi*(dados_cubagem["di"])^2/40000)


# Volume seccional pelo método de Smalian

dados_cubagem$volume_seccional <- 0

for (i in 2:(length(dados_cubagem$gi))) {

  # 
  if (dados_cubagem$arv[i] == dados_cubagem$arv[i-1]) {
    
    # Case seja a última seção, calcula o volume de ponta.
    if(dados_cubagem$hi[i] == dados_cubagem$HT[i]) { 
      
      # Volume de ponta
      dados_cubagem$volume_seccional[i] <- (dados_cubagem$gi[i-1]/3)*(dados_cubagem$hi[i]-dados_cubagem$hi[i-1])
      
    }else{
      
      # volume
      dados_cubagem$volume_seccional[i] <- (dados_cubagem$gi[i] + dados_cubagem$gi[i-1])*(dados_cubagem$hi[i]-dados_cubagem$hi[i-1])/2
    }
    
  }else{
    dados_cubagem$volume_seccional[i] <- 0
  }
}

# Agrupar dados por árvore.
dados_cubagem_agrupados <- dados_cubagem %>% 
  group_by(arvore_id = arv) %>% 
  summarise(volume_arvore = sum(volume_seccional), DAP = max(DAP), altura = max(HT))

# Salvar dados agrupados em arquivo .csv
write.csv(dados_cubagem_agrupados, file = "dados_cubagem_volume.csv", row.names = FALSE)  
  
if (file.exists("dados_cubagem_volume.csv")) {
  print("O arquivo 'dados_cubagem_volume.csv' foi criado no diretório atual.")
}  

# Gráfico de dispersão DAP x  Volume.
grafico1 <- ggplot(dados_cubagem_agrupados, aes(x = DAP, y = volume_arvore)) +
                geom_point() +
                labs( x = "DAP (cm)", y = "Volume da árvore (m³)") +
                ggtitle("Gráfico de dispersão da relação entre DAP e volume") +
                theme_classic()

ggsave("grafico_DAP_volume.png", plot = grafico1, width = 8, height = 6, bg = "white")

grafico2 <- ggplot(dados_cubagem_agrupados, aes(x = altura, y = volume_arvore)) +
  geom_point() +
  labs( x = "altura (m)", y = "Volume da árvore (m³)") +
  ggtitle("Gráfico de dispersão da relação entre altura e volume") +
  theme_classic()

ggsave("grafico_altura_volume.png", plot = grafico2, width = 8, height = 6, bg = "white")

grid.arrange(grafico1, grafico2, ncol = 2)
