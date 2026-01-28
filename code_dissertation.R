####################################################
{ 
  # Check if the packages that we need are installed
  want = c("tidyverse", "readr","readxl","parallel", "psych","tseries", "fGarch",
           "faraway","dplyr","lmtest","sandwich", "forecast","vars","urca",
           "portes","DistributionUtils","tseries","rugarch", "rmgarch","MSwM",
           "PerformanceAnalytics","reshape2","FinTS","xtable","moments","nortest",
           "strucchange","ggplot2","quantmod","knitr","kableExtra")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
} # import packages R
####################################################
{
Trump_1 <- as.Date(c("2017-01-20", "2021-01-20"))
data_marcacao <- as.Date("2018-07-06")
covid_inicio <- as.Date("2020-03-11")
covid_fim <- as.Date("2023-05-05")
russia_ucrania <- as.Date("2022-02-24")
inicio_Trump2 <- as.Date(c("2024-11-04","2025-12-10"))} # Definir datas relevantes
####################################################
{
  commodity_index <- read_delim("Downloads/paper_dissertation/commodities_index.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(Date = col_date(format = "%d/%m/%Y")), 
                                locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                trim_ws = TRUE)
  ret_cols <- c("BCOM","BCOMGR","BCOMLI","BCOMSO","BCOMCN","BCOMSY","BCOMLC","BCOMLH")
  base_r <- commodity_index %>% filter(Date >="2015-01-01") %>%
    dplyr::select("Date","BCOM","BCOMGR","BCOMLI","BCOMSO","BCOMCN","BCOMSY","BCOMLC","BCOMLH") %>% 
    rename(data = Date)
  base_r[ , sapply(base_r, is.numeric)] <-
    lapply(base_r[ , sapply(base_r, is.numeric)], na.approx, na.rm = FALSE)
  base_r <- base_r %>% drop_na()
  
} ## Criando Banco de Dados
####################################################
{  
  base_r_long <- pivot_longer(base_r%>% dplyr::select(c(data,BCOM,BCOMGR, BCOMLI, BCOMSO,BCOMSY,BCOMCN,BCOMLC, BCOMLH)), cols = -data, names_to = "serie", values_to = "valor")
  ggplot(base_r_long, aes(x = data)) +
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3) +
    geom_line(aes(y = valor, color = serie))+
    # Linhas verticais com rótulos
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(base_r_long$valor), label = "Trade War", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
    annotate("text", x = covid_inicio, y = max(base_r_long$valor), label = "beginning of Covid", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
    annotate("text", x = covid_fim, y = max(base_r_long$valor), label = "End of Covid", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
    annotate("text", x = russia_ucrania, y = max(base_r_long$valor), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(inicio_Trump2), linetype = "dashed", color = "black") +
    annotate("text", x = inicio_Trump2, y = max(base_r_long$valor), label = "Trump 2", color = "black", vjust = -1, hjust = -0.1) +
    
    # Configurações do gráfico
    labs(title = "",
         x = "",
         y = "Valor",
         color = "Índices") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
    # Adicionando nova paleta de cores (exemplo com cores específicas)
    #scale_color_manual(values = c(  "#1f77b4", "#8c564b", "#2ca02c", "#17becf", "#9467bd", "")) +
    # Ou usando uma paleta do RColorBrewer:
     scale_color_brewer(palette = "Set1") +
    labs(x = '', y = '', title = '') +
    theme_classic() +
    theme(plot.margin = margin(t = 0, r = 20, b = 20, l = 20),
          legend.position = "bottom")  # Move a legenda para a parte inferior
} # 1  grafico
####################################################
{
  base_ind = mutate(base_r, dia = wday(base_r$data, label = TRUE))
  base_ind = arrange(base_ind, (dia))
  base_ind = base_ind[base_ind$dia=="Wed",]
  summary(base_ind[-1])
  base_ind <- base_ind %>% dplyr::select(c(data,BCOM,BCOMGR, BCOMLI, BCOMSO,BCOMSY,BCOMCN,BCOMLC, BCOMLH))
  base_norm_ind = base_ind
  base_norm_ind = data.frame(lapply(base_ind[-1], function(x) (x/x[1])*100))
  base_norm_ind$data = base_ind$data
  base_norm_ind = base_norm_ind %>% relocate(data, .before = `BCOM`)
} # precos normalizados
####################################################
{
  # Transformar a base para formato longo
  base_long <- base_norm_ind %>%dplyr::select(c(data,BCOM,BCOMGR, BCOMLI, BCOMSO)) %>%
    pivot_longer(cols = -data, names_to = "Indice", values_to = "Valor")
  
  # Criar o gráfico de linha
  ggplot(base_long, aes(x = data)) +
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3) +
    geom_line(aes(y = Valor, color = Indice)) +
    # Linhas verticais com rótulos
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(base_norm_ind$BCOM), label = "Trade War", color = "black", vjust = -6.5, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
    annotate("text", x = covid_inicio, y = max(base_norm_ind$BCOM), label = "beginning of Covid", color = "black", vjust = -6.5, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
    annotate("text", x = covid_fim, y = max(base_norm_ind$BCOM), label = "End of Covid", color = "black", vjust = -6.5, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
    annotate("text", x = russia_ucrania, y = max(base_norm_ind$BCOM), label = "Russia Ukraine", color = "black", vjust = -6.5, hjust = -0.02) +
    
    geom_vline(xintercept = as.numeric(inicio_Trump2), linetype = "dashed", color = "black") +
    annotate("text", x = inicio_Trump2, y = max(base_norm_ind$BCOM), label = "Trump 2", color = "black", vjust = -6.5, hjust = -0.1) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 years")+
    labs(x='', y='',
         title = '')+
    theme_classic() +
    theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
          legend.position = "bottom") # Ajusta as margens
  
  base_long <- base_norm_ind %>%dplyr::select(c(data,BCOM,BCOMSY,BCOMCN, BCOMLC, BCOMLH)) %>%
    pivot_longer(cols = -data, names_to = "Indice", values_to = "Valor")
  
  ggplot(base_long, aes(x = data)) +
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3) +
    geom_line(aes(y = Valor, color = Indice)) +
    # Linhas verticais com rótulos
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(base_norm_ind$BCOMSY), label = "Trade War", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
    annotate("text", x = covid_inicio, y = max(base_norm_ind$BCOMSY), label = "beginning of Covid", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
    annotate("text", x = covid_fim, y = max(base_norm_ind$BCOMSY), label = "End of Covid", color = "black", vjust = -1, hjust = -0.1) +
    
    geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
    annotate("text", x = russia_ucrania, y = max(base_norm_ind$BCOMSY), label = "Russia Ukraine", color = "black", vjust = -1, hjust = -0.04) +
    
    geom_vline(xintercept = as.numeric(inicio_Trump2), linetype = "dashed", color = "black") +
    annotate("text", x = inicio_Trump2, y = max(base_norm_ind$BCOMSY), label = "Trump 2", color = "black", vjust = -1, hjust = -0.1) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 years")+
    labs(x='', y='',
         title = '')+
    theme_classic() +
    theme(plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
          legend.position = "bottom") # Ajusta as margens
  
} # grafico 2
####################################################
{
  base_ind_rt <- data.frame(lapply(base_norm_ind[-1], function(x) c(NA, diff(log(x)))))
  base_ind_rt$data = base_norm_ind$data 
  summary(base_ind_rt[-1])
  base_ind_rt = base_ind_rt %>% drop_na() %>% relocate(data, .before = BCOM)
  summary(base_ind_rt[-1])
  describe(base_ind_rt[-1])
  
} # retorno dos indices
####################################################
{# Pacotes necessários
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(fitdistrplus)
  
  # Remove a primeira coluna ("data") e transforma para formato longo
  dados_long <- base_ind_rt %>%
    dplyr::select(-data) %>%
    pivot_longer(cols = everything(),
                 names_to = "Ativo",
                 values_to = "Retorno") %>%
    drop_na()
  
  # Função para ajustar distribuição t e plotar comparação
  plot_tstudent_comparison <- function(ativo_nome) {
    
    dados <- dados_long %>% filter(Ativo == ativo_nome)
    r <- dados$Retorno
    
    # Centraliza e escala
    r_central <- scale(r)
    
    # Ajusta distribuição t via máxima verossimilhança
    fit_t <- fitdistr(r_central, densfun = "t")
    
    df <- fit_t$estimate["df"]
    mean_r <- mean(r, na.rm = TRUE)
    sd_r <- sd(r, na.rm = TRUE)
    
    # Gera density teórica com base nos parâmetros ajustados
    x_seq <- seq(min(r, na.rm = TRUE), max(r, na.rm = TRUE), length.out = 300)
    dens_t <- dt((x_seq - mean_r) / sd_r, df = df) / sd_r
    dens_df <- data.frame(x = x_seq, density = dens_t)
    
    # Gráfico comparando distribuição empírica vs t-Student
    ggplot() +
      geom_histogram(aes(x = r, y = ..density..),
                     bins = 40, fill = "skyblue", color = "white", alpha = 0.6) +
      geom_line(data = dens_df, aes(x = x, y = density),
                color = "red", size = 1) +
      labs(title = paste0("Distribuição empírica vs t-Student - ", ativo_nome),
           subtitle = paste("Graus de liberdade estimados (df):", round(df, 2)),
           x = "Retornos", y = "density") +
      theme_minimal(base_size = 13)
  }
  
  # Exemplo: gerar para um ativo específico
  plot_tstudent_comparison("BCOMGR")
  unique(dados_long$Ativo) %>%
    lapply(plot_tstudent_comparison)
  
} # grafico individual retornos comparado com t-student
{# Pacotes necessários
  library(ggplot2)
  library(reshape2)
  library(MASS)  # para ajuste t-Student
  
  # Remove a coluna de datas
  dados_ret <- base_ind_rt[, -1]
  
  # Converte os dados para formato longo (tidy)
  dados_long <- melt(dados_ret, variable.name = "Serie", value.name = "Retorno")
  
  # Remove NA
  dados_long <- na.omit(dados_long)
  
  # Função auxiliar para gerar density teórica da t-Student ajustada
  ajusta_t <- function(x) {
    fit <- fitdistr(x, "t")  # ajusta distribuição t
    df <- fit$estimate["df"]
    mu <- fit$estimate["m"]
    sigma <- fit$estimate["s"]
    list(df = df, mu = mu, sigma = sigma)
  }
  
  # Cria um data frame com as curvas teóricas da t ajustada
  dens_t <- do.call(rbind, lapply(split(dados_long, dados_long$Serie), function(df) {
    fit <- ajusta_t(df$Retorno)
    xs <- seq(min(df$Retorno), max(df$Retorno), length.out = 200)
    data.frame(
      Serie = unique(df$Serie),
      x = xs,
      y = dt((xs - fit$mu)/fit$sigma, df = fit$df)/fit$sigma
    )
  }))
  
  # Plot único com todas as séries
  # Distribuição empírica dos retornos vs t-Student ajustada
  ggplot() +
    geom_density(data = dados_long, aes(x = Retorno, fill = Serie), alpha = 0.3, color = NA) +
    geom_line(data = dens_t, aes(x = x, y = y, color = Serie), size = 1) +
    facet_wrap(~Serie, scales = "free", ncol = 3) +  # todas no mesmo painel (organizadas)
    labs(
      title = "",
      x = "",
      y = "density"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
} # grafico distribuição dos Retorno logarítmico comparado com t-student
{
  # Pacotes necessários
  library(ggplot2)
  library(reshape2)
  
  # Remove a coluna de datas e reorganiza em formato longo
  dados_ret <- base_ind_rt[, -1]
  dados_long <- melt(dados_ret, variable.name = "Serie", value.name = "Retorno")
  dados_long <- na.omit(dados_long)
  
  # Gráfico de density dos retornos em painel
  ggplot(dados_long, aes(x = Retorno, fill = Serie)) +
    geom_density(alpha = 0.6, color = NA) +
    facet_wrap(~Serie, scales = "free", ncol = 3) +
    labs(
      title = "Distribuição empírica dos retornos das commodities",
      x = "Retorno logarítmico",
      y = "density"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
} # grafico distribuição dos Retorno logarítmico
{
  library(ggplot2)
  library(reshape2)
  
  # Reorganiza os dados em formato longo
  dados_long <- melt(base_ind_rt, id.vars = "data", 
                     variable.name = "Serie", value.name = "Retorno")
  dados_long <- na.omit(dados_long)
  
  # Gráfico de retornos no tempo em painel
  ggplot(dados_long, aes(x = data, y = Retorno)) +
    # Área de destaque (período de Trump, por exemplo)
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3, inherit.aes = FALSE) +
    
    geom_line(color = "#1f78b4", linewidth = 0.4) +
    facet_wrap(~Serie, scales = "free_y", ncol = 3) +
    
    # Linhas verticais e rótulos
    geom_vline(xintercept = data_marcacao, linetype = "dashed", color = "black", size = 0.4) +
    geom_vline(xintercept = covid_inicio, linetype = "dashed", color = "black", size = 0.4) +
    geom_vline(xintercept = covid_fim, linetype = "dashed", color = "black", size = 0.4) +
    geom_vline(xintercept = russia_ucrania, linetype = "dashed", color = "black", size = 0.4) +
    geom_vline(xintercept = inicio_Trump2, linetype = "dashed", color = "black", size = 0.4) +
    
    # Anotações: usar annotate() com y fixo e tamanho ajustado
    annotate("text", x = data_marcacao, y = 0.13, label = "Trade War", angle = 0,hjust=-.1, vjust = 1.5, size = 1.5) +
    annotate("text", x = covid_inicio, y = 0.13, label = "beginning of Covid-19", angle = 0,hjust=-.1, vjust = 1.5, size = 1.5) +
    annotate("text", x = covid_fim, y = 0.13, label = "End of Covid-19", angle = 0,hjust=-.1, vjust = 1.5, size = 1.5) +
    annotate("text", x = russia_ucrania, y = 0.13, label = "Rússia-Ukraine", angle = 0, hjust=-.04,vjust = 1.5, size = 1.5) +
    annotate("text", x = inicio_Trump2, y = 0.13, label = "Trump 2", angle = 0,hjust=-.1, vjust = 1.5, size = 1.5) +
    
    # Eixo x ajustado
    scale_x_date(
      date_labels = "%Y",          # Mostra apenas o ano
      date_breaks = "1 year",      # Quebra a cada 1 ano
      expand = expansion(mult = c(0.01, 0.02))  # Pequena margem
    ) +
    
    labs(
      title = "",
      x = "",
      y = "Log-Return"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 13),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1), # Inclina e reduz o texto
      axis.text.y = element_text(size = 9)
    )
  
} # grafico Retorno logarítmico
####################################################
{
  # Carregar base de dados
  base_ind_rt$data <- as.Date(base_ind_rt$data)
  base_ind_rt <- as.data.frame(base_ind_rt)
  
  # Seleção de commodities para análise
  commodities <- c("BCOMGR", "BCOMLI", "BCOMSO") 
  base_agre = base_ind_rt %>% dplyr::select(c(data,BCOM,BCOMGR,BCOMLI,BCOMSO))
} # Seleção de commodities para análise
####################################################
{
  # Transformar a base para formato longo
  base_long <- base_agre %>%
    pivot_longer(cols = -data, names_to = "Indice", values_to = "Valor")
  
  # Criar o gráfico de linha com nova paleta e legenda na base
  ggplot(base_long, aes(x = data)) +
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3) +
    geom_line(aes(y = Valor, color = Indice)) +
    geom_vline(xintercept = data_marcacao, linetype = "dashed", color = "black", size = 0.5) +
    annotate("text", x = data_marcacao, 
             y = max(base_long$Valor),label = "Trade War", vjust = -0.5, hjust = -0.1) +
    geom_vline(xintercept = covid_inicio, linetype = "dashed", color = "black", size = 0.5) +
    annotate("text", x = covid_inicio, 
             y = max(base_long$Valor), label = "beginning of Covid-19", vjust = -0.5, hjust = -0.1) +
    geom_vline(xintercept = covid_fim, linetype = "dashed", color = "black", size = 0.5) +
    annotate("text", x = covid_fim, 
             y = max(base_long$Valor),  label = "End of Covid-19", vjust = -0.5, hjust = -0.1) +
    geom_vline(xintercept = russia_ucrania, linetype = "dashed", color = "black", size = 0.5) +
    annotate("text", x = russia_ucrania, 
             y = max(base_long$Valor),label = "Rússia-Ukraine", vjust =-0.5, hjust = -0.1) +
    geom_vline(xintercept = inicio_Trump2, linetype = "dashed", color = "black", size = 0.5) +
    annotate("text", x = inicio_Trump2, 
             y = max(base_long$Valor),label = "Trump 2", vjust =-0.5, hjust = -0.1) +    
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    # Adicionando nova paleta de cores (exemplo com cores específicas)
    scale_color_manual(values = c(  "#1f77b4", "#8c564b", "#2ca02c", "#17becf", "#9467bd")) +
    # Ou usando uma paleta do RColorBrewer:
    # scale_color_brewer(palette = "Set1") +
    labs(x = '', y = '', title = '',color = "Índices") +
    theme_classic() +
    theme(plot.margin = margin(t = 0, r = 20, b = 20, l = 20),
          legend.position = "bottom")  # Move a legenda para a parte inferior
  
} # Grafico 3
####################################################
{
  # Funções LPM e UPM
  lpm <- function(retornos, tau = 0, order = 1) {
    mean(pmax(tau - retornos, 0)^order)
  }
  
  upm <- function(retornos, tau = 0, order = 1) {
    mean(pmax(retornos - tau, 0)^order)
  }
  
  # Seleciona as colunas com retornos (excluindo a coluna 'data')
  variaveis <- colnames(base_ind_rt)[-1]
  
  # Cria uma lista para guardar os resultados
  resultados <- list()
  
  # Loop pelas variáveis
  for (var in variaveis) {
    ret <- base_ind_rt[[var]]
    
    # Remove NAs
    ret <- na.omit(ret)
    
    # Se tiver dados suficientes
    if (length(ret) > 0) {
      lpm_val <- lpm(ret, tau = 0, order = 1)
      upm_val <- upm(ret, tau = 0, order = 1)
      ratio <- ifelse(lpm_val == 0, NA, upm_val / lpm_val)  # evita divisão por zero
      
      resultados[[var]] <- c(LPM = lpm_val, UPM = upm_val, LPM_Ratio = ratio)
    }
  }
  
  # Converte lista em data.frame
  resultados_df <- do.call(rbind, resultados)
  resultados_df <- data.frame(Variável = rownames(resultados_df), resultados_df, row.names = NULL)
  
  # Visualiza
  print(resultados_df)
  
  
  # Estatísticas descritivas
  estatistica <- describe(base_ind_rt[-1], skew =TRUE, type =  3)
  estatistica_final <- cbind(estatistica, resultados_df[, -1])  # Remove a coluna 'Variável' que já está implícita no rowname
  
  kbl(estatistica_final, caption = "Estatísticas Descritivas das Variáveis", format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
} # Estatísticas descritivas e Funções LPM e UPM
####################################################
{
  library(corrplot)
  library(ggplot2)
  library(reshape2)
  {
    # Matriz de Correlation
    # Calcular a matriz de Correlation da base
    cor_matrix <- cor(dplyr::select(base_ind_rt,c(BCOM,BCOMGR,BCOMLI,BCOMSO,BCOMSY,BCOMCN,
                                                  BCOMLC,BCOMLH)),
                      use = "complete.obs", method = "pearson")
    kbl(cor_matrix, caption = "Correlation Incondicional", format = "latex", booktabs = TRUE) %>%
      kable_styling(latex_options = c("hold_position"))
  }# Matriz de Correlation
  # Gráfico 2: Heatmap com ggplot2
  cor_melt <- melt(cor_matrix)
  
  # Criar o mapa de calor com valores numéricos
  ggplot(data = cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Adiciona valores arredondados
    scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", midpoint = 0) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "", x = "", y = "", fill = "Correlation")
}  # Criar o mapa de calor com valores numéricos
####################################################
{
  library(strucchange)
  library(knitr)
  library(kableExtra)
  
  # Converter a coluna de datas
  base_ind$data <- as.Date(base_ind$data)
  
  # Variáveis de interesse
  variaveis <- c("BCOM", "BCOMGR", "BCOMLI", "BCOMSO")
  
  # Inicializa tabela
  tabela_final <- data.frame()
  
  for (var in variaveis) {
    y <- base_ind[[var]]
    
    # Estimar o modelo geral
    bp_model <- breakpoints(y ~ 1)
    
    # Quebra 0 (sem breakpoints): usa modelo linear simples
    bic_0 <- round(BIC(lm(y ~ 1)), 2)
    linha_0 <- data.frame(
      Variável = var,
      Quebras = 0,
      Quebra_1 = NA,
      Quebra_2 = NA,
      Quebra_3 = NA,
      Quebra_4 = NA,
      Quebra_5 = NA,
      BIC = bic_0
    )
    tabela_final <- rbind(tabela_final, linha_0)
    
    # Para m = 1 a 5
    for (m in 1:5) {
      break_filled <- rep(NA, 5)
      
      # Tenta extrair modelo com m quebras do modelo geral
      bp_m_model <- tryCatch(
        breakpoints(bp_model, breaks = m),
        error = function(e) NULL
      )
      
      if (!is.null(bp_m_model)) {
        bp_m <- bp_m_model$breakpoints
        
        if (!is.null(bp_m)) {
          break_dates <- as.character(base_ind$data[bp_m])
          break_filled[1:length(break_dates)] <- break_dates
        }
        
        bic_val <- round(BIC(bp_m_model), 2)
      } else {
        bic_val <- NA
      }
      
      linha <- data.frame(
        Variável = var,
        Quebras = m,
        Quebra_1 = break_filled[1],
        Quebra_2 = break_filled[2],
        Quebra_3 = break_filled[3],
        Quebra_4 = break_filled[4],
        Quebra_5 = break_filled[5],
        BIC = bic_val
      )
      
      tabela_final <- rbind(tabela_final, linha)
    }
  }
  
  # Gera a tabela em LaTeX
  kable(
    tabela_final,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste de Bai & Perron: Datas de Quebra e Valores de BIC"
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
} # Teste de Bai & Perron em nivel
{library(strucchange)
  
  # Inicializa tabela de resultados
  tabela_testes <- data.frame()
  
  # Suas variáveis
  variaveis <- c("BCOM", "BCOMGR", "BCOMLI", "BCOMSO")
  
  # Loop por variável
  for (var in variaveis) {
    y <- base_ind[[var]]
    
    # Modelo com 1 a 5 quebras
    modelo_bp <- tryCatch({
      breakpoints(y ~ 1)
    }, error = function(e) NULL)
    
    # Teste supF (1 quebra)
    supF_res <- tryCatch({
      sctest(y ~ 1, type = "supF")
    }, error = function(e) NA)
    
    estat_supF <- if (!is.na(supF_res)[1]) round(supF_res$statistic, 3) else NA
    pval_supF  <- if (!is.na(supF_res)[1]) round(supF_res$p.value, 4) else NA
    
    # Teste Fseq (sequencial)
    fseq_res <- tryCatch({
      sctest(y ~ 1, type = "Fseq")
    }, error = function(e) NA)
    
    estat_fseq <- if (!is.na(fseq_res)[1]) round(fseq_res$statistic, 3) else NA
    pval_fseq  <- if (!is.na(fseq_res)[1]) round(fseq_res$p.value, 4) else NA
    
    # Adiciona à tabela
    tabela_testes <- rbind(tabela_testes, data.frame(
      Variável = var,
      SupF_Stat = estat_supF,
      SupF_pval = pval_supF,
      Fseq_Stat = estat_fseq,
      Fseq_pval = pval_fseq
    ))
  }
  
  # Exibir resultados
  print(tabela_testes)
  
  kable(
    tabela_testes,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste SupF de Bai & Perron"
  ) %>%
    kable_styling(latex_options = c("hold_position"))
} # test SupF de Bai & Perror em nivel
####################################################
####################################################
{
  library(strucchange)
  library(knitr)
  library(kableExtra)
  
  # Converter a coluna de datas
  base_agre$data <- as.Date(base_agre$data)
  
  # Variáveis de interesse
  variaveis <- c("BCOM", "BCOMGR", "BCOMLI", "BCOMSO")
  
  # Inicializa tabela
  tabela_final <- data.frame()
  
  for (var in variaveis) {
    y <- base_agre[[var]]
    
    # Estimar o modelo geral
    bp_model <- breakpoints(y ~ 1)
    
    # Quebra 0 (sem breakpoints): usa modelo linear simples
    bic_0 <- round(BIC(lm(y ~ 1)), 2)
    linha_0 <- data.frame(
      Variável = var,
      Quebras = 0,
      Quebra_1 = NA,
      Quebra_2 = NA,
      Quebra_3 = NA,
      Quebra_4 = NA,
      Quebra_5 = NA,
      BIC = bic_0
    )
    tabela_final <- rbind(tabela_final, linha_0)
    
    # Para m = 1 a 5
    for (m in 1:5) {
      break_filled <- rep(NA, 5)
      
      # Tenta extrair modelo com m quebras do modelo geral
      bp_m_model <- tryCatch(
        breakpoints(bp_model, breaks = m),
        error = function(e) NULL
      )
      
      if (!is.null(bp_m_model)) {
        bp_m <- bp_m_model$breakpoints
        
        if (!is.null(bp_m)) {
          break_dates <- as.character(base_agre$data[bp_m])
          break_filled[1:length(break_dates)] <- break_dates
        }
        
        bic_val <- round(BIC(bp_m_model), 2)
      } else {
        bic_val <- NA
      }
      
      linha <- data.frame(
        Variável = var,
        Quebras = m,
        Quebra_1 = break_filled[1],
        Quebra_2 = break_filled[2],
        Quebra_3 = break_filled[3],
        Quebra_4 = break_filled[4],
        Quebra_5 = break_filled[5],
        BIC = bic_val
      )
      
      tabela_final <- rbind(tabela_final, linha)
    }
  }
  
  # Gera a tabela em LaTeX
  kable(
    tabela_final,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste de Bai & Perron: Datas de Quebra e Valores de BIC"
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
} # Teste de Bai & Perron
{library(strucchange)
  
  # Inicializa tabela de resultados
  tabela_testes <- data.frame()
  
  # Suas variáveis
  variaveis <- c("BCOM", "BCOMGR", "BCOMLI", "BCOMSO")
  
  # Loop por variável
  for (var in variaveis) {
    y <- base_agre[[var]]
    
    # Modelo com 1 a 5 quebras
    modelo_bp <- tryCatch({
      breakpoints(y ~ 1)
    }, error = function(e) NULL)
    
    # Teste supF (1 quebra)
    supF_res <- tryCatch({
      sctest(y ~ 1, type = "supF")
    }, error = function(e) NA)
    
    estat_supF <- if (!is.na(supF_res)[1]) round(supF_res$statistic, 3) else NA
    pval_supF  <- if (!is.na(supF_res)[1]) round(supF_res$p.value, 4) else NA
    
    # Teste Fseq (sequencial)
    fseq_res <- tryCatch({
      sctest(y ~ 1, type = "Fseq")
    }, error = function(e) NA)
    
    estat_fseq <- if (!is.na(fseq_res)[1]) round(fseq_res$statistic, 3) else NA
    pval_fseq  <- if (!is.na(fseq_res)[1]) round(fseq_res$p.value, 4) else NA
    
    # Adiciona à tabela
    tabela_testes <- rbind(tabela_testes, data.frame(
      Variável = var,
      SupF_Stat = estat_supF,
      SupF_pval = pval_supF,
      Fseq_Stat = estat_fseq,
      Fseq_pval = pval_fseq
    ))
  }
  
  # Exibir resultados
  print(tabela_testes)
  
  kable(
    tabela_testes,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste SupF de Bai & Perron"
  ) %>%
    kable_styling(latex_options = c("hold_position"))
} # test SupF de Bai & Perror
####################################################
{
  # Testes preliminares
  realizar_testes <- function(retornos) {
    data.frame(
      shapiro_estatistica = shapiro.test(retornos)$statistic,
      shapiro_pvalue =shapiro.test(retornos)$p.value,
      sf_statistica = sf.test(retornos)$statistic,
      sf_pvalue = sf.test(retornos)$p.value,
      ADF_statistica = adf.test(retornos)$statistic,
      ADF_pvalue = adf.test(retornos)$p.value,
      pp_estatistica = pp.test(retornos)$statistic,
      pp_pvalue = pp.test(retornos)$p.value,
      JarqueBera_estatistica = jarque.bera.test(retornos)$statistic,
      JarqueBera_pvalue = jarque.bera.test(retornos)$p.value,
      ARCH_LM_5_estatistica = ArchTest(retornos, lags = 5)$statistic,
      ARCH_LM_5_pvalue = ArchTest(retornos, lags = 5)$p.value,
      ARCH_LM_10_estatistica = ArchTest(retornos, lags = 10)$statistic,
      ARCH_LM_10_pvalue =ArchTest(retornos, lags = 10)$p.value
    )
  }
} # Testes preliminares
####################################################
{
  # Especificações dos modelos
  spec_garch <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )

  spec_dcc <- dccspec(
    uspec = multispec(replicate(2, spec_garch)),
    dccOrder = c(1, 1),
    distribution = "mvt"
  )

}# Especificações dos modelos GARCH
####################################################
{
  # Estimações e testes de robustez
  resultados_testes <- list()
  resultados_dcc <- list()
  resultados_robustez <- list()
  results_df <- list()
  resultados <- list()
  
  VaR_data <- data.frame()
  CoVaR_data <- data.frame()
  DeltaCoVaR_data <- data.frame()
} # criando as tabelas de resultados
####################################################
{
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    resultados_testes[[commodity]] <- realizar_testes(retornos[, commodity])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    AIC = as.numeric(rugarch::infocriteria(fit_dcc)[1])
    BIC =  as.numeric(rugarch::infocriteria(fit_dcc)[2])
    Shibata =  as.numeric(rugarch::infocriteria(fit_dcc)[3])
    Hosking_1pvalue = as.numeric(portest(residuos,lags= 1,test="Hosking")["p-value"])
    Hosking_1statistic = as.numeric(portest(residuos,lags= 1,test="Hosking")["statistic"])
    Hosking_5pvalue =as.numeric( portest(residuos,lags= 5,test="Hosking")["p-value"])
    Hosking_5statistic =as.numeric( portest(residuos,lags= 5,test="Hosking")["statistic"])
    Hosking_10pvalue = as.numeric(portest(residuos,lags= 10,test="Hosking")["p-value"])
    Hosking_10statistic =as.numeric( portest(residuos,lags= 10,test="Hosking")["statistic"])
    LjungBox_1pvalue  = as.numeric(Box.test(residuos, lag = 1, type = "Ljung-Box")$p.value)
    LjungBox_1statistic = as.numeric(Box.test(residuos, lag = 1, type = "Ljung-Box")$statistic)
    LjungBox_5pvalue = as.numeric(Box.test(residuos, lag = 5, type = "Ljung-Box")$p.value)
    LjungBox_5statistic = as.numeric(Box.test(residuos, lag = 5, type = "Ljung-Box")$statistic)
    LjungBox_10pvalue = as.numeric(Box.test(residuos, lag = 10, type = "Ljung-Box")$p.value)
    LjungBox_10statistic = as.numeric(Box.test(residuos, lag = 10, type = "Ljung-Box")$statistic)
    ARCH_LM_1pvalue = as.numeric(ArchTest(residuos, lags = 1)$p.value)
    ARCH_LM_1statistic =as.numeric(ArchTest(residuos, lags = 1)$statistic)
    
    resultados_robustez[[commodity]] <- data.frame(
      AIC, BIC, Shibata, Hosking_1pvalue,  Hosking_1statistic, Hosking_5pvalue,
      Hosking_5statistic, Hosking_10pvalue, Hosking_10statistic, LjungBox_1pvalue,
      LjungBox_1statistic, LjungBox_5pvalue, LjungBox_5statistic,LjungBox_10pvalue,
      LjungBox_10statistic, ARCH_LM_1pvalue,ARCH_LM_1statistic
    )
    
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity, VaR_1 = VaR_asset, VaR_5 = VaR_asset_1)
    CoVaR_data <- data.frame(Commodity = commodity, CoVaR_1 = DCOVaR, CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity, DeltaCoVaR_1 = DDeltaCoVaR, DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    
    # Armazenar resultados
    resultados[[commodity]] <- list(Teste_preliminares = resultados_testes,
                                    DCC_GARCH= resultados_dcc,
                                    Teste_robustez= resultados_robustez,
                                    COVAR_DVAR = results_df)
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    
    
  } 
} # estimando os modelos(GARCH, VaR, COVaR e Delta COVaR)
{
  # Função para definir os níveis de significância
  significancia <- function(pval) {
    if (pval <= 0.001) {
      return("***")
    } else if (pval <= 0.01) {
      return("**")
    } else if (pval <= 0.05) {
      return("*")
    } else if (pval <= 0.1) {
      return(".")
    } else {
      return("ns")
    }
  }
  
  # Criar a tabela formatada
  
  
  # Converter lista para data.frame organizado
  tabela <- lapply(names(resultados_testes), function(ativo) {
    df <- resultados_testes[[ativo]]
    df_formatado <- data.frame(
      Ativo = ativo,
      Shapiro = paste0(round(df$shapiro_estatistica, 3), " (", significancia(df$shapiro_pvalue), ")"),
      SF = paste0(round(df$sf_statistica, 3), " (", significancia(df$sf_pvalue), ")"),
      ADF = paste0(round(df$ADF_statistica, 3), " (", significancia(df$ADF_pvalue), ")"),
      PP = paste0(round(df$pp_estatistica, 3), " (", significancia(df$pp_pvalue), ")"),
      JarqueBera = paste0(round(df$JarqueBera_estatistica, 3), " (", significancia(df$JarqueBera_pvalue), ")"),
      ARCH_LM_5 = paste0(round(df$ARCH_LM_5_estatistica, 3), " (", significancia(df$ARCH_LM_5_pvalue), ")"),
      ARCH_LM_10 = paste0(round(df$ARCH_LM_10_estatistica, 3), " (", significancia(df$ARCH_LM_10_pvalue), ")")
    )
    return(df_formatado)
  }) %>% bind_rows()
  
  # Gerar tabela LaTeX usando kableExtra
  
  latex_table <- tabela %>% t() %>%
    kable("latex", booktabs = TRUE, align = "c", caption = "Resultados dos Testes Estatísticos") %>%
    kable_styling(latex_options = c("hold_position"))
  
  print(latex_table)

  
} # tabela testes preliminares 
{
  # Função para limpar nomes das colunas e linhas
  limpar_nomes <- function(x) {
    if (is.matrix(x)) {
      rownames(x) <- gsub("\\[.*?\\]", "", rownames(x))  # Remove colchetes nos nomes das linhas
      rownames(x) <- gsub("\\.", "", rownames(x))        # Remove pontos nos nomes das linhas
      colnames(x) <- gsub("\\[.*?\\]", "", colnames(x))  # Remove colchetes nos nomes das colunas
      colnames(x) <- gsub("\\.", "", colnames(x))        # Remove pontos nos nomes das colunas
    }
    return(as.data.frame(x))  # Converte matriz para data frame
  }
  
  # Função para adicionar nível de significância ao coeficiente
  formatar_coeficiente <- function(est, pval) {
    if (pval <= 0.01) {
      return(sprintf("%.4f ***", est))
    } else if (pval <= 0.05) {
      return(sprintf("%.4f **", est))
    } else if (pval <= 0.10) {
      return(sprintf("%.4f *", est))
    } else {
      return(sprintf("%.4f", est))
    }
  }
  
  # Criar tabela com coeficientes formatados
  DCC_results_table <- do.call(cbind, lapply(names(resultados_dcc), function(commodity) {
    dados_limpos <- limpar_nomes(resultados_dcc[[commodity]])
    
    # Extrair coeficiente estimado e p-value
    estimativas <- dados_limpos[,1]
    pvalores <- dados_limpos[, 4]
    
    # Formatar coeficientes com níveis de significância
    coef_formatado <- mapply(formatar_coeficiente, estimativas, pvalores)
    
    # Criar data frame
    data.frame(coef_formatado, row.names = rownames(dados_limpos))
  }))
  
  # Nomear colunas com as commodities
  colnames(DCC_results_table) <- names(resultados_dcc)
  
  # Criar painel "a"
  panel_a <- kbl(DCC_results_table, caption = "Panel a) DCC-GARCH Model Statistics", format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c("Coeficients" = length(names(resultados_dcc)))) %>%
    footnote(general = "Note: 1. *, **, ***, correspond to the significance level of 1%, 5% and 10% respectively.")
  
  # Panel b) Robustness tests
  # Converter lista para data.frame organizado
  robustness_table <- lapply(names(resultados_robustez), function(ativo) {
    df <- resultados_robustez[[ativo]]
    df_formatado <- data.frame(
      Ativo = ativo,
      AIC = paste0(round(df$AIC, 3)),
      BIC = paste0(round(df$BIC, 3)),
      Shibata = paste0(round(df$Shibata, 3)),
      Hosking_1 = paste0(round(df$Hosking_1statistic, 3), " (", significancia(df$Hosking_1pvalue), ")"),
      Hosking_5 = paste0(round(df$Hosking_5statistic, 3), " (", significancia(df$Hosking_5pvalue), ")"),
      Hosking_10 = paste0(round(df$Hosking_10statistic, 3), " (", significancia(df$Hosking_10pvalue), ")"),
      LjungBox_1 = paste0(round(df$LjungBox_1statistic, 3), " (", significancia(df$LjungBox_1pvalue), ")"),
      LjungBox_5 = paste0(round(df$LjungBox_5statistic, 3), " (", significancia(df$LjungBox_5pvalue), ")"),
      LjungBox_10 = paste0(round(df$LjungBox_10statistic, 3), " (", significancia(df$LjungBox_10pvalue), ")"),
      ARCH_LM_1 = paste0(round(df$ARCH_LM_1statistic, 3), " (", significancia(df$ARCH_LM_1pvalue), ")")
    )
    return(df_formatado)
  }) %>% bind_rows()
  
  robustness_table <- robustness_table %>% t()
  panel_b <- kbl(robustness_table, caption = "Panel b) Robustness tests", format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
  # Verifique se a transposição gerou o resultado desejad
  
  # Imprimir os painéis
  print(panel_a)
  print(panel_b)
} # tabela resultado DCC-GARCH e testes de robustez
####################################################
{
  library(ggplot2)
  library(dplyr)
  library(xtable)
  library(patchwork)
  library(scales)
  # Lista para armazenar resultados
  tabela_lista <- list()
  graficos_lista <- list()
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    corr <- rcor(fit_dcc)
    correlacao <- corr[1, 2, ]
    
    df_cor <- data.frame(
      correlacao = correlacao,
      lag_corr = lag(correlacao)
    ) %>% na.omit()
    
    modelo_base <- lm(correlacao ~ lag_corr, data = df_cor)
    modelo_markov <- msmFit(modelo_base, k = 2, p = 0, sw = rep(TRUE, 3))
    
    regimes_probs <- modelo_markov@Fit@smoProb
    regime_discreto <- apply(regimes_probs, 1, which.max)[-1]
    
    df_plot <- data.frame(
      Data = base_agre$data[seq_along(correlacao)][-1],
      Correlacao = df_cor$correlacao,
      Regime = factor(regime_discreto),
      Commodity = commodity)
    
    # Adiciona bloco de regime
    regime_blocks <- df_plot %>%
      mutate(index = row_number()) %>%
      mutate(change = Regime != lag(Regime, default = first(Regime))) %>%
      mutate(block = cumsum(change)) %>%
      group_by(block, Regime, Commodity) %>%
      summarise(
        start = first(Data),
        end = last(Data),
        .groups = "drop"
      )
    
    graficos_lista[[commodity]] <- list(df = df_plot, rects = regime_blocks)
    # Blocos de regime
    regime_blocks <- df_plot %>%
      mutate(change = Regime != lag(Regime, default = first(Regime))) %>%
      mutate(block = cumsum(change)) %>%
      group_by(block, Regime, Commodity) %>%
      summarise(
        duracao = n(),
        start = first(Data),
        end = last(Data),
        .groups = "drop"
      )
    
    # Frequência (número de blocos por regime)
    frequencias <- regime_blocks %>%
      group_by(Regime) %>%
      summarise(frequencia = n(), .groups = "drop")
    
    # Estatísticas por regime
    resumo <- df_plot %>%
      group_by(Regime) %>%
      summarise(
        duracao_total = n(),
        media_corr = mean(Correlacao),
        desvio_corr = sd(Correlacao),
        min_corr = min(Correlacao),
        max_corr = max(Correlacao),
        .groups = "drop"
      ) %>%
      left_join(frequencias, by = "Regime") %>%
      mutate(
        duracao_media = duracao_total / frequencia,
        amplitude_corr = max_corr - min_corr,
        Commodity = commodity,
        regime = Regime
      )
    
    # Corrigir duração total se necessário
    total_max <- nrow(df_cor)
    if (sum(resumo$duracao_total) > total_max) {
      excesso <- sum(resumo$duracao_total) - total_max
      resumo$duracao_total[1] <- resumo$duracao_total[1] - excesso
    }
    
    # Probabilidades de permanência (P_ii)
    trans_mat <- modelo_markov@transMat
    resumo$P_ii <- diag(trans_mat)
    
    # Volatility relativa
    volat <- resumo$desvio_corr
    if (length(volat) == 2) {
      resumo$Volatility_relativa <- volat / min(volat)
    } else {
      resumo$Volatility_relativa <- NA
    }
    
    # Duração máxima de bloco por regime
    duracao_max <- regime_blocks %>%
      group_by(Regime) %>%
      summarise(duracao_max_bloco = max(duracao), .groups = "drop")
    
    resumo <- left_join(resumo, duracao_max, by = "Regime")
    
    tabela_lista[[commodity]] <- resumo
  }
  
  # Juntar tudo
  df_todos <- do.call(rbind, lapply(graficos_lista, `[[`, "df"))
  rects_todos <- do.call(rbind, lapply(graficos_lista, `[[`, "rects"))
  
  grafico_facetado <- ggplot(df_todos, aes(x = Data)) +
    geom_rect(data = rects_todos,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Regime),
              alpha = 0.2, inherit.aes = FALSE) +
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(df_todos$Correlacao)*0.75,
             label = "Trade War", color = "black", vjust = -1, hjust = -0.1, size = 2.5) +
    geom_line(aes(y = Correlacao), color = "blue", size = 0.7) +
    facet_wrap(~ Commodity, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(x = "Data", y = "Correlation") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(-0.2, 0.8, by = 0.1)
    )+
    scale_fill_manual(values = c("1" = "black", "2" = "red")) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "bottom")
  
  print(grafico_facetado)
  
  # Unir resultados
  tabela_final <- bind_rows(tabela_lista)
  
  # Ajustar colunas
  tabela_final <- tabela_final %>%
    dplyr::select(Commodity, regime, frequencia, duracao_media, duracao_total,
                  duracao_max_bloco, media_corr, desvio_corr, amplitude_corr,
                  P_ii, Volatility_relativa)
  
  # Exportar tabela LaTeX
  print(
    xtable(
      tabela_final,
      digits = c(0, 0, 0, 0, 1, 0, 3, 3, 3, 3, 3, 3),
      caption = "Estatísticas dos Regimes de Correlation por Commodity",
      label = "tab:estatisticas_regime",
      align = c("l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c","c")
    ),
    include.rownames = FALSE,
    caption.placement = "top",
    sanitize.text.function = identity
  )
  
} # Tabela e grafico do modelo de Markov Switching 
{library(ggplot2)
  library(dplyr)
  library(xtable)
  
  dir.create("graficos_markov", showWarnings = FALSE)  # Criar pasta para gráficos
  tabela_lista <- list()
  tabela_transicoes <- list()  # Guardar tabelas para exportar depois
  
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    corr <- rcor(fit_dcc)
    correlacao <- corr[1, 2, ]
    
    df_cor <- data.frame(
      correlacao = correlacao,
      lag_corr = lag(correlacao)
    ) %>% na.omit()
    
    modelo_base <- lm(correlacao ~ lag_corr, data = df_cor)
    modelo_markov <- msmFit(modelo_base, k = 2, p = 0, sw = rep(TRUE, 3))
    
    regimes_probs <- modelo_markov@Fit@filtProb
    regime_discreto <- apply(regimes_probs, 1, which.max)
    
    df_plot <- data.frame(
      Data = base_agre$data[seq_along(correlacao)][-1],
      Correlacao = df_cor$correlacao,
      Regime = factor(regime_discreto),
      Commodity = commodity
    )
    
    # Estatísticas do regime
    resumo <- df_plot %>%
      group_by(Regime) %>%
      summarise(desvio_corr = sd(Correlacao), .groups = "drop") %>%
      arrange(desvio_corr) %>%
      mutate(Regime_Label = c("Baixa Volatility", "Alta Volatility"))
    
    df_plot$Regime_Label <- factor(resumo$Regime_Label[match(df_plot$Regime, resumo$Regime)])
    
    # ▸ Gerar gráfico e salvar
    p <- ggplot(df_plot, aes(x = Data, y = Correlacao)) +
      geom_line() +
      geom_point(aes(color = Regime_Label), size = 0.8) +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = paste("Regimes de Correlation -", commodity),
           x = "Tempo", y = "Correlation DCC", color = "Regime") +
      theme_minimal()
    
    # ▸ Matriz de transição LaTeX
    trans_mat_df <- data.frame(
      De_Para = c("1→1", "1→2", "2→1", "2→2"),
      Probabilidade = as.vector(modelo_markov@transMat)
    )
    
    tabela_transicoes[[commodity]] <- xtable(
      trans_mat_df,
      caption = paste("Matriz de Transição de Regimes -", commodity),
      label = paste0("tab:transicao_", commodity)
    )
    
    # ▸ Estatísticas do regime (completa)
    # ... [use seu bloco já existente aqui para gerar `resumo` e `tabela_lista[[commodity]]` como antes] ...
  }
  for (commodity in names(tabela_transicoes)) {
    print(tabela_transicoes[[commodity]],
          include.rownames = FALSE, booktabs = TRUE,
          caption.placement = "top", sanitize.text.function = identity)
  }
  # ▸ Criar uma tabela combinada
  tabela_transicoes_unica <- bind_rows(
    lapply(names(tabela_transicoes), function(nome) {
      df <- as.data.frame(tabela_transicoes[[nome]])
      df$Commodity <- nome
      df
    })
  )
  
  # Reorganizar colunas
  tabela_transicoes_unica <- tabela_transicoes_unica[, c("Commodity", "De_Para", "Probabilidade")]
  
  # Exportar
  print(xtable(
    tabela_transicoes_unica,
    caption = "Matriz de Transição de Regimes para Cada Commodity",
    label = "tab:transicao_todas"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  caption.placement = "top",
  sanitize.text.function = identity)
  
  
} # Matriz de Transição de Regimes
####################################################
{
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
  
    
  # Cálculo da assimetria e curtose dos resíduos padronizados
  residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
  residuos_commodity <- na.omit(residuos_commodity)
  skew <- moments::skewness(residuos_commodity)
  kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    
    # VaR Cornish-Fisher dinâmico
    VaR_CF <- z_cf * sigma_t[, 2]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_5 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      CFVaR = as.numeric(VaR_CF),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    # Retornos reais do ativo
    retornos_ativos <- retornos[, commodity]
    
    # Backtest VaR Normal (1%)
    violacoes_VaR <- which(retornos_ativos < VaR_asset)
    n_violacoes <- length(violacoes_VaR)
    
    # Backtest VaR Cornish-Fisher
    violacoes_CFVaR <- which(retornos_ativos < VaR_CF)
    n_violacoes_CF <- length(violacoes_CFVaR)
    
    # Proporção de violações
    p_violacoes <- n_violacoes / length(VaR_asset)
    p_violacoes_CF <- n_violacoes_CF / length(VaR_CF)
    
    # Teste Kupiec para VaR Normal
    test_norm <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_asset)
    
    # Teste Kupiec para VaR Cornish-Fisher
    test_cf <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_CF)
    
    # Armazenar resultados
    resultados[[commodity]] <- list(DCC_GARCH= resultados_dcc,
                                    COVAR_DVAR = results_df)
    # Armazenar os resultados no objeto resultados
    resultados[[commodity]]$Backtest_VaR <- list(
      VaR_Normal = list(Violacoes = n_violacoes, Proporcao = p_violacoes),
      VaR_CFisher = list(Violacoes = n_violacoes_CF, Proporcao = p_violacoes_CF)
    )
    resultados[[commodity]]$Kupiec_Test <- list(
      Normal = test_norm,
      CornishFisher = test_cf
    )
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    # Adiciona as colunas necessárias para gráfico
    df_plot <- data.frame(
      Date = base_agre$data,
      Return = as.numeric(retornos_ativos),
      VaR_Normal = as.numeric(VaR_asset),
      VaR_CF = as.numeric(VaR_CF)
    )
    
  } 
} # estimando os modelos(GARCH, VaR, CF VaR, COVaR e Delta COVaR)
{
  library(dplyr)
  library(knitr)
  
  kupiec_summary <- do.call(rbind, lapply(names(resultados), function(commodity) {
    test_norm <- resultados[[commodity]]$Kupiec_Test$Normal
    test_cf <- resultados[[commodity]]$Kupiec_Test$CornishFisher
    
    data.frame(
      Commodity = commodity,
      Modelo = c("Normal", "Cornish-Fisher"),
      `LR Estatística` = c(test_norm$uc.LRstat, test_cf$uc.LRstat),
      `Valor-p` = c(test_norm$uc.LRp, test_cf$uc.LRp),
      `Excedências` = c(sum(resultados[[commodity]]$Backtest_VaR$VaR_Normal$Violacoes),
                        sum(resultados[[commodity]]$Backtest_VaR$VaR_CFisher$Violacoes)),
      `Esperado` = rep(0.01 * nrow(resultados[[commodity]]$COVAR_DVAR[[commodity]]), 2)
    )
  }))
  melhor_modelo_df <- kupiec_summary %>%
    group_by(Commodity) %>%
    summarize(
      Melhor_Modelo = case_when(
        all(`Valor.p` > 0.05) ~ "Ambos aceitos",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05 & `Modelo` == "Cornish-Fisher" & `Valor.p` <= 0.05) ~ "Normal",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05 & `Modelo` == "Normal" & `Valor.p` <= 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05) ~ "Normal",
        TRUE ~ "Ambos rejeitados"
      )
    )
  
  kupiec_summary <- left_join(kupiec_summary, melhor_modelo_df, by = "Commodity")
  
  
  # Exportar como LaTeX com kable
  kable(kupiec_summary, format = "latex", booktabs = TRUE, digits = 4,
        caption = "Teste de cobertura (Kupiec) para os modelos VaR por commodity.",
        col.names = c("Commodity", "Modelo", "Estatística LR", "Valor-p", "Excedências", "Esperado", "Melhor modelo"))
  
} # tabela do Teste de cobertura (Kupiec) para os modelos VaR por commodity
{
  
  library(ggplot2)
  
  # Criar e salvar gráficos separados para cada commodity
  for (commodity in names(resultados)) {
    
    df_plot <- data.frame(
      Date = resultados[[commodity]]$COVAR_DVAR[[commodity]]$Date,
      Return = resultados[[commodity]]$COVAR_DVAR[[commodity]]$Asset2_Return,
      VaR_Normal = resultados[[commodity]]$COVAR_DVAR[[commodity]]$DVaR,
      VaR_CF = resultados[[commodity]]$COVAR_DVAR[[commodity]]$CFVaR
    )
    
    p <- ggplot(df_plot, aes(x = Date)) +
      geom_line(aes(y = Return), color = "black", alpha = 0.6) +
      geom_line(aes(y = VaR_Normal), color = "red", linetype = "dashed") +
      geom_line(aes(y = VaR_CF), color = "blue", linetype = "dotted") +
      labs(title = paste("VaR Normal vs Cornish-Fisher -", commodity),
           y = "Retorno e VaR",
           x = "Data") +
      theme_minimal()
    
    print(p)  # mostra o gráfico no console
    
    
  }
  
} # grafico comparativo VaR e CF VaR
####################################################
{
  # Inicializar data.frame para armazenar os resultados
  tabela_ks <- data.frame(
    Commodity = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop para extrair resultados e montar a tabela
  for (commodity in names(resultados)) {
    if (!is.null(resultados[[commodity]]$KS_test)) {
      tabela_ks <- rbind(tabela_ks, data.frame(
        Commodity = commodity,
        Statistic = resultados[[commodity]]$KS_test$statistic,
        P_Value = resultados[[commodity]]$KS_test$p_value
      ))
    }
  }
  
  library(xtable)
  
  # Criar tabela LaTeX
  tabela_latex <- xtable(tabela_ks, 
                         caption = "Resultados do Teste Kolmogorov-Smirnov para Dependências de Cauda",
                         label = "tab:ks_test")
  print(tabela_latex, include.rownames = FALSE)
  
} # Teste Kolmogorov-Smirnov para Dependências de Cauda
####################################################
{
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    
    # Cálculo da assimetria e curtose dos resíduos padronizados
    residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
    residuos_commodity <- na.omit(residuos_commodity)
    skew <- moments::skewness(residuos_commodity)
    kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    
    # VaR Cornish-Fisher dinâmico
    VaR_CF <- z_cf * sigma_t[, 2]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_5 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      CFVaR = as.numeric(VaR_CF),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    # Retornos reais do ativo
    retornos_ativos <- retornos[, commodity]
    
    # Backtest VaR Normal (1%)
    violacoes_VaR <- which(retornos_ativos < VaR_asset)
    n_violacoes <- length(violacoes_VaR)
    
    # Backtest VaR Cornish-Fisher
    violacoes_CFVaR <- which(retornos_ativos < VaR_CF)
    n_violacoes_CF <- length(violacoes_CFVaR)
    
    # Proporção de violações
    p_violacoes <- n_violacoes / length(VaR_asset)
    p_violacoes_CF <- n_violacoes_CF / length(VaR_CF)
    
    # Teste Kupiec para VaR Normal
    test_norm <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_asset)
    
    # Teste Kupiec para VaR Cornish-Fisher
    test_cf <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_CF)
    
    # Armazenar resultados
    resultados[[commodity]] <- list(DCC_GARCH= resultados_dcc,
                                    COVAR_DVAR = results_df)
    # Armazenar os resultados no objeto resultados
    resultados[[commodity]]$Backtest_VaR <- list(
      VaR_Normal = list(Violacoes = n_violacoes, Proporcao = p_violacoes),
      VaR_CFisher = list(Violacoes = n_violacoes_CF, Proporcao = p_violacoes_CF)
    )
    resultados[[commodity]]$Kupiec_Test <- list(
      Normal = test_norm,
      CornishFisher = test_cf
    )
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    # Adiciona as colunas necessárias para gráfico
    df_plot <- data.frame(
      Date = base_agre$data,
      Return = as.numeric(retornos_ativos),
      VaR_Normal = as.numeric(VaR_asset),
      VaR_CF = as.numeric(VaR_CF)
    )
    
  } 
} # estimando os modelos(GARCH, VaR, CF VaR, COVaR e Delta COVaR)
{
  library(dplyr)
  library(knitr)
  
  kupiec_summary <- do.call(rbind, lapply(names(resultados), function(commodity) {
    test_norm <- resultados[[commodity]]$Kupiec_Test$Normal
    test_cf <- resultados[[commodity]]$Kupiec_Test$CornishFisher
    
    data.frame(
      Commodity = commodity,
      Modelo = c("Normal", "Cornish-Fisher"),
      `LR Estatística` = c(test_norm$uc.LRstat, test_cf$uc.LRstat),
      `Valor-p` = c(test_norm$uc.LRp, test_cf$uc.LRp),
      `Excedências` = c(sum(resultados[[commodity]]$Backtest_VaR$VaR_Normal$Violacoes),
                        sum(resultados[[commodity]]$Backtest_VaR$VaR_CFisher$Violacoes)),
      `Esperado` = rep(0.01 * nrow(resultados[[commodity]]$COVAR_DVAR[[commodity]]), 2)
    )
  }))
  melhor_modelo_df <- kupiec_summary %>%
    group_by(Commodity) %>%
    summarize(
      Melhor_Modelo = case_when(
        all(`Valor.p` > 0.05) ~ "Ambos aceitos",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05 & `Modelo` == "Cornish-Fisher" & `Valor.p` <= 0.05) ~ "Normal",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05 & `Modelo` == "Normal" & `Valor.p` <= 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05) ~ "Normal",
        TRUE ~ "Ambos rejeitados"
      )
    )
  
  kupiec_summary <- left_join(kupiec_summary, melhor_modelo_df, by = "Commodity")
  
  
  # Exportar como LaTeX com kable
  kable(kupiec_summary, format = "latex", booktabs = TRUE, digits = 4,
        caption = "Teste de cobertura (Kupiec) para os modelos VaR por commodity.",
        col.names = c("Commodity", "Modelo", "Estatística LR", "Valor-p", "Excedências", "Esperado", "Melhor modelo"))
  
} # tabela do Teste de cobertura (Kupiec) para os modelos VaR por commodity
####################################################
{
  results_df1 <- list()
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    
    # Cálculo da assimetria e curtose dos resíduos padronizados
    residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
    residuos_commodity <- na.omit(residuos_commodity)
    skew <- moments::skewness(residuos_commodity)
    kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    # Quantil ajustado com expansão de Cornish-Fisher (nível 99%)
    z_cf1 <- q_1 + (1/6)*(q_1^2 - 1)*skew + (1/24)*(q_1^3 - 3)*kurt - (1/36)*(2*q_1^3 - 5*q_1)*skew^2
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR  Cornish-Fisher dinâmico dinâmico para cada tempo
    VaR_BCOM <-  z_cf * sigma_t[, 1]
    VaR_BCOM_1 <- z_cf1 * sigma_t[, 1]
    
    # VaR Cornish-Fisher dinâmico
    
    VaR_asset <-  z_cf * sigma_t[, 2]
    VaR_asset_1 <- z_cf1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <-  z_cf * sigma_t[, 2] +  z_cf * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- z_cf1 * sigma_t[, 2] + z_cf1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_99 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df1[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    
    
  } 
} # estimando os modelos(GARCH, VaR, COVaR e Delta COVaR usando a expansão de Corner fisher)
{
  
  library(patchwork)
  library(scales)
  # Função para consolidar e plotar todos os gráficos por análise
  plot_all_results <- function(results_df_list, commodities, asset_base = "BCOM") {
    library(ggplot2)
    library(dplyr)
    
    # Consolidar todos os resultados com uma coluna "Par"
    all_data <- bind_rows(lapply(commodities, function(com) {
      df <- results_df_list[[com]]
      df$Par <- paste(asset_base, com, sep = " - ")
      return(df)
    }))
    
    # Gráfico de Volatility Condicional
    p1 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(all_data$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(all_data$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(all_data$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(all_data$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.04, size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(all_data$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_line(aes(y = Asset1_Volatility), color = "blue") +
      geom_line(aes(y = Asset2_Volatility), color = "red") +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      labs(title = "", x = "", y = "Volatility") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0) )
    
    # Gráfico de Correlation Dinâmica
    p2 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(results_df1$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(results_df1$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(results_df1$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(results_df1$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(results_df1$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_line(aes(y = Correlation), color = "blue") +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        breaks = seq(-1, 1, by = 0.10))+
      labs(title = "", x = "", y = "Correlation") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
    
    # Gráfico de COVaR / DeltaCoVaR
    p3 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(results_df1$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(results_df1$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(results_df1$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(results_df1$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(results_df1$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_line(aes(y = DCOVaR, color = "DCOVaR")) +
      geom_line(aes(y = DDeltaCoVaR, color = "DDeltaCoVaR")) +
      geom_line(aes(y = DVaR, color = "CFVaR")) +
      geom_line(aes(y = UCOVaR, color = "UCOVaR")) +
      geom_line(aes(y = UDeltaCoVaR, color = "UDeltaCoVaR")) +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        breaks = seq(-0.2, 0.2, by = 0.05)
      )+
      labs(title = "", x = "", y = "") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "bottom")
    
    # Imprimir os gráficos
    # Combinar os gráficos: p1 e p2 lado a lado, p3 embaixo
    print(p1)
    print(p2)
    print(p3)
  }
  plot_all_results(results_df1, commodities) 
}# Plotar gráficos para cada par de ativos
{
  # Exemplo: datas devem ser ajustadas conforme sua base
  choques <- list(
    "Pre_EUA_China" = as.Date(c("2016-01-01", "2017-12-31")),
    "EUA_China"     = as.Date(c("2018-01-01", "2020-01-31")),
    "Covid"         = as.Date(c("2020-02-01", "2021-12-31")),
    "Russia_Ucrania"= as.Date(c("2022-01-01", "2023-12-31")),
    "EUA_China_2"= as.Date(c("2024-11-01", "2025-06-18"))
  )
  library(dplyr)
  
  tabela_resumo <- data.frame()
  
  for (commodity in commodities) {
    df <- results_df1[[commodity]]
    for (choque in names(choques)) {
      intervalo <- choques[[choque]]
      
      df_filtrado <- df %>%
        filter(Date >= intervalo[1], Date <= intervalo[2])
      
      media_covar <- mean(df_filtrado$DCOVaR, na.rm = TRUE)
      media_dcovar <- mean(df_filtrado$DDeltaCoVaR, na.rm = TRUE)
      
      tabela_resumo <- rbind(tabela_resumo, data.frame(
        Choque = choque,
        Commodity = commodity,
        CoVaR = media_covar,
        DeltaCoVaR = media_dcovar
      ))
    }
  }
  library(xtable)
  
  # Ajuste de nomes mais amigáveis
  tabela_resumo$Choque <- factor(tabela_resumo$Choque,
                                 levels = names(choques),
                                 labels = c("Pré-EUA-China", "Guerra EUA-China",
                                            "COVID-19", "Rússia-Ukraine","Trump 2")
  )
  
  # Gerar tabela LaTeX
  tabela_latex <- xtable(tabela_resumo, digits = c(0, 0, 0, 4, 4),
                         caption = "Resumo de CoVaR e ∆CoVaR por commodity e por choque",
                         label = "tab:covar_choques")
  
  print(tabela_latex, include.rownames = FALSE, booktabs = TRUE)
  
} # Calcular médias de CoVaR e ∆CoVaR por período
{
  set.seed(123)  # Reprodutibilidade
  n_boot <- 1000  # Número de reamostragens
  tabela_bootstrap <- data.frame()
  
  for (commodity in commodities) {
    df <- results_df1[[commodity]]
    for (choque in names(choques)) {
      intervalo <- choques[[choque]]
      df_filtrado <- df %>%
        filter(Date >= intervalo[1], Date <= intervalo[2])
      
      n <- nrow(df_filtrado)
      
      if (n < 30) next  # Ignorar janelas muito curtas
      
      boot_covars <- numeric(n_boot)
      boot_dcovars <- numeric(n_boot)
      
      for (i in 1:n_boot) {
        sample_indices <- sample(1:n, replace = TRUE)
        boot_sample <- df_filtrado[sample_indices, ]
        
        boot_covars[i] <- mean(boot_sample$DCOVaR, na.rm = TRUE)
        boot_dcovars[i] <- mean(boot_sample$DDeltaCoVaR, na.rm = TRUE)
      }
      
      tabela_bootstrap <- rbind(tabela_bootstrap, data.frame(
        Choque = choque,
        Commodity = commodity,
        CoVaR_Mean = mean(boot_covars),
        CoVaR_Low = quantile(boot_covars, 0.025),
        CoVaR_Up = quantile(boot_covars, 0.975),
        DCoVaR_Mean = mean(boot_dcovars),
        DCoVaR_Low = quantile(boot_dcovars, 0.025),
        DCoVaR_Up = quantile(boot_dcovars, 0.975)
      ))
    }
  }
  library(xtable)
  
  tabela_bootstrap$Choque <- factor(tabela_bootstrap$Choque,
                                    levels = names(choques),
                                    labels = c("Pré-EUA-China", "Guerra EUA-China",
                                               "COVID-19", "Rússia-Ukraine","Trump 2")
  )
  
  print(xtable(tabela_bootstrap, digits = 4,
               caption = "CoVaR e ∆CoVaR com intervalos de confiança via Bootstrap (95%)",
               label = "tab:covar_bootstrap"),
        include.rownames = FALSE, booktabs = TRUE)
  
} #  Intervalos de Confiança para CoVaR e ∆CoVaR via Bootstrap
####################################################
####################################################
{
  # Carregar base de dados
  base_ind_rt$data <- as.Date(base_ind_rt$data)
  base_ind_rt <- as.data.frame(base_ind_rt)
  
  # Seleção de commodities para análise
  commodities <- c("BCOMCN","BCOMSY", "BCOMLC","BCOMLH") 
  base_agre = base_ind_rt %>% dplyr::select(c(data,BCOM, BCOMCN,BCOMSY, BCOMLC,BCOMLH))
} # Seleção de commodities para análise
####################################################
{
  library(strucchange)
  library(knitr)
  library(kableExtra)
  
  # Converter a coluna de datas
  base_ind$data <- as.Date(base_ind$data)
  
  # Variáveis de interesse
  variaveis <- c("BCOM","BCOMCN","BCOMSY", "BCOMLC","BCOMLH")
  
  # Inicializa tabela
  tabela_final <- data.frame()
  
  for (var in variaveis) {
    y <- base_ind[[var]]
    
    # Estimar o modelo geral
    bp_model <- breakpoints(y ~ 1)
    
    # Quebra 0 (sem breakpoints): usa modelo linear simples
    bic_0 <- round(BIC(lm(y ~ 1)), 2)
    linha_0 <- data.frame(
      Variável = var,
      Quebras = 0,
      Quebra_1 = NA,
      Quebra_2 = NA,
      Quebra_3 = NA,
      Quebra_4 = NA,
      Quebra_5 = NA,
      BIC = bic_0
    )
    tabela_final <- rbind(tabela_final, linha_0)
    
    # Para m = 1 a 5
    for (m in 1:5) {
      break_filled <- rep(NA, 5)
      
      # Tenta extrair modelo com m quebras do modelo geral
      bp_m_model <- tryCatch(
        breakpoints(bp_model, breaks = m),
        error = function(e) NULL
      )
      
      if (!is.null(bp_m_model)) {
        bp_m <- bp_m_model$breakpoints
        
        if (!is.null(bp_m)) {
          break_dates <- as.character(base_ind$data[bp_m])
          break_filled[1:length(break_dates)] <- break_dates
        }
        
        bic_val <- round(BIC(bp_m_model), 2)
      } else {
        bic_val <- NA
      }
      
      linha <- data.frame(
        Variável = var,
        Quebras = m,
        Quebra_1 = break_filled[1],
        Quebra_2 = break_filled[2],
        Quebra_3 = break_filled[3],
        Quebra_4 = break_filled[4],
        Quebra_5 = break_filled[5],
        BIC = bic_val
      )
      
      tabela_final <- rbind(tabela_final, linha)
    }
  }
  
  # Gera a tabela em LaTeX
  kable(
    tabela_final,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste de Bai & Perron: Datas de Quebra e Valores de BIC"
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
} # Teste de Bai & Perron em nivel
{library(strucchange)
  
  # Inicializa tabela de resultados
  tabela_testes <- data.frame()
  
  # Suas variáveis
  variaveis <- c("BCOM","BCOMCN","BCOMSY", "BCOMLC","BCOMLH")
  
  # Loop por variável
  for (var in variaveis) {
    y <- base_ind[[var]]
    
    # Modelo com 1 a 5 quebras
    modelo_bp <- tryCatch({
      breakpoints(y ~ 1)
    }, error = function(e) NULL)
    
    # Teste supF (1 quebra)
    supF_res <- tryCatch({
      sctest(y ~ 1, type = "supF")
    }, error = function(e) NA)
    
    estat_supF <- if (!is.na(supF_res)[1]) round(supF_res$statistic, 3) else NA
    pval_supF  <- if (!is.na(supF_res)[1]) round(supF_res$p.value, 4) else NA
    
    # Teste Fseq (sequencial)
    fseq_res <- tryCatch({
      sctest(y ~ 1, type = "Fseq")
    }, error = function(e) NA)
    
    estat_fseq <- if (!is.na(fseq_res)[1]) round(fseq_res$statistic, 3) else NA
    pval_fseq  <- if (!is.na(fseq_res)[1]) round(fseq_res$p.value, 4) else NA
    
    # Adiciona à tabela
    tabela_testes <- rbind(tabela_testes, data.frame(
      Variável = var,
      SupF_Stat = estat_supF,
      SupF_pval = pval_supF,
      Fseq_Stat = estat_fseq,
      Fseq_pval = pval_fseq
    ))
  }
  
  # Exibir resultados
  print(tabela_testes)
  
  kable(
    tabela_testes,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste SupF de Bai & Perron"
  ) %>%
    kable_styling(latex_options = c("hold_position"))
} # test SupF de Bai & Perror em nivel
####################################################
####################################################
{
  library(strucchange)
  library(knitr)
  library(kableExtra)
  
  # Converter a coluna de datas
  base_agre$data <- as.Date(base_agre$data)
  
  # Variáveis de interesse
  variaveis <- c("BCOM","BCOMCN","BCOMSY", "BCOMLC","BCOMLH")
  
  # Inicializa tabela
  tabela_final <- data.frame()
  
  for (var in variaveis) {
    y <- base_agre[[var]]
    
    # Estimar o modelo geral
    bp_model <- breakpoints(y ~ 1)
    
    # Quebra 0 (sem breakpoints): usa modelo linear simples
    bic_0 <- round(BIC(lm(y ~ 1)), 2)
    linha_0 <- data.frame(
      Variável = var,
      Quebras = 0,
      Quebra_1 = NA,
      Quebra_2 = NA,
      Quebra_3 = NA,
      Quebra_4 = NA,
      Quebra_5 = NA,
      BIC = bic_0
    )
    tabela_final <- rbind(tabela_final, linha_0)
    
    # Para m = 1 a 5
    for (m in 1:5) {
      break_filled <- rep(NA, 5)
      
      # Tenta extrair modelo com m quebras do modelo geral
      bp_m_model <- tryCatch(
        breakpoints(bp_model, breaks = m),
        error = function(e) NULL
      )
      
      if (!is.null(bp_m_model)) {
        bp_m <- bp_m_model$breakpoints
        
        if (!is.null(bp_m)) {
          break_dates <- as.character(base_agre$data[bp_m])
          break_filled[1:length(break_dates)] <- break_dates
        }
        
        bic_val <- round(BIC(bp_m_model), 2)
      } else {
        bic_val <- NA
      }
      
      linha <- data.frame(
        Variável = var,
        Quebras = m,
        Quebra_1 = break_filled[1],
        Quebra_2 = break_filled[2],
        Quebra_3 = break_filled[3],
        Quebra_4 = break_filled[4],
        Quebra_5 = break_filled[5],
        BIC = bic_val
      )
      
      tabela_final <- rbind(tabela_final, linha)
    }
  }
  
  # Gera a tabela em LaTeX
  kable(
    tabela_final,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste de Bai & Perron: Datas de Quebra e Valores de BIC"
  ) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"))
} # Teste de Bai & Perron on diff
{library(strucchange)
  
  # Inicializa tabela de resultados
  tabela_testes <- data.frame()
  
  # Suas variáveis
  variaveis <- c("BCOM","BCOMCN","BCOMSY", "BCOMLC","BCOMLH")
  
  # Loop por variável
  for (var in variaveis) {
    y <- base_agre[[var]]
    
    # Modelo com 1 a 5 quebras
    modelo_bp <- tryCatch({
      breakpoints(y ~ 1)
    }, error = function(e) NULL)
    
    # Teste supF (1 quebra)
    supF_res <- tryCatch({
      sctest(y ~ 1, type = "supF")
    }, error = function(e) NA)
    
    estat_supF <- if (!is.na(supF_res)[1]) round(supF_res$statistic, 3) else NA
    pval_supF  <- if (!is.na(supF_res)[1]) round(supF_res$p.value, 4) else NA
    
    # Teste Fseq (sequencial)
    fseq_res <- tryCatch({
      sctest(y ~ 1, type = "Fseq")
    }, error = function(e) NA)
    
    estat_fseq <- if (!is.na(fseq_res)[1]) round(fseq_res$statistic, 3) else NA
    pval_fseq  <- if (!is.na(fseq_res)[1]) round(fseq_res$p.value, 4) else NA
    
    # Adiciona à tabela
    tabela_testes <- rbind(tabela_testes, data.frame(
      Variável = var,
      SupF_Stat = estat_supF,
      SupF_pval = pval_supF,
      Fseq_Stat = estat_fseq,
      Fseq_pval = pval_fseq
    ))
  }
  
  # Exibir resultados
  print(tabela_testes)
  
  kable(
    tabela_testes,
    format = "latex",
    booktabs = TRUE,
    caption = "Resultados do Teste SupF de Bai & Perron"
  ) %>%
    kable_styling(latex_options = c("hold_position"))
} # test SupF de Bai & Perror on diff
####################################################
{
  # Testes preliminares
  realizar_testes <- function(retornos) {
    data.frame(
      shapiro_estatistica = shapiro.test(retornos)$statistic,
      shapiro_pvalue =shapiro.test(retornos)$p.value,
      sf_statistica = sf.test(retornos)$statistic,
      sf_pvalue = sf.test(retornos)$p.value,
      ADF_statistica = adf.test(retornos)$statistic,
      ADF_pvalue = adf.test(retornos)$p.value,
      pp_estatistica = pp.test(retornos)$statistic,
      pp_pvalue = pp.test(retornos)$p.value,
      JarqueBera_estatistica = jarque.bera.test(retornos)$statistic,
      JarqueBera_pvalue = jarque.bera.test(retornos)$p.value,
      ARCH_LM_5_estatistica = ArchTest(retornos, lags = 5)$statistic,
      ARCH_LM_5_pvalue = ArchTest(retornos, lags = 5)$p.value,
      ARCH_LM_10_estatistica = ArchTest(retornos, lags = 10)$statistic,
      ARCH_LM_10_pvalue =ArchTest(retornos, lags = 10)$p.value
    )
  }
} # Testes preliminares
####################################################
{
  # Especificações dos modelos
  spec_garch <- ugarchspec(
    variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  
  spec_dcc <- dccspec(
    uspec = multispec(replicate(2, spec_garch)),
    dccOrder = c(1, 1),
    distribution = "mvt"
  )
  
}# Especificações dos modelos GARCH
####################################################
{
  # Estimações e testes de robustez
  resultados_testes <- list()
  resultados_dcc <- list()
  resultados_robustez <- list()
  results_df <- list()
  resultados <- list()
  
  VaR_data <- data.frame()
  CoVaR_data <- data.frame()
  DeltaCoVaR_data <- data.frame()
} # criando as tabelas de resultados
####################################################
{
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    resultados_testes[[commodity]] <- realizar_testes(retornos[, commodity])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    AIC = as.numeric(rugarch::infocriteria(fit_dcc)[1])
    BIC =  as.numeric(rugarch::infocriteria(fit_dcc)[2])
    Shibata =  as.numeric(rugarch::infocriteria(fit_dcc)[3])
    Hosking_1pvalue = as.numeric(portest(residuos,lags= 1,test="Hosking")["p-value"])
    Hosking_1statistic = as.numeric(portest(residuos,lags= 1,test="Hosking")["statistic"])
    Hosking_5pvalue =as.numeric( portest(residuos,lags= 5,test="Hosking")["p-value"])
    Hosking_5statistic =as.numeric( portest(residuos,lags= 5,test="Hosking")["statistic"])
    Hosking_10pvalue = as.numeric(portest(residuos,lags= 10,test="Hosking")["p-value"])
    Hosking_10statistic =as.numeric( portest(residuos,lags= 10,test="Hosking")["statistic"])
    LjungBox_1pvalue  = as.numeric(Box.test(residuos, lag = 1, type = "Ljung-Box")$p.value)
    LjungBox_1statistic = as.numeric(Box.test(residuos, lag = 1, type = "Ljung-Box")$statistic)
    LjungBox_5pvalue = as.numeric(Box.test(residuos, lag = 5, type = "Ljung-Box")$p.value)
    LjungBox_5statistic = as.numeric(Box.test(residuos, lag = 5, type = "Ljung-Box")$statistic)
    LjungBox_10pvalue = as.numeric(Box.test(residuos, lag = 10, type = "Ljung-Box")$p.value)
    LjungBox_10statistic = as.numeric(Box.test(residuos, lag = 10, type = "Ljung-Box")$statistic)
    ARCH_LM_1pvalue = as.numeric(ArchTest(residuos, lags = 1)$p.value)
    ARCH_LM_1statistic =as.numeric(ArchTest(residuos, lags = 1)$statistic)
    
    resultados_robustez[[commodity]] <- data.frame(
      AIC, BIC, Shibata, Hosking_1pvalue,  Hosking_1statistic, Hosking_5pvalue,
      Hosking_5statistic, Hosking_10pvalue, Hosking_10statistic, LjungBox_1pvalue,
      LjungBox_1statistic, LjungBox_5pvalue, LjungBox_5statistic,LjungBox_10pvalue,
      LjungBox_10statistic, ARCH_LM_1pvalue,ARCH_LM_1statistic
    )
    
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity, VaR_1 = VaR_asset, VaR_5 = VaR_asset_1)
    CoVaR_data <- data.frame(Commodity = commodity, CoVaR_1 = DCOVaR, CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity, DeltaCoVaR_1 = DDeltaCoVaR, DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    
    # Armazenar resultados
    resultados[[commodity]] <- list(Teste_preliminares = resultados_testes,
                                    DCC_GARCH= resultados_dcc,
                                    Teste_robustez= resultados_robustez,
                                    COVAR_DVAR = results_df)
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    
    
  } 
} # estimando os modelos(GARCH, VaR, COVaR e Delta COVaR)
{
  # Função para definir os níveis de significância
  significancia <- function(pval) {
    if (pval <= 0.001) {
      return("***")
    } else if (pval <= 0.01) {
      return("**")
    } else if (pval <= 0.05) {
      return("*")
    } else if (pval <= 0.1) {
      return(".")
    } else {
      return("ns")
    }
  }
  
  # Criar a tabela formatada
  
  
  # Converter lista para data.frame organizado
  tabela <- lapply(names(resultados_testes), function(ativo) {
    df <- resultados_testes[[ativo]]
    df_formatado <- data.frame(
      Ativo = ativo,
      Shapiro = paste0(round(df$shapiro_estatistica, 3), " (", significancia(df$shapiro_pvalue), ")"),
      SF = paste0(round(df$sf_statistica, 3), " (", significancia(df$sf_pvalue), ")"),
      ADF = paste0(round(df$ADF_statistica, 3), " (", significancia(df$ADF_pvalue), ")"),
      PP = paste0(round(df$pp_estatistica, 3), " (", significancia(df$pp_pvalue), ")"),
      JarqueBera = paste0(round(df$JarqueBera_estatistica, 3), " (", significancia(df$JarqueBera_pvalue), ")"),
      ARCH_LM_5 = paste0(round(df$ARCH_LM_5_estatistica, 3), " (", significancia(df$ARCH_LM_5_pvalue), ")"),
      ARCH_LM_10 = paste0(round(df$ARCH_LM_10_estatistica, 3), " (", significancia(df$ARCH_LM_10_pvalue), ")")
    )
    return(df_formatado)
  }) %>% bind_rows()
  
  # Gerar tabela LaTeX usando kableExtra
  
  latex_table <- tabela %>% t() %>%
    kable("latex", booktabs = TRUE, align = "c", caption = "Resultados dos Testes Estatísticos") %>%
    kable_styling(latex_options = c("hold_position"))
  
  print(latex_table)
  
  
} # tabela testes preliminares 
{
  # Função para limpar nomes das colunas e linhas
  limpar_nomes <- function(x) {
    if (is.matrix(x)) {
      rownames(x) <- gsub("\\[.*?\\]", "", rownames(x))  # Remove colchetes nos nomes das linhas
      rownames(x) <- gsub("\\.", "", rownames(x))        # Remove pontos nos nomes das linhas
      colnames(x) <- gsub("\\[.*?\\]", "", colnames(x))  # Remove colchetes nos nomes das colunas
      colnames(x) <- gsub("\\.", "", colnames(x))        # Remove pontos nos nomes das colunas
    }
    return(as.data.frame(x))  # Converte matriz para data frame
  }
  
  # Função para adicionar nível de significância ao coeficiente
  formatar_coeficiente <- function(est, pval) {
    if (pval <= 0.01) {
      return(sprintf("%.4f ***", est))
    } else if (pval <= 0.05) {
      return(sprintf("%.4f **", est))
    } else if (pval <= 0.10) {
      return(sprintf("%.4f *", est))
    } else {
      return(sprintf("%.4f", est))
    }
  }
  
  # Criar tabela com coeficientes formatados
  DCC_results_table <- do.call(cbind, lapply(names(resultados_dcc), function(commodity) {
    dados_limpos <- limpar_nomes(resultados_dcc[[commodity]])
    
    # Extrair coeficiente estimado e p-value
    estimativas <- dados_limpos[,1]
    pvalores <- dados_limpos[, 4]
    
    # Formatar coeficientes com níveis de significância
    coef_formatado <- mapply(formatar_coeficiente, estimativas, pvalores)
    
    # Criar data frame
    data.frame(coef_formatado, row.names = rownames(dados_limpos))
  }))
  
  # Nomear colunas com as commodities
  colnames(DCC_results_table) <- names(resultados_dcc)
  
  # Criar painel "a"
  panel_a <- kbl(DCC_results_table, caption = "Panel a) DCC-GARCH Model Statistics", format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    add_header_above(c("Coeficients" = length(names(resultados_dcc)))) %>%
    footnote(general = "Note: 1. *, **, ***, correspond to the significance level of 1%, 5% and 10% respectively.")
  
  # Panel b) Robustness tests
  # Converter lista para data.frame organizado
  robustness_table <- lapply(names(resultados_robustez), function(ativo) {
    df <- resultados_robustez[[ativo]]
    df_formatado <- data.frame(
      Ativo = ativo,
      AIC = paste0(round(df$AIC, 3)),
      BIC = paste0(round(df$BIC, 3)),
      Shibata = paste0(round(df$Shibata, 3)),
      Hosking_1 = paste0(round(df$Hosking_1statistic, 3), " (", significancia(df$Hosking_1pvalue), ")"),
      Hosking_5 = paste0(round(df$Hosking_5statistic, 3), " (", significancia(df$Hosking_5pvalue), ")"),
      Hosking_10 = paste0(round(df$Hosking_10statistic, 3), " (", significancia(df$Hosking_10pvalue), ")"),
      LjungBox_1 = paste0(round(df$LjungBox_1statistic, 3), " (", significancia(df$LjungBox_1pvalue), ")"),
      LjungBox_5 = paste0(round(df$LjungBox_5statistic, 3), " (", significancia(df$LjungBox_5pvalue), ")"),
      LjungBox_10 = paste0(round(df$LjungBox_10statistic, 3), " (", significancia(df$LjungBox_10pvalue), ")"),
      ARCH_LM_1 = paste0(round(df$ARCH_LM_1statistic, 3), " (", significancia(df$ARCH_LM_1pvalue), ")")
    )
    return(df_formatado)
  }) %>% bind_rows()
  
  robustness_table <- robustness_table %>% t()
  panel_b <- kbl(robustness_table, caption = "Panel b) Robustness tests", format = "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = c("hold_position"))
  # Verifique se a transposição gerou o resultado desejad
  
  # Imprimir os painéis
  print(panel_a)
  print(panel_b)
} # tabela resultado DCC-GARCH e testes de robustez
####################################################
{
  library(ggplot2)
  library(dplyr)
  library(xtable)
  library(patchwork)
  library(scales)
  # Lista para armazenar resultados
  tabela_lista <- list()
  graficos_lista <- list()
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    corr <- rcor(fit_dcc)
    correlacao <- corr[1, 2, ]
    
    df_cor <- data.frame(
      correlacao = correlacao,
      lag_corr = lag(correlacao)
    ) %>% na.omit()
    
    modelo_base <- lm(correlacao ~ lag_corr, data = df_cor)
    modelo_markov <- msmFit(modelo_base, k = 2, p = 0, sw = rep(TRUE, 3))
    
    regimes_probs <- modelo_markov@Fit@smoProb
    regime_discreto <- apply(regimes_probs, 1, which.max)[-1]
    
    df_plot <- data.frame(
      Data = base_agre$data[seq_along(correlacao)][-1],
      Correlacao = df_cor$correlacao,
      Regime = factor(regime_discreto),
      Commodity = commodity)
    
    # Adiciona bloco de regime
    regime_blocks <- df_plot %>%
      mutate(index = row_number()) %>%
      mutate(change = Regime != lag(Regime, default = first(Regime))) %>%
      mutate(block = cumsum(change)) %>%
      group_by(block, Regime, Commodity) %>%
      summarise(
        start = first(Data),
        end = last(Data),
        .groups = "drop"
      )
    
    graficos_lista[[commodity]] <- list(df = df_plot, rects = regime_blocks)
    # Blocos de regime
    regime_blocks <- df_plot %>%
      mutate(change = Regime != lag(Regime, default = first(Regime))) %>%
      mutate(block = cumsum(change)) %>%
      group_by(block, Regime, Commodity) %>%
      summarise(
        duracao = n(),
        start = first(Data),
        end = last(Data),
        .groups = "drop"
      )
    
    # Frequência (número de blocos por regime)
    frequencias <- regime_blocks %>%
      group_by(Regime) %>%
      summarise(frequencia = n(), .groups = "drop")
    
    # Estatísticas por regime
    resumo <- df_plot %>%
      group_by(Regime) %>%
      summarise(
        duracao_total = n(),
        media_corr = mean(Correlacao),
        desvio_corr = sd(Correlacao),
        min_corr = min(Correlacao),
        max_corr = max(Correlacao),
        .groups = "drop"
      ) %>%
      left_join(frequencias, by = "Regime") %>%
      mutate(
        duracao_media = duracao_total / frequencia,
        amplitude_corr = max_corr - min_corr,
        Commodity = commodity,
        regime = Regime
      )
    
    # Corrigir duração total se necessário
    total_max <- nrow(df_cor)
    if (sum(resumo$duracao_total) > total_max) {
      excesso <- sum(resumo$duracao_total) - total_max
      resumo$duracao_total[1] <- resumo$duracao_total[1] - excesso
    }
    
    # Probabilidades de permanência (P_ii)
    trans_mat <- modelo_markov@transMat
    resumo$P_ii <- diag(trans_mat)
    
    # Volatility relativa
    volat <- resumo$desvio_corr
    if (length(volat) == 2) {
      resumo$Volatility_relativa <- volat / min(volat)
    } else {
      resumo$Volatility_relativa <- NA
    }
    
    # Duração máxima de bloco por regime
    duracao_max <- regime_blocks %>%
      group_by(Regime) %>%
      summarise(duracao_max_bloco = max(duracao), .groups = "drop")
    
    resumo <- left_join(resumo, duracao_max, by = "Regime")
    
    tabela_lista[[commodity]] <- resumo
  }
  
  # Juntar tudo
  df_todos <- do.call(rbind, lapply(graficos_lista, `[[`, "df"))
  rects_todos <- do.call(rbind, lapply(graficos_lista, `[[`, "rects"))
  
  grafico_facetado <- ggplot(df_todos, aes(x = Data)) +
    geom_rect(data = rects_todos,
              aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = Regime),
              alpha = 0.2, inherit.aes = FALSE) +
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(df_todos$Correlacao)*0.75,
             label = "Trade War", color = "black", vjust = -1, hjust = -0.1, size = 2.5) +
    geom_line(aes(y = Correlacao), color = "blue", size = 0.7) +
    facet_wrap(~ Commodity, scales = "free_y", ncol = 2) +
    theme_minimal() +
    labs(x = "Data", y = "Correlation") +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      breaks = seq(-0.2, 0.8, by = 0.1)
    )+
    scale_fill_manual(values = c("1" = "black", "2" = "red")) +
    theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "bottom")
  
  print(grafico_facetado)
  
  # Unir resultados
  tabela_final <- bind_rows(tabela_lista)
  
  # Ajustar colunas
  tabela_final <- tabela_final %>%
    dplyr::select(Commodity, regime, frequencia, duracao_media, duracao_total,
                  duracao_max_bloco, media_corr, desvio_corr, amplitude_corr,
                  P_ii, Volatility_relativa)
  
  # Exportar tabela LaTeX
  print(
    xtable(
      tabela_final,
      digits = c(0, 0, 0, 0, 1, 0, 3, 3, 3, 3, 3, 3),
      caption = "Estatísticas dos Regimes de Correlation por Commodity",
      label = "tab:estatisticas_regime",
      align = c("l", "l", "c", "c", "c", "c", "c", "c", "c", "c", "c","c")
    ),
    include.rownames = FALSE,
    caption.placement = "top",
    sanitize.text.function = identity
  )
  
} # Tabela e grafico do modelo de Markov Switching 
{library(ggplot2)
  library(dplyr)
  library(xtable)
  
  dir.create("graficos_markov", showWarnings = FALSE)  # Criar pasta para gráficos
  tabela_lista <- list()
  tabela_transicoes <- list()  # Guardar tabelas para exportar depois
  
  for (commodity in commodities) {
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    corr <- rcor(fit_dcc)
    correlacao <- corr[1, 2, ]
    
    df_cor <- data.frame(
      correlacao = correlacao,
      lag_corr = lag(correlacao)
    ) %>% na.omit()
    
    modelo_base <- lm(correlacao ~ lag_corr, data = df_cor)
    modelo_markov <- msmFit(modelo_base, k = 2, p = 0, sw = rep(TRUE, 3))
    
    regimes_probs <- modelo_markov@Fit@filtProb
    regime_discreto <- apply(regimes_probs, 1, which.max)
    
    df_plot <- data.frame(
      Data = base_agre$data[seq_along(correlacao)][-1],
      Correlacao = df_cor$correlacao,
      Regime = factor(regime_discreto),
      Commodity = commodity
    )
    
    # Estatísticas do regime
    resumo <- df_plot %>%
      group_by(Regime) %>%
      summarise(desvio_corr = sd(Correlacao), .groups = "drop") %>%
      arrange(desvio_corr) %>%
      mutate(Regime_Label = c("Baixa Volatility", "Alta Volatility"))
    
    df_plot$Regime_Label <- factor(resumo$Regime_Label[match(df_plot$Regime, resumo$Regime)])
    
    # ▸ Gerar gráfico e salvar
    p <- ggplot(df_plot, aes(x = Data, y = Correlacao)) +
      geom_line() +
      geom_point(aes(color = Regime_Label), size = 0.8) +
      scale_color_manual(values = c("blue", "red")) +
      labs(title = paste("Regimes de Correlation -", commodity),
           x = "Tempo", y = "Correlation DCC", color = "Regime") +
      theme_minimal()
    
    # ▸ Matriz de transição LaTeX
    trans_mat_df <- data.frame(
      De_Para = c("1→1", "1→2", "2→1", "2→2"),
      Probabilidade = as.vector(modelo_markov@transMat)
    )
    
    tabela_transicoes[[commodity]] <- xtable(
      trans_mat_df,
      caption = paste("Matriz de Transição de Regimes -", commodity),
      label = paste0("tab:transicao_", commodity)
    )
    
    # ▸ Estatísticas do regime (completa)
    # ... [use seu bloco já existente aqui para gerar `resumo` e `tabela_lista[[commodity]]` como antes] ...
  }
  for (commodity in names(tabela_transicoes)) {
    print(tabela_transicoes[[commodity]],
          include.rownames = FALSE, booktabs = TRUE,
          caption.placement = "top", sanitize.text.function = identity)
  }
  # ▸ Criar uma tabela combinada
  tabela_transicoes_unica <- bind_rows(
    lapply(names(tabela_transicoes), function(nome) {
      df <- as.data.frame(tabela_transicoes[[nome]])
      df$Commodity <- nome
      df
    })
  )
  
  # Reorganizar colunas
  tabela_transicoes_unica <- tabela_transicoes_unica[, c("Commodity", "De_Para", "Probabilidade")]
  
  # Exportar
  print(xtable(
    tabela_transicoes_unica,
    caption = "Matriz de Transição de Regimes para Cada Commodity",
    label = "tab:transicao_todas"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  caption.placement = "top",
  sanitize.text.function = identity)
  
  
} # Matriz de Transição de Regimes
####################################################
{
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    
    # Cálculo da assimetria e curtose dos resíduos padronizados
    residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
    residuos_commodity <- na.omit(residuos_commodity)
    skew <- moments::skewness(residuos_commodity)
    kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    
    # VaR Cornish-Fisher dinâmico
    VaR_CF <- z_cf * sigma_t[, 2]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_5 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      CFVaR = as.numeric(VaR_CF),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    # Retornos reais do ativo
    retornos_ativos <- retornos[, commodity]
    
    # Backtest VaR Normal (1%)
    violacoes_VaR <- which(retornos_ativos < VaR_asset)
    n_violacoes <- length(violacoes_VaR)
    
    # Backtest VaR Cornish-Fisher
    violacoes_CFVaR <- which(retornos_ativos < VaR_CF)
    n_violacoes_CF <- length(violacoes_CFVaR)
    
    # Proporção de violações
    p_violacoes <- n_violacoes / length(VaR_asset)
    p_violacoes_CF <- n_violacoes_CF / length(VaR_CF)
    
    # Teste Kupiec para VaR Normal
    test_norm <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_asset)
    
    # Teste Kupiec para VaR Cornish-Fisher
    test_cf <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_CF)
    
    # Armazenar resultados
    resultados[[commodity]] <- list(DCC_GARCH= resultados_dcc,
                                    COVAR_DVAR = results_df)
    # Armazenar os resultados no objeto resultados
    resultados[[commodity]]$Backtest_VaR <- list(
      VaR_Normal = list(Violacoes = n_violacoes, Proporcao = p_violacoes),
      VaR_CFisher = list(Violacoes = n_violacoes_CF, Proporcao = p_violacoes_CF)
    )
    resultados[[commodity]]$Kupiec_Test <- list(
      Normal = test_norm,
      CornishFisher = test_cf
    )
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    # Adiciona as colunas necessárias para gráfico
    df_plot <- data.frame(
      Date = base_agre$data,
      Return = as.numeric(retornos_ativos),
      VaR_Normal = as.numeric(VaR_asset),
      VaR_CF = as.numeric(VaR_CF)
    )
    
  } 
} # estimando os modelos(GARCH, VaR, CF VaR, COVaR e Delta COVaR)
{
  library(dplyr)
  library(knitr)
  
  kupiec_summary <- do.call(rbind, lapply(names(resultados), function(commodity) {
    test_norm <- resultados[[commodity]]$Kupiec_Test$Normal
    test_cf <- resultados[[commodity]]$Kupiec_Test$CornishFisher
    
    data.frame(
      Commodity = commodity,
      Modelo = c("Normal", "Cornish-Fisher"),
      `LR Estatística` = c(test_norm$uc.LRstat, test_cf$uc.LRstat),
      `Valor-p` = c(test_norm$uc.LRp, test_cf$uc.LRp),
      `Excedências` = c(sum(resultados[[commodity]]$Backtest_VaR$VaR_Normal$Violacoes),
                        sum(resultados[[commodity]]$Backtest_VaR$VaR_CFisher$Violacoes)),
      `Esperado` = rep(0.01 * nrow(resultados[[commodity]]$COVAR_DVAR[[commodity]]), 2)
    )
  }))
  melhor_modelo_df <- kupiec_summary %>%
    group_by(Commodity) %>%
    summarize(
      Melhor_Modelo = case_when(
        all(`Valor.p` > 0.05) ~ "Ambos aceitos",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05 & `Modelo` == "Cornish-Fisher" & `Valor.p` <= 0.05) ~ "Normal",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05 & `Modelo` == "Normal" & `Valor.p` <= 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05) ~ "Normal",
        TRUE ~ "Ambos rejeitados"
      )
    )
  
  kupiec_summary <- left_join(kupiec_summary, melhor_modelo_df, by = "Commodity")
  
  
  # Exportar como LaTeX com kable
  kable(kupiec_summary, format = "latex", booktabs = TRUE, digits = 4,
        caption = "Teste de cobertura (Kupiec) para os modelos VaR por commodity.",
        col.names = c("Commodity", "Modelo", "Estatística LR", "Valor-p", "Excedências", "Esperado", "Melhor modelo"))
  
} # tabela do Teste de cobertura (Kupiec) para os modelos VaR por commodity
{
  
  library(ggplot2)
  
  # Criar e salvar gráficos separados para cada commodity
  for (commodity in names(resultados)) {
    
    df_plot <- data.frame(
      Date = resultados[[commodity]]$COVAR_DVAR[[commodity]]$Date,
      Return = resultados[[commodity]]$COVAR_DVAR[[commodity]]$Asset2_Return,
      VaR_Normal = resultados[[commodity]]$COVAR_DVAR[[commodity]]$DVaR,
      VaR_CF = resultados[[commodity]]$COVAR_DVAR[[commodity]]$CFVaR
    )
    
    p <- ggplot(df_plot, aes(x = Date)) +
      geom_line(aes(y = Return), color = "black", alpha = 0.6) +
      geom_line(aes(y = VaR_Normal), color = "red", linetype = "dashed") +
      geom_line(aes(y = VaR_CF), color = "blue", linetype = "dotted") +
      labs(title = paste("VaR Normal vs Cornish-Fisher -", commodity),
           y = "Retorno e VaR",
           x = "Data") +
      theme_minimal()
    
    print(p)  # mostra o gráfico no console
    
    
  }
  
} # grafico comparativo VaR e CF VaR
####################################################
{
  # Inicializar data.frame para armazenar os resultados
  tabela_ks <- data.frame(
    Commodity = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop para extrair resultados e montar a tabela
  for (commodity in names(resultados)) {
    if (!is.null(resultados[[commodity]]$KS_test)) {
      tabela_ks <- rbind(tabela_ks, data.frame(
        Commodity = commodity,
        Statistic = resultados[[commodity]]$KS_test$statistic,
        P_Value = resultados[[commodity]]$KS_test$p_value
      ))
    }
  }
  
  library(xtable)
  
  # Criar tabela LaTeX
  tabela_latex <- xtable(tabela_ks, 
                         caption = "Resultados do Teste Kolmogorov-Smirnov para Dependências de Cauda",
                         label = "tab:ks_test")
  print(tabela_latex, include.rownames = FALSE)
  
} # Teste Kolmogorov-Smirnov para Dependências de Cauda
####################################################
{
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    
    # Cálculo da assimetria e curtose dos resíduos padronizados
    residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
    residuos_commodity <- na.omit(residuos_commodity)
    skew <- moments::skewness(residuos_commodity)
    kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR dinâmico para cada tempo
    VaR_BCOM <- q * sigma_t[, 1]
    VaR_BCOM_1 <- q_1 * sigma_t[, 1]
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    
    # VaR Cornish-Fisher dinâmico
    VaR_CF <- z_cf * sigma_t[, 2]
    
    VaR_asset <- q * sigma_t[, 2]
    VaR_asset_1 <- q_1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <- q * sigma_t[, 2] + q * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- q_1 * sigma_t[, 2] + q_1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_5 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      CFVaR = as.numeric(VaR_CF),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    # Retornos reais do ativo
    retornos_ativos <- retornos[, commodity]
    
    # Backtest VaR Normal (1%)
    violacoes_VaR <- which(retornos_ativos < VaR_asset)
    n_violacoes <- length(violacoes_VaR)
    
    # Backtest VaR Cornish-Fisher
    violacoes_CFVaR <- which(retornos_ativos < VaR_CF)
    n_violacoes_CF <- length(violacoes_CFVaR)
    
    # Proporção de violações
    p_violacoes <- n_violacoes / length(VaR_asset)
    p_violacoes_CF <- n_violacoes_CF / length(VaR_CF)
    
    # Teste Kupiec para VaR Normal
    test_norm <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_asset)
    
    # Teste Kupiec para VaR Cornish-Fisher
    test_cf <- VaRTest(alpha = 0.01, actual = retornos_ativos, VaR = VaR_CF)
    
    # Armazenar resultados
    resultados[[commodity]] <- list(DCC_GARCH= resultados_dcc,
                                    COVAR_DVAR = results_df)
    # Armazenar os resultados no objeto resultados
    resultados[[commodity]]$Backtest_VaR <- list(
      VaR_Normal = list(Violacoes = n_violacoes, Proporcao = p_violacoes),
      VaR_CFisher = list(Violacoes = n_violacoes_CF, Proporcao = p_violacoes_CF)
    )
    resultados[[commodity]]$Kupiec_Test <- list(
      Normal = test_norm,
      CornishFisher = test_cf
    )
    # Teste Kolmogorov-Smirnov para dependências de cauda
    lower_tail <- DDeltaCoVaR  # Delta CoVaR para cauda inferior
    upper_tail <- UDeltaCoVaR  # Delta CoVaR para cauda superior
    
    # Teste KS para comparar distribuições
    ks_test_result <- ks.test(lower_tail, upper_tail, alternative = "greater")
    
    # Armazenar resultados do teste KS
    resultado_KS <- list(
      statistic = ks_test_result$statistic,
      p_value = ks_test_result$p.value
    )
    
    # Armazenar junto com os resultados
    resultados[[commodity]]$KS_test <- resultado_KS
    # Adiciona as colunas necessárias para gráfico
    df_plot <- data.frame(
      Date = base_agre$data,
      Return = as.numeric(retornos_ativos),
      VaR_Normal = as.numeric(VaR_asset),
      VaR_CF = as.numeric(VaR_CF)
    )
    
  } 
} # estimando os modelos(GARCH, VaR, CF VaR, COVaR e Delta COVaR)
{
  library(dplyr)
  library(knitr)
  
  kupiec_summary <- do.call(rbind, lapply(names(resultados), function(commodity) {
    test_norm <- resultados[[commodity]]$Kupiec_Test$Normal
    test_cf <- resultados[[commodity]]$Kupiec_Test$CornishFisher
    
    data.frame(
      Commodity = commodity,
      Modelo = c("Normal", "Cornish-Fisher"),
      `LR Estatística` = c(test_norm$uc.LRstat, test_cf$uc.LRstat),
      `Valor-p` = c(test_norm$uc.LRp, test_cf$uc.LRp),
      `Excedências` = c(sum(resultados[[commodity]]$Backtest_VaR$VaR_Normal$Violacoes),
                        sum(resultados[[commodity]]$Backtest_VaR$VaR_CFisher$Violacoes)),
      `Esperado` = rep(0.01 * nrow(resultados[[commodity]]$COVAR_DVAR[[commodity]]), 2)
    )
  }))
  melhor_modelo_df <- kupiec_summary %>%
    group_by(Commodity) %>%
    summarize(
      Melhor_Modelo = case_when(
        all(`Valor.p` > 0.05) ~ "Ambos aceitos",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05 & `Modelo` == "Cornish-Fisher" & `Valor.p` <= 0.05) ~ "Normal",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05 & `Modelo` == "Normal" & `Valor.p` <= 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Cornish-Fisher" & `Valor.p` > 0.05) ~ "Cornish-Fisher",
        any(`Modelo` == "Normal" & `Valor.p` > 0.05) ~ "Normal",
        TRUE ~ "Ambos rejeitados"
      )
    )
  
  kupiec_summary <- left_join(kupiec_summary, melhor_modelo_df, by = "Commodity")
  
  
  # Exportar como LaTeX com kable
  kable(kupiec_summary, format = "latex", booktabs = TRUE, digits = 4,
        caption = "Teste de cobertura (Kupiec) para os modelos VaR por commodity.",
        col.names = c("Commodity", "Modelo", "Estatística LR", "Valor-p", "Excedências", "Esperado", "Melhor modelo"))
  
} # tabela do Teste de cobertura (Kupiec) para os modelos VaR por commodity
####################################################
{
  results_df1 <- list()
  for (commodity in commodities) {
    print(paste("Processando:", commodity))
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    fit_dcc <- dccfit(spec_dcc, data = retornos)
    coef_dcc <- fit_dcc@mfit$matcoef
    resultados_dcc[[commodity]] <- coef_dcc
    residuos <- as.vector(fit_dcc@mfit[["stdresid"]])
    
    
    # Cálculo da assimetria e curtose dos resíduos padronizados
    residuos_commodity <- fit_dcc@mfit[["stdresid"]][, 2]  # resíduos da commodity
    residuos_commodity <- na.omit(residuos_commodity)
    skew <- moments::skewness(residuos_commodity)
    kurt <- moments::kurtosis(residuos_commodity) - 3  # excesso de curtose
    # Estimação de VaR, CoVaR e Delta CoVaR
    # Quantis desejados (nível de significância)
    q <- qnorm(0.01)   # para 1%
    q_1 <- qnorm(0.99) # para 99%
    
    # Quantil ajustado com expansão de Cornish-Fisher (nível 1%)
    z_cf <- q + (1/6)*(q^2 - 1)*skew + (1/24)*(q^3 - 3)*kurt - (1/36)*(2*q^3 - 5*q)*skew^2
    # Quantil ajustado com expansão de Cornish-Fisher (nível 99%)
    z_cf1 <- q_1 + (1/6)*(q_1^2 - 1)*skew + (1/24)*(q_1^3 - 3)*kurt - (1/36)*(2*q_1^3 - 5*q_1)*skew^2
    # Volatilitys condicionais e Correlation dinâmica
    sigma_t <- sigma(fit_dcc)
    corr <- rcor(fit_dcc)
    corr_t <- corr[1, 2, ]
    
    # VaR  Cornish-Fisher dinâmico dinâmico para cada tempo
    VaR_BCOM <-  z_cf * sigma_t[, 1]
    VaR_BCOM_1 <- z_cf1 * sigma_t[, 1]
    
    # VaR Cornish-Fisher dinâmico
    
    VaR_asset <-  z_cf * sigma_t[, 2]
    VaR_asset_1 <- z_cf1 * sigma_t[, 2]
    
    # CoVaR dinâmico
    DCOVaR <-  z_cf * sigma_t[, 2] +  z_cf * corr_t * (VaR_BCOM / VaR_asset) * sigma_t[, 2]
    UCOVaR <- z_cf1 * sigma_t[, 2] + z_cf1 * corr_t * (VaR_BCOM_1 / VaR_asset_1) * sigma_t[, 2]
    
    # Delta CoVaR
    DDeltaCoVaR <- DCOVaR - VaR_asset
    UDeltaCoVaR <- UCOVaR - VaR_asset_1
    
    
    VaR_data <- data.frame(Commodity = commodity,
                           VaR_1 = VaR_asset,
                           VaR_99 = VaR_asset_1,
                           VaR_CF_1 = VaR_CF)
    CoVaR_data <- data.frame(Commodity = commodity,
                             CoVaR_1 = DCOVaR,
                             CoVaR_99 = UCOVaR)
    DeltaCoVaR_data <-data.frame(Commodity = commodity,
                                 DeltaCoVaR_1 = DDeltaCoVaR,
                                 DeltaCoVaR_99 = UDeltaCoVaR)
    
    # Criar data frame com resultados
    results_df1[[commodity]] <- data.frame(
      Date = base_agre$data,
      Asset1_Volatility = as.numeric(sigma_t[, 1]),
      Asset2_Volatility = as.numeric(sigma_t[, 2]),
      Correlation = as.numeric(corr_t),
      DCOVaR = as.numeric(DCOVaR),
      UCOVaR = as.numeric(UCOVaR),
      DDeltaCoVaR = as.numeric(DDeltaCoVaR),
      UDeltaCoVaR = as.numeric(UDeltaCoVaR),
      DVaR = as.numeric(VaR_asset),
      Asset1_Return = retornos[, "BCOM"],
      Asset2_Return = retornos[, commodity]
    )
    
    
  } 
} # estimando os modelos(GARCH, VaR, COVaR e Delta COVaR usando a expansão de Corner fisher)
{
  
  library(patchwork)
  library(scales)
  # Função para consolidar e plotar todos os gráficos por análise
  plot_all_results <- function(results_df_list, commodities, asset_base = "BCOM") {
    library(ggplot2)
    library(dplyr)
    
    # Consolidar todos os resultados com uma coluna "Par"
    all_data <- bind_rows(lapply(commodities, function(com) {
      df <- results_df_list[[com]]
      df$Par <- paste(asset_base, com, sep = " - ")
      return(df)
    }))
    
    # Gráfico de Volatility Condicional
    p1 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(all_data$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(all_data$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(all_data$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(all_data$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.04, size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(all_data$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_line(aes(y = Asset1_Volatility), color = "blue") +
      geom_line(aes(y = Asset2_Volatility), color = "red") +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(labels = percent_format(accuracy = 1))+
      labs(title = "", x = "", y = "Volatility") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0) )
    
    # Gráfico de Correlation Dinâmica
    p2 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(results_df1$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(results_df1$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(results_df1$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(results_df1$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(results_df1$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_line(aes(y = Correlation), color = "blue") +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        breaks = seq(-1, 1, by = 0.10))+
      labs(title = "", x = "", y = "Correlation") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
    
    # Gráfico de COVaR / DeltaCoVaR
    p3 <- ggplot(all_data, aes(x = Date)) +
      geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_rect(aes(xmin = inicio_Trump2[1], xmax = inicio_Trump2[2], ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
      annotate("text", x = data_marcacao, y = max(results_df1$asset1), label = "Trade War", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
      annotate("text", x = covid_inicio, y = max(results_df1$asset1), label = "beginning of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
      annotate("text", x = covid_fim, y = max(results_df1$asset1), label = "End of Covid", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
      annotate("text", x = russia_ucrania, y = max(results_df1$asset1), label = "Rússia Ukraine", color = "black", vjust = -1, hjust = -.1, size = 2) +
      geom_vline(xintercept = as.numeric(inicio_Trump2[1]), linetype = "dashed", color = "black") +
      annotate("text", x = inicio_Trump2[1], y = max(results_df1$asset1), label = "Trump 2", color = "black", vjust = -1, hjust = -.1,size = 2) +
      geom_line(aes(y = DCOVaR, color = "DCOVaR")) +
      geom_line(aes(y = DDeltaCoVaR, color = "DDeltaCoVaR")) +
      geom_line(aes(y = DVaR, color = "CFVaR")) +
      geom_line(aes(y = UCOVaR, color = "UCOVaR")) +
      geom_line(aes(y = UDeltaCoVaR, color = "UDeltaCoVaR")) +
      scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        breaks = seq(-0.2, 0.2, by = 0.05)
      )+
      labs(title = "", x = "", y = "") +
      facet_wrap(~ Par, scales = "free_y", ncol = 2)+
      theme_classic() +
      theme(axis.text.x = element_text(angle = -45, hjust = 0), legend.position = "bottom")
    
    # Imprimir os gráficos
    # Combinar os gráficos: p1 e p2 lado a lado, p3 embaixo
    print(p1)
    print(p2)
    print(p3)
  }
  plot_all_results(results_df1, commodities) 
}# Plotar gráficos para cada par de ativos
{
  # Exemplo: datas devem ser ajustadas conforme sua base
  choques <- list(
    "Pre_EUA_China" = as.Date(c("2016-01-01", "2017-12-31")),
    "EUA_China"     = as.Date(c("2018-01-01", "2020-01-31")),
    "Covid"         = as.Date(c("2020-02-01", "2021-12-31")),
    "Russia_Ucrania"= as.Date(c("2022-01-01", "2023-12-31")),
    "EUA_China_2"= as.Date(c("2024-11-01", "2025-06-18"))
  )
  library(dplyr)
  
  tabela_resumo <- data.frame()
  
  for (commodity in commodities) {
    df <- results_df1[[commodity]]
    for (choque in names(choques)) {
      intervalo <- choques[[choque]]
      
      df_filtrado <- df %>%
        filter(Date >= intervalo[1], Date <= intervalo[2])
      
      media_covar <- mean(df_filtrado$DCOVaR, na.rm = TRUE)
      media_dcovar <- mean(df_filtrado$DDeltaCoVaR, na.rm = TRUE)
      
      tabela_resumo <- rbind(tabela_resumo, data.frame(
        Choque = choque,
        Commodity = commodity,
        CoVaR = media_covar,
        DeltaCoVaR = media_dcovar
      ))
    }
  }
  library(xtable)
  
  # Ajuste de nomes mais amigáveis
  tabela_resumo$Choque <- factor(tabela_resumo$Choque,
                                 levels = names(choques),
                                 labels = c("Pré-EUA-China", "Guerra EUA-China",
                                            "COVID-19", "Rússia-Ukraine","Trump 2")
  )
  
  # Gerar tabela LaTeX
  tabela_latex <- xtable(tabela_resumo, digits = c(0, 0, 0, 4, 4),
                         caption = "Resumo de CoVaR e ∆CoVaR por commodity e por choque",
                         label = "tab:covar_choques")
  
  print(tabela_latex, include.rownames = FALSE, booktabs = TRUE)
  
} # Calcular médias de CoVaR e ∆CoVaR por período
{
  set.seed(123)  # Reprodutibilidade
  n_boot <- 1000  # Número de reamostragens
  tabela_bootstrap <- data.frame()
  
  for (commodity in commodities) {
    df <- results_df1[[commodity]]
    for (choque in names(choques)) {
      intervalo <- choques[[choque]]
      df_filtrado <- df %>%
        filter(Date >= intervalo[1], Date <= intervalo[2])
      
      n <- nrow(df_filtrado)
      
      if (n < 30) next  # Ignorar janelas muito curtas
      
      boot_covars <- numeric(n_boot)
      boot_dcovars <- numeric(n_boot)
      
      for (i in 1:n_boot) {
        sample_indices <- sample(1:n, replace = TRUE)
        boot_sample <- df_filtrado[sample_indices, ]
        
        boot_covars[i] <- mean(boot_sample$DCOVaR, na.rm = TRUE)
        boot_dcovars[i] <- mean(boot_sample$DDeltaCoVaR, na.rm = TRUE)
      }
      
      tabela_bootstrap <- rbind(tabela_bootstrap, data.frame(
        Choque = choque,
        Commodity = commodity,
        CoVaR_Mean = mean(boot_covars),
        CoVaR_Low = quantile(boot_covars, 0.025),
        CoVaR_Up = quantile(boot_covars, 0.975),
        DCoVaR_Mean = mean(boot_dcovars),
        DCoVaR_Low = quantile(boot_dcovars, 0.025),
        DCoVaR_Up = quantile(boot_dcovars, 0.975)
      ))
    }
  }
  library(xtable)
  
  tabela_bootstrap$Choque <- factor(tabela_bootstrap$Choque,
                                    levels = names(choques),
                                    labels = c("Pré-EUA-China", "Guerra EUA-China",
                                               "COVID-19", "Rússia-Ukraine","Trump 2")
  )
  
  print(xtable(tabela_bootstrap, digits = 4,
               caption = "CoVaR e ∆CoVaR com intervalos de confiança via Bootstrap (95%)",
               label = "tab:covar_bootstrap"),
        include.rownames = FALSE, booktabs = TRUE)
  
} #  Intervalos de Confiança para CoVaR e ∆CoVaR via Bootstrap
####################################################
{
  library(quadprog)
  
  resultados_pesos <- list()
  resultados_hedge <- list()
  
  for (commodity in commodities) {
    
    retornos <- na.omit(base_agre[, c("BCOM", commodity)])
    resultados_testes[[commodity]] <- realizar_testes(retornos[, commodity])
    
    # Estimar DCC
    fit_dcc <- dccfit(spec_dcc, data = retornos)

    
    # Extrair Correlation e desvios padrão
    corr <- rcor(fit_dcc)            # Correlation dinâmica
    h <- rcov(fit_dcc)               # matriz de covariância dinâmica
    
    pesos_list <- matrix(NA, nrow = dim(h)[3], ncol = 2)  # para guardar os pesos
    hedge_ratio <- numeric(dim(h)[3])  
    
    for (t in 1:dim(h)[3]) {
      
      Sigma_t <- h[,,t]  # matriz de covariância no tempo t
      
      # Configurando o problema para quadprog
      Dmat <- 2 * Sigma_t
      dvec <- c(0, 0)     # minimizar risco, sem preferência de retorno
      Amat <- cbind(c(1,1), diag(2))  # restrição: soma dos pesos = 1 e pesos >= 0
      bvec <- c(1, 0, 0)
      
      # Resolver otimização
      sol <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
      pesos_list[t, ] <- sol$solution  # salvar os pesos otimizados
      # Hedge Ratio Kroner & Sultan
      hedge_ratio[t] <- Sigma_t[1,2] / Sigma_t[2,2]  # Cov(Commodity, BCOM) / Var(BCOM)
    }
    
    # Guardar os resultados
    resultados_pesos[[commodity]] <- data.frame(
      Data = base_agre$data[!is.na(rowSums(retornos))],
      Peso_BCOM = pesos_list[,1],
      Peso_Commodity = pesos_list[,2]
    )
    resultados_hedge[[commodity]] <- hedge_ratio
    
    
  }
  tabela_latex <- data.frame(
    Commodity = commodities,
    Peso_Medio_BCOM = sapply(resultados_pesos, function(x) mean(x$Peso_BCOM, na.rm = TRUE)),
    Peso_Medio_Commodity = sapply(resultados_pesos, function(x) mean(x$Peso_Commodity, na.rm = TRUE)),
    Hedge_Medio = sapply(resultados_hedge, function(x) mean(x, na.rm = TRUE))
  )
  
  # Visualizar
  kable(tabela_latex, format = "latex", digits = 3,
        col.names = c("Commodity", "w_BCOM", "w_COM", "Hedge Ratio"),
        caption = "Summary Statistics for Portfolio Weights and Hedge Ratios.")
  
}  # Hedge Ratio Kroner & Sultan
{
  library(ggplot2)
  
  for (commodity in commodities) {
    df_pesos <- resultados_pesos[[commodity]]
    
    p <- ggplot(df_pesos, aes(x = Data)) +
      geom_line(aes(y = Peso_BCOM, color = "BCOM")) +
      geom_line(aes(y = Peso_Commodity, color = commodity)) +
      labs(title = paste("Pesos do Portfólio de Risco Mínimo -", commodity),
           y = "Peso no Portfólio",
           x = "Data") +
      scale_color_manual(values = c("BCOM" = "blue", commodity = "darkgreen")) +
      theme_minimal()
    
    print(p)
  }
  
}  # Hedge Ratio Kroner & Sultan grafico
{
  library(ggplot2)
  dir.create("graficos_hedge", showWarnings = FALSE)
  
  for (commodity in commodities) {
    df_pesos <- resultados_pesos[[commodity]]
    df_pesos$HedgeRatio <- resultados_hedge[[commodity]]
    
    p1 <- ggplot(df_pesos, aes(x = Data)) +
      geom_line(aes(y = Peso_BCOM, color = "BCOM")) +
      geom_line(aes(y = Peso_Commodity, color = commodity)) +
      labs(title = paste("Pesos ótimos ao longo do tempo -", commodity),
           y = "Peso", x = "Data", color = "Ativo") +
      theme_minimal()
    p2 <- ggplot(df_pesos, aes(x = Data, y = HedgeRatio)) +
      geom_line(color = "darkgreen") +
      labs(title = paste("Hedge Ratio Dinâmico -", commodity),
           y = "Hedge Ratio", x = "Data") +
      theme_minimal()
    
   print(p1)
   print(p2)
  }
  tabela_latex <- data.frame(
    Commodity = commodities,
    Peso_Medio_BCOM = sapply(resultados_pesos, function(x) mean(x$Peso_BCOM, na.rm = TRUE)),
    SD_Peso_BCOM = sapply(resultados_pesos, function(x) sd(x$Peso_BCOM, na.rm = TRUE)),
    Peso_Medio_COM = sapply(resultados_pesos, function(x) mean(x$Peso_Commodity, na.rm = TRUE)),
    SD_Peso_COM = sapply(resultados_pesos, function(x) sd(x$Peso_Commodity, na.rm = TRUE)),
    Hedge_Medio = sapply(resultados_hedge, function(x) mean(x, na.rm = TRUE)),
    SD_Hedge = sapply(resultados_hedge, function(x) sd(x, na.rm = TRUE))
  )
  library(xtable)
  print(
    xtable(
      tabela_latex,
      digits = c(0, 0, 3, 3, 3, 3, 3, 3),
      caption = "Estatísticas de Pesos Ótimos e Hedge Ratios Dinâmicos por Commodity",
      label = "tab:hedge_stats"
    ),
    include.rownames = FALSE, booktabs = TRUE
  )
  

} # Estatísticas de Pesos Ótimos e Hedge Ratios Dinâmicos por Commodity
{
  choques <- list(
    "Pre_EUA_China" = as.Date(c("2016-01-01", "2017-12-31")),
    "EUA_China"     = as.Date(c("2018-01-01", "2020-01-31")),
    "COVID"         = as.Date(c("2020-02-01", "2021-12-31")),
    "Russia_Ucrania"= as.Date(c("2022-01-01", "2023-12-31")),
    "EUA_China_2"= as.Date(c("2024-11-01", "2025-06-18"))
  )
  library(dplyr)
  
  # Resultado: lista com um data.frame por commodity
  hedge_summary <- data.frame()
  
  for (commodity in commodities) {
    df_pesos <- resultados_pesos[[commodity]]
    df_pesos$HedgeRatio <- resultados_hedge[[commodity]]
    
    for (choque in names(choques)) {
      intervalo <- choques[[choque]]
      
      media_choque <- df_pesos %>%
        filter(Data >= intervalo[1], Data <= intervalo[2]) %>%
        summarise(Media_Hedge = mean(HedgeRatio, na.rm = TRUE),
                  Desvio_Hedge = sd(HedgeRatio, na.rm = TRUE)) %>%
        mutate(Commodity = commodity, Choque = choque)
      
      hedge_summary <- bind_rows(hedge_summary, media_choque)
    }
  }
  
  # Reordenar choques
  hedge_summary$Choque <- factor(hedge_summary$Choque,
                                 levels = names(choques),
                                 labels = c("Pré-EUA-China", "Guerra EUA-China", "COVID-19", "Rússia-Ukraine","Trump 2"))
  library(xtable)
  hedge_summary = hedge_summary %>% relocate( Choque, Commodity, .before = Media_Hedge)
  print(xtable(hedge_summary,
               digits = c(0, 0, 0, 3, 3),
               caption = "Média e Desvio Padrão do Hedge Ratio por Commodity e por Choque",
               label = "tab:hedge_por_choque"),
        include.rownames = FALSE, booktabs = TRUE)
  library(ggplot2)
  
  # Converter choques em faixas para gráfico
  choques_df <- do.call(rbind, lapply(names(choques), function(nome) {
    data.frame(
      inicio = choques[[nome]][1],
      fim = choques[[nome]][2],
      evento = nome
    )
  }))
  
  df_hedge_all <- data.frame()
  
  for (commodity in commodities) {
    df_temp <- resultados_pesos[[commodity]]
    df_temp$HedgeRatio <- resultados_hedge[[commodity]]
    df_temp$Par <- commodity
    df_hedge_all <- rbind(df_hedge_all, df_temp)
  }
  ggplot(df_hedge_all, aes(x = Data, y = HedgeRatio)) +
    geom_rect(aes(xmin = Trump_1[1], xmax = Trump_1[2], ymin = -Inf, ymax = Inf),
              fill = "gray", alpha = 0.3) +
    geom_vline(xintercept = as.numeric(data_marcacao), linetype = "dashed", color = "black") +
    annotate("text", x = data_marcacao, y = max(df_hedge_all$HedgeRatio)*0.7, label = "Trade War", color = "black", vjust = -1, hjust = 1.1, size = 2.5) +
    geom_vline(xintercept = as.numeric(covid_inicio), linetype = "dashed", color = "black") +
    annotate("text", x = covid_inicio, y = max(df_hedge_all$HedgeRatio)*0.7, label = "beginning of Covid", color = "black", vjust = -1, hjust = 1.1, size = 2.5) +
    geom_vline(xintercept = as.numeric(covid_fim), linetype = "dashed", color = "black") +
    annotate("text", x = covid_fim, y = max(df_hedge_all$HedgeRatio)*0.7, label = "End of Covid", color = "black", vjust = -1, hjust = 1.1, size = 2.5) +
    geom_vline(xintercept = as.numeric(russia_ucrania), linetype = "dashed", color = "black") +
    annotate("text", x = russia_ucrania, y = max(df_hedge_all$HedgeRatio)*0.7, label = "Rússia Ukraine", color = "black", vjust = -1, hjust = 1.1, size = 2.5) +
    geom_vline(xintercept = as.numeric(inicio_Trump2), linetype = "dashed", color = "black") +
    annotate("text", x = inicio_Trump2, y = max(df_hedge_all$HedgeRatio)*0.7, label = "Trump 2", color = "black", vjust = -1, hjust = 1.1,size = 2.5) +
    geom_line(color = "black") +
    facet_wrap(~ Par, scales = "free_y", ncol = 2) +
    scale_x_date(date_labels = "%m/%Y", date_breaks = "6 month") +
    scale_y_continuous(breaks = seq(-0.5, 1.30, by = 0.15))+
    labs(title = "",
         x = "", y = "Hedge Ratio") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
    ###Grafico Hedge Ratios Dinâmicos por Commodity
  
} # Média e Desvio Padrão do Hedge Ratio por Commodity e por Choque
####################################################

