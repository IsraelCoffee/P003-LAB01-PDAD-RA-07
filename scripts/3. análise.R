## Paranoá ----

# Filtrando só moradores paranoá (5307)

amostra_paranoa_mor <- amostra_mor |> 
  filter(A01uf == 53,
         localidade == 5307) 

## Perfil demográfico Paranoá --------------------------------------------------

# Sexo ----

amostra_paranoa_mor |> 
  mutate(
    E03 = factor(
      case_when(
        E03 == 1 ~ "Masculino",
        E03 == 2 ~ "Feminino"
      )
    )
  ) |> 
  group_by(E03) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Idade ----

amostra_paranoa_mor |> 
  mutate(
    faixa_etaria = cut(
      idade_calculada,
      breaks = c(seq(0, 80, by = 5), Inf),
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24",
        "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65-69", "70-74",
        "75-79", "80+"
      ),
      right = FALSE
    )
  ) |> 
  group_by(faixa_etaria) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Cor / raça ----

amostra_paranoa_mor |> 
  mutate(
    E05 = factor(
      case_when(
        E05 %in% c(2, 4) ~ "Negra",
        E05 %in% c(1, 3, 5) ~ "Não negra"
      )
    )
  ) |> 
  group_by(E05) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# conferindo totais, todo tem que dar: 55551

#sum(sexo$total)
#sum(idade$total)
#sum(raca.cor$total)

## Composição domiciliar -------------------------------------------------------

# Filtrando só domicilios paranoá (5307)

amostra_paranoa_dom <- amostra_dom |> 
  filter(A01uf == 53,
         localidade == 5307)

# Quantidade de pessoas no domicílio ----

amostra_paranoa_dom |> 
  group_by(A01npessoas) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Composição domiciliar ----

amostra_paranoa_dom |> 
  mutate(
    arranjos = factor(
      case_when(
        arranjos == 1 ~ "Unipessoal",
        arranjos == 2 ~ "Monoparental feminino",
        arranjos == 3 ~ "Casal com 1 filho",
        arranjos == 4 ~ "Casal com 2 filho",
        arranjos == 5 ~ "Casal com 3 filhos ou mais",
        arranjos == 6 ~ "Casal sem filhos",
        arranjos == 7 ~ "Outro perfil"
      )
    )
  ) |> 
  group_by(arranjos) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Moradores de 0 a 12 anos ----

amostra_paranoa_dom |>
  group_by(A01ncriancas) |>
  summarise(total = survey_total(vartype = "cv"),
            pct   = survey_prop(vartype = "cv")) |>
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

## Escolaridade ----------------------------------------------------------------

# Nível de instrução ----

# Nível de instrução ----

amostra_paranoa_mor |> 
  mutate(
    nivel_instrucao = factor(
      case_when(
        escolaridade == 1 ~ "Sem instrução",
        escolaridade == 2 ~ "Fundamental incompleto ou equivalente",
        escolaridade == 3 ~ "Fundamental completo ou equivalente",
        escolaridade == 4 ~ "Médio incompleto ou equivalente",
        escolaridade == 5 ~ "Médio completo ou equivalente",
        escolaridade == 6 ~ "Superior incompleto ou equivalente",
        escolaridade == 7 ~ "Superior completo",
        escolaridade == 8 ~ "Sem classificação"
      ),
      # mantém a ordem do nível de instrução (sem isso, o R ordena por ordem alfabética)
      levels = c(
        "Sem instrução",
        "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente",
        "Médio incompleto ou equivalente",
        "Médio completo ou equivalente",
        "Superior incompleto ou equivalente",
        "Superior completo",
        "Sem classificação"
      )
    )
  ) |> 
  group_by(nivel_instrucao) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Frequenta instituição de ensino ----

amostra_paranoa_mor |> 
  mutate(
    freq_ensino = factor(
      case_when(
        H02 == 1     ~ "Sim",
        H02 == 2     ~ "Não",
        H02 == 88888 ~ "Não sabe"
      )
    )
  ) |> 
  group_by(freq_ensino) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Tipo de escola que frequenta ----

amostra_paranoa_mor |> 
  mutate(
    tipo_escola = factor(
      case_when(
        H04_1 == 1 ~ "Pública",
        H04_1 == 2 ~ "Particular",
        H04_1 == 88888 ~ "Não sabe",
        H04_1 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(tipo_escola) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

## Mercado de trabalho ---------------------------------------------------------

# Trabalhou nos últimos 30 dias?

amostra_paranoa_mor |> 
  mutate(
    trabalhou_30d = factor(
      case_when(
        I04 == 1     ~ "Sim",
        I04 == 2     ~ "Não",
        I04 == 88888 ~ "Não sabe",
        I04 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(trabalhou_30d) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Regime de trabalho ----
# Só ocupados: quem trabalhou nos últimos 30 dias (I04 == 1)

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    regime_trabalho = factor(
      case_when(
        I07 == 1     ~ "Presencial",
        I07 == 2     ~ "Teletrabalho/remoto",
        I07 == 3     ~ "Híbrido",
        I07 == 88888 ~ "Não sabe",
        I07 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(regime_trabalho) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Posição na ocupação ----
# Só ocupados

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    posicao_ocupacao = factor(
      case_when(
        I13 == 1     ~ "Empregado no setor público",
        I13 == 2     ~ "Militar",
        I13 == 3     ~ "Empregado no setor privado",
        I13 == 4     ~ "Empregado doméstico",
        I13 == 5     ~ "Estágio remunerado",
        I13 == 6     ~ "Aprendiz",
        I13 == 7     ~ "Conta própria ou autônomo",
        I13 == 8     ~ "Empregador",
        I13 == 9     ~ "Serviço militar obrigatório",
        I13 == 10    ~ "Trabalhador não remunerado",
        I13 == 88888 ~ "Não sabe",
        I13 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(posicao_ocupacao) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

# Possui CTPS assinada ----
# Só ocupados

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    ctps = factor(
      case_when(
        I18 == 1     ~ "Sim",
        I18 == 2     ~ "Não",
        I18 == 88888 ~ "Não sabe",
        I18 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(ctps) |> 
  summarise(
    total = survey_total(vartype = "cv"),
    pct = survey_prop(vartype = "cv")
  ) |> 
  mutate(
    total    = round(total),
    total_cv = paste0(round(total_cv * 100, digits = 2), "%"),
    pct      = paste0(round(pct * 100, digits = 2), "%"),
    pct_cv   = paste0(round(pct_cv * 100, digits = 2), "%")
  )

## Rendimento ------------------------------------------------------------------

# Resultado estranho
amostra_mor |> 
  filter(A01uf == 53,
         localidade == 5307) |> 
  mutate(
    localidade = factor(
      case_when(
        localidade == 5307 ~ "paranoá"
      )
    )
  ) |> 
  group_by(localidade) |> 
  summarise(
    media   = survey_mean(renda_ind_r, vartype = "cv", na.rm = TRUE),
    mediana = survey_median(renda_ind_r, vartype = "cv", na.rm = TRUE)
  ) |> 
  mutate(
    media      = round(media, digits = 2),
    media_cv   = paste0(round(media_cv * 100, digits = 2), "%"),
    mediana    = round(mediana, digits = 2),
    mediana_cv = paste0(round(mediana_cv * 100, digits = 2), "%")
  )


# Verificando a distribuição de renda para entender a diferença gritante da média e mediana

dis.renda <- amostra_paranoa_mor |> 
  mutate(
    faixa_renda = cut(
      renda_ind_r,
      breaks = c(0, 500, 1000, 2000, 3000, 5000, Inf),
      labels = c(
        "Até 500",
        "500 a 1.000",
        "1.000 a 2.000",
        "2.000 a 3.000",
        "3.000 a 5.000",
        "Acima de 5.000"
      ),
      right = FALSE
    )
  ) |> 
  group_by(faixa_renda) |> 
  summarise(
    total = survey_total(vartype = c("cv", "ci"), na.rm = TRUE)
  )

# Visulaizando a distribuição de renda
dis.renda |> 
  ggplot(aes(x = faixa_renda, y = total)) +
  geom_col(fill = "#FF4F00") +
  geom_errorbar(aes(ymin = total_low, ymax = total_upp), width = 0.2) +
  labs(
    title = "Distribuição da renda no Paranoá",
    x = "Faixa de renda (R$)",
    y = "População estimada",
    caption = "Fonte: PDAD-A 2024"
  ) +
  theme_minimal()
