## Paranoá ----

# Filtrando só moradores paranoá (5307)

amostra_paranoa_mor <- amostra_mor |> 
  filter(A01uf == 53,
         localidade == 5307) 

## PERFIL DEMOGRÁFICO ----------------------------------------------------------

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

## COMPOSIÇÃO DOMICILIAR -------------------------------------------------------

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

## ESCOLARIDADE ----------------------------------------------------------------

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

## MERCADO DE TRABALHO ---------------------------------------------------------

# Trabalhou nos últimos 30 dias? ----

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

## RENDIMENTO ------------------------------------------------------------------

# Renda individual ----
# Resultado estranho
#amostra_mor |> 
#  filter(A01uf == 53,
#         localidade == 5307) |> 
#  mutate(
#    localidade = factor(
#      case_when(
#        localidade == 5307 ~ "paranoá"
#      )
#    )
#  ) |> 
#  group_by(localidade) |> 
#  summarise(
#    media   = survey_mean(renda_ind_r, vartype = "cv", na.rm = TRUE),
#    mediana = survey_median(renda_ind_r, vartype = "cv", na.rm = TRUE)
#  ) |> 
#  mutate(
#    media      = round(media, digits = 2),
#    media_cv   = paste0(round(media_cv * 100, digits = 2), "%"),
#    mediana    = round(mediana, digits = 2),
#    mediana_cv = paste0(round(mediana_cv * 100, digits = 2), "%")
#  )


# Verificando a distribuição de renda para entender a diferença gritante da média e mediana

#dis.renda <- amostra_paranoa_mor |> 
#  mutate(
#    faixa_renda = cut(
#      renda_ind_r,
#      breaks = c(0, 500, 1000, 2000, 3000, 5000, Inf),
#      labels = c(
#        "Até 500",
#        "500 a 1.000",
#        "1.000 a 2.000",
#        "2.000 a 3.000",
#        "3.000 a 5.000",
#        "Acima de 5.000"
#      ),
#     right = FALSE
#    )
#  ) |> 
#  group_by(faixa_renda) |> 
#  summarise(
#    total = survey_total(vartype = c("cv", "ci"), na.rm = TRUE)
#  )

# Visulaizando a distribuição de renda
#fig <- dis.renda |> 
#  ggplot(aes(x = faixa_renda, y = total)) +
#  geom_col(fill = "#FF4F00") +
#  geom_errorbar(aes(ymin = total_low, ymax = total_upp), width = 0.2) +
#  labs(
#   title = "Distribuição da renda no Paranoá",
#    x = "Faixa de renda (R$)",
#    y = "População estimada",
#    caption = "Fonte: PDAD-A 2024"
#  ) +
#  theme_minimal()

# Renda domiciliar per capita (variável derivada) ----

# renda per capta
#amostra_paranoa_dom |> 
#  mutate(
#    localidade = factor(
#      case_when(
#        localidade == 5307 ~ "paranoá"
#      )
#    )) |> 
#  group_by(localidade) |> 
#  summarise(
#    media = survey_mean(renda_domiciliar_pc_r, vartype = "cv"),
#    mediana = survey_median(renda_domiciliar_pc_r, vartype = "cv")
#  )

# Distribuição
#amostra_paranoa_dom |>
#  mutate(
#    renda_familiar = cut(
#      renda_domiciliar_pc_r,
#      breaks = c(0, 500, 1000, 3000, 7000, 8000, 9000, Inf),
#      labels = c(
#        "Até 500",
#        "500 a 1.000",
#        "1.000 a 3.000",
#        "3.000 a 7000",
#        "7.000 a 8.000",
#        "8.000 a 9.000",
#        "Acima de 9.000"
#      ),
#      right = FALSE
#    )
#  ) |>
#  group_by(renda_familiar) |> 
#  summarise(
#    total = survey_total(vartype = c("cv", "ci"), na.rm = TRUE)
#  )

# Renda domiciliar (Soma dos rendimentos dos moradores) ----
#amostra_paranoa_dom |> 
#  mutate(
#    localidade = factor(
#      case_when(
#        localidade == 5307 ~ "paranoá"
#     )
#    )) |> 
#  group_by(localidade) |> 
#  summarise(
#    media = survey_mean(renda_domiciliar_r, vartype = "cv"),
#    mediana = survey_median(renda_domiciliar_r, vartype = "cv")
#  )

#amostra_paranoa_dom |>
#  mutate(
#    renda_dom = cut(
#      renda_domiciliar_r,
#      breaks = c(0, 500, 1000, 2000, 3000, 7000, Inf),
#      labels = c(
#        "Até 500",
#        "500 a 1.000",
#        "1.000 a 2.000",
#        "2.000 a 3.000",
#        "3.000 a 7.000",
#        "Acima de 7.000"
#      ),
#      right = FALSE
#    )
#  ) |> 
#  group_by(renda_dom) |> 
#  summarise(
#    total = survey_total(vartype = c("cv", "ci"), na.rm = TRUE)
#  )

## CONDIÇÕES HABITACIONAIS -----------------------------------------------------

# Tipo do domicílio ----
amostra_paranoa_dom |> 
  mutate(
    tipo_domicilio = factor(
      case_when(
        B02 == 1 ~ "Casa",
        B02 == 2 ~ "Apartamento",
        B02 == 3 ~ "Cômodo",
      )
    )) |> 
  group_by(tipo_domicilio) |> 
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

# Situação do domicílio ----
amostra_paranoa_dom |> 
  mutate(
    situacao_domicilio = factor(
      case_when(
        B03 == 1 ~ "Próprio, já pago (quitado)",
        B03 == 2 ~ "Próprio, ainda pagando (em aquisição)",
        B03 == 3 ~ "Alugado",
        B03 == 4 ~ "Cedido pelo empregador",
        B03 == 5 ~ "Cedido por outro"
      ),
      levels = c(
        "Próprio, já pago (quitado)",
        "Próprio, ainda pagando (em aquisição)",
        "Alugado",
        "Cedido pelo empregador",
        "Cedido por outro"
    ))
  ) |> 
  group_by(situacao_domicilio) |> 
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

# Possui escritura definitiva registrada em cartório ----
amostra_paranoa_dom |> 
  mutate(
    escritura_imovel = factor(
      case_when(
        B06 == 1     ~ "Sim",
        B06 == 2     ~ "Não",
        B06 == 88888 ~ "Não sabe",
        B06 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    )) |> 
  group_by(escritura_imovel) |> 
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

# Quantidade de cômodos ----
amostra_paranoa_dom |> 
  group_by(B10) |> 
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

# Quantidade de cômodos servindo permanentemente de dormitórios ----
amostra_paranoa_dom |> 
  group_by(B11) |> 
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

# Quantidade de banheiros e/ou sanitários
amostra_paranoa_dom |> 
  group_by(B12) |> 
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

## INTRAESTRUTURA URBANA -------------------------------------------------------

# A rua de acesso principal asfaltada/pavimentada ------
amostra_paranoa_dom |> 
  mutate(
    asfalto_pavimento = factor(
      case_when(
        B17_1 == 1 ~ "Sim",
        B17_1 == 2 ~ "Não",
        B17_1 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(asfalto_pavimento) |> 
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
  
# A rua tem calçada ----

amostra_paranoa_dom |> 
  mutate(
    calcada = factor(
      case_when(
        B17_2 == 1 ~ "Sim",
        B17_2 == 2 ~ "Não",
        B17_2 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(calcada) |> 
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

# Na rua tem iluminação ----
amostra_paranoa_dom |> 
  mutate(
    iluminacao = factor(
      case_when(
        B17_3 == 1 ~ "Sim",
        B17_3 == 2 ~ "Não",
        B17_3 == 88888 ~ "Não sabe"
        ),
        levels = c("Sim", "Não", "Não sabe")
      )) |> 
  group_by(iluminacao) |> 
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

# Na rua existe bueiro ou boca de lobo? ----
amostra_paranoa_dom |> 
  mutate(
    bueiro = factor(
      case_when(
        B17_4 == 1 ~ "Sim",
        B17_4 == 2 ~ "Não",
        B17_4 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(bueiro) |> 
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

# Existe descarte inadequado de lixo (entulho, jogado em local impróprio) ----
amostra_paranoa_dom |> 
  mutate(
    des_lixo = factor(
      case_when(
        B18_1 == 1 ~ "Sim",
        B18_1 == 2 ~ "Não",
        B18_1 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(des_lixo) |> 
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

# Existe esgoto a céu aberto ----
amostra_paranoa_dom |> 
  mutate(
    esgoto_aberto = factor(
      case_when(
        B18_2 == 1 ~ "Sim",
        B18_2 == 2 ~ "Não",
        B18_2 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |>
  group_by(esgoto_aberto) |> 
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

# existem ruas que alagam ----
amostra_paranoa_dom |> 
  mutate(
    rua_alaga = factor(
      case_when(
        B18_3 == 1 ~ "Sim",
        B18_3 == 2 ~ "Não",
        B18_3 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(rua_alaga) |> 
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

## SANEAMENTO BáSICO -----------------------------------------------------------

# Tem acesso à Rede Geral da CAESB/ Saneago de abastecimento de água? ----
amostra_paranoa_dom |> 
  mutate(
    abasteciamento_agua = factor(
      case_when(
        B13 == 1 ~ "Sim",
        B13 == 2 ~ "Não",
        B13 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(abasteciamento_agua) |> 
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
  
# Tem acesso à Rede Geral da CAESB/Saneago de coleta de esgoto? ----

amostra_paranoa_dom |> 
  mutate(
    coleta_esgoto = factor(
      case_when(
        B14 == 1 ~ "Sim",
        B14 == 2 ~ "Não",
        B14 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(coleta_esgoto) |> 
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

# Tem acesso à Rede Geral da Neoenergia/Enel de energia elétrica? ----

amostra_paranoa_dom |> 
  mutate(
    acesso_energia = factor(
      case_when(
        B15 == 1 ~ "Sim",
        B15 == 2 ~ "Não",
        B15 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(acesso_energia) |> 
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

# O caminhão do serviço público de limpeza urbana (SLU) recolhe o lixo no seu domicílio/no seu condomínio? ----

amostra_paranoa_dom |> 
  mutate(
    recolhe_lixo = factor(
      case_when(
        B16 == 1 ~ "Sim",
        B16 == 2 ~ "Não",
        B16 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(recolhe_lixo) |> 
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

## MOBILIDADE URBANA -----------------------------------------------------------

# Quantidade de automóveis ----

amostra_paranoa_dom |>
  filter(C01 == 1) |> 
  group_by(C01_1) |> 
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

# Quantidade de motocicletas ----
amostra_paranoa_dom |> 
  filter(C02 == 1) |> 
  group_by(C02_1) |> 
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

# Transporte utilizado para estudar ----
amostra_paranoa_mor |> 
  mutate(
    transporte_escola = factor(
      case_when(
        H05_11 == 1 ~ "Ônibus",
        H05_11 == 2 ~ "Escolar Público",
        H05_11 == 3 ~ "Escolar Privado",
        H05_11 == 4 ~ "Privado (empresa de aplicativo, táxi etc)",
        H05_11 == 5 ~ "Automóvel",
        H05_11 == 6 ~ "Metrô",
        H05_11 == 7 ~ "Motocicleta",
        H05_11 == 8 ~ "Bicicleta",
        H05_11 == 9 ~ "A pé",
        H05_11 == 10 ~ "Carroça ou cavalo",
        H05_11 == 88888 ~ "Não sabe",
        H05_11 == 99999 ~ "Não se aplica",
      ),
      levels = c(
        "Ônibus", "Escolar Público", "Escolar Privado",
        "Privado (empresa de aplicativo, táxi etc)", "Automóvel", "Metrô",
        "Motocicleta", "Bicicleta", "A pé", "Carroça ou cavalo", "Não sabe",
        "Não se aplica"
      )
    )) |> 
  group_by(transporte_escola) |> 
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

# Transporte utilizado para trabalhar ----

amostra_paranoa_mor |> 
  mutate(
    transporte_trabalhar = factor(
      case_when(
        I09_9 == 1 ~ "Ônibus",
        I09_9 == 2 ~ "Automóvel",
        I09_9 == 3 ~ "Transporte Privado (empresa de aplicativo, táxi, fornecido pela empresa)",
        I09_9 == 4 ~ "Metrô",
        I09_9 == 5 ~ "Motocicleta",
        I09_9 == 6 ~ "Bicicleta",
        I09_9 == 7 ~ "A pé",
        I09_9 == 8 ~ "Carroça ou cavalo",
        I09_9 == 88888 ~ "Não sabe",
        I09_9 == 99999 ~ "Não se aplica",
      ),
      levels = c(
        "Ônibus", "Automóvel",
        "Transporte Privado (empresa de aplicativo, táxi, fornecido pela empresa)",
        "Metrô", "Motocicleta", "Bicicleta", "A pé", "Carroça ou cavalo", 
        "Não sabe", "Não se aplica"
      )
    )) |> 
  group_by(transporte_trabalhar) |> 
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

# Tempo de deslocamento para o trabalho ----

amostra_paranoa_mor |> 
  mutate(
    tempo_serviço = factor(
      case_when(
        I10 == 1 ~ "Até 15 minutos",
        I10 == 2 ~ "Acima de 15 minutos até 30 minutos",
        I10 == 3 ~ "Acima de 30 minutos até 45 minutos",
        I10 == 4 ~ "Acima de 45 minutos até 1 hora",
        I10 == 5 ~ "Acima de 1 hora até 1 hora e 15 minutos",
        I10 == 6 ~ "Acima de 1 hora e 15 minutos até 1 hora e 30 minutos",
        I10 == 7 ~ "Acima de 1 hora e 30 minutos até 1 hora e 45 minutos",
        I10 == 8 ~ "Acima de 1 hora e 45 minutos até 2 horas",
        I10 == 9 ~ "Acima de 2 horas",
        I10 == 88888 ~ "Não sabe",
        I10 == 99999 ~ "Não se aplica"
      ),
      levels = c(
        "Até 15 minutos",
        "Acima de 15 minutos até 30 minutos",
        "Acima de 30 minutos até 45 minutos",
        "Acima de 45 minutos até 1 hora",
        "Acima de 1 hora até 1 hora e 15 minutos",
        "Acima de 1 hora e 15 minutos até 1 hora e 30 minutos",
        "Acima de 1 hora e 30 minutos até 1 hora e 45 minutos",
        "Acima de 1 hora e 45 minutos até 2 horas",
        "Acima de 2 horas",
        "Não sabe",
        "Não se aplica"
      )
    )) |> 
  group_by(tempo_serviço) |> 
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

## ACESSO A SAÚDE --------------------------------------------------------------

# Atendimento em saúde ----

amostra_paranoa_mor |> 
  mutate(
    saude_aten = factor(
      case_when(
        G01 == 1 ~ "Sim",
        G01 == 2 ~ "Não",
        G01 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(saude_aten) |> 
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

# Localidade procurada para atendimento ----

amostra_paranoa_mor |> 
  mutate(
    local_procurado = factor(
      case_when(
        G04 == 0     ~ "Na Região Administrativa",
        G04 == 40    ~ "No município",
        G04 == 1     ~ "Plano Piloto",
        G04 == 2     ~ "Gama",
        G04 == 3     ~ "Taguatinga",
        G04 == 4     ~ "Brazlândia",
        G04 == 5     ~ "Sobradinho",
        G04 == 6     ~ "Planaltina",
        G04 == 7     ~ "Paranoá",
        G04 == 8     ~ "Núcleo Bandeirante",
        G04 == 9     ~ "Ceilândia",
        G04 == 10    ~ "Guará",
        G04 == 11    ~ "Cruzeiro",
        G04 == 12    ~ "Samambaia",
        G04 == 13    ~ "Santa Maria",
        G04 == 14    ~ "São Sebastião",
        G04 == 15    ~ "Recanto das Emas",
        G04 == 16    ~ "Lago Sul",
        G04 == 17    ~ "Riacho Fundo",
        G04 == 18    ~ "Lago Norte",
        G04 == 19    ~ "Candangolândia",
        G04 == 20    ~ "Águas Claras",
        G04 == 21    ~ "Riacho Fundo II",
        G04 == 22    ~ "Sudoeste e Octogonal",
        G04 == 23    ~ "Varjão",
        G04 == 24    ~ "Park Way",
        G04 == 25    ~ "SCIA",
        G04 == 26    ~ "Sobradinho II",
        G04 == 27    ~ "Jardim Botânico",
        G04 == 28    ~ "Itapoã",
        G04 == 29    ~ "SIA",
        G04 == 30    ~ "Vicente Pires",
        G04 == 31    ~ "Fercal",
        G04 == 32    ~ "Sol Nascente / Pôr do Sol",
        G04 == 33    ~ "Arniqueira",
        G04 == 34    ~ "Arapoanga",
        G04 == 35    ~ "Água Quente",
        G04 == 41    ~ "Águas Lindas de Goiás",
        G04 == 42    ~ "Alexânia",
        G04 == 43    ~ "Cidade Ocidental",
        G04 == 44    ~ "Cristalina",
        G04 == 45    ~ "Cocalzinho de Goiás",
        G04 == 46    ~ "Formosa",
        G04 == 47    ~ "Luziânia",
        G04 == 48    ~ "Novo Gama",
        G04 == 49    ~ "Padre Bernardo",
        G04 == 50    ~ "Planaltina de Goiás",
        G04 == 51    ~ "Santo Antônio do Descoberto",
        G04 == 52    ~ "Valparaíso de Goiás",
        G04 == 53    ~ "Outros municípios de Goiás",
        G04 == 54    ~ "No domicílio",
        G04 == 55    ~ "Pela internet (Telemedicina)",
        G04 == 56    ~ "Outros locais",
        G04 == 88888 ~ "Não sabe",
        G04 == 99999 ~ "Não se aplica"
      )
    )) |> 
  group_by(local_procurado) |> 
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

# Possui plano de saúde ----

amostra_paranoa_mor |> 
  mutate(
    plano_saude = factor(
      case_when(
        G05 == 1 ~ "Sim",
        G05 == 2 ~ "Não",
        G05 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(plano_saude) |> 
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

## ACESSO A TECNOLOGIA ---------------------------------------------------------

# Acesso domiciliar à internet ----

amostra_paranoa_dom |> 
  mutate(
    dom_acesso_internet = factor(
      case_when(
        C05 == 1 ~ "Sim, próprio",
        C05 == 2 ~ "Sim, compartilhado com outro(s) domicílio(s)",
        C05 == 3 ~ "Não",
        C05 == 8888 ~ "Não sabe"
      ),
      levels = c("Sim, próprio", "Sim, compartilhado com outro(s) domicílio(s)",
                 "Não", "Não sabe")
    )) |> 
  group_by(dom_acesso_internet) |> 
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

# Acesso individual à internet ----

amostra_paranoa_mor |> 
  mutate(
    ind_acesso_internet = factor(
      case_when(
        F04 == 1 ~ "Sim",
        F04 == 2 ~ "Não",
        F04 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    )) |> 
  group_by(ind_acesso_internet) |> 
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

# Acessou internet pelo computador ----

amostra_paranoa_mor |> 
  mutate(
    com_acesso_internet = factor(
      case_when(
        F05_1 == 1 ~ "Sim",
        F05_1 == 2 ~ "Não",
        F05_1 == 88888 ~ "Não sabe",
        F05_1 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    )) |> 
  group_by(com_acesso_internet) |> 
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

# Acessou internet pelo Celula/tablet ----

amostra_paranoa_mor |> 
  mutate(
    cel_tab_acesso_internet = factor(
      case_when(
        F05_2 == 1 ~ "Sim",
        F05_2 == 2 ~ "Não",
        F05_2 == 88888 ~ "Não sabe",
        F05_2 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    )) |> 
  group_by(cel_tab_acesso_internet) |> 
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