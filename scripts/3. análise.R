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

sum(sexo$total)
sum(idade$total)
sum(raca.cor$total)
 
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

# Posição na ocupação ----
# Só ocupados: quem trabalhou nos últimos 30 dias (I04 == 1)

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

# Local do trabalho principal ----
# Só ocupados. O 0 ("na RA") e o 7 ("Paranoá") são o mesmo lugar para quem mora no Paranoá

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    local_trabalho = factor(
      case_when(
        I08 %in% c(0, 7)          ~ "No Paranoá",
        I08 == 1                  ~ "Plano Piloto",
        I08 %in% c(2:6, 8:35)     ~ "Outras RAs do DF",
        I08 %in% 41:55            ~ "Goiás",
        I08 == 56                 ~ "No domicílio",
        I08 %in% c(40, 57, 58)    ~ "Outros ou vários locais",
        I08 == 88888              ~ "Não sabe",
        I08 == 99999              ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(local_trabalho) |> 
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

# Meio de transporte mais utilizado ----
# Só ocupados

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    transporte = factor(
      case_when(
        I09_9 == 1     ~ "Ônibus",
        I09_9 == 2     ~ "Automóvel",
        I09_9 == 3     ~ "Transporte privado",
        I09_9 == 4     ~ "Metrô",
        I09_9 == 5     ~ "Motocicleta",
        I09_9 == 6     ~ "Bicicleta",
        I09_9 == 7     ~ "A pé",
        I09_9 == 8     ~ "Carroça ou cavalo",
        I09_9 == 88888 ~ "Não sabe",
        I09_9 == 99999 ~ "Não se aplica"
      )
    )
  ) |> 
  group_by(transporte) |> 
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

# Tempo de deslocamento até o trabalho ----
# Só ocupados. Os levels mantêm a ordem do tempo (sem eles, o R ordena por ordem alfabética)

amostra_paranoa_mor |> 
  filter(I04 == 1) |> 
  mutate(
    tempo_desloc = factor(
      case_when(
        I10 == 1     ~ "Até 15 min",
        I10 == 2     ~ "15 a 30 min",
        I10 == 3     ~ "30 a 45 min",
        I10 == 4     ~ "45 min a 1h",
        I10 == 5     ~ "1h a 1h15",
        I10 == 6     ~ "1h15 a 1h30",
        I10 == 7     ~ "1h30 a 1h45",
        I10 == 8     ~ "1h45 a 2h",
        I10 == 9     ~ "Mais de 2h",
        I10 == 88888 ~ "Não sabe",
        I10 == 99999 ~ "Não se aplica"
      ),
      levels = c(
        "Até 15 min", "15 a 30 min", "30 a 45 min", "45 min a 1h",
        "1h a 1h15", "1h15 a 1h30", "1h30 a 1h45", "1h45 a 2h",
        "Mais de 2h", "Não sabe", "Não se aplica"
      )
    )
  ) |> 
  group_by(tempo_desloc) |> 
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



