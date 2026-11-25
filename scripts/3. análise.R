## Paranoá ----

# Filtrando só moradores paranoá (5307)

amostra_paranoa_mor <- amostra_mor |> 
  filter(A01uf == 53,
         localidade == 5307) 

## Perfil demográfico Paranoá ----

# Sexo ----

sexo <- amostra_paranoa_mor |> 
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

idade <- amostra_paranoa_mor |> 
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

raca.cor <- amostra_paranoa_mor |> 
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
 
## Composição domiciliar ----

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

# Moradores de 0 a 12 anos
  














