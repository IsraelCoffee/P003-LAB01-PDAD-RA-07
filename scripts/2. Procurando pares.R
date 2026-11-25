# Objetivo: Encontrar RAs que são pares (comparáveis) à RA Paranoá.
# Para isso, precisa satisfazer 2 tópicos:
# i) Precisa estar no mesmo grupo de renda;
# ii) Ter uma quantidade de população próxima ou igual.


# Grupo de renda ----

# Este bloco serve de conferência: lista as RAs de baixa renda a partir
# dos dados. O resultado é comparado com o cod_baixa mais abaixo.
RAs <- amostra_mor |> 
  filter(A01uf == 53) |> 
  mutate(
    localidade = factor(
      case_when(
        localidade == 5301 ~ "Plano Piloto",
        localidade == 5302 ~ "Gama",
        localidade == 5303 ~ "Taguatinga",
        localidade == 5304 ~ "Brazlândia",
        localidade == 5305 ~ "Sobradinho",
        localidade == 5306 ~ "Planaltina",
        localidade == 5307 ~ "Paranoá",
        localidade == 5308 ~ "Núcleo Bandeirante",
        localidade == 5309 ~ "Ceilândia",
        localidade == 5310 ~ "Guará",
        localidade == 5311 ~ "Cruzeiro",
        localidade == 5312 ~ "Samambaia",
        localidade == 5313 ~ "Santa Maria",
        localidade == 5314 ~ "São Sebastião",
        localidade == 5315 ~ "Recanto das Emas",
        localidade == 5316 ~ "Lago Sul",
        localidade == 5317 ~ "Riacho Fundo",
        localidade == 5318 ~ "Lago Norte",
        localidade == 5319 ~ "Candangolândia",
        localidade == 5320 ~ "Águas Claras",
        localidade == 5321 ~ "Riacho Fundo II",
        localidade == 5322 ~ "Sudoeste e Octogonal",
        localidade == 5323 ~ "Varjão",
        localidade == 5324 ~ "Park Way",
        localidade == 5325 ~ "SCIA",
        localidade == 5326 ~ "Sobradinho II",
        localidade == 5327 ~ "Jardim Botânico",
        localidade == 5328 ~ "Itapoã",
        localidade == 5329 ~ "SIA",
        localidade == 5330 ~ "Vicente Pires",
        localidade == 5331 ~ "Fercal",
        localidade == 5332 ~ "Sol Nascente / Pôr do Sol",
        localidade == 5333 ~ "Arniqueira",
        localidade == 5334 ~ "Arapoanga",
        localidade == 5335 ~ "Água Quente",
        TRUE ~ NA_character_
      )
    )
  ) |> 
  group_by(grupo_ped, localidade) |> 
  mutate(
    grupo_ped = factor(
      case_when(
        grupo_ped == 1 ~ "Alta renda",  
        grupo_ped == 2 ~ "Média-alta renda",  
        grupo_ped == 3 ~ "Média-baixa renda",  
        grupo_ped == 4 ~ "Baixa renda"  
      )
    )
  ) |> 
  filter(grupo_ped == "Baixa renda") |> 
  summarise() |> 
  pull(localidade)


# População ----

# Códigos de localidade das 13 RAs de baixa renda
cod_baixa <- c("Água Quente" = 5335, "Arapoanga" = 5334, "Brazlândia" = 5304,
               "Fercal" = 5331, "Itapoã" = 5328, "Paranoá" = 5307,
               "Planaltina" = 5306, "Recanto das Emas" = 5315,
               "Riacho Fundo II" = 5321, "São Sebastião" = 5314,
               "SCIA" = 5325, "Sol Nascente / Pôr do Sol" = 5332,
               "Varjão" = 5323)

# Conferência: o cod_baixa precisa ter as mesmas RAs do bloco acima.
# Se os dados mudarem e as listas divergirem, o script para aqui com um erro.
stopifnot(setequal(as.character(RAs), names(cod_baixa)))

# População estimada por RA
pop_ras <- amostra_mor |>
  filter(A01uf == 53, localidade %in% cod_baixa) |>
  group_by(localidade) |>
  summarise(
    populacao = survey_total(vartype = "ci"),   # soma de peso_mor, com IC
    n_amostra = unweighted(n())                 # quantos moradores entrevistados
  ) |>
  mutate(RA = names(cod_baixa)[match(localidade, cod_baixa)]) |>
  mutate(
    populacao = round(populacao),
    populacao_low = round(populacao_low),
    populacao_upp = round(populacao_upp)
  ) |> 
  arrange(desc(populacao))

# Puxa a população do Paranoá
pop_paranoa <- pop_ras$populacao[pop_ras$localidade == 5307]

# Calcula a razão em relação à população do Paranoá
pop_ras <- pop_ras |>
  mutate(razao = populacao / pop_paranoa) |>   # 1 = mesma população do Paranoá
  arrange(abs(razao - 1))

# Critério: população entre 75% e 130% da população do Paranoá
faixa_pop <- c(0.75, 1.30)

# Filtra as RAs dentro da faixa (inclui o próprio Paranoá)
ras_filtr <- pop_ras |> 
  filter(between(razao, faixa_pop[1], faixa_pop[2])) |> 
  select(RA, localidade, populacao)
