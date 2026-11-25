# Obetivo: Encotrar RAs que são pares(comparaveis) a RA Paranoá.
# Para isso, precisa satifazer 2 topicos:
# i) Presisa está no mesmo grupo de renda;
# ii) Ter uma quantidade de população proxima ou igual.


# Grupo de renda ----

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

print(RAs)













