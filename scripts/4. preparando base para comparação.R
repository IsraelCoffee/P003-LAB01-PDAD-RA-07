# Renomeando variaveis Moradores

amostra_mor |> 
  
  # Filtrando para DF, paranoá e seus pares ----
  filter(
    A01uf == 53,
    localidade %in% c(5307, 5334, 5328, 5304, 5321)
  ) |> 
  
  # Renomeando localidade ----
  mutate(
    localidade = factor(
      case_when(
        localidade == 5307 ~ "Paranoá", 
        localidade == 5334 ~ "Arapoanga", 
        localidade == 5328 ~ "Itapoã", 
        localidade == 5304 ~ "Brazlândia", 
        localidade == 5321 ~ "Riacho Fundo II"
      ),
      levels = c("Paranoá", "Arapoanga", "Itapoã", "Brazlândia", "Riacho Fundo II")
    ),
    
    # sexo do morador ----
    sexo = factor(
      case_when(
        E03 == 1 ~ "Masculino",
        E03 == 2 ~ "Feminino"
    ),
      levels = c("Masculino", "Feminino")
    ),
    
    # Faixa etaria ----
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
    ),
    
    # Cor / raça ----
    cor.raca = factor(
      case_when(
        E05 %in% c(2, 4) ~ "Negra",
        E05 %in% c(1, 3, 5) ~ "Não negra"
      ),
      levels = c("Negra", "Não negra")
    ),
    
    # Nível de instrução escolar ----
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
      levels = c(
        "Sem instrução", "Fundamental incompleto ou equivalente",
        "Fundamental completo ou equivalente", "Médio incompleto ou equivalente",
        "Médio completo ou equivalente", "Superior incompleto ou equivalente",
        "Superior completo","Sem classificação"
      )),
    
    # Frequenta qual tipo de escola?
    freq_ensino = factor(
      case_when(
        H02 == 1     ~ "Sim",
        H02 == 2     ~ "Não",
        H02 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    ),
    
    # Tipo de escola que frequenta ----
    tipo_escola = factor(
      case_when(
        H04_1 == 1 ~ "Pública",
        H04_1 == 2 ~ "Particular",
        H04_1 == 88888 ~ "Não sabe",
        H04_1 == 99999 ~ "Não se aplica"
      ),
      levels = c("Pública", "Particular", "Não sabe", "Não se aplica")
    ),
    
    # Trabalhou nos últimos 30 dias? ----
    trabalhou_30d = factor(
      case_when(
        I04 == 1     ~ "Sim",
        I04 == 2     ~ "Não",
        I04 == 88888 ~ "Não sabe",
        I04 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    ),
    
    # Refime de trabalho ---- 
    regime_trabalho = factor(
      case_when(
        I07 == 1     ~ "Presencial",
        I07 == 2     ~ "Teletrabalho/remoto",
        I07 == 3     ~ "Híbrido",
        I07 == 88888 ~ "Não sabe",
        I07 == 99999 ~ "Não se aplica"
      ),
      levels = c("Presencial", "Teletrabalho/remoto", "Híbrido", "Não sabe",
                 "Não se aplica")
    ),
    
    # Ocupação de trabalho ----
    posicao_ocupacao = factor(
      case_when(
        I13 == 1        ~ "Empregado no setor público",
        I13 %in%c(2, 9) ~ "Militar",
        I13 == 3        ~ "Empregado no setor privado",
        I13 == 4        ~ "Empregado doméstico",
        I13 %in% c(5,6) ~ "Estágio remunerado e Aprendiz",
        I13 == 7        ~ "Conta própria ou autônomo",
        I13 == 8        ~ "Empregador",
        I13 == 10       ~ "Trabalhador não remunerado",
        I13 == 88888    ~ "Não sabe",
        I13 == 99999    ~ "Não se aplica"
      ),
      levels = c("Empregado no setor público", "Militar",
                 "Empregado no setor privado", "Empregado doméstico",
                 "Estágio remunerado e Aprendiz", "Conta própria ou autônomo",
                 "Empregador", "Trabalhador não remunerado", "Não sabe",
                 "Não se aplica")
    ),
    
    # Se Possui CTPS assinada ----
    ctps = factor(
      case_when(
        I18 == 1     ~ "Sim",
        I18 == 2     ~ "Não",
        I18 == 88888 ~ "Não sabe",
        I18 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    ),
    
    # Transporte utilizado para estudar ----
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
        "Não se aplica")
    ),
    
    # Transporte utilizado para trabalhar ----
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
        "Não sabe", "Não se aplica")
    ),
    
    # Tempo de deslocamento para o trabalho ----
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
        "Até 15 minutos", "Acima de 15 minutos até 30 minutos",
        "Acima de 30 minutos até 45 minutos", "Acima de 45 minutos até 1 hora",
        "Acima de 1 hora até 1 hora e 15 minutos", 
        "Acima de 1 hora e 15 minutos até 1 hora e 30 minutos",
        "Acima de 1 hora e 30 minutos até 1 hora e 45 minutos",
        "Acima de 1 hora e 45 minutos até 2 horas", "Acima de 2 horas",
        "Não sabe", "Não se aplica"
      )
    ),
    
    # Atendimento em saúde ----
    saude_atendimento = factor(
      case_when(
        G01 == 1 ~ "Sim",
        G01 == 2 ~ "Não",
        G01 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    ),
    
    #  RA procurada para atendimento ----
    RA_procurada = factor(
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
    ),
    
    # Possui plano de saúde ----
    plano_saude = factor(
      case_when(
        G05 == 1 ~ "Sim",
        G05 == 2 ~ "Não",
        G05 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    ),
    
    # Acesso individual à internet ----
    ind_acesso_internet = factor(
      case_when(
        F04 == 1 ~ "Sim",
        F04 == 2 ~ "Não",
        F04 == 88888 ~ "Não sabe"
      ),
      levels = c("Sim", "Não", "Não sabe")
    ),
    
    # Acessou internet pelo computador ----
    comp_acesso_internet = factor(
      case_when(
        F05_1 == 1 ~ "Sim",
        F05_1 == 2 ~ "Não",
        F05_1 == 88888 ~ "Não sabe",
        F05_1 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    ),
    
    # Acessou internet pelo Celula/tablet ----
    cel_tab_acesso_internet = factor(
      case_when(
        F05_2 == 1 ~ "Sim",
        F05_2 == 2 ~ "Não",
        F05_2 == 88888 ~ "Não sabe",
        F05_2 == 99999 ~ "Não se aplica"
      ),
      levels = c("Sim", "Não", "Não sabe", "Não se aplica")
    ),
    )