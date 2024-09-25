

#' Markowitz Criterion
#'
#' Implementation of An Markowitz Criterion
#' More information about the method at  https://doi.org/10.9771/1516-9022rene.v5i2.6769
#' More information about the implementation at https://github.com/luana1909/Markowtiz/blob/main/DESCRIPTION
#' @name markowitzcalc
#' @importFrom dplyr mutate left_join group_by summarise arrange mutate_at vars pull filter rename inner_join select
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magrittr %>%
#' @importFrom utils count.fields read.csv
#' @import tidyverse
#' @param criterios A dataframe with weights for each criteria
#' @param criterios A dataframe with the values of the criteria for each alternative
#' @param lambda_selec A number defining the degree of risk appetite
#' @returns dataframe with all comparations between alternatives
#' @export
#' @examples
#' criterios <- data.frame(criterio = c('a1', 'a2', 'a3'),
#'                         peso = c(0.25, 0.5, 0.25))
#' alternativas <- data.frame(alternativas = c('outdoor', 'televisao', 'jornal'),
#'                            a1 = c(12, 36, -3),
#'                            a2 = c(-6, 12, 60),
#'                            a3 = c(24, 48, 30))
#' result <- markowitzcalc(criterios, alternativas)

utils::globalVariables(c("esperanca", "across", "vars", "valor", "peso", "concorrencia","%>%","variancia","desvio_padrao","arrange","esp_dev","pivot_wider","ausente", "filter","pull","na.omit","inner_join","delta_esperanca","delta_desvio","rename","Alternativa 2","Alternativa 1","Delta Esperanca","Delta Desvio", "Delta Esp/Delta","Delta Esp/Delta Desvio","case_when"))

markowitzcalc <- function(criterios, alternativas, lambda_selec = NULL){


  concorrencias <- alternativas %>%
    mutate_at(vars(-alternativas), ~as.numeric(.x)) %>%
    pivot_longer(cols = -alternativas,
                 names_to = "criterio",
                 values_to = "valor") %>%
    left_join(criterios, by = "criterio") %>%
    mutate(concorrencia = valor * peso)

  esperancas <- alternativas %>%
    mutate_at(vars(-alternativas), ~as.numeric(.x)) %>%
    pivot_longer(cols = -alternativas,
                 names_to = "criterio",
                 values_to = "valor") %>%
    left_join(criterios, by = "criterio") %>%
    mutate(concorrencia = valor * peso) %>%
    group_by(alternativas) %>%
    summarise(esperanca = sum(concorrencia))

  variancias <- concorrencias %>%
    left_join(esperancas, by = "alternativas") %>%
    mutate(variancia = peso * (valor - esperanca)^2) %>%
    group_by(alternativas) %>%
    summarise(desvio_padrao = sqrt(sum(variancia)))

  tabela_delta <- esperancas %>%
    left_join(variancias, by = "alternativas") %>%
    mutate(esp_dev = esperanca/desvio_padrao) %>%
    arrange(-esp_dev)

  tabela_deltav2 <- expand.grid(alternativa1 = tabela_delta$alternativas,
                                alternativa2 = tabela_delta$alternativas) %>%
    mutate(ausente = NA) %>%
    pivot_wider(id_cols = alternativa1, names_from = alternativa2, values_from = ausente)

  tabela_deltav3 <- expand.grid(alternativa1 = tabela_delta$alternativas,
                                alternativa2 = tabela_delta$alternativas) %>%
    mutate(ausente = NA) %>%
    pivot_wider(id_cols = alternativa1, names_from = alternativa2, values_from = ausente)

  for(i in 1:(nrow(tabela_delta)-1)){

    alternativa1 <- tabela_delta$alternativas[i]
    esperanca1 <- tabela_delta %>% filter(alternativas == alternativa1) %>% pull(esperanca)
    desvio1 <- tabela_delta %>% filter(alternativas == alternativa1) %>% pull(desvio_padrao)

    for(j in (i+1):nrow(tabela_delta)){

      alternativa2 <- tabela_delta$alternativas[j]
      esperanca2 <- tabela_delta %>% filter(alternativas == alternativa2) %>% pull(esperanca)
      desvio2 <- tabela_delta %>% filter(alternativas == alternativa2) %>% pull(desvio_padrao)
      tabela_deltav2[j, i+1] <- esperanca1 - esperanca2
      tabela_deltav3[j, i+1] <- desvio1 - desvio2

    }

  }

  delta_esp <- tabela_deltav2 %>%
    pivot_longer(-alternativa1, names_to = "alternativa2", values_to = "delta_esperanca") %>%
    na.omit()

  delta_desv <- tabela_deltav3 %>%
    pivot_longer(-alternativa1, names_to = "alternativa2", values_to = "delta_desvio") %>%
    na.omit()

  tabela_delta_conhecido <- delta_esp %>%
    dplyr::inner_join(delta_desv, by = c("alternativa1", "alternativa2")) %>%
    mutate(`Delta Esp/Delta Desvio` = round(delta_esperanca/delta_desvio,2)) %>%
    rename(`Alternativa 1` = alternativa1,
           `Alternativa 2` = alternativa2,
           `Delta Esperanca` = delta_esperanca,
           `Delta Desvio` = delta_desvio) %>%
    select(`Alternativa 2`, `Alternativa 1`, `Delta Esperanca`, `Delta Desvio`,
                  `Delta Esp/Delta Desvio`)

  if(!is.null(lambda_selec)){

    tabela_delta_conhecido <- tabela_delta_conhecido %>%
      mutate(`Preferencia da Acao` = case_when(`Delta Esp/Delta Desvio` > lambda_selec ~ as.character(`Alternativa 2`),
                                               `Delta Esp/Delta Desvio` < lambda_selec ~ as.character(`Alternativa 1`),
                                               TRUE ~ "Alternativas Equivalentes")) %>%
      rename(`Compara A(k)` = `Alternativa 2`,
             `Com A(i)` = `Alternativa 1`)

  }

  return(tabela_delta_conhecido)

}
