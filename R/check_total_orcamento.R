#' Total do orçamento fiscal e investimento SIGPLAN vs SISOR
#'
#' Verificar se o valor total do orçamento (orçamento fiscal e orçamento de 
#' investimento das empresas controladas) coincide com a projeção do PPAG 
#' para o ano seguinte
#' @export
check_total_orcamento_fiscal <- function(base_qdd_fiscal, acoes_planejamento, verbose = NULL) {
  base_qdd_fiscal <- as_data_table(base_qdd_fiscal)
  acoes_planejamento <- as_data_table(acoes_planejamento)
  
  sisor <- base_qdd_fiscal[, sum(VL_LOA_DESP)]
  sigplan <- acoes_planejamento[is_deleted_acao == FALSE & 
                                          identificador_tipo_acao_cod %in% c(1, 2, 4, 7, 9), 
                                        sum(vr_meta_orcamentaria_ano0)]
  result <- isTRUE(all.equal(sisor, sigplan))
  if (!result) {
    log_error("O total do sisor (R$ {pp(sisor)}) é diferente do sigplan (R$ {pp(sigplan)}) em {pp(sisor - sigplan)}. Vide detalhes em: https://t.ly/N_7oM")
    result
  }
  result
}