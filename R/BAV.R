.datatable.aware = TRUE 
if (getRversion() >= '2.15.1') 
  utils::globalVariables(c('.', 'Ret', 'sma_col', 'sig'), utils::packageName()) 
 
#' @importFrom magrittr '%$%' '%T>%' 
#' @importFrom data.table ':=' 
#' @title BAV
#' @description Below AVerage.
#' @param deploy boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE
#' @param model_units numeric, how many units to allocate to this model
#' @return An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
#' @details Enter at close and hold for 24 hours if today's return is below its SMA (lookback length being optimized monthly using last year's data).
#' @export 
#' @importFrom data.table data.table shift melt merge.data.table
#' @importFrom TTR SMA
#' @importFrom xts xts merge.xts
#' @importFrom stats sd na.omit lag
#' @importFrom zoo index na.locf
BAV <- function(deploy = TRUE, model_units = 1)
{

  symbol <- 'SPY'
  rets <- clhelpers::make_rets_xts(symbol, 430*deploy + -1*!deploy) 
  sma_dt <- data.table::data.table(rets, keep.rownames = TRUE) %T>%
    {names(.)[2] <- 'Ret'}
  n_vect <- c(2:9, seq(10, 95, 5), seq(100, 200, 10))

  for (i in seq(n_vect)) {
    sma_dt[, paste0(i) := data.table::shift(
      Ret < (-TTR::SMA(abs(Ret), i))) %T>% 
      {.[!.] <- NA}
    ]
  }

  x_rets <- xts::xts(sma_dt[,-c(1:2)], sma_dt[,index]) * as.numeric(rets)
  ep <- clean_endpoints(x_rets)

  sma_rownum_dt <- lapply(1:(length(ep)-12), function(x) {
    sma_subset <- x_rets[c((ep[x]+1):ep[(x+12)]),]
    sharpes <- apply(sma_subset, 2, function(y) {
      mean(y, na.rm = TRUE) / stats::sd(y, na.rm = TRUE)
    }) %>%
      stats::na.omit() %>% t
    xts::xts(max.col(sharpes, ties.method = 'first'), 
      utils::tail(zoo::index(sma_subset), 1))
  }) %>%
    do.call(what = rbind) %>%
    xts::merge.xts(rets) %>%
    .[,1] %>%
    stats::lag() %>%
    zoo::na.locf() %>%
    stats::na.omit() %>%
    `names<-`('sma_col') %>%
    data.table::data.table(keep.rownames = TRUE)

  sma_dt[,-2] %>%
    data.table::melt(id.vars = 'index', variable.name = 'sma_col', 
      value.name = 'sig') %>%
    .[, sma_col := as.numeric(sma_col)] %>%
    data.table::merge.data.table(sma_rownum_dt) %$%
    xts::xts(sig, index) %>%
    {. * model_units} %T>%
    {.[is.na(.)] <- 0} %>%
    utils::tail((-1)^!deploy) %>%
    `names<-`(symbol)

}

