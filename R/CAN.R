#' @title CAN
#' @description CANary.
#' @param deploy boolean, TRUE if the model should be deployed in a live trading environment, FALSE if it's just being used for testing/research, Default: TRUE
#' @param model_units, how many units to allocate to this model
#' @return An xts of units to be combined with an unconditional return series to form portfolio (ie condition) returns.
#' @details This is the KDA algorithm (reference below) but only ever goes long SPY.
#' @references
#' \url{https://quantstrattrader.com/2019/02/27/kda-robustness-results/}
#' @export 
#' @importFrom parallel mclapply
#' @importFrom xts xts
#' @importFrom zoo index
CAN <- function(deploy = TRUE, model_units = 1.5)
{

  asset_symbols <- c('SPY', 'VWO', 'AGG')

  units_can <- function(rets_xts, model_units)
  {

    rets <- rets_xts
    ep <- clean_endpoints(rets)
    ccr <- get('calculate_cumulative_returns')

    units_list <- parallel::mclapply(seq(length(ep)-12), function(x) {

      ret_subset <- rets[(ep[x]+1):ep[x+12],]
      ep_sub <- ep[x:(x+12)]
      six_months <- rets[(ep_sub[7]+1):ep_sub[13],]
      three_months <- rets[(ep_sub[10]+1):ep_sub[13],]
      one_month <- rets[(ep_sub[12]+1):ep_sub[13],]

      moms <- ccr(one_month)*12 + ccr(three_months)*4 + ccr(six_months)*2 + 
        ccr(ret_subset) 

      invested_assets <- data.frame(SPY = moms[,'SPY'] > 0)
      pct_aggressive <- mean(moms[,2:3] > 0)
      invested_assets <- invested_assets * pct_aggressive
      if (pct_aggressive == 1) invested_assets <- invested_assets * model_units

      xts::xts(invested_assets, order.by = utils::tail(zoo::index(ret_subset), 1))

    })

    do.call(rbind, units_list)

  }

  if (deploy) {

    clhelpers::make_rets_xts(asset_symbols) %>% 
      units_can(model_units) %>%
      utils::tail(2 - any(clhelpers::show_special_day() == 'EOM')) %>% 
      utils::head(1)

  } else {

    clhelpers::make_rets_xts(asset_symbols, -1) %>% units_can(model_units)

  }

}
 
