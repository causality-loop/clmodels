clean_endpoints <- function(rets_xts, offset = 0) 
{
  ep <- xts::endpoints(rets_xts) + offset
  ep[ep < 1] <- 1
  ep[ep > nrow(rets_xts)] <- nrow(rets_xts)
  ep <- unique(ep)
  epDiff <- diff(ep)
  if(xts::last(epDiff)==1) ep <- ep[-length(ep)]
  ep
}

