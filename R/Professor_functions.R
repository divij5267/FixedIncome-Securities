#' Convert Year, Month, Day to Date
#'
#' @title Convert to Date Object
#' @description Creates a Date object from separate year, month, and day values
#'
#' @param yyyy Year (numeric)
#' @param mm Month (numeric)
#' @param dd Day (numeric)
#'
#' @return A Date object
#' @export
#'
#' @examples
#' DATE(2024, 2, 5)
DATE <- function(yyyy,mm,dd){
  dte <- as.Date(sprintf("%i-%i-%i",yyyy,mm,dd), format = "%Y-%m-%d")
  return(dte)
}

#' Zero-Coupon Bond Price Calculator
#'
#' @title Calculate Zero-Coupon Bond Price
#' @description Calculates the price of a zero-coupon bond given yield, time to maturity, and payment frequency
#'
#' @param zcb.yield Yield rate (decimal)
#' @param ttm Time to maturity in years
#' @param freq Payment frequency per year (default = 2 for semi-annual)
#'
#' @return Price of the zero-coupon bond (per 100 par value)
#' @export
#'
#' @examples
#' zcb.price(0.05, 2, 2)  # 5% yield, 2 years to maturity, semi-annual compounding
zcb.price <- function(zcb.yield,ttm,freq=2) {
  return( 100/(1+zcb.yield/freq)^(freq*ttm))
}

#' Zero-Coupon Bond Yield Calculator
#'
#' @title Calculate Zero-Coupon Bond Yield
#' @description Calculates the yield of a zero-coupon bond given price, time to maturity, and payment frequency
#'
#' @param zcb.price Price of the zero-coupon bond
#' @param ttm Time to maturity in years
#' @param freq Payment frequency per year (default = 2 for semi-annual)
#'
#' @return Yield of the zero-coupon bond (as decimal)
#' @export
#'
#' @examples
#' zcb.yield(95, 2, 2)  # Price of 95, 2 years to maturity, semi-annual compounding
zcb.yield <- function(zcb.price,ttm,freq=2) {
  return( freq * ( (100/zcb.price)^(1/(freq*ttm))-1) )
}

#' Enhanced Date Conversion
#'
#' @title Enhanced Date Conversion
#' @description Converts strings to Date objects with multiple format support
#'
#' @param x Character string representing a date
#'
#' @return A Date object
#' @details Attempts to parse dates in the following formats:
#'   - YYYY-MM-DD
#'   - MM/DD/YYYY
#'   - YYYY/MM/DD
#'   - Month DD,YYYY
#' @export
#'
#' @examples
#' as.Date2("2024-02-05")
#' as.Date2("02/05/2024")
as.Date2 <- function(x) {
  tryfmt <- c("%Y-%m-%d","%m/%d/%Y","%Y/%m/%d","%b %d,%Y")
  return(as.Date(x,tryFormats=tryfmt))
}

#' Generic Function for Yield Curve Fitting
#'
#' @title Generic Function for Yield Curve Fitting
#' @description Implements a 5th degree polynomial function for yield curve fitting
#'
#' @param ttm Time to maturity
#' @param parm Vector of 6 parameters [a0, a1, a2, a3, a4, a5]
#'
#' @return Fitted yield value
#' @details Uses the formula: a0 + a1*ln(1+t) + a2*ln(1+t)^2 + a3*ln(1+t)^3 + a4*ln(1+t)^4 + a5*ln(1+t)^5
#' @export
#'
#' @examples
#' parms <- c(0.05, 0.02, 0.01, 0.005, 0.002, 0.001)
#' gfun4(2, parms)
gfun4 <- function(ttm,parm) {
  a0 = parm[1]
  a1 = parm[2]
  a2 = parm[3]
  a3 = parm[4]
  a4 = parm[5]
  a5 = parm[6]
  ltm = log(1+ttm)
  tmp <- a0 + a1*ltm + a2 * ltm^2 + a3 * ltm^3 + a4 * ltm^4 + a5 * ltm^5
  return(tmp)
}

#' Calculate Bond Convexity
#'
#' @title Calculate Bond Convexity
#' @description Calculates the convexity of a bond using total cash flow analysis
#'
#' @param settle Settlement date
#' @param mature Maturity date
#' @param coupon Annual coupon rate (as decimal)
#' @param freq Payment frequency per year (default = 2 for semi-annual)
#' @param yield Yield to maturity
#' @param convention Day count convention
#' @param comp.freq Compounding frequency (defaults to payment frequency)
#'
#' @return Convexity measure of the bond
#' @export
#'
#' @examples
#' bond.convexity(
#'   settle = as.Date("2024-02-05"),
#'   mature = as.Date("2029-02-05"),
#'   coupon = 0.05,
#'   yield = 0.06,
#'   freq = 2,
#'   convention = "30/360"
#' )
bond.convexity <-
  function(settle,mature,coupon,freq=2,yield,convention,comp.freq=freq) {
    z <- as.data.frame(bond.TCF(settle,mature,coupon,freq,convention))
    cf <- z$cf
    t <- z$t
    r <- yield
    m <- comp.freq
    return ( 1/sum( cf/(1+r/m)^(t*m) ) * sum( t*(t+1/m)*cf/(1+r/m)^(t*m+2) ) )
  }
