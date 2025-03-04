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

bond.full.price<- function(value.date,maturity, coupon.rate,coupon.freq, yield.rate,conv,comp.freq,redemption_value = 100){
  bdfp <- return(bond.price(value.date,maturity, coupon.rate,coupon.freq, yield.rate,conv,comp.freq,redemption_value = 100) +
                   bond.TCF(value.date,maturity, coupon.rate,coupon.freq,conv,redemption_value = 100)$accrued)}


spot_rates_recurrsive <- function(ttm, pfull, coupon, freq = 2, plot = TRUE) {
  # Validate inputs
  n <- length(ttm)
  if (length(pfull) != n || length(coupon) != n) {
    stop("All input vectors (ttm, pfull, coupon) must have the same length")
  }

  # Sort by time to maturity (ascending order)
  ord <- order(ttm)
  ttm <- ttm[ord]
  pfull <- pfull[ord]
  coupon <- coupon[ord]

  # Create the A matrix
  A <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    A[i,i] <- 1 + coupon[i]/freq
    if (i > 1) {
      for (j in 1:(i-1)) {
        A[i,j] <- coupon[i]/freq
      }
    }
  }

  # Create the b vector
  b <- matrix(pfull/100, nrow = n, ncol = 1)

  # Solve the system to get discount factors
  D <- solve(A, b)
  # Calculate spot rates (zero-coupon yields)
  spot_rates <- zcb.yield(D*100, ttm, freq)

  # Create a data frame with results
  results <- data.frame(
    Time_to_Maturity = ttm,
    Discount_Factor = as.numeric(D),
    Spot_Rate = spot_rates
  )

  # Plot the spot rate curve if requested
  if (plot) {
    plot_spot_curve(results)
  }

  return(results)
}

# Function to plot the spot rate curve
plot_spot_curve <- function(results) {
  # Create the plot
  plot(results$Time_to_Maturity, results$Spot_Rate * 100,
       type = "o", col = "blue", pch = 16,
       xlab = "Time to Maturity (years)",
       ylab = "Spot Rate (%)",
       main = "Spot Rate Curve",
       ylim = c(0, max(results$Spot_Rate * 100) * 1.1))

  # Add grid and smooth curve
  grid()
  if (length(results$Time_to_Maturity) > 3) {
    # Add a smoothed line if we have enough points
    smooth_curve <- smooth.spline(results$Time_to_Maturity, results$Spot_Rate * 100)
    lines(smooth_curve, col = "red", lwd = 2)
    legend("topright", legend = c("Actual Rates", "Smoothed Curve"),
           col = c("blue", "red"), lty = 1, pch = c(16, NA), bty = "n")
  }
}
spot_rates_yield_reg <- compare_yield_models <- function(ttm, spot, plot = TRUE) {
    # Prepare data frame
    data <- data.frame(ttm = ttm, spot = spot)
    data$ltm <- log(1 + ttm)

    # Fit the four regression models
    yreg1 <- lm(spot ~ ttm + log(ttm), data = data)
    yreg2 <- lm(spot ~ ttm + I(1/ttm), data = data)
    yreg3 <- lm(spot ~ ttm + log(1 + ttm) + I(1/(1 + ttm) - 1), data = data)
    yreg4 <- lm(spot ~ ltm + I(ltm^2) + I(ltm^3) + I(ltm^4) + I(ltm^5), data = data)

    # Compare adjusted R-squared values
    adj.r.squared <- c(
      yreg1 = summary(yreg1)$adj.r.squared,
      yreg2 = summary(yreg2)$adj.r.squared,
      yreg3 = summary(yreg3)$adj.r.squared,
      yreg4 = summary(yreg4)$adj.r.squared
    )

    # Find the best model
    best_model_name <- names(which.max(adj.r.squared))
    best_model <- switch(best_model_name,
                         yreg1 = yreg1,
                         yreg2 = yreg2,
                         yreg3 = yreg3,
                         yreg4 = yreg4)

    # Calculate predicted values for all models
    data$pred1 <- predict(yreg1)
    data$pred2 <- predict(yreg2)
    data$pred3 <- predict(yreg3)
    data$pred4 <- predict(yreg4)

    # Create a comparison plot if requested
    if (plot) {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        message("Package ggplot2 is needed for plotting. Installing it...")
        install.packages("ggplot2")
        library(ggplot2)
      } else {
        library(ggplot2)
      }

      p <- ggplot(data, aes(x = ttm, y = spot * 100, color = "actual")) +
        geom_point(alpha = 0.5) +
        geom_line(aes(y = pred1 * 100, color = "fit1"), linetype = "longdash", lwd = 0.6) +
        geom_line(aes(y = pred2 * 100, color = "fit2"), linetype = "longdash", lwd = 0.6) +
        geom_line(aes(y = pred3 * 100, color = "fit3"), linetype = "longdash", lwd = 0.6) +
        geom_line(aes(y = pred4 * 100, color = "fit4"), linetype = "longdash", lwd = 0.6) +
        scale_colour_manual("Model",
                            breaks = c("actual", "fit1", "fit2", "fit3", "fit4"),
                            values = c("blue", "green", "black", "purple", "red"),
                            labels = c("Actual",
                                       "Model 1: ttm + log(ttm)",
                                       "Model 2: ttm + 1/ttm",
                                       "Model 3: ttm + log(1+ttm) + 1/(1+ttm)-1",
                                       "Model 5th-degree polynomial")) +
        xlab("Time-to-maturity (years)") +
        ylab("Spot rate (%)") +
        ggtitle("Yield Curve Model Comparison",
                subtitle = paste("Best model:", best_model_name,
                                 "| Adj. R²:", round(max(adj.r.squared), 4))) +
        theme_minimal() +
        theme(legend.position = "bottom",
              legend.text = element_text(size = 8),
              plot.subtitle = element_text(face = "italic"))

      print(p)
    }

    # Return results
    return(list(
      models = list(yreg1 = yreg1, yreg2 = yreg2, yreg3 = yreg3, yreg4 = yreg4),
      adj_r_squared = adj.r.squared,
      best_model_name = best_model_name,
      best_model = best_model,
      data = data,
      description = c(
        "Model 1" = "spot ~ ttm + log(ttm)",
        "Model 2" = "spot ~ ttm + 1/ttm",
        "Model 3" = "spot ~ ttm + log(1+ttm) + (1/(1+ttm)-1)",
        "Model 4" = "spot ~ 5th-degree polynomial of log(1+ttm)"
      )
    ))
  }

# Function to generate predictions using the best model
predict_spot_rates <- function(yield_model_result, new_ttm) {
  # Get the best model
  best_model <- yield_model_result$best_model
  best_model_name <- yield_model_result$best_model_name

  # Prepare new data for prediction
  new_data <- data.frame(ttm = new_ttm)

  # Add any transformed variables needed by models
  new_data$ltm <- log(1 + new_ttm)

  # Generate predictions
  predicted_spots <- predict(best_model, newdata = new_data)

  # Return results
  return(data.frame(
    ttm = new_ttm,
    predicted_spot = predicted_spots
  ))
}
