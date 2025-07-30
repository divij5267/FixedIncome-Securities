# FixedIncomeMQM

An R package for fixed income securities analysis and calculations, providing tools for bond pricing, yield curve analysis, and financial modeling.

> **Note:** This README is currently a placeholder and will be updated with more detailed documentation, examples, and usage guidelines in future releases.

## Installation

### From GitHub (Development Version)
```r
# Install devtools if you haven't already
if (!require(devtools)) install.packages("devtools")

# Install the package
devtools::install_github("divij5267/FixedIncomeMQM")
```

### Dependencies
This package depends on:
- `jrvFinance` - For core financial calculations
- `data.table` - For efficient data manipulation
- `nloptr` - For numerical optimization

## Features

### Core Functions

#### Date Utilities
- `DATE(yyyy, mm, dd)` - Convert year, month, day to Date object
- `as.Date2(x)` - Enhanced date conversion with multiple format support

#### Zero-Coupon Bond Analysis
- `zcb.price(zcb.yield, ttm, freq)` - Calculate zero-coupon bond price
- `zcb.yield(zcb.price, ttm, freq)` - Calculate zero-coupon bond yield

#### Bond Analysis
- `bond.convexity(settle, mature, coupon, freq, yield, convention)` - Calculate bond convexity
- `bond.full.price(...)` - Calculate full bond price including accrued interest

#### Yield Curve Analysis
- `gfun4(ttm, parm)` - 5th degree polynomial function for yield curve fitting
- `spot_rates_recurrsive(ttm, pfull, coupon, freq, plot)` - Recursive spot rate calculation

## Usage Examples

### Basic Bond Calculations
```r
library(FixedIncomeMQM)

# Calculate zero-coupon bond price
price <- zcb.price(0.05, 2, 2)  # 5% yield, 2 years, semi-annual
print(price)

# Calculate bond convexity
convexity <- bond.convexity(
  settle = as.Date("2024-02-05"),
  mature = as.Date("2029-02-05"),
  coupon = 0.05,
  yield = 0.06,
  freq = 2,
  convention = "30/360"
)
print(convexity)
```

### Date Handling
```r
# Create date from components
my_date <- DATE(2024, 2, 5)

# Enhanced date parsing
date1 <- as.Date2("2024-02-05")
date2 <- as.Date2("02/05/2024")
```

### Yield Curve Analysis
```r
# Example parameters for yield curve fitting
parms <- c(0.05, 0.02, 0.01, 0.005, 0.002, 0.001)
fitted_yield <- gfun4(2, parms)

# Spot rate calculation
ttm <- c(0.5, 1, 1.5, 2)
prices <- c(98.5, 97.2, 95.8, 94.1)
coupons <- c(0.03, 0.035, 0.04, 0.045)

spot_rates <- spot_rates_recurrsive(ttm, prices, coupons, freq = 2, plot = TRUE)
```

## Documentation

For detailed documentation of individual functions, use:
```r
?function_name
# Example:
?zcb.price
?bond.convexity
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## Issues

Report bugs and request features on the [GitHub issues page](https://github.com/divij5267/FixedIncomeMQM/issues).

## License

This package is licensed under the terms specified in the DESCRIPTION file.

## Author

Jane Doe (jane@example.com)

## Version

Current version: 0.2.0
