# DemProjR
R implementation of the CCMPP used in the Spectrum software suite.

## How to use this code
This package is not listed in CRAN. If you have the `devtools` package installed, you can install `DemProjR` in R via

```
> devtools::install_github("rlglaubius/DemProjR")
```

Once you have DemProjR installed, you can run a projection using the example demographic inputs for Switzerland installed with this package:
```
> library(DemProjR)
> upd = system.file("extdata", "Switzerland_756_22.upd", package="DemProjR")
> par = read_upd(upd)
> proj = demproj(par, proj.method=ccmpp_migr_end, spec.fert=TRUE)
```

## Getting demographic input files
If you have Spectrum installed, you can find other UPD files with demographic inputs in
the Avenir Health `%appdata%` folder. This will be something like `C:/Users/<UserName>/AppData/Roaming/Avenir Health/Spectrum/UNPopDataByCountry`.
UPD files are based on regular releases of the United Nations Population Division World Population Prospects (WPP), and stored in folders by revision (e.g., `2022` for the 2022 WPP revision). If you do not see the country you are looking for, you can download these from within in Spectrum, or directly from [Avenir Health's Country Data Pack](https://avenirhealth.org/software-spectrum.php). If you downloaded these from within Spectrum, but don't have a folder for WPP 2022, you may need to install a more recent version of Spectrum. We recommend Spectrum version 6.29 or later.
