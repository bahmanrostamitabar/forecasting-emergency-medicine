# Forecasting emergency medicine

Paper on Forecasting Welsh Ambulance Call-outs by Bahman Rostami-Tabar and Rob J Hyndman

## Folders:

* `data` contains raw data and cleaned/wrangled data (stored as rds) in `gts` and `tsibble` formats.
* `rscripts` contains all R scripts used in experiments and in final version
* `manuscript` contains our main paper
* `img` contains figures for the paper not created using R

## To reproduce results for paper

Run the following scripts in this order

* **Prepare data**: `rscript/data/incidents.R`
* **Generate forecasts and evaluation summaries**: `rscript/hts/forecast_hts.R`

## Count series papers:

* [Hierarchical dynamic models for multivariate times series of counts](https://www.intlpress.com/site/pub/pages/journals/items/sii/content/vols/0007/0004/a011/)
* [paperpile](https://paperpile.com/shared/P1EJ9e)
* [Dynamic Models for Time Series of Counts with a Marketing Application](https://books.google.com.au/books?hl=en&lr=&id=X4dUCwAAQBAJ&oi=fnd&pg=PA425&dq=ravishanker+count&ots=KXLJCU9cK-&sig=l6jx7R8ZHpR8qRu9oPILBaqrhms&redir_esc=y#v=onepage&q=ravishanker%20count&f=false)
* https://paperpile.com/shared/IB0z2K
* https://cran.r-project.org/web/packages/glarma/vignettes/glarma.pdf
* https://www.intlpress.com/site/pub/pages/journals/items/sii/content/vols/0007/0004/a011/
* https://paperpile.com/shared/P1EJ9e

## R packages:

* https://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf
* https://cran.r-project.org/web/packages/glarma/vignettes/glarma.pdf
* https://cran.r-project.org/web/packages/GAS/index.html
* weather data: http://data.ceda.ac.uk/badc/ukmo-midas-open/data/uk-daily-temperature-obs/dataset-version-202007/south-glamorgan/01272_cardiff-bute-park/qc-version-1

##  More resources:

 * https://cran.r-project.org/web/views/TimeSeries.html
 * check: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
 * tscount: https://cran.r-project.org/web/packages/tscount/vignettes/tsglm.pdf
 * https://pipiras.sites.oasis.unc.edu/timeseries/Nonlinear_2_-_Count_time_series_-_Menu.html
 * https://hidda.github.io/forecasting/articles/extra/CHILI_tscount.html
 * https://www.youtube.com/watch?v=6mIUmAUj0I0
 * https://masalmon.eu/2017/02/12/wikideaths1_ts/
 * file:///Users/bahmanrostami-tabar/Downloads/2015_31_report.pdf
 * https://hidda.github.io/forecasting/
 * https://hidda.github.io/forecasting/articles/CHILI_glarma.html
