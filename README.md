
<!-- README.md is generated from README.Rmd. Please edit that file -->

# COVID-19’ impact on fashion retailing consumer behaviour

<!-- badges: start -->

<!-- badges: end -->

### Objective:

The goal of this repository is to document through open-source data the
impact of COVID-19 on customer’ behaviour in the fashion retailing
sector. Last updated at 2020-04-21.

### Methodology:

The methodology is simple - the following main steps are followed:

1.  Google search data of general fashion categories is extracted for
    all the countries that have suffered since more than two weeks ago
    from 75 deaths caused by COVID-19.
2.  Similar as in an event study, the event 0 date is selected for each
    country based on when the country reached 75 cummulative deaths
    caused by COVID-19. Countries, which have not reached this point for
    longer than two weeks, have been removed.
3.  Based on the search data before event 0, a search estimation is made
    if COVID-19 would not have happened. For this step, the [prophet
    forecasting method](https://facebook.github.io/prophet/) is chosen
    to predict Google searches for each fashion category for each
    country.
4.  Last but not least, expected predicted results are compared with
    real search values for each category at each country. Results can be
    found [here as plot
    images](https://github.com/matbmeijer/covid19_fashion_trends/tree/master/output/plots).

### Results:

Here you can find a Trelliscope Web App with the results:

  - <https://matbmeijer.github.io/covid19_fashion_trends/docs>

If you prefer a more detailed analysis in image form, you can find it in
the following
    folder:

  - <https://github.com/matbmeijer/covid19_fashion_trends/blob/master/output/plots>

Here is an example for
Spain:

<center>

<img src="https://github.com/matbmeijer/covid19_fashion_trends/blob/master/output/plots/ES_plot2.png?raw=true">

</center>

### Coverage

| Fashion Category |
| :--------------- |
| Bag              |
| Belt             |
| Blouse           |
| Cape             |
| Cardigan         |
| Coat             |
| Collar           |
| Dress            |
| Glasses          |
| Glove            |
| Hat              |
| Headband         |
| Hood             |
| Jacket           |
| Jumpsuit         |
| Pants            |
| Scarf            |
| Shirt            |
| Shoe             |
| Shorts           |
| Skirt            |
| Stockings        |
| Sweater          |
| Sweatshirt       |
| T-Shirt          |
| Tie              |
| Tights           |
| Top              |
| Vest             |
| Wallet           |
| Watch            |
| Sock             |
| Hair Accessory   |
| Head Covering    |
| Umbrella         |
| Leg Warmer       |

| Covered Countries |
| :---------------- |
| Austria           |
| Belgium           |
| Brazil            |
| Canada            |
| China             |
| Denmark           |
| Ecuador           |
| France            |
| Germany           |
| Iran              |
| Ireland           |
| Italy             |
| Korea, South      |
| Netherlands       |
| Philippines       |
| Portugal          |
| Romania           |
| Spain             |
| Sweden            |
| Switzerland       |
| Turkey            |
| United Kingdom    |
| US                |

### Data sources:

  - Google searches - [Google Trends](https://www.google.com/trends)
  - COVID 19 deaths - [Johns Hopkins Whiting School of
    Engineering](https://github.com/CSSEGISandData)
  - Fashion categories -
    [iFashionist](https://github.com/MalongTech/imaterialist-product-2019)
  - [Inidividuals using the internet (%) - World
    Bank](https://data.worldbank.org/indicator/IT.NET.USER.ZS)
  - Expected results calculated with Prophet Forecasting Procedure
    (<https://facebook.github.io/prophet/>)
