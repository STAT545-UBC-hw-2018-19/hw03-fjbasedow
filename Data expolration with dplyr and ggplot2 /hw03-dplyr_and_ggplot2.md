STAT545 Homework 3
================
Frederike Basedow
26 September 2018

``` r
library(tidyverse)
library(gapminder)
library(knitr)
```

1.  Get the maximum and minimum of GDP per capita for all continents.

``` r
range(gapminder$gdpPercap)
```

    ## [1]    241.1659 113523.1329

Looks like the gdp increased in every continent with time, although the change in Afrika and the Americas in minimal.

1.  Look at the spread of GDP per capita within the continents.

``` r
# group gapminder by continent
gr_cont <- gapminder %>% group_by(continent)

# calculate lowest, highest and mean value of gdp per continent
mean_gdp_cont <- gr_cont %>% summarize(Mean=mean(gdpPercap))
min_gdp_cont <- gr_cont %>% summarize(Min=min(gdpPercap))
max_gdp_cont <- gr_cont %>% summarize(Max=max(gdpPercap))

# merge results together into one data frame
mm_gdp_cont <- merge(mean_gdp_cont, min_gdp_cont, by="continent")
sum_gdp_cont <- merge(mm_gdp_cont, max_gdp_cont, by="continent")

# rename continent variable
sum_gdp_cont <- rename(sum_gdp_cont, Continent=continent)

# show results in nice table
kable(sum_gdp_cont)
```

| Continent |       Mean|         Min|        Max|
|:----------|----------:|-----------:|----------:|
| Africa    |   2193.755|    241.1659|   21951.21|
| Americas  |   7136.110|   1201.6372|   42951.65|
| Asia      |   7902.150|    331.0000|  113523.13|
| Europe    |  14469.476|    973.5332|   49357.19|
| Oceania   |  18621.609|  10039.5956|   34435.37|

``` r
# make boxplot that shows gdp stats per country on a log scale
gapminder %>%  
  ggplot(aes(continent, gdpPercap)) +
  scale_y_log10() +
  geom_boxplot() +
  labs(x="Continent", y="GDP per capita")
```

![](hw03-dplyr_and_ggplot2_files/figure-markdown_github/unnamed-chunk-3-1.png)

Problem: cannot pipe gapminder into base R funtions `summary()` or `range`. This makes it hard to use these function for previously sorted data. Feel like my solution is very long and I am wondering if there is a shorter way to do this.

1.  Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population. Just try something other than the plain vanilla mean.

2.  How is life expectancy changing over time on different continents?

3.  Report the absolute and/or relative abundance of countries with low life expectancy over time by continent: Compute some measure of worldwide life expectancy – you decide – a mean or median or some other quantile or perhaps your current age. Then determine how many countries on each continent have a life expectancy less than this benchmark, for each year.

4.  Find countries with interesting stories. Open-ended and, therefore, hard. Promising but unsuccessful attempts are encouraged. This will generate interesting questions to follow up on in class.

Or, make up your own! Between the dplyr coverage in class and the list above, I think you get the idea.
