# COVID REPORT
# Evil data team
# 03.23.2022

# Here's what we could manage. Sorry. You do the rest.

# packages
  library(tidyverse)
  library(scales)
  library(lfe)
  library(stargazer)
  install.packages("stargazer")
  # uses data.table (fread) for "fast read" of large csv data
  # and stargazer for regression tables


# create the dataset ------------------
## 2021-2022 deaths (BIG FILES)
  covid = data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') |>
    filter(!is.na(fips), state != 'Puerto Rico') |>
    select(fips, county, state, date, deaths) |>
    group_by(fips, county, state) |>
    summarise(deaths = max(deaths, na.rm = T) - min(deaths, na.rm = T))

## estimated mask usage from July 2020 survey
  mask = read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') |>
    mutate(
      fips = as.integer(COUNTYFP),
      always.mask = ALWAYS, #always masking
      .keep = 'none'
    ) # for merging   

## prep CDC data from directory 
  
  vax = read_csv('cdc vax mar1.csv') |>
    filter(
      FIPS != 'UNK', 
      Recip_State != 'VI', 
      Completeness_pct > 0, 
      !is.na(Administered_Dose1_Recip)
    ) |> # drop unknown/incomplete/questionable reports
    mutate(
      fips = as.integer(FIPS), 
      population = Census2019,
      vax.complete = Series_Complete_Pop_Pct, # percent vaxd
      svi.index = SVI_CTGY, # social vulnerability index
      .keep = 'none'
    )  

## merge  
  covid =
    left_join(covid, mask) |>
    left_join(vax) |>
    mutate(deaths.scaled = deaths / population * 100000) |>
    ungroup() # scale by population
  
  rm(mask, vax)
  
  summary(covid)

  
# COVID deaths nationally ----------
## VIZ  
  covid |>
    ggplot(aes(x = 1+deaths)) +
    geom_histogram(fill = '#69b3a2', color = "white") +
    scale_x_log10() + # to mitigate skew
    theme_minimal()


    
# Trying again
  
  covid |>
    ggplot(aes(x = 1+deaths)) +
    geom_histogram(fill = '#69b3a2', color = "white") +
    scale_x_log10() + 
    theme_minimal() + 
  labs(
    title = "Distribution of COVID-19 Deaths",
    x = "Number of Deaths",  
    y = "Frequency") +
    theme(
      plot.title = element_text(hjust = 0.5))
    
## stat summary and more
  summary(covid$deaths)
  # find some examples?


# Mask usage -----------------------
## VIZ: "Always wears a mask"
  covid |>
    ggplot(aes(x = always.mask)) +
    geom_histogram(fill = "#7894EA", color = 'white') +
    scale_x_log10() + 
    theme_minimal() + 
    labs(
      title = "COVID-19 Masking Rates",
      x = "Mask Rate (%)",  
      y = "Frequency") +
    theme(
      plot.title = element_text(hjust = 0.5))

## helpers
  summary(covid$always.mask)

# Rates of vaccination -------------

# VIZ: overall vax rates
  covid |>
    ggplot(aes(x = vax.complete)) +
    geom_histogram(fill = "#CCCCFF", color = 'white')  +
    scale_x_log10() + 
    theme_minimal() + 
    labs(
      title = "COVID-19 Vaccination Rates",
      x = "Vaccination Rate",  
      y = "Frequency") +
    theme(
      plot.title = element_text(hjust = 0.5))
  
summary(covid$vax.complete)

# VIZ: vax rates by Social Vulnerability Index category
  
  # box plots
  covid |>
    ggplot(aes(y = vax.complete, x = svi.index, color = svi.index)) +
    geom_boxplot() # drop the NA category; it's awful

# find high/low counties
  covid |>
    select(vax.complete, state, county) |>
    filter(vax.complete %in% c(min(vax.complete, na.rm = T), 
                               max(vax.complete, na.rm = T)))


# Impact on 2022 COVID deaths ------
## regression estimates
  mods = list(
    m1 = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
    m2 = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
    m3 = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
  )

# regression table
  stargazer::stargazer(
    mods,
    keep.stat = 'n', type = 'text', # change to html in Rmd
    add.lines = list(c('State fixed effects','Yes','Yes','Yes'))
  )  

## Top 5 deaths ----
  
  topdeaths = covid |>
    select(deaths, county, state) |>  
    top_n(10, deaths)  
  
  ggplot(topdeaths, aes(x = county, y = deaths)) +
    geom_bar(stat = "identity", fill = "#69b3a2") +  
    theme_minimal() +
    labs(
      title = "COVID-19 Deaths by County",
      x = "County",
      y = "Deaths"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    )
  
  
## Top 5 mask rates
  
  topmasks = covid |>
    select(always.mask, county, state) |>
    top_n(10, always.mask)  
  
  ggplot(topmasks, aes(x = county, y = always.mask)) +
    geom_bar(stat = "identity", fill = "#7894EA") +  
    theme_minimal() +  
    labs(
      title = "COVID-19 Mask Rates by County", 
      x = "County",  
      y = "Mask Rates"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  
    )
  
## Top 5 vax rates
  
  top5vax = covid |>
    select(vax.complete, county, state) |>
    top_n(10, vax.complete)

  topvax = covid |>
    select(vax.complete, county, state) |>
    arrange(desc(vax.complete)) |>
    slice_head(n = 10)
  
  vax = covid |>
    filter(vax.complete == 95)
  
  vax2 = covid |>
    filter(vax.complete != 95)
  
  # Select the top 10 counties with the highest vaccination rates other than 95%
  topvax = vax2 |>
    arrange(desc(vax.complete)) |>
    slice_head(n = 10)
  
  ggplot(topvax, aes(x = county, y = vax.complete)) +
    geom_bar(stat = "identity", fill = "#CCCCFF") +  
    theme_minimal() +  
    labs(
      title = "COVID-19 Vaccination Rates by County", 
      x = "County",  
      y = "Vaccination Rates"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)  
    )
  