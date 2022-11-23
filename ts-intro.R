## ---------------------------------------------------------------------------
url_orig = 'https://github.com/bdemeshev/webinar_eusp_forecasting_r_2021_03_13/raw/main/original_data.xls'


## ---------------------------------------------------------------------------
library(rio) # импорт-экспорт данных
library(tidyverse) # визуализация + обработка данных 
library(fpp3) # пачка пакетов по времен рядам


## ---------------------------------------------------------------------------
d = import('original_data.csv')
colnames(d) = d[3, ]
d1 = d[-(1:3), ]
d2 = d1[, -2]
colnames(d2)[1:2] = c('region', 'period')

# export(d2, 'd2.csv')
# d2 = import('d2.csv')

unique(d2$region)
d3 = filter(d2, !str_detect(period, '-'))
d4 = separate(d3, period, c('percode', 'month'), sep = ' ')

d5 = pivot_longer(d4, cols=`2006`:`2020`,
                  names_to = 'year',
                  values_to = 'marriage')
d6 = separate(d5, region, c('regcode', 'region'), 
              sep = ' ', extra = 'merge')
d7 = select(d6, -percode)

# export(d7, 'd7.csv')
# d7 = import('d7.csv')
glimpse(d7)

d8 = mutate(d7, regcode = as.numeric(regcode))

d9 = mutate(d8, marriage = as.numeric(str_remove(marriage, ',')))

d10 = mutate(d9, month = str_replace(month, 'май', 'мая'))


d11 = mutate(d10, date = paste0('01-', month, '-', year))
glimpse(d11)

d12 = mutate(d11, date = dmy(date))

# export(d12, 'd12.csv')
# d12 = import('d12.csv')


## ---------------------------------------------------------------------------
data = import('d12.csv')
glimpse(data)

rus = filter(data, regcode == 643)
rus1 = select(rus, date, marriage)

rus2 = mutate(rus1, date = yearmonth(date))
rus3 = arrange(rus2, date)

marr = as_tsibble(rus3, index = date)
marr


## ---------------------------------------------------------------------------
autoplot(marr, marriage)
gg_season(marr, marriage)
gg_tsdisplay(marr, marriage)


## ---------------------------------------------------------------------------
mod_table = model(marr, 
  base = NAIVE(marriage),
  ets_aaa = ETS(marriage ~ error('A') + trend('A') + season('A')),
  log_ets_aaa = ETS(log(marriage) ~ error('A') + trend('A') + season('A'))
)

report(select(mod_table, ets_aaa))

autoplot(components(select(mod_table, ets_aaa)))


## ---------------------------------------------------------------------------
fcst = forecast(mod_table, h = '2 years')
fcst
autoplot(fcst, filter(marr, date > ymd('2018-01-01')))

