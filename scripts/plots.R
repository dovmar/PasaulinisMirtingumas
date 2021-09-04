# Duomenų nuskaitymas ----------

library(tidyverse)
library(highcharter)
library(hrbrthemes)
library(ggrepel)
library(countrycode)
library(here)

x <- readxl::read_xlsx(here("data/input/global_mortality.xlsx"))

x <- x %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

x <- x %>% janitor::clean_names(case = "sentence")
names(x)[4:35] <- c(
  "Širdies ligos", "Vėžiai", "Kvepavimo ligos", "Diabetas", "Demencija", "Apatinių kvepavimo takų infekcijos",
  "Naujagimių mirtys", "Viduriavimo ligos", "Kelių įvykiai", "Inkstų ligos", "Tuberkuliozė", "Kepenų ligos", "Virškinimo ligos", "AIDS",
  "Savižudybė", "Maliarija", "Žmogžudystės", "Neprievalgis", "Meningitas", "Proteinų trūkumas", "Skendimas", "Mirtys gimdant", "Parkinsono liga",
  "Alkoholis", "Virškinimo infekcijos", "Narkotikai", "Hepatitas", "Gaisras", "Šaltis arba karštis", "Gamtos katastrofos", "Konfliktai", "Terorizmas"
)

urlico <- "url(https://raw.githubusercontent.com/tugmaks/flags/2d15d1870266cf5baefb912378ecfba418826a79/flags/flags-iso/flat/24/%s.png)"

x_1 <- x %>%
  mutate(countrycode = countrycode(Country, origin = "country.name", destination = "iso2c")) %>%
  mutate(
    marker = sprintf(urlico, countrycode),
    marker = map(marker, function(x) list(symbol = x)),
    flagicon = sprintf(urlico, countrycode),
    flagicon = str_replace_all(flagicon, "url\\(|\\)", ""),
    continent = countrycode(Country, origin = "country.name", destination = "continent"),
    CountryLT = countrycode(Country, origin = "country.name", destination = "cldr.variant.lt"),
    continent = factor(continent, labels = c("Afrika", "Amerikos", "Azija", "Europa", "Okeanija"))
  )


y <- read_csv(here("data/input/WPP2019_Period_Indicators_Medium.csv"))

y_1 <- y %>%
  filter(MidPeriod %in% 1960:2018) %>%
  mutate(
    continent = countrycode(Location, origin = "country.name", destination = "continent"),
    continent = factor(continent, labels = c("Afrika", "Amerikos", "Azia", "Europa", "Okeanija")),
    LocationLT = countrycode(Location, origin = "country.name", destination = "cldr.variant.lt")
  ) %>%
  drop_na(continent)


thm <- hc_theme_merge(
  hc_theme_smpl(),
  hc_theme(
    colors = c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#e4d354", "#2b908f", "#f45b5b", "#91e8e1")
  )
)




# 1 grafikas ---------------------




x_8 <- x %>% filter(Country == "World", Year == 2016)

x_8 <- x_8 %>%
  select(1, 4:10) %>%
  pivot_longer(2:8) %>%
  rbind(., x %>%
          filter(Year == 2016, Country == "World") %>%
          select(-(2:10)) %>%
          pivot_longer(-1) %>% drop_na() %>% group_by(Country) %>% summarize(Kitos = sum(value)) %>% pivot_longer(-1))


x_9 <- x %>%
  filter(Country == "World") %>%
  select(1, 3:10) %>%
  pivot_longer(3:9) %>%
  rbind(., x %>%
          filter(Country == "World") %>%
          select(-(c(2, 4:10))) %>%
          pivot_longer(-(1:2)) %>% drop_na() %>% group_by(Country, Year) %>%
          summarize(Kitos = sum(value)) %>% pivot_longer(-(1:2))) %>%
  group_nest(name) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(x = Year, y = value), drop = TRUE),
    data = map(data, list_parse)
  ) %>%
  left_join(x_8, by = c("name" = "name"))

p1 <- hchart(x_9,
       type = "column", hcaes(x = name, y = value, name = name),
       colorByPoint = TRUE, showInLegend = FALSE, pointWidth = 50,
       pointPadding = 0, groupPadding = 0,
       pointPlacement = "on"
) %>%
  hc_xAxis(type = "category") %>%
  hc_tooltip(
    pointFormatter = tooltip_chart(
      accesor = "data",
      hc_opts = list(
        chart = list(type = "line"), credits = list(enabled = FALSE),
        plotOptions = list(line = list(marker = list(enabled = FALSE), label = list(enabled = FALSE), lineWidth = 3))
      ),
      height = 225, width = 300
    ),
    useHTML = TRUE
  ) %>%
  hc_title(text = "Mirties priežasčių pokytis", align = "left") %>%
  hc_yAxis(title = list(text = "Mirtys"), labels = list(format = "{value}%")) %>%
  hc_xAxis(
    title = list(text = "Kategorija"), tickmarkPlacement = "on", min = -1,
    max = 8.5, showFirstLabel = FALSE,
    showLastLabel = FALSE
  ) %>%
  hc_add_theme(thm)




# 2 -------------------------





x_10 <- x %>%
  filter(Country == "World") %>%
  pivot_longer(-(1:3)) %>%
  drop_na() %>%
  group_by(name) %>%
  mutate(max = max(value)) %>%
  ungroup() %>%
  mutate(perc = value / max * 100)

p2 <- hchart(x_10, type = "heatmap", hcaes(x = Year, y = name, value = perc)) %>%
  hc_colorAxis(
    stops = color_stops(10, viridisLite::inferno(10, direction = -1, begin = 0.1)), showLastLabel = FALSE,
    labels = list(format = "{value}%", size = 10)
  ) %>%
  hc_size(height = 750) %>%
  hc_tooltip(formatter = JS("function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ': ' +
  Highcharts.numberFormat(this.point.value, 2) + '%';
}")) %>%
  hc_title(text = "Mirčių kiekio palyginimas su kiekvienos kategorijos maksimumu", align = "left") %>%
  hc_yAxis(title = list(text = "Kategorija"), gridLineWidth = 1, gridLineColor = "white") %>%
  hc_xAxis(gridLineWidth = 1, gridLineColor = "white", title = list(text = "Metai")) %>%
  hc_add_theme(thm)



# 3 ---------------------




x_6 <- x_1 %>%
  filter(Country == "World", Year == 2016) %>%
  select(1:35) %>%
  pivot_longer(4:35)

x_7 <- x_1 %>%
  drop_na(countrycode) %>%
  select(CountryLT, 2:35, marker) %>%
  pivot_longer(4:35) %>%
  group_by(name, Year) %>%
  slice_max(value, n = 1) %>%
  nest(-name) %>%
  mutate(
    type = "line", id = name,
    data = map(data, mutate_mapping, hcaes(x = Year, y = value)),
    data = map(data, list_parse)
  )


p3 <- hchart(x_6, type = "pie", hcaes(name = name, y = value, drilldown = name), dataLabels = list(enabled = FALSE), innerSize = "70%") %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(x_7)
  ) %>%
  hc_plotOptions(line = list(marker = list(enabled = TRUE), tooltip = list(
    useHTML = TRUE, headerFormat = "<b>{point.x}</b><br>",
    pointFormat = "<b>Šalis: </b>{point.CountryLT}<br><b>Dalis mirčių: </b>{point.y}%"
  ))) %>%
  hc_xAxis(title = list(text = "Metai")) %>%
  hc_yAxis(title = list(text = "Mirtys"), labels = list(format = "{value}%")) %>%
  hc_tooltip(
    pointFormat = "<b>{point.value}%</b>", useHTML = TRUE, style = list(fontSize = "15px"),
    headerFormat = "<b><div style='font-size:20px;color:{point.color}'>{point.key}</div></b>"
  ) %>%
  hc_title(text = "Šalys, pirmaujančios pagal mirties priežastį", align = "left") %>%
  hc_add_theme(thm)



# 4 ---------------------


which <- y_1 %>%
  group_by(Location) %>%
  select(Location, MidPeriod, LEx) %>%
  distinct(Location, MidPeriod, .keep_all = TRUE) %>%
  pivot_wider(c(Location, MidPeriod), names_from = MidPeriod, values_from = LEx) %>%
  drop_na() %>%
  mutate(diff = `2018` - `1963`) %>%
  arrange(diff) %>%
  pull(Location)

y_9 <- y_1 %>%
  mutate(group = ifelse(Location %in% which[1:5], "Smallest",
                        ifelse(Location %in% rev(which)[1:5], "Largest", "0")
  )) %>%
  filter(group != "0")


world <- y %>%
  filter(Location == "World") %>%
  select(-Location)

p4 <- ggplot(y_9, aes(x = MidPeriod, y = LEx, color = continent, group = LocationLT)) +
  geom_line(size = 2) +
  facet_wrap(vars(group, LocationLT),
             ncol = 5,
             labeller = labeller(group = function(x) {
               substr(x, 0, 0)
             }, Location = label_value)
  ) +
  scale_color_viridis_d("Žemynas") +
  theme_ipsum(
    base_size = 25, plot_title_size = 30, subtitle_size = 25,
    axis_title_size = 20, strip_text_size = 25, axis_text_size = 20
  ) +
  theme(panel.grid.minor = element_blank(), legend.position = "right") +
  labs(
    title = "Didžiausi ir mažiausi gyvenimo trukmės padidėjimai",
    subtitle = "Palygininus su pasauliniu augimu"
  ) +
  xlab("Metai") +
  ylab("Gyvenimo trukmė") +
  scale_x_continuous(breaks = c(1960, 1990, 2020), limits = c(1959, 2020)) +
  geom_line(data = world, inherit.aes = FALSE, aes(x = MidPeriod, y = LEx), color = "black", size = 2, alpha = 0.2)



# 5 -------------





small <- y_1 %>%
  filter(MidPeriod == 2018) %>%
  mutate(lab = ifelse((CDR < 4) | (CBR > 40) | (CDR > 12), LocationLT, ""))

p5 <- ggplot(subset(y_1, MidPeriod == 2018), aes(CDR, CBR, color = continent)) +
  geom_vline(xintercept = 8, size = 1) +
  geom_hline(yintercept = 27.5, size = 1) +
  geom_point(size = 6) +
  ylim(5, 50) +
  xlim(0, 16) +
  theme_ipsum(
    base_size = 25, plot_title_size = 30,
    subtitle_size = 25, axis_title_size = 20, strip_text_size = 25, axis_text_size = 20
  ) +
  theme(panel.grid.minor = element_blank(), legend.position = "right") +
  geom_text_repel(data = small, aes(CDR, CBR, label = lab), force = 30, inherit.aes = FALSE, size = 5) +
  scale_color_viridis_d("Žemynas", guide = guide_legend(title.position = "top")) +
  theme(legend.position = "bottom") +
  labs(title = "Mirtys ir gimimai 1000 gyventojų 2018 metais") +
  xlab("Mirtys 1000 gyventojų") +
  ylab("Gimimai 1000 gyventojų")



# 6 ----------------------



y_5 <- y_1 %>%
  filter(MidPeriod == 2018) %>%
  mutate(ratio = round(LExFemale / LExMale, 2)) %>%
  arrange(desc(ratio))

max <- y_5 %>%
  slice_max(ratio, n = 10) %>%
  mutate(cat = "max")

min <- y_5 %>%
  slice_min(ratio, n = 10) %>%
  mutate(cat = "min")

drilldown <- rbind(max, min) %>%
  group_nest(cat) %>%
  mutate(
    id = cat,
    colorKey = "LEx",
    type = "column",
    data = map(data, mutate_mapping, hcaes(y = ratio, name = LocationLT)),
    data = map(data, list_parse)
  )

values <- sprintf("{point.%s}", names(y_5)[c(27, 11, 12, 13, 28)])
tltip <- tooltip_table(c("Žemynas", "Gyvenimo trukmė", "Vyrų gyvenimo trukmė", "Moterų gyvenimo trukmė", "Santykis"), values)

y_6 <- y_5 %>%
  drop_na(ratio) %>%
  filter(ratio == min(ratio) | ratio == max(ratio)) %>%
  mutate(cat = ifelse(ratio == min(ratio), "min", "max"))


p6 <- hchart(y_6, hcaes(name = LocationLT, y = ratio, drilldown = cat),
       type = "column", colorKey = "LEx", showInLegend = FALSE
) %>%
  hc_colorAxis(
    min = 59, max = 80, stops = color_stops(10, RColorBrewer::brewer.pal(n = 10, name = "Spectral")),
    title = list(text = "Gyvenimo trukmė")
  ) %>%
  hc_yAxis(
    min = 0.8, title = list(text = "Moterų ir vyrų gyvenimo trukmės santykis"),
    plotLines = list(
      list(
        label = list(text = ""),
        color = "#FF0000",
        width = 2,
        value = 1,
        zIndex = 0
      )
    )
  ) %>%
  hc_drilldown(
    allowPointDrilldown = TRUE,
    series = list_parse(drilldown)
  ) %>%
  hc_xAxis(type = "category", title = list(text = "Šalis")) %>%
  hc_tooltip(pointFormat = tltip, useHTML = TRUE, zIndex = 11) %>%
  hc_title(text = "Šalis su didžiausiu ir mažiausiu moterų ir vyrų gyvenimo trukmės santykiu 2018 metais", align = "left") %>%
  hc_add_theme(thm)



# 7 ------------------



y_8 <- y_1 %>%
  filter(MidPeriod == 2018) %>%
  mutate(
    ratio2 = round(DeathsMale / DeathsFemale, 5),
    ratio = round(LExFemale / LExMale, 5)
  )


p7 <- ggplot(y_8, aes(ratio, ratio2, color = continent)) +
  geom_jitter(width = 0.002, height = 0.002, size = 8.5, alpha = 0.8) +
  geom_text_repel(
    data = subset(y_8, ratio2 > 1.8 | ratio2 < 1 | ratio > 1.14),
    aes(label = LocationLT), show.legend = FALSE, size = 7, force = 2, nudge_y = -0.3, nudge_x = -0.003
  ) +
  ylim(-0.2, 3.5) +
  xlim(0.98, 1.2) +
  geom_hline(yintercept = 1, color = "black") +
  annotate(
    geom = "segment", x = 1.1, y = 0.3, xend = 1.2, yend = 0.3,
    arrow = arrow(length = unit(0.5, "cm")), size = 1
  ) +
  geom_vline(xintercept = 1, color = "black") +
  annotate(geom = "text", x = 1.15, y = 0, label = "Didesnė moterų gyvenimo trukmė", size = 8) +
  annotate(
    geom = "segment", y = 1.3, x = 0.99, yend = 3, xend = 0.99,
    arrow = arrow(length = unit(0.5, "cm")), size = 1
  ) +
  scale_color_viridis_d("Žemynas", guide = guide_legend(title.position = "top")) +
  labs(title = "Gyvenimo trukmės ir mirčių santykiai 2018 metais") +
  xlab("Gyvenimo trukmės santykis") +
  ylab("Mirčių santykis") +
  annotate(geom = "text", x = 0.983, y = 2, label = "Daugiau vyrų mirčių", angle = 90, size = 8) +
  theme_ipsum(base_size = 25, plot_title_size = 30, axis_title_size = 20) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")



# 8 ---------------



values <- c("{point.CountryLT}", paste0(sprintf("{point.%s}", names(x_1)[c(18, 27)]), "%"))
tltip <- tooltip_table(c("Šalis", names(x_1)[c(18, 27)]), values)


x_2 <- x_1 %>%
  filter(Year == 2016) %>%
  drop_na(countrycode, continent)
p8 <- highchart() %>%
  hc_add_series(x_2,
                type = "scatter", hcaes(Alkoholis, Savižudybė, group = continent),
                marker = list(symbol = "circle"),
                states = list(hover = list(halo = list(size = 20, opacity = 0.55)))
  ) %>%
  hc_title(text = "Mirčių dalis dėl alkoholio ir savyžudybių 2016 metais", align = "left") %>%
  hc_tooltip(pointFormat = tltip, useHTML = TRUE) %>%
  hc_xAxis(
    title = list(text = "Mirtys dėl alkoholio"),
    minRange = 1, labels = list(format = "{value}%")
  ) %>%
  hc_yAxis(
    title = list(text = "Mirtys dėl savižudybės"),
    minRange = 1, labels = list(format = "{value}%")
  ) %>%
  hc_legend(verticalAlign = "middle", align = "right", layout = "vertical") %>%
  hc_chart(zoomType = "xy") %>%
  hc_add_theme(thm)


# 9 ---------------



values <- c("{point.CountryLT}", paste0(sprintf("{point.%s}", names(x_1)[c(17, 21)]), "%"))
tltip <- tooltip_table(c("Šalis", names(x_1)[c(17, 21)]), values)


p9 <- highchart() %>%
  hc_add_series(x_2,
                type = "scatter", hcaes(x = Neprievalgis, y = AIDS, group = continent),
                marker = list(symbol = "circle"),
                states = list(hover = list(halo = list(size = 20, opacity = 0.55)))
  ) %>%
  hc_title(text = "Mirčių dalis dėl neprievalgio ir ŽIV/AIDS 2016 metais", align = "left") %>%
  hc_tooltip(pointFormat = tltip, useHTML = TRUE) %>%
  hc_xAxis(
    title = list(text = "Mirtys nuo neprievalgio"),
    minRange = 1, labels = list(format = "{value}%")
  ) %>%
  hc_yAxis(
    title = list(text = "Mirtys nuo ŽIV/AIDS"),
    minRange = 1, labels = list(format = "{value}%")
  ) %>%
  hc_legend(verticalAlign = "middle", align = "right", layout = "vertical") %>%
  hc_chart(zoomType = "xy") %>%
  hc_add_theme(thm)



# 10 -----------------------



y_2 <- y %>%
  filter(Location %in% c("World", "Africa", "Asia", "Europe", "Oceania", "South America", "Northern America")) %>%
  filter(MidPeriod %in% 1960:2050) %>%
  mutate(Location = factor(Location,
                           labels = c("Afrika", "Azia", "Europa", "Šiaurės Amerika", "Okeanija", "Pietų Amerika", "Pasaulis")
  ))


y_3 <- y_2 %>%
  distinct(Location, MidPeriod, .keep_all = TRUE) %>%
  select(Location, CDR, CBR) %>%
  group_by(Location) %>%
  do(data = list(sequence = list(.$CDR, .$CBR))) %>%
  pull(data) %>%
  map(~ map(.x, transpose))

y_3 <- map(y_3, ~ list(.x))

y_4 <- y_2 %>%
  select(Location) %>%
  rename(name = Location) %>%
  unique()
y_4$data <- y_3
y_4 <- list_parse(y_4)


p10 <- highchart() %>%
  hc_yAxis(max = 50, min = 0, title = list(text = "Gimimai 1000 gyventojų")) %>%
  hc_xAxis(max = 25, min = 0, title = list(text = "Mirtys 1000 gyventojų")) %>%
  hc_chart(type = "scatter") %>%
  hc_plotOptions(scatter = list(marker = list(symbol = "circle", radius = 8), zIndex = 10)) %>%
  hc_add_series_list(y_4) %>%
  hc_motion(
    enabled = TRUE,
    labels = unique(y_2$MidPeriod),
    series = 0:6,
    updateInterval = 10
  ) %>%
  hc_legend(align = "right") %>%
  hc_title(text = "Metiniai gimimai ir mirtys 1000 gyventojų", align = "left") %>%
  hc_subtitle(text = "Įskaitant ateities projekcijas", align = "left") %>%
  hc_tooltip(pointFormat = "<b>Mirtys:</b> {point.x}<br><b>Gimimai:</b> {point.y}", style = list(fontSize = "15px")) %>%
  hc_add_series(tibble(x = c(0, 25), y = c(0, 25)),
                type = "line", hcaes(x = x, y = y),
                color = "black", zIndex = 0, marker = list(enabled = FALSE),
                states = list(hover = list(enabled = FALSE)),
                showInLegend = FALSE, opacity = 0.6,
                tooltip = list(headerFormat = "Lygybė<br>", useHTML = TRUE)
  ) %>%
  hc_annotations(list(
    draggable = "",
    labelOptions = list(padding = 10, backgroundColor = "transparent", style = list(color = "black")),
    labels = list(list(
      point = list(x = 20, y = 22, xAxis = 0, yAxis = 0),
      text = "Mirčių ir gimimų lygybė<br>"
    ))
  )) %>%
  hc_add_theme(thm) %>%
  hc_size(width = 700)


## 11 ---------------------



x_3 <- x %>%
  filter(Year == 2016) %>%
  filter(str_detect(Country, "SDI"))
x_3 <- x_3 %>%
  select(1, 4:10, "AIDS") %>%
  pivot_longer(2:9)

x_4 <- x %>%
  filter(Year == 2016) %>%
  filter(str_detect(Country, "SDI")) %>%
  select(-(2:10), -"AIDS") %>%
  pivot_longer(-1) %>%
  drop_na() %>%
  group_by(Country) %>%
  summarize(Kitos = sum(value)) %>%
  pivot_longer(-1)

x_3 <- rbind(x_3, x_4)
x_3$Country <- factor(x_3$Country,
                      labels = c("Vidutinis-aukštas SDI", "Aukštas SDI", "Vidutinis-žemas SDI", "Žemas SDI", "Vidutinis SDI")
)

p11 <- highchart() %>%
  hc_add_series(x_3, type = "line", hcaes(x = name, y = value, group = Country)) %>%
  hc_xAxis(type = "category") %>%
  hc_chart(polar = TRUE) %>%
  hc_title(text = "Mirtingumo profilis pagal sociodemografinį indeksą", align = "left") %>%
  hc_tooltip(useHTML = TRUE) %>%
  hc_yAxis(labels = list(format = "{value}%")) %>%
  hc_add_theme(thm)


# 12 --------------------------



y_7 <- left_join(x_1 %>%
                   filter(Year == 2016) %>%
                   drop_na(countrycode, continent), subset(y_1, MidPeriod == 2018), by = c("Country" = "Location")) %>%
  drop_na(LEx) %>%
  mutate(levels = cut_interval(LEx, n = 4, labels = c("Žema", "Vidutinė-žema", "Vidutinė-aukšta", "Aukšta"))) %>%
  select(1, 4:35, levels) %>%
  mutate(across(everything(), ~ replace_na(.x, 0))) %>%
  pivot_longer(2:33) %>%
  group_by(levels, name) %>%
  summarize(m = mean(value)) %>%
  ungroup() %>%
  group_by(levels) %>%
  mutate(n = sum(m), m = round(m / n * 100, 2)) %>%
  group_by(levels) %>%
  arrange(.by_group = TRUE, desc(m)) %>%
  arrange(desc(levels)) %>%
  ungroup() %>%
  nest(-name) %>%
  mutate(
    data = map(data, mutate_mapping, hcaes(name = levels, y = m)),
    data = map(data, list_parse),
    type = "bar",
    showInLegend = FALSE
  )


p12 <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(bar = list(stacking = "normal", opacity = 0.8, states = list(hover = list(opacity = 1, borderColor = "black")))) %>%
  hc_add_series_list(list_parse(y_7)) %>%
  hc_yAxis(title = list(text = "Mirčių dalis"), labels = list(format = "{value}%")) %>%
  hc_xAxis(
    type = "category",
    categories = c("Žema", "Vidutinė-žema", "Vidutinė-aukšta", "Aukšta"), title = list(text = "Vidutinė gyvenimo trukmė")
  ) %>%
  hc_title(
    align = "left",
    text = "Mirtingumo profilis pagal vidutinę gyvenimo trukmę"
  ) %>%
  hc_add_theme(thm) %>%
  hc_tooltip(
    style = list(fontSize = "20px"),
    useHTML = TRUE,
    pointFormat = "<b><div style='font-size:20px;color:{point.color}'>{series.name}: {point.m}%</b>",
    headerFormat = "<span style='font-size:16px'>{point.key} gyvenimo trukmė</span>"
  )



# Išsaugojimas ----------


in_names <- paste("p",1:12,sep="")
save(list=in_names, file=here("data/output/plots.Rda"))
