library(tidyverse)
library(santoku)
library(ggokabeito)
library(scales)
library(datos)

datos <- datos::pinguinos
df <- count(datos, estado_civil)

df

ggplot(
  data = df,
  mapping = aes(
    x = fct_reorder(estado_civil, n, .desc = TRUE),
    y = n
  )
) +
  geom_col() +
  theme_minimal()

df <- datos |> 
  select(
    edad, raza
  ) |> 
  mutate(
    grupo_edad = chop_pretty(edad),
    grupo_edad = fct_na_value_to_level(grupo_edad,"Desconocido")
  ) |> 
  count(raza,grupo_edad) |> 
  mutate(
    prop = n/sum(n),
    .by = grupo_edad
  )

df


ggplot(
  data = df,
  mapping = aes(
    x = grupo_edad,
    y = n,
    fill = raza
  )
) +
  geom_col()

ggplot(
  data = df,
  mapping = aes(
    x = grupo_edad,
    y = n,
    fill = raza,
    label = comma(n)
  )
) +
  geom_col(
    data = select(df, -raza),
    fill = "#F3EEEA"
  ) +
  geom_col() +
  geom_text(vjust = -0.5, color = "#191919") +
  scale_fill_okabe_ito() +
  scale_y_continuous(
    labels = comma_format()
  ) +
  facet_wrap(~raza, ncol = 2) +
  labs(
    title = "Composición Demográfica por Edad y Raza",
    subtitle = "Distribución de las Categorías Blanca, Negra y Otra por Grupos de Edad",
    x = "Edad",
    y = "Cantidad de personas",
    fill = "Raza"
  ) +
  theme_minimal(base_size = 12)+
  theme(
    legend.position = "top",
    legend.justification = "left",
    axis.text = element_text(),
    panel.grid.major.y = element_line(
      color = "#E8E2E2"
    ),
    axis.title.x = element_text(
      margin = margin(t = 10)
      ),
    axis.title.y = element_text(
      margin = margin(r = 10)
      ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(
      color = "#776B5D"
    ),
    axis.text.x = element_text(
      color = "#776B5D"
    )
  )

