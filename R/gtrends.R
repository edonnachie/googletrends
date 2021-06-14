library(tidyverse)
library(gtrendsR)

search_terms <- tribble(
  ~ "category", ~ "search_term",
  "Covid-19", "Corona test",
  "Covid-19", "Arzt",
  "Covid-19", "Covid-19",
  "Covid-19", "Mask")


gtrends_res <- search_terms %>% 
  ## Collect trends from 2020-01-01 until yesterday
    mutate(gtrend_long = map(
    search_term,
    ~ gtrends(
        keyword = .,
        geo = "DE-BY",
        time = paste("2020-01-01", Sys.Date() - 1),
        # time = paste("2020-01-01", Sys.Date() - 1),
        hl = "de",
        low_search_volume = TRUE
        )
    )) %>% 
  ## Collect trends for the past month
  mutate(gtrend_month = map(
    search_term,
    ~ gtrends(
        keyword = .,
        geo = "DE-BY",
        time = paste(Sys.Date() - 31, Sys.Date() - 1),
        # time = paste("2020-01-01", Sys.Date() - 1),
        hl = "de",
        low_search_volume = TRUE
        )
    )) %>% 
  ## Extract interest over time
  mutate(
    interest_over_time_long = map(
      gtrend_long, ~ mutate(
        .$interest_over_time,
        hits = case_when(
          is.integer(hits) ~ as.numeric(hits),
          is.character(hits) ~ coalesce(as.numeric(hits), 0),
          TRUE ~ as.numeric(hits)
          ))
      )
    ) %>% 
  mutate(
    interest_over_time_month = map(
      gtrend_month, ~ mutate(
        .$interest_over_time,
        hits = case_when(
          is.integer(hits) ~ as.numeric(hits),
          is.character(hits) ~ coalesce(as.numeric(hits), 0),
          TRUE ~ as.numeric(hits)
          ))
      )
    )

saveRDS(gtrends_res, file = "data/google_search_trends.rds")


# Plot
p_gtrends <- gtrends_res %>%
  select(search_term, interest_over_time_long) %>%
  unnest(cols = interest_over_time_long) %>%
  mutate(date = lubridate::as_date(date)) %>%
  ggplot(aes(x = date, y = hits)) +
  geom_line() +
  scale_x_date(
    breaks = as.Date(c("2020-01-01", "2020-07-01", "2021-01-01")),
    date_minor_breaks = "1 month",
    date_labels = "%Y-%m"
    ) +
  facet_wrap(~search_term) +
  labs(x = "",
       y = "Relative Search Frequency",
       title = "Google Search Trends during the Covid-19 Pandemic",
       subtitle = "Users located in Bavaria, Germany",
       caption = "Source: Google Search Trends")

ggsave(p_gtrends, file = "figures/gtrends_corona.png",
       width = 14, height = 10, units = "cm", dpi = 300)
