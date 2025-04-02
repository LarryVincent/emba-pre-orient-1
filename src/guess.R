library(tidyverse)
library(here)
library(scales)
library(hrbrthemes)
library(tidyfinance)
library(fmpapi)
library(ggrepel)

theme_set(theme_ipsum(base_family = "Aktiv Grotesk"))
pal <- c("#E9D48B", "#C1272D", "#246A9B","#1F8C84", "#111111", "#5060AD", "#D87E25")
thermo <- c("#C1272D", "#D94C5C", "#E9D48B", "#47A2EB", "#246A9B")
cool_colors <- c("#5C5470", "#B9B4C7", "#352F44")



# Stock Performance -------------------------------------------------------

prices <- download_data(
  type = "stock_prices",
  symbols = c("GES", "GAP", "LEVI", "ANF", "URBN"),
  start_date = '2021-01-01',
  end_date = '2025-04-01'
)

prices |> 
  filter(!symbol %in% c("ANF", "URBN")) |> 
  mutate(
    event = ifelse(date == "2024-02-16" & symbol == "GES", 19, NA),
    event = ifelse(date == "2024-04-03" & symbol == "GES", 19, event),
    event_label = ifelse(date == "2024-02-16" & symbol == "GES", "r+b announced", NA),
    event_label = ifelse(date == "2024-04-03" & symbol == "GES", "r+b closed", event_label),
    end_label = ifelse(date == "2025-03-31", symbol, NA)
  ) |> 
  ggplot(aes(x=date, y=adjusted_close, color=symbol, group=symbol, shape=event)) +
  geom_line(show.legend = FALSE) +
  geom_point(size = 5, color = "black") +
  geom_text(aes(label = event_label), size = 4, vjust = -1, color = "black", fontface="bold") +
  geom_text(aes(label = end_label), size = 4, hjust = 0, color = "black", fontface = "bold", show.legend = FALSE) +
  scale_shape_identity() +
  scale_color_manual(values = c("grey80", pal[2], "grey80")) +
  scale_x_date(expand = expansion(mult = c(0,.1))) +
  labs(
    x="",
    y="",
    title = "Guess? stock price between 2021 and Today"
  )

returns <- prices |> 
  filter(!symbol %in% c("ANF", "URBN")) |> 
  arrange(date) |> 
  mutate(ret = adjusted_close / lag(adjusted_close) - 1) |> 
  select(symbol, date, ret) |> 
  drop_na(ret)

quantile_5 <- quantile(returns$ret, probs = 0.05)

returns |>
  ggplot(aes(x = ret)) +
  geom_histogram(bins = 100, fill=pal[5], color = "white") +
  geom_vline(aes(xintercept = quantile_5),
             linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily Costco stock returns"
  ) +
  scale_x_continuous(labels = percent)


returns |>
  group_by(symbol) |> 
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    )
  ))

returns |>
  group_by(year = year(date)) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  ggplot(aes(x=year, y=daily_mean)) +
  geom_col() +
  geom_text(aes(label = number(daily_mean, accuracy = 0.001)), color = "white", fontface = "bold", vjust=1)


# Financial Statements ----------------------------------------------------

# fmp_get(
#   resource = "balance-sheet-statement", 
#   symbol = c("MSFT"), 
#   params = list(period = "annual", limit = 5)
# )
# 
# constituents <- download_data_constituents("Dow Jones Industrial Average") |> 
#   pull(symbol)

constituents <- c("GAP", "GES", "LEVI")

params <- list(period = "annual", limit = 5)

balance_sheet_statements <- constituents |> 
  map_df(
    \(x) fmp_get(resource = "balance-sheet-statement", symbol = x, params = params)
  )

income_statements <- constituents |> 
  map_df(
    \(x) fmp_get(resource = "income-statement", symbol = x, params = params)
  )

cash_flow_statements <- constituents |> 
  map_df(
    \(x) fmp_get(resource = "cash-flow-statement", symbol = x, params = params)
  )

balance_sheets_statements <- balance_sheet_statements |> 
  mutate(
    current_ratio = total_current_assets / total_assets,
    quick_ratio = (total_current_assets - inventory) / total_current_liabilities,
    cash_ratio = cash_and_cash_equivalents / total_current_liabilities,
    label = symbol
  )

balance_sheets_statements |> 
  filter(calendar_year == 2024 & !is.na(label)) |> 
  select(symbol, contains("ratio")) |> 
  pivot_longer(-symbol) |> 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) |> 
  ggplot(aes(x = value, y = name, fill = fct_rev(symbol))) +
  geom_col(position = "dodge", color="white") +
  scale_x_continuous(labels = percent) + 
  scale_fill_manual(values = c("#F6C44A", "#BB271A", "#265CA5")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = NULL, y = NULL, fill = NULL,
    title = "Liquidity ratios for selected retailers, 2024"
  )

balance_sheets_statements <- balance_sheets_statements |> 
  mutate(
    debt_to_equity = total_debt / total_equity,
    debt_to_asset = total_debt / total_assets
  )

income_statements <- income_statements |> 
  mutate(
    interest_coverage = operating_income / interest_expense,
    label = symbol,
  )

balance_sheets_statements |> 
  ggplot(aes(x = calendar_year, y = debt_to_asset,
             color = symbol)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = percent) +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = "Debt-to-asset ratios of selected stocks between 2020 and 2024"
  ) 

selected_colors <- c("#265CA5", "#BB271A", "#F6C44A")

balance_sheets_statements |> 
  filter(calendar_year == 2024) |> 
  ggplot(aes(x = debt_to_asset,
             y = fct_reorder(symbol, debt_to_asset),
             fill = label)) +
  geom_col() +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = selected_colors) +
  labs(
    x = NULL, y = NULL, color = NULL,
    title = "Debt-to-asset ratios of retailers, 2024"
  ) + 
  theme(legend.position = "none")

income_statements |> 
  filter(calendar_year == 2024) |> 
  select(symbol, interest_coverage, calendar_year) |> 
  left_join(
    balance_sheets_statements,
    join_by(symbol, calendar_year)
  ) |> 
  ggplot(aes(x = debt_to_asset, y = interest_coverage, color = label)) +
  geom_point(size = 2) +
  geom_label_repel(aes(label = label), seed = 42, box.padding = 0.75) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = selected_colors) +
  labs(
    x = "Debt-to-Asset", y = "Interest Coverage",
    title = "Debt-to-asset ratios and interest coverages for Vincent 2024"
  ) +
  theme(legend.position = "none")

combined_statements <- balance_sheets_statements |> 
  select(symbol, calendar_year, label, current_ratio, quick_ratio, cash_ratio,
         debt_to_equity, debt_to_asset, total_assets, total_equity, inventory) |> 
  left_join(
    income_statements |> 
      select(symbol, calendar_year, interest_coverage, revenue, cost_of_revenue,
             selling_general_and_administrative_expenses, interest_expense,
             gross_profit, net_income),
    join_by(symbol, calendar_year)
  ) |> 
  left_join(
    cash_flow_statements |> 
      select(symbol, calendar_year, accounts_receivables),
    join_by(symbol, calendar_year)
  )

combined_statements <- combined_statements |> 
  mutate(
    asset_turnover = revenue / total_assets,
    inventory_turnover = cost_of_revenue / inventory,
    receivables_turnover = revenue / accounts_receivables
  )



# My Set ------------------------------------------------------------------

prices_daily <- download_data(
  type = "stock_prices",
  symbols = constituents,
  start_date = "2022-01-01",
  end_date = "2025-03-31"
)

prices_daily |>
  ggplot(aes(
    x = date,
    y = adjusted_close,
    color = symbol
  )) +
  geom_line(aes(group=symbol), show.legend = TRUE) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Stock prices of LV index constituents"
  ) 

prices_daily |> 
  group_by(symbol) |> 
  summarise(
    median = median(adjusted_close),
    mu = mean(adjusted_close),
    sd = sd(adjusted_close),
    min = min(adjusted_close),
    max = max(adjusted_close),
    first = first(adjusted_close),
    last = last(adjusted_close)
  ) |> 
  mutate(
    delta = last-first,
    d_pct = delta/first
  ) |> 
  arrange(desc(delta)) 



# Liquidity Etc -----------------------------------------------------------

financial_ratios <- combined_statements |> 
  filter(calendar_year == 2024) |> 
  select(symbol, 
         contains(c("ratio", "margin", "roe", "_to_", "turnover", "interest_coverage"))) |> 
  pivot_longer(cols = -symbol) |> 
  mutate(
    type = case_when(
      name %in% c("current_ratio", "quick_ratio", "cash_ratio") ~ "Liquidity Ratios",
      name %in% c("debt_to_equity", "debt_to_asset", "interest_coverage") ~ "Leverage Ratios",
      name %in% c("asset_turnover", "inventory_turnover", "receivables_turnover") ~ "Efficiency Ratios",
      name %in% c("gross_margin", "profit_margin", "after_tax_roe") ~ "Profitability Ratios"
    )
  ) 

financial_ratios |> 
  group_by(type, name) |> 
  arrange(desc(value)) |> 
  mutate(rank = row_number()) |> 
  group_by(symbol, type) |> 
  summarize(rank = mean(rank), 
            .groups = "drop") |> 
  ggplot(aes(x = rank, y = type, color = symbol)) +
  geom_point(shape = 17, size = 4) +
  scale_color_manual(values = selected_colors) + 
  labs(
    x = "Average rank", y = NULL, color = NULL,
    title = "Average rank among Dow Jones index constituents for selected stocks"
  ) 


combined_statements <- combined_statements |> 
  mutate(
    gross_margin = gross_profit / revenue,
    profit_margin = net_income / revenue,
    after_tax_roe = net_income / total_equity
  )

combined_statements |> 
  mutate(
    label = case_when(
      symbol == "AAPL" ~ "AAPL",
      symbol == "TGT" ~ "TGT",
      TRUE ~ label
    )
  ) |> 
  filter(calendar_year == 2024, !symbol %in% c("AAPL", "AMZN")) |> 
  ggplot(aes(x = gross_margin, y = profit_margin, color = label)) +
  geom_point(size = 2) +
  geom_label(aes(label = label)) +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0.2, .2))) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0.2, .2))) + 
  scale_color_manual(values = selected_colors) + 
  labs(
    x = "Gross margin", y = "Profit margin",
    title = "Gross and profit margins for Retailers, 2024"
  )  +
  theme(legend.position = "none")


market_cap <- constituents |> 
  map_df(
    \(x) fmp_get(
      resource = "historical-market-capitalization", 
      x, 
      list(from = "2024-01-01", to = "2024-12-31")
    )
  ) 

combined_statements_ff <- combined_statements |> 
  filter(calendar_year == 2024) |> 
  left_join(market_cap, join_by(symbol)) |> 
  left_join(
    balance_sheet_statements |> 
      filter(calendar_year == 2024) |> 
      select(symbol, total_assets_lag = total_assets), 
    join_by(symbol)
  ) |> 
  mutate(
    size = log(market_cap),
    book_to_market = total_equity / market_cap,
    operating_profitability = (revenue - cost_of_revenue - selling_general_and_administrative_expenses - interest_expense) / total_equity,
    investment = total_assets / total_assets_lag
  )


combined_statements |> 
  filter(calendar_year > 2020) |> 
  mutate(
    end_label = ifelse(calendar_year == 2024, label, NA)
  ) |> 
  ggplot(aes(x=calendar_year, y=profit_margin, group = label, color = label)) +
  geom_hline(yintercept = 0, color = pal[2], alpha = 0.3, linetype = "dashed", linewidth = 0.9) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_text(aes(label = end_label), size=5, hjust = 0, color = "black", fontface="bold", show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15), add = c(0, .2))) +
  scale_color_manual(values = c("grey80", pal[2], "grey80")) +
  scale_y_percent(breaks = seq(-0.1, .1, .01)) +
  labs(
    x="Year", y="Profit Margin",
    title = "Retail Profit Margins"
  )

combined_statements |> 
  filter(calendar_year > 2020) |> 
  mutate(
    end_label = ifelse(calendar_year == 2024, label, NA),
    g_turn = ifelse(symbol == "GES", inventory_turnover, NA)
  ) |> 
  ggplot(aes(x=calendar_year, y=inventory_turnover, group = label, color = label)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_text(aes(label = end_label), size=5, hjust = 0, color = "black", fontface="bold", nudge_x = .05, show.legend = FALSE) +
  geom_text(aes(label = number(g_turn, accuracy = 0.1)), color = "black", size = 5, nudge_y = 0.2, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15), add = c(0, .2))) +
  scale_color_manual(values = c("grey80", pal[2], "grey80")) + 
  labs(
    x="Year", y="Turnover Ratio",
    title = "Inventory Turnover"
  )


shares_outstanding <- constituents |> 
  map_df(\(x) fmp_get(
    resource = "key-metrics", 
    symbol = x, 
    params = list(period = "annual", limit = 5)
  ))

shares_outstanding |> 
  filter(calendar_year > 2020) |> 
  mutate(
    end_label = ifelse(calendar_year == 2024, symbol, NA),
    g_turn = ifelse(symbol == "GES", inventory_turnover, NA)
  ) |> 
  ggplot(aes(x=calendar_year, y=net_income_per_share, group = symbol, color = symbol)) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_point(size = 4, show.legend = FALSE) +
  geom_text(aes(label = end_label), size=5, hjust = 0, color = "black", fontface="bold", nudge_x = .05, show.legend = FALSE) +
  geom_text(aes(label = number(g_turn, accuracy = 0.1)), color = "black", size = 5, nudge_y = 0.2, show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.15), add = c(0, .2))) +
  scale_y_comma(accuracy = 0.1, prefix = "$") +
  scale_color_manual(values = c("grey80", pal[2], "grey80")) + 
  labs(
    x="Year", y="EPS",
    title = "Earnings Per Share"
  )

