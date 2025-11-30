devtools::load_all(".")

# Swiss GDP data URL (CSV format)
swiss_gdp_url <- "https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"

serve(
  new_board(
    blocks = c(
      data = new_read_block(
        path = swiss_gdp_url,
        source = "url",
        args = list(
          sep = ","
        )
      )
    )
  )
)
