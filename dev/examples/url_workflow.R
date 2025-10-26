library(blockr.ui)
library(blockr.core)

# devtools::load_all("../blockr.ui") # ???
devtools::load_all("blockr.io") # ???
# devtools::load_all("../blockr.core") # ???

# Swiss GDP data URL (CSV format)
swiss_gdp_url <- "https://www.seco.admin.ch/dam/seco/en/dokumente/Wirtschaft/Wirtschaftslage/BIP_Daten/ch_seco_gdp_csv.csv.download.csv/ch_seco_gdp.csv"

blockr.core::serve(
  blockr.ui::new_dag_board(
    blocks = c(
      data = new_read_block(
        path = swiss_gdp_url,
        source = "url"
      )
    )
  )
)
