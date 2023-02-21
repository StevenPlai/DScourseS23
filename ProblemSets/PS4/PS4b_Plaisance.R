library(sparklyr)
library(tidyverse)

spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

as_tibble()