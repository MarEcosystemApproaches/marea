library(marea)
plots_dir <- "C:/git/Maritimes/RVTransmogrifier/plots"

marea::plot(readRDS(paste0(plots_dir,"/RV_SUMMER_BMASS_30.rds")), style = "ribbon")
marea::plot(readRDS(paste0(plots_dir,"/RV_SUMMER_ABUND_590.rds")), style = "ribbon")


data <- loadRVData(cxn = getCxn())
