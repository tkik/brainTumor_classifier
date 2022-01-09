# 2021-12-25
# mc
# functions for Shiny app building

library(minfi)
library(flextable)
library(dplyr)
library(knitr)
library(kableExtra)
library(pagedown)

render_simp_report = function(path, gender, material_type, updata_folder = "up_data/") {
  #path="data"
  #sentrix_id ="GSM2940725_10003886252_R05C02"
  sentrix_id <- gsub(updata_folder,"",path)
  sentrix_id <- gsub("_Grn.idat.*|_Red.idat.*","",sentrix_id)
  #gender="Female"
  #material_type = "FFPE"
  supplier_diagnosis = "GBM, RTK II"
  sample_identifier="Sample_1"
  
  rmarkdown::render(
    "complete_report_fs.Rmd", params = list(
      path=path,
      sentrix_id=sentrix_id,
      sample_identifier=sample_identifier,
      gender=gender,
      material_type=material_type,
      supplier_diagnosis=supplier_diagnosis
    ),
    #output_file = paste0("report/Report-", sentrix_id, ".html")
    output_file = paste0("report/report.html"),
  )
}

# temp test function/mc
test_pipline <- function(path){
  sentrix_id ="GSM2940725_10003886252_R05C02"
  #gender="Female"
  #material_type = "FFPE"
  supplier_diagnosis = "GBM, RTK II"
  sample_identifier="Sample_1"
  
  # from rmd
  #fp <- file.path(path,sentrix_id) # if path "" - error
  fp <- file.path(path)
  
  cat(path, "\n")
  cat(fp, "\n")
  cat("Exists fp:",file.exists(fp))
  
  RGset <- read.metharray(fp,verbose=TRUE )
  
  return(T)
}


validate_files <- function(updata_folder = "up_data/"){
  #' @title Validate input .idat files
  #' @description Check if user loaded two Red and Grn files with the same
  #' sentrix_id. Files were copied to `updata_folder` directory. One filename 
  #' (including _Grn.idat or _Red.idat) will used for read.metharray()
  #' @return TRUE or FALSE
  #' @author mchepeleva

  check_res <- list(Red = F, Grn = F, sentrix_id = F)
  files_list <- list.files(updata_folder)
  
  check_res$Red <- sum(base::grepl("_Red.idat$|_Red.idat.gz$", files_list)) > 0
  check_res$Grn <- sum(base::grepl("_Grn.idat$|_Grn.idat.gz$", files_list)) > 0
  
  sids <- unlist(lapply(files_list, function(x) gsub("_Grn.idat.*|_Red.idat.*", "", x)))
  if(sum(sids != sids[1]) == 0){
    check_res$sentrix_id <- T
    cat("Check sentrix_id: TRUE\n")
  } else{
    cat("Check sentrix_id: FALSE!", 
        "\nsentrix_ids are:", sids)
  }
 return(check_res)
}
