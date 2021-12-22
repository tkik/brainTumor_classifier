#please install the following packages:
# library(minfi)
# library(limma)
# library(openxlsx)
# library(randomForest)
# library(glmnet)
# library(HandTill2001)
# library(knitr)
# library(kableExtra)
# library(flextable)
# library(conumee)
# library(tidyverse)
# library(rmarkdown)


###Edit the working directory####
setwd("P:/9_brain_classifier/Prediction/")


render_report = function(path, sentrix_id, sample_identifier, gender, material_type, supplier_diagnosis) {
  rmarkdown::render(
    "complete_report_fs.Rmd", params = list(
      path=path,
      sentrix_id=sentrix_id,
      sample_identifier=sample_identifier,
      gender=gender,
      material_type=material_type,
      supplier_diagnosis=supplier_diagnosis
    ),
    output_file = paste0("report/Report-", sentrix_id, ".html")
  )
}

#### 1. add the path of the idat files
#### 2. Add the sentrix id and the sentrix position, separated with _
#### 3. fill in the clinical information, which is gender, material type, supplier diagnosis and sample identifier.
#### Importantly the gender has to be either Male of Female - it won't throw an error, but will use the predicted gender instead
#### The material type has to be either FFPE or Frozen. Anything else will result in an error.

render_report(path="P:/9_brain_classifier/data/idats_for_RT_classifier/",
              sentrix_id ="201247460038_R04C01", gender="Female", material_type = "FFPE",
              supplier_diagnosis = "GBM, RTK II", sample_identifier="Sample_1")




