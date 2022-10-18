###################################################
# Header
#
# title: Render Code Group 2C.R
# authors: Suyog Dharmadhikari & Magaritte Nguyen
# date: 2022-10-05
# purpose: Code to create the README.md file should be included in your repo in a 
#          separate R script (.R file).
#
###################################################

#Code to create the README.md file should be included in your repo in a separate R script (.R file).
rmarkdown::render("Project_2_C.Rmd", 
                  output_format = "github_document", 
                  output_file = "README.md",
                  output_options = list(
                    toc = TRUE, 
                    toc_depth = 2,
                    number_sections = FALSE,
                    code_folding = "show",
                    theme = "readable",
                    df_print = "default"
                  )
)



