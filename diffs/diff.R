library(latexdiffr)
Sys.setenv("LATEXDIFF_PATH" = "c:/Users/michael.schramm/AppData/Roaming/TinyTeX/texmf-dist/scripts/latexdiff/latexdiff.pl")
tinytex::tlmgr_path("add")
latexdiff("frontiers_submission/frontiers_submission.tex", 
          "frontiers_submission/frontiers_submission_revision_01.tex",
          output = "frontiers_submission/diff",
          quiet = FALSE,
          compile = FALSE)

# 
# tinytex::latexmk("diff.tex",
#                  engine = "xelatex",
#                  bib_engine = "biber",
#                  pdf_file = "frontiers_submission/diff.pdf")

system2(command = "latexmk",
        args = c("-pdf", "diff.tex", "-f"))
