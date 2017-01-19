import re
import subprocess
import os
import shutil

INFILE = 'netinf_vignette.Rmd'
OUTFILE = 'readme.Rmd'

readme_header = '---\ntitle: "NetworkInference"\nauthor: "Fridolin Linder"\ndate: "`r Sys.Date()`"\noutput: rmarkdown::github_document\nbibliography: bibliography.bib\n---'

# Read vignette
with open(INFILE, 'r') as infile, open(OUTFILE, 'w') as outfile:
    text = infile.read()
    text = re.sub('---(.|\n)+---', readme_header, text)
    outfile.write(text)

process = subprocess.Popen(["/usr/bin/Rscript render_readme.R"], shell=True)
process.wait()

#if not os.path.exists("../readme_files"):
#    os.makedirs("../readme_files")
#    os.makedirs("../readme_files")
#
shutil.rmtree('../readme_files')
shutil.move('readme_files', '../readme_files')
os.rename("readme.md", "../README.md")
os.remove("readme.html")