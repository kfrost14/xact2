setwd("R:/Development/Kali/metalsapp")
library(shiny)

shell("start R:\\Programs\\Firefox\\firefox.exe http://localhost:8109/")
runApp(launch.browser = F, port=8109)
