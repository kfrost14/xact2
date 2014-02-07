xact2
=====

xact2 is a Shiny web application that displays hourly data from the Xact monitor using plots from the openair package.

It requires the installation of rCharts from GitHub using the devtools package

  require(devtools)
  install_github('rCharts', 'ramnathv')

The app can be run from your desktop using the runGitHub() function from the Shiny package

  require(shiny)
  runGitHub('xact2', 'kfrost14')
