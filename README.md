# LightConverter
A shiny software allowing replicability between two different light source detectors. 

Light source detectors often have limited response ranges, which can also vary in quality. As an example, two detectors whose response ranges are between 400 and 700nm, with one being a bell shaped curve, and the other being mostly flat, will show different values for a similar light source. This is usually not a problem when working with solar light, but can cause issues when using artifial light. Moreover, this can lead to over or underestimation of the actual amount of light being emitted, hindering replicability of scientific results. This software aims to provide a simple mathematical tool to know which value should be shown on a detector, to match the same amount of light shown by a different detector. For this, you will need both detectors response spectra, as well as the light source emission spectrum.

# Requirements
You need to have R and RStudio with the lastest available version

# Installation
Download the zip file and extract the content in a convenient file
Open the .RProj file using RStudio
Install all the following packages before first use. You can copy-paste the following lines in your RStudio console

```
install.packages(ggplot2)
install.packages(shiny)
install.packages(dplyr)
```

# Usage
The files you use must be in .csv and made of 2 columns, the first one indicating the wavelengths and the second one indicating the response. For the detectors, please have the wavelength as round numbers. If the manufacturer of the detector does not provide the full response data, you can use [WebPlotDigitizer](https://automeris.io/WebPlotDigitizer.html) to trace the data from a plot. For the emission spectrum, it doesn't matter if the wavelength values are round or not.

Load your datasets using the different buttons, the plot will show up as you load the data. If your light emission spectra is in power units (W/m²/nm), a checkbox is available to convert it to µmol of photons/m²/s, a common unit of light measurement for photosynthetic organisms. 

A "unit response" dataset is provided as an exemple, covering 100-1000nm range with a response of 1. 
