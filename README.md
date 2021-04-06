# Introduction 
This is a simple package that provides a color pallete in R, that is based on the Kantar Color scheme (as guessed from the Office themes).
# Getting Started
Clone and install the package as normal. 

You use it as following
```
library(ggplot2)
library(kantarPal)
ggplot(data = iris, 
        aes(x=Sepal.Length,y=Sepal.Width, color = Species)) + 
        geom_point() + 
        kantaRpal::scale_color_kantar()
```


# Contribute
This was a first attempt and a quick hack. You are invited to help refine the 
package by using Pull Requests.
