library(imager)
library(tidyverse)

setwd("/Users/nelvis/Documents/R/idee")

set_dim <- function(x = NULL, y = NULL, source) {
  
  if (is.null(x)) {
    x <- width(source)/height(source)*y
  } else {
    y <- height(source)/width(source)*x
  }
  
  print(
    paste0("Total number of tiles needed: ",
           round(x*y))
  )
  
  return(c(round(x),
           round(y))
  )
}

closest <- function(c1, c2, c3, dataframe) {
  
  distances <- mutate(dataframe,
                      c.1 = c.1 - c1,
                      c.2 = c.2 - c2,
                      c.3 = c.3 - c3,
                      d = sqrt(c.1^2+c.2^2+c.3^2)
                      )
  
  dataframe[which.min(distances$d),]
}

images <- map(0:22,
                  ~ if (.x != 15) {
                    load.image(
                      paste0("7052-",
                             formatC(100 + 5*.x,
                                     width = 3,
                                     flag = 0),
                             "_1.jpg") 
                    ) %>% 
                      as.data.frame(wide = "c")
                  }
) %>% 
  compact() %>% 
  setNames(LETTERS[1:length(.)])

images_lab <- map(0:22,
                  ~ if (.x != 15) {
                    load.image(
                      paste0("7052-",
                             formatC(100 + 5*.x,
                                     width = 3,
                                     flag = 0),
                             "_1.jpg") 
                    ) %>%
                      sRGBtoLab() %>% 
                      as.data.frame(wide = "c")
                  }
) %>% 
  compact() %>% 
  setNames(LETTERS[1:length(.)])

meancolours <- imap(images,
                        ~ {
                          df <- as.data.frame(.x, wide = "c") %>% 
                            filter(!(c.1 == 1 & c.2 == 1 & c.3 == 1))
                          c1 <- mean(df$c.1)
                          c2 <- mean(df$c.2)
                          c3 <- mean(df$c.3)
                          
                          return(c(c1, c2, c3, .y))
                        }) %>% 
  {Reduce(rbind, .)} %>% 
  as.data.frame() %>% 
  setNames(c("c.1", "c.2", "c.3", "ID"))

images_letters <- pmap(meancolours,
      ~ {
        imfill(10,10, val = as.numeric(c(..1, ..2, ..3))) %>%
          draw_text(4,0,..4,col= 1 - as.numeric(c(..1, ..2, ..3)), fsize = 10)
        }) %>%
  setNames(LETTERS[1:length(.)])

# take mean over whole image

meancolours_lab <- imap(images_lab,
                        ~ {
                          df <- as.data.frame(.x, wide = "c") %>% 
                            filter(!(c.1 == 100 & c.2 == 0 & c.3 == 0))
                          c1 <- mean(df$c.1)
                          c2 <- mean(df$c.2)
                          c3 <- mean(df$c.3)
                          
                          return(c(c1, c2, c3))
                        }) %>% 
  {Reduce(rbind, .)} %>% 
  as.data.frame() %>% 
  setNames(c("c.1", "c.2", "c.3"))

ID_lab <- meancolours_lab %>% 
  mutate(ID = LETTERS[1:nrow(.)])

# load source
source <- "bom-omb1.png" %>% 
  load.image()

source_lab <- source %>% 
  sRGBtoLab()

# reduce dimension
y_tiles <- 50
(tiles <- set_dim(y = y_tiles,
                  source = source_lab))

output <- resize(source_lab,
                 round(tiles[[1]]),
                 round(tiles[[2]]),
                 interpolation_type = 1)
plot(output)

# elaboration

dataframe <- as.data.frame(output, wide = "c")

colours <- dataframe %>% 
  select(c("c.1", "c.2", "c.3")) %>% 
  pmap_dfr(~ closest(c1 = ..1,
                     c2 = ..2,
                     c3 = ..3,
                     meancolours_lab)
  )

dataframe %<>% mutate(
  c.1 = colours$c.1,
  c.2 = colours$c.2,
  c.3 = colours$c.3
  ) %>% 
  inner_join(ID_lab,
             by = c("c.1", "c.2", "c.3")
  )

(count <- dataframe %>% 
  group_by(ID) %>% 
  dplyr::summarise(n = n()))

mosaic_lab <- dataframe %>% 
  pivot_longer(c(c.1, c.2, c.3), names_to = "cc", values_to = "value") %>% 
  mutate(cc = ifelse(cc == "c.1", 
                     1,
                     ifelse(cc == "c.2",
                            2,
                            3))
  ) %>% 
  as.cimg(dims = dim(output)) %T>%
  plot()

mosaic <- mosaic_lab %>% 
  LabtosRGB()

layout(t(1:2))
plot(source)
plot(mosaic)
# every package contains ca. 300 stones, costs 5.5€


# Letters

layout(t(1:1))
map(unique(dataframe$x), 
    ~ {
      images_x <- images_letters[filter(dataframe, x == .x)$ID]
      imappend(images_x, axis = "y")
    }) %>% 
  as.list() %>%
  imappend(axis = "x") %>% 
  plot()
