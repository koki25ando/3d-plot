library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
library(data.table)

#(https://www.basketball-reference.com/leaders/)
ss <- fread("https://s3-ap-southeast-2.amazonaws.com/playerinfomation/Seasons_Stats.csv", data.table = FALSE)
player <- fread("https://s3-ap-southeast-2.amazonaws.com/playerinfomation/Players+(2).csv", data.table = FALSE)
sp <- merge(ss, player, by.x = "Player", by.y = "Player")
names(sp)[44] <- paste("FTP")

sp1 <- sp %>% 
  group_by(Player) %>% 
  mutate(rook_year = min(Year), last_year = max(Year)) %>% 
  mutate(FTP1 = mean(FTP)) %>% 
  distinct(Player, .keep_all = TRUE) %>% 
  filter(FTP1 < .99 & FTP1 > 0) %>% 
  filter(Pos %in% c("C","PF","PG","SF", "SG"))


function(input, output){
  output$scatter = renderPlotly({
    sp1 %>% 
      plot_ly(x = ~height, y = ~weight, z = ~FTP1, color = ~Pos,
              alpha = .9,
              text = ~paste("Player:", Player),
              zaxis = TRUE,
              marker = list(size = 6)) %>% 
      layout(scene = list(xaxis = list(title = 'Height'),
                          yaxis = list(title = 'Weight'),
                          zaxis = list(title = 'FT%')))
  })
}