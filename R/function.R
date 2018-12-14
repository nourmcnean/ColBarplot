#'@title ColBarplot
#'
#'@description colourful barplots
#'@param data data to plot
#'@param d.x values of the data
#'@param d.y names of the barplot
#'@param lab.x label of x
#'@param lab.y label of y
#'@param d.title title of the barplot
#'@param d.source source of the data
#'@param color.pal1 coulour palette
#'@param color.pal2 coulour palette
#'@export ColBarplot
#'@return null


ColBarplot<- function(data,d.x,d.y,lab.x,lab.y,d.title,d.source,color.pal1,color.pal2)
  {library(RColorBrewer)
  library(ggplot2)
  library(ggpubr)
  cols <- colorRampPalette(brewer.pal(12, color.pal1))
  myPal <- cols(length(unique(d.x)))
  colourCount =length(unique(d.x))
  dd=ggplot(data, aes(x = reorder(d.x, -d.y),d.y,fill=d.x))+
    geom_bar(stat="identity")+
    geom_text(aes(label=d.y), hjust=1, color="black", size=3)+
    labs( x=lab.x , y=lab.y)+
    theme_minimal()+theme(legend.position="none")+
    coord_flip()+
    scale_fill_manual(values = colorRampPalette(brewer.pal(12, color.pal2))(colourCount))

   annotate_figure(dd,
                  top = text_grob(d.title, color = "black", face = "bold", size = 14),
                  bottom = text_grob(d.source, color = "black",
                                     hjust = 1, x = 1, face = "italic", size = 10))
}
