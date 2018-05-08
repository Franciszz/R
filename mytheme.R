# -*- coding: utf-8 -*-
library(ggplot2)
library(ggthemes)
library(magrittr)
windowsFonts(font_title=windowsFont('carbon'),
             font_text=windowsFont('Times New Roman'),
             font_anno=windowsFont('微软雅黑'))
theme_os0 <- function(base_size=12,base_family='font_title',
                     base_color='white',
                     panel_color='transparent'){
  theme_so = theme_foundation(base_size=base_size,base_family=base_family)+
    theme(line=element_line(colour='black',linetype=1),
          rect=element_rect(fill=base_color,linetype=1,colour=NA),
          text=element_text(family=base_family,face='plain'),
          aspect.ratio = 2/3,
          #图表元素
          plot.background = element_rect(fill=base_color),
          plot.title = element_text(size=base_size,hjust=0.5,vjust=1,margin=margin(b=1*base_size)),
          plot.subtitle = element_text(size=0.9*base_size,hjust=0,vjust=1,margin=margin(b=0.8*base_size)),
          plot.caption = element_text(size=0.75*base_size,hjust=0,vjust=1,margin=margin(t=0.5*base_size)),
          plot.margin = margin(c(1,1.2,1,1.2)*base_size,unit='pt'),
          #面板元素
          panel.background = element_rect(fill=panel_color,colour=NA,linetype=1),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = margin(0.25*base_size,unit='pt'),
          #坐标轴
          axis.line = element_line(size=rel(0.75)*base_size*0.1,linetype=1,color='black'),
          axis.title = element_text(size=0.82*base_size,family=base_family,
                                    margin=margin(t=0.5*base_size,b=0.5*base_size)),
          axis.text = element_text(size=0.75*base_size,family='font_text'),
          axis.ticks.length = unit(0.25*base_size,'pt'),
          axis.line.x = element_line(size=rel(0.08)*base_size),
          axis.line.y = element_line(size=rel(0.08)*base_size),
          axis.title.x = element_text(margin=margin(t=0.375*base_size,unit='pt')),
          axis.title.y = element_text(margin=margin(r=0.375*base_size,unit='pt'),angle=90),
          axis.text.x = element_text(vjust=0,margin=margin(t=0.25*base_size,unit='pt')),
          axis.text.y = element_text(hjust=0,margin=margin(r=0.25*base_size,unit='pt')),
          #图例
          legend.background = element_rect(linetype = 0,fill=base_color),
          legend.spacing = unit(0.6*base_size,'pt'),
          legend.margin = margin(c(1,1.2,1,1.2)*0.125*base_size,unit='pt'),
          legend.key = element_rect(fill=panel_color,linetype = 0),
          legend.key.height = unit(rel(0.8)*base_size*0.08,'lines'),
          legend.key.width = unit(rel(0.8)*base_size*0.12,'lines'),
          legend.title = element_text(size=0.75*base_size,family=base_family,vjust=0),
          legend.title.align = 1,
          legend.text = element_text(size=0.75*base_size,family='font_text'),
          legend.text.align = 0,
          legend.position = 'top',legend.direction = 'horizontal',
          legend.justification = 'center',
          legend.box = 'vertical',
          #分面
          strip.background = element_rect(fill=panel_color,linetype=0),
          strip.text= element_text(size=0.8*base_size,face='bold',family=base_family),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),
          complete = TRUE
          )
    }
theme_os <- function(base_size=12,base_family='font_anno',
                     base_color='white',inblank=TRUE,
                     panel_color='transparent'){
  theme_so = theme_foundation(base_size=base_size,base_family=base_family)+
    theme_gray() %+replace%
    theme(line=element_line(colour='black',size=rel(0.08)*base_size,linetype=1,lineend="butt"),
          rect=element_rect(fill=base_color,size=NA,linetype=1,colour=NA),
          text=element_text(family=base_family,face='plain',colour='black'),
          aspect.ratio = 2/3,
          #图表元素
          plot.background = element_rect(fill=base_color,colour=NA), 
          plot.title = element_text(size=base_size,hjust=0.5,vjust=1,margin=margin(b=1*base_size)),
          plot.subtitle = element_text(size=0.9*base_size,hjust=0,vjust=1,margin=margin(b=0.8*base_size)),
          plot.caption = element_text(size=0.75*base_size,hjust=0,vjust=1,margin=margin(t=0.5*base_size)),
          plot.margin = margin(c(1,1.2,1,1.2)*base_size,unit='pt'),
          #面板元素
          panel.background = element_rect(fill=panel_color,colour=NA,linetype=1),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = margin(0.25*base_size,unit='pt'),
          #坐标轴
          axis.line = element_line(linetype=1,color='black'),
          axis.title = element_text(size=0.82*base_size,family=base_family,
                                    margin=margin(t=0.5*base_size,b=0.5*base_size)),
          axis.text = element_text(size=0.75*base_size,family='font_text'), 
          axis.text.x.top = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks = element_line(colour='black',size=rel(0.05)*base_size*0.2),
          axis.ticks.length = unit(0.25*base_size,'pt'),
          #图例
          legend.background = element_rect(linetype = 0,fill=base_color),
          legend.spacing = unit(0.6*base_size,'pt'),
          legend.box.spacing = unit(0.6*base_size,'pt'),
          legend.margin = margin(c(1,1.2,1,1.2)*0.125*base_size,unit='pt'),#the margin around each legend (margin)
          legend.box.margin = margin(c(1,1.2,1,1.2)*0.125*base_size,unit='pt'),#the margin around each legend (margin)
          legend.key = element_rect(fill=panel_color,linetype = 0),
          legend.key.size = unit(rel(0.8)*base_size*0.08,'lines'),
          legend.key.height = unit(rel(0.8)*base_size*0.08,'lines'),
          legend.key.width = unit(rel(0.8)*base_size*0.12,'lines'),
          legend.title = element_text(size=0.75*base_size,family=base_family,vjust=0),
          legend.title.align = 1,
          legend.text = element_text(size=0.75*base_size,family='font_text'),
          legend.text.align = 0,
          legend.justification = 'center',
          legend.position = 'top',
          legend.direction = 'horizontal', 
          legend.box = 'vertical',
          #分面
          strip.background = element_rect(fill=panel_color,linetype=0,colour=NA), 
          strip.text= element_text(size=0.8*base_size,face='bold',family=base_family),
          strip.placement = 'inside',
          strip.switch.pad.grid = unit(0.01*base_size,'cm'),
          strip.switch.pad.wrap = unit(0.01*base_size,'cm'),
          strip.placement.x = NULL, strip.placement.y = NULL,
          strip.text.x = element_text(), 
          strip.text.y = element_text(angle = -90),
          complete = FALSE
    )
  if(inblank){
    theme_so <- theme_so+theme(
      axis.line.x = element_line(size=rel(0.08)*base_size*0.2),
      axis.line.y = element_line(size=rel(0.08)*base_size*0.2),
      axis.title.x = element_text(margin=margin(t=0.375*base_size,unit='pt')), 
      axis.title.y = element_text(margin=margin(r=0.375*base_size,unit='pt'),angle=90),
      axis.text.x = element_text(vjust=0,margin=margin(t=0.25*base_size,unit='pt')), 
      axis.text.y = element_text(hjust=0,margin=margin(r=0.25*base_size,unit='pt'))
    )
  }
  else{
    theme_so = theme_so+theme(
      axis.line.x = element_blank(),axis.line.y = element_blank(),
      axis.title.x = element_blank(),axis.title.y = element_blank(),
      axis.text.x = element_blank(),axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  }
  theme_so
}
theme_osv <- theme_os()%+replace%
  theme(legend.position = 'right',
        legend.direction = 'vertical',
        legend.box = 'horizaontal')
