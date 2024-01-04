peri_theme <- theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    axis.title.x = element_text(size = 12),
                    axis.text.x = element_text(size = 10, colour = "black"), 
                    axis.title.y = element_text(size = 12), 
                    axis.text.y = element_text(size = 10, colour = "black"), 
                    legend.text = element_text(size = 10), 
                    legend.title = element_text(size = 12), 
                    axis.line.y = element_line(colour = "black"), 
                    axis.line.x = element_line(colour = "black"),
                    plot.title = element_text(hjust = 0.5, size=12), 
                    plot.subtitle = element_text(hjust = 0.5, size=12),
                    legend.key=element_blank())

dodge=position_dodge(0.8)

peri_figure <- theme(panel.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank(), 
                     axis.title.x = element_text(size = 6), 
                     axis.text.x = element_text(size =5, colour = "black"), 
                     axis.title.y = element_text(size = 6),
                     axis.text.y = element_text(size = 5, colour = "black"),
                     axis.line.y = element_line(colour = "black", linewidth = 0.4), 
                     axis.line.x = element_line(colour = "black", linewidth = 0.4),
                     axis.ticks = element_line(colour = "black", linewidth = 0.3),
                     plot.title = element_text(hjust = 0.5, size=6),
                     plot.subtitle = element_text(hjust = 0.5, size=5),
                     legend.text = element_text(size = 5), legend.title = element_text(size =5),
                     legend.key=element_blank(),
                     plot.margin = margin(t = 0,  # Top margin
                                          r = 0,  # Right margin
                                          b = 0,  # Bottom margin
                                          l = 0))
peri_figure_supp <- theme(panel.background = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     plot.background = element_blank(), 
                     axis.title.x = element_text(size = 7), 
                     axis.text.x = element_text(size =6, colour = "black"), 
                     axis.title.y = element_text(size = 7),
                     axis.text.y = element_text(size = 6, colour = "black"),
                     axis.line.y = element_line(colour = "black", linewidth = 0.4), 
                     axis.line.x = element_line(colour = "black", linewidth = 0.4),
                     axis.ticks = element_line(colour = "black", linewidth = 0.3),
                     plot.title = element_text(hjust = 0.5, size=7),
                     plot.subtitle = element_text(hjust = 0.5, size=5),
                     legend.text = element_text(size = 5), legend.title = element_text(size =5),
                     legend.key=element_blank(),
                     plot.margin = margin(t = 1,  # Top margin
                                          r = 1,  # Right margin
                                          b = 1,  # Bottom margin
                                          l = 1))

peri_geneplots <- theme(panel.background = element_blank(), 
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        plot.background = element_blank(), 
                        axis.title.x = element_text(size = 7), 
                        axis.text.x = element_text(size =6, colour = "black"), 
                        axis.title.y = element_text(size = 7),
                        axis.text.y = element_text(size = 6, colour = "black"),
                        axis.line.y = element_line(colour = "black", linewidth = 0.4), 
                        axis.line.x = element_line(colour = "black", linewidth = 0.4),
                        axis.ticks = element_line(colour = "black", linewidth = 0.3),
                        plot.title = element_text(hjust = 0.5, size=7),
                        plot.subtitle = element_text(hjust = 0.5, size=5),
                        legend.text = element_text(size = 5), legend.title = element_text(size =5),
                        legend.key=element_blank(),
                        plot.margin = margin(t = 1,  # Top margin
                                             r = 6,  # Right margin
                                             b = 1,  # Bottom margin
                                             l = 1))
                                          


theme.size=5
geom.text.size=(5/14)*theme.size
status_cols<- c("#86AD44","#E54849","#414042")
status_cols2<- c("#E54849","#414042")
dj_col<- c("#00A08A", "#F2AD00")
dj_col2<- c("#00A08A", "#005448")

tissue_cols<- list(GON=c("#2A9D8F","#69CCBE","#A1E5D9"),PIT=c("#2A9D8F","#69CCBE","#A1E5D9"),VMH=c("#E76F51", "#EF8E7B","#F4ABA2"),AH=c("#E76F51", "#EF8E7B","#F4ABA2"),PVN=c("#E76F51", "#EF8E7B","#F4ABA2"), POM=c("#E76F51", "#EF8E7B","#F4ABA2"), ICO=c("#E76F51", "#EF8E7B","#F4ABA2"),GCT=c("#E76F51", "#EF8E7B","#F4ABA2"), TNA=c("#E76F51", "#EF8E7B","#F4ABA2"), AI=c("#E9C46A","#EFD497","#F7E3BA"), LS=c("#F4A261","#F4B184","#F4C6A6"), BSTm=c("#F4A261","#F4B184","#F4C6A6"))