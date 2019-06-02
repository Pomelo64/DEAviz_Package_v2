# path = "/Users/Shaahin/Downloads/banks_paneldata.csv"
panel_data <- read_csv("/Users/Shaahin/Downloads/banks_paneldata.csv")
panel_scaled <- scale(panel_data[,-c(1,2)])
prcomp_result <- prcomp(panel_scaled)
coordinates <- prcomp_result$x[,1:2]
#crs_efficiency <- crs_eff(panel_data[,-c(1,2)],number_of_inputs)$eff
#vrs_efficiency <- vrs_eff(panel_data[,-c(1,2)],number_of_inputs)$eff

supp <- data.frame(coordinates)



data <- panel_data[,-c(1,2)]


z1 <- data.frame(panel_data[,c(1,2)], prcomp_result$x[, 1:2] )
#z1$crs_color <- ifelse(test = supp$crs_efficiency ==1 , "Efficient", "Inefficient")
#z1$vrs_color <- ifelse(test = supp$vrs_efficiency ==1 , "Efficient", "Inefficient")


z2 <- data.frame(Variables = (rownames(prcomp_result$rotation)), prcomp_result$rotation[, 1:2])

total_min <-  min(z1$PC1,z1$PC2)
total_max <- max( z1$PC1,z1$PC2)

vector_size = 2
text_size = 3

g <-
        ggplot2::ggplot(z1, aes(x = PC1,y = PC2)) +
        ggplot2::geom_point( size=1, alpha = 0.5 ) +
        ggplot2::geom_text(aes(label = Bank)) +
        #ggplot2::scale_colour_manual(name = "DMU: CRS Eff.", values =  c("Efficient"="gold" , "Inefficient"="skyblue")) +
        ggplot2::geom_segment(data=z2, aes(PC1*vector_size, PC2*vector_size, xend=0, yend=0), col="red",alpha = 0.7 ) +
        ggplot2::geom_text(data=z2 , aes(x = PC1*vector_size,y =  PC2*vector_size, label = Variables ), col="red", alpha = 0.7, size = text_size )

g + gganimate::transition_time(Year) +  labs(title = "Year: {frame_time}")
#ggplotly(g)
