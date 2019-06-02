# pca biplot with ggfortify

library(ggfortify)

panel_data <- read_csv("/Users/Shaahin/Downloads/banks_paneldata.csv")
panel_scaled <- scale(panel_data[,-c(1,2)])
prcomp_result <- prcomp(panel_scaled)

number_of_inputs = 2



unique_periods = unique(panel_data$Year)

crs_efficiency <- c()
vrs_efficiency <- c()

for (period in unique_periods){
        #dataset <- panel_data %>% filter(Year = period) %>% select(-c("Year","Bank"))
        dataset <- panel_data[panel_data$Year == period , -c(1,2)]
        dataset <- data.frame(dataset)
        t_crs_efficiency <- crs_eff(dataset,number_of_inputs)$eff
        t_vrs_efficiency <- vrs_eff(dataset,number_of_inputs)$eff
        crs_efficiency <- c(crs_efficiency,t_crs_efficiency)
        vrs_efficiency <- c(vrs_efficiency,t_vrs_efficiency)
}

panel_data$crs_efficiency <- trunc(round(crs_efficiency,2)*100)
panel_data$vrs_efficiency <- trunc(round(vrs_efficiency,2)*100)



gg <- ggplot2::autoplot(prcomp_result, data = panel_data,
               loadings = TRUE, loadings.colour = 'blue',
               loadings.label = TRUE, loadings.label.size = 4, loadings.label.colour = "green")

gg +  ggplot2::geom_text(aes(x = PC1 , y = PC2 , label = Bank, color = crs_efficiency, size = crs_efficiency)) +
 ggplot2::geom_text(aes(x = PC1 , y = PC2 , label = crs_efficiency, color = crs_efficiency, size = crs_efficiency, hjust = 0 , vjust = -0.5)) +
        ggplot2::scale_color_gradient(low = "blue", high = "red") +
        #ggplot2::geom_point(aes(x = PC1 , y = PC2 , color = crs_efficiency), size = 3) +
        gganimate::transition_time(Year) +  labs(title = "Year: {frame_time}")
