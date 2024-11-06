# LIMPIEZA DE DATOS
datos <- readRDS("ventas_tienda_202510.rds")

# Mostrar resumen y contar valores faltantes
summary(datos)
sapply(datos, function(x) sum(is.na(x)))

# Eliminar filas con valores NA
datos <- na.omit(datos)

# Visualización de valores atípicos con boxplots
boxplot(datos$Precio, main = "Boxplot de Precio", ylab = "Precio", col = "lightblue")
boxplot(datos$Cantidad, main = "Boxplot de Cantidad", ylab = "Cantidad", col = "lightgreen")

# Función para eliminar outliers usando IQR
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  x[x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value)]
}

# Filtrar outliers en Precio y Cantidad
datos <- datos[datos$Precio %in% remove_outliers(datos$Precio) &
                 datos$Cantidad %in% remove_outliers(datos$Cantidad), ]

# Filtrar edades coherentes
datos <- datos[datos$Edad >= 18 & datos$Edad <= 100, ]

# Convertir FechaCompra y MétodoPago a formatos adecuados
datos$FechaCompra <- as.Date(datos$FechaCompra, format = "%Y-%m-%d")
datos$MétodoPago <- as.factor(datos$MétodoPago)

# Crear la columna TotalVenta
datos$TotalVenta <- datos$Cantidad * datos$Precio - datos$Descuento

# Ver resumen del dataset limpio
summary(datos)

# Guardar el dataset limpio en un archivo CSV con UTF-8
write.csv(datos, file = "ventas_tienda_limpio.csv", row.names = FALSE, fileEncoding = "UTF-8")
