# Ruta del archivo original
input_file <- "C:/Users/julyh/OneDrive/Escritorio/competencia_02.csv"

# Ruta del archivo comprimido
output_file <- "C:/Users/julyh/OneDrive/Escritorio/competencia_02.csv.gz"

# Comprimir el archivo
writeLines(readLines(input_file), gzfile(output_file))

cat("Archivo comprimido como:", output_file, "\n")
