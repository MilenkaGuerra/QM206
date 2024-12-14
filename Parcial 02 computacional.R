#EJEMPLO 1 
animales<-c("Perro","Gato","Elefante","Águila","Tiburón")
tipos<-c("Mamiferos","Mamiferos","Mamiferos","Ave","Pez")
pesos<-c(15,5,5000,7,800)#en kilogramos 

#Creamos un data frame
zoologico<-data.frame(animal=animales,tipo=tipos,peso=pesos)
zoologico

head(zoologico)

#estructura del dataframe

str(zoologico)

zoologico$animal

zoologico$tipos

#resumen estadistico del dataframe 

summary(zoologico)

dim(zoologico)

#escogemos varias columnas 

zoologico[,c("animal","peso")]

zoologico[zoologico$peso>10,]

#EJEMPLO 2 Llamamos datos de internet 
animals<-read.csv("https://raw.githubusercontent.com/vincentarelbundock/Rdatasets/refs/heads/master/csv/MASS/Animals.csv")
animals

library(dplyr)

heavy_animals <- animals%>% filter(body > 50)

head(heavy_animals)

sum_body <- sum(animals$body)
sum_body

sum_brain <- sum(animals$brain)
sum_brain

animals<- animals%>% mutate(total = body + brain)

head(animals)

#EJEMPLO3 llamamos datos de un archivo en nuestra Pc

cars93<-read.csv("cars93.csv")
cars93

names(cars93)=c("Filas","Fabricante","Modelo","Tipo","Precio minimo","Precio","Precio maximo","Millas por galon en ciudad","Millas por galon en carretera","Bolsas de aire","Transmisión","Cilindros","Tamaño del motor","Caballo de fuerza","RPM","Revoluciones por milla","Man.trans.avail","Capacidad del tanque combustible","Pasajeros","Longitud","Distancia entre ejes","ancho","Gira el circulo","Espacio en el asiento trasero","Sala de equipaje","peso","origen","Marca")
names(cars93)
cars93

cars93[,c("Marca","Precio","origen")]

dim(cars93)[2]

expensive_cars93 <- cars93 %>% filter(Precio > 20)
head(expensive_cars93)

df<-cars93 %>%filter(Fabricante=="Chevrolet")
df2 <- cars93 %>% mutate(suma = Precio + `Precio maximo`)
promedio_precio <- mean(cars93$Precio, na.rm = TRUE)
promedio_precio

cars93 %>% group_by(Fabricante) %>% summarize(n=n(),promedio=mean(Precio),std=sd(Precio))

cars93 %>%select(Precio,Marca,Fabricante)

cars93 %>% arrange(Precio)


#Ejemplo 5
# Creamos nuestros vectores
estudiantes <- c("Juan", "Lucía", "Pedro", "Ana", "Sofía")
materias <- c("Matemáticas", "Física", "Química", "Historia", "Biología")
calificaciones <- c(85, 90, 78, 88, 92)

# Creamos un data frame
notas <- data.frame(Estudiante = estudiantes, Materia = materias, Calificación = calificaciones)
print(notas)

# Calcular el promedio de las calificaciones
promedio <- mean(notas$Calificación)
cat("Promedio de calificaciones:", promedio, "\n")

# Filtramos los estudiantes con calificaciones mayores a 80
mejores_estudiantes <- subset(notas, Calificación > 80)

# Mostramos los mejores estudiantes
print("Estudiantes con calificaciones mayores a 80:")
print(mejores_estudiantes)

#Ejemplo 6 
empleados <- data.frame(
  Nombre = c("Ana", "Luis", "Carlos", "Marta", "José", "Lucía", "Pedro", "Isabel", "Juan", "Clara"),
  Departamento = c("Ventas", "Ventas", "Ventas", "Finanzas", "Finanzas", "Finanzas", "TI", "TI", "Marketing", "Marketing"),
  Horas_trabajadas = c(40, 45, 38, 35, 40, 50, 42, 38, 45, 43),
  Productividad = c(120, 110, 130, 100, 115, 140, 150, 125, 105, 110)
)

# Ver los primeros datos
head(empleados)

# Creamos categorias de productividad
empleados <- empleados %>%
  mutate(
    Rango_productividad = cut(Productividad, 
                              breaks = c(0, 100, 130, Inf),
                              labels = c("Baja", "Media", "Alta"))
  )

# Ver los datos con la nueva columna
head(empleados)

#agrupamoos por departamento 
estadisticas_departamento <- empleados %>%
  group_by(Departamento) %>%
  summarize(
    Promedio_horas = mean(Horas_trabajadas, na.rm = TRUE),
    Promedio_productividad = mean(Productividad, na.rm = TRUE),
    Empleados = n()
  )

estadisticas_departamento

# Filtrar empleados con alta productividad
empleados_altos <- empleados %>%
  filter(Rango_productividad == "Alta")
head(empleados_altos)




