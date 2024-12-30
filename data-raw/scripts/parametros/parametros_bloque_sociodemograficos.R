


# Nivel de educacion
p_educacion_jefe_hogar_tit <-
  diccionario |>
  filter(llave == 'educacion_jefe_hogar') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_educacion_jefe_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(educacion_jefe_hogar) |>
  asignar_colores()


orden_educacion_jefe_hogar <-
  c("Educación básica incompleta o inferior / Sin estudios",
    "Básica completa",
    "Media incompleta",
    "Media completa o técnica incompleta",
    "Técnica completa / Universitaria incompleta",
    "Universitaria completa",
    "Post Grado (Magister, Doctor o Equivalente)")



# ocupacion del jefe
p_ocupacion_jefe_hogar_tit <-
  diccionario |>
  filter(llave == 'ocupacion_jefe_hogar') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_ocupacion_jefe_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(ocupacion_jefe_hogar) |>
  asignar_colores()

orden_ocupacion_jefe_hogar<- rev(c("Trabajos menores ocasionales e informales (lavado, aseo, servicio doméstico ocasional, “pololos”, cuidador de autos, limosna)",
                         "Oficio menor, obrero no calificado, jornalero, servicio doméstico con contrato",
                         "Obrero calificado, capataz, junior, micro empresario (kiosko, taxi, comercio menor, ambulante)",
                         "Empleado administrativo medio y bajo, vendedor, secretaria, jefe de sección. Técnico especializado. Profesional independiente de carreras técnicas (contador, analista de sistemas, diseñador, músico) Profesor primario o secundario",
                         "Ejecutivo medio, ejecutivo medio-alto, gerentes, subgerentes, gerente general de empresa media o pequeña. Profesional independiente de carreras tradicionales (abogado, médico, arquitecto, ingeniero, agrónomo)",
                         "Alto ejecutivo (gerente general) de empresa grande. Directores de grandes empresas. Empresarios propietarios de empresas medianas y grandes. Profesionales independientes de gran prestigio."))



# Numero de miembros en la familia
p_personas_viven_hogar_tit <-
  diccionario |>
  filter(llave == 'personas_viven_hogar') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_personas_viven_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(personas_viven_hogar) |>
  asignar_colores()


orden_personas_viven_hogar <-
  rev(c("1","2","3","4","5","6","7","Más de 7"))

# Ingreso hogar
p_ingreso_mensual_hogar_tit <-
  diccionario |>
  filter(llave == 'ingreso_mensual_hogar') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_ingreso_mensual_hogar <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(ingreso_mensual_hogar) |>
  asignar_colores()


orden_ingreso_mensual_hogar <-
  rev(c("$0 a $328 mil",
    "$329 mil a $574 mil",
    "$575 mil a $1,0 millones",
    "$1,1 millones a $1,7 millones",
    "$1,8 millones a $3,0 millones",
    "$3,1 millones a $5,3 millones",
    "$5,4 millones o más",
    "No responde"))


# Vive en la comuna

p_vivienda_comuna_tit <-
  diccionario |>
  filter(llave == 'vivienda_comuna') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

colores_vivienda_comuna <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(vivienda_comuna) |>
  asignar_colores()

orden_vivienda_comuna <-
  rev(c("En esta vivienda",  "En otra vivienda","No"))

# Asistencia a la escuela
p_asiste_educacion_tit <-
  diccionario |>
  filter(llave == 'asiste_educacion') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_asiste_educacion <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(asiste_educacion) |>
  asignar_colores()


orden_asiste_educacion <-
  c("Sí", "No asiste actualmente","Nunca asistió")


# grado Curso aprobado

p_grado_curso_aprobado_tit <-
  diccionario |>
  filter(llave == 'grado_curso_aprobado') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_grado_curso_aprobado <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(grado_curso_aprobado) |>
  asignar_colores()


orden_grado_curso_aprobado <-
  rev(c("0",  "1°",  "2°",  "3°",  "4°",  "5°",  "6°", "7°", "8°"))


# Curso aprobado
p_curso_aprobado_tit <-
  diccionario |>
  filter(llave == 'curso_aprobado') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_curso_aprobado <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(curso_aprobado) |>
  asignar_colores()


orden_curso_aprobado <-
  rev(c("Preescolar",
    "Especial o Diferencial" ,"Básica o Primaria",
    "Media o Secundaria",
    "Superior"))


#


#


# origen indigena
p_perteneciente_pueblo_indigena_tit <-
  diccionario |>
  filter(llave == 'perteneciente_pueblo_indigena') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)

# Trabaja actualmente
p_semana_pasada_trabajo_tit <-
  diccionario |>
  filter(llave == 'semana_pasada_trabajo') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 75)


# Razon no trabajo
p_razon_no_trabajo_tit <-
  diccionario |>
  filter(llave == 'razon_no_trabajo') |>
  pull(pregunta) |>
  stringr::str_wrap(width = 55)

colores_razon_no_trabajo <-
  bd_respuestas_efectivas |>
  as_tibble() |>
  distinct(razon_no_trabajo) |>
  asignar_colores()






