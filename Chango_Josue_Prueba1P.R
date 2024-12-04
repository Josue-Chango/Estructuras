###########################################
# RESOLUCION PRUEBA PRIMER PARCIAL - ESTADISTICA A0501
# NOMBRE: JOSUE ANDEL CHANGO PARRA
# NRC:1271
# FECHA:27/11/2024
###########################################
# NOTA: # La prueba dura 1 hora (60 minutos) y consta de 10 preguntas aleatorias
# Las preguntas de la prueba se encuentran en  aula virtual.
# Primero, descargue el archivo "Envios.xlsx" y carguelo en R Studio
# El archivo contiene informaci?n de envios realizados por una empresa de log?stica:
# TYPE: Tipo de pago del envio: Debit, Tranfer, Cash, Payment
# REAL: Tiempo real de entrega del env?o
# SCHEDULED: Tiempo planificado de entrega del env?o
# STATUS: Estado del env?o: Advance, Late, on time, cancelled
# Debe resolver en este archivo script y registrar sus respuestas en dicha prueba.
# Al finalizar su prueba, debe guardar el archivo script con sus datos, de la
# siguiente manera: "Apellido_Nombre_Prueba1P.R"
# Luego debe subir este archivo al aula virtual, en un plazo maximo de 10 minutos
# de finalizada la prueba. Si no envia el archivo script, su nota sera de CERO.
# No es necesario copiar los enunciados de los ejercicios.
# Debe interpretar todos los resultados y respuestas usando comentarios.
# Trate en lo posible de NO REDONDEAR los calculos

###########################################
# RESOLUCION PREGUNTA 1

Probabilidad_tarde_transferencia = sum(Envios$Status == "Late delivery" & Envios$Type == "TRANSFER") / sum(Envios$Type == "TRANSFER")
Probabilidad_tarde_transferencia

###########################################
# RESOLUCION PREGUNTA 2

P_a_tiempo = sum(Envios$Status == "Shipping on time") / nrow(Envios)
P_pago_efectivo = sum(Envios$Type == "CASH") / nrow(Envios)
P_a_tiempo_y_pago_efectivo = sum(Envios$Status == "Shipping on time" & Envios$Type == "CASH") / nrow(Envios)
P_a_tiempo_o_pago_efectivo = P_a_tiempo + P_pago_efectivo - P_a_tiempo_y_pago_efectivo
P_a_tiempo_o_pago_efectivo

###########################################
# RESOLUCION PREGUNTA 3

clase_modal = as.numeric(names(which.max(table(Envios$Scheduled))))
clase_modal

###########################################
# RESOLUCION PREGUNTA 4

envios_inferiores_2_dias = sum(Envios$Scheduled < 2)
total_envios = nrow(Envios)
porcentaje_inferiores_2_dias = (envios_inferiores_2_dias / total_envios) * 100
porcentaje_inferiores_2_dias



###########################################
# RESOLUCION PREGUNTA 5


###########################################
# RESOLUCION PREGUNTA 6

P_a_tiempo = sum(Envios$Status == "Shipping on time") / nrow(Envios)
P_a_adelantado = sum(Envios$Status == "Advance shipping") / nrow(Envios)
P_a_tiempo_y_adelantado = sum(Envios$Status == "Shipping on time" & Envios$Status == "Advance shipping") / nrow(Envios)
P_a_tiempo_o_adelantado = P_a_tiempo + P_a_adelantado - P_a_tiempo_y_adelantado
P_puntual_o_adelantado_y_efectivo = sum(Envios$Status "Shipping on time" & Envios$Status == "Advance shipping" & Envios$Type == "CASH") / nrow(Envios)
//P_puntual_o_adelantado_y_efectivo = sum(Envios$Status "Shipping on time" & Envios$Status == "Advance shipping" & Envios$Type == "CASH") / nrow(Envios)


###########################################
# RESOLUCION PREGUNTA 7

###########################################
# RESOLUCION PREGUNTA 8

###########################################
# RESOLUCION PREGUNTA 9

###########################################
# RESOLUCION PREGUNTA 10

