distritais <- dados %>%
  filter(tipo_vara=='civel', distrital) %>%
  group_by(data, municipio) %>%
  summarise(dist=sum(p_civel_4_1_1 + p_civel_4_1_2 + p_civel_4_1_3)) %>%
  ungroup %>%
  group_by(municipio) %>%
  summarise(dist_m=mean(dist, na.rm=T)) %>%
  arrange(desc(dist_m))

iniciais <- dados %>%
  filter(entrancia=='INICIA', tipo_vara=='civel') %>%
  group_by(data, comarca) %>%
  summarise(dist=sum(p_civel_4_1_1 + p_civel_4_1_2 + p_civel_4_1_3)) %>%
  ungroup %>%
  group_by(comarca) %>%
  summarise(dist_m=mean(dist, na.rm=T)) %>%
  arrange(dist_m)

mean(iniciais$dist_m)
median(iniciais$dist_m)

qplot(iniciais$dist_m)
qplot(distritais$dist_m)
