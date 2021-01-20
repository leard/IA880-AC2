################################################################################
#Bibliotecas
################################################################################
library(barcode)
library(stringr)

################################################################################
#Funções Auxiliares

getSpikes <- function(spikes, index_min=1000.0, index_max=4000.0, n=20) {
  
  spikes_temp = spikes[(spikes$V1 >= index_min) & (spikes$V1 <= index_max), ]
  index_spikes = unique(spikes_temp$V2)
  
  if(n > 2){
    #Amostragem
    index_samples = sample(index_spikes[c(-1, -length(index_spikes))], n-2)
    #Ordenação com inclusão do primeiro e ultimo motoneuronio
    index_samples = sort(c(index_samples, 
                           index_spikes[c(1, length(index_spikes))]))
  }else{
    index_samples = index_spikes
  }
  
  names = c()
  spikes_train = list()

  for (item in index_samples) {
    
    spike_temp = spikes_temp$V1[spikes_temp$V2 == item]
    
    names = c(names, paste(c("MN",formatC(item, 
                                          width = 3, 
                                          format = "d", 
                                          flag = "0")), collapse = "."))

    spikes_train = append(spikes_train, list(spike_temp/1000.0))
  }
  
  names(spikes_train) = names
  
  return(list("spikes_train"=spikes_train, "index_samples"=index_samples))
}

getInterSpikes <- function(spikes, index_min=1000.0, index_max=4000.0) {
  
  spikes_temp = spikes[(spikes$V1 >= index_min) & (spikes$V1 <= index_max), ]
  index_spikes = unique(spikes_temp$V2)

  names = c()
  spikeIntervals = list()
  
  for (item in index_spikes) {
    spikes_item = spikes_temp$V1[spikes_temp$V2 == item]
    
    # Existe interspike se houver pelo menos dois spikes
    if(length(spikes_item) < 2){
      spikeIntervals = append(spikeIntervals, list())
    }
    else{
      names = c(names, 
                paste(c("MN", 
                        formatC(item, 
                                width = 3, 
                                format = "d", 
                                flag = "0")), 
                      collapse = "."))
      
      spikeIntervals = append(spikeIntervals, list(diff(spikes_item, 1)))  
      #list(spikes_item[-1] - spikes_item[-length(spikes_item)]))  
    }
  }
  names(spikeIntervals) = names
  
  return(spikeIntervals)
}

sd.pop <- function(x, na.rep=NA) {
  sqrt(sum((x - mean(x, na.rm = TRUE))**2)/length(x))
}

sd.sample <- function(x) {
  if(length(x) == 1) 
    return(NA)
  return(sd(x, na.rm = TRUE))
}

cv.pop <- function(x) {
  return(sd.pop(x)/mean(x, na.rm = TRUE))
}

cv.sample <- function(x) {
  if(length(x) == 1) 
    return(NA)
  return(sd.sample(x)/mean(x, na.rm = TRUE))
}

getInterSpikesVariability <- function(spikes) {
  spikes_mean = list("Mean"=lapply(spikes, mean, na.rm = TRUE))
  spikes_sd_pop = list("Sd_Pop"=lapply(spikes, sd.pop))
  spikes_sd_sample = list("Sd_Samp"=lapply(spikes, sd.sample))
  spikes_cv_pop = list("Cv_Pop"=lapply(spikes, cv.pop))
  spikes_cv_sample = list("Cv_Samp"=lapply(spikes, cv.sample))
  
  return(c(spikes_mean, 
           spikes_sd_pop, 
           spikes_sd_sample, 
           spikes_cv_pop, 
           spikes_cv_sample))
}

print_hist <- function(spikes, 
                       x_lim=1000, 
                       y_lim=1000, 
                       bins=50, 
                       main_label="Histogram", 
                       x_label="Mean ISI (ms)", 
                       y_label="Number of Elements") {
  hist(spikes, 
       breaks = bins, 
       xlab = x_label, 
       ylab = y_label, 
       main = main_label, 
       xlim = c(0, x_lim),
       ylim = c(0, y_lim))
}

plotSpikes <- function(spikes, 
                       index_min=1000.0, 
                       index_max=4000.0, 
                       rescale = 1000.0,
                       main_label="", 
                       x_label="Tempo (s)", 
                       y_label="Unidade Motora") {
  plot(  
    spikes$V1[(spikes$V1 >= index_min) & (spikes$V1 <= index_max)]/rescale, 
    spikes$V2[(spikes$V1 >= index_min) & (spikes$V1 <= index_max)],
    type = "p",
    pch = ".",
    cex = 2.0,
    main = main_label,
    xlab = x_label,
    ylab = y_label)
}

process_emg <- function(folder_files = "./Tarefa 02/pm1",
                        pattern = "EMG",
                        m_wave_limit = 25.0) {
  
  #print(folder_files)
  emg_files <- list.files(folder_files, 
                          pattern = paste0("\\.", pattern, ".txt$"))
  
  stimulus_points = sort(as.double(str_match(emg_files, 
                                             paste0("Soleus_*(.*?).",
                                                    pattern, ".txt"))[,2]))
  
  
  m_wave_amplitude = c()
  h_wave_amplitude = c()
  m_h_waves = list()
  
  for(file_emg in emg_files){
    emg_data <- read.table(paste(folder_files, file_emg, sep="/"),)
    
    m_h_waves = append(m_h_waves, list(emg_data$V2))
    
    m_wave = emg_data[emg_data$V1 <= m_wave_limit, ]
    h_wave = emg_data[emg_data$V1 > m_wave_limit,]
    
    m_wave_point = max(m_wave$V2) + abs(min(m_wave$V2))
    h_wave_point = max(h_wave$V2) + abs(min(h_wave$V2))
    
    m_wave_amplitude = append(m_wave_amplitude, m_wave_point)
    h_wave_amplitude = append(h_wave_amplitude, h_wave_point)
  }
  
  m_h_waves = list("amplitude"=m_h_waves, "time"=emg_data$V1)
  
  recruitment_curve = list("intensity"=stimulus_points, 
                           "m_wave_amplitude"=m_wave_amplitude , 
                           "h_wave_amplitude"=h_wave_amplitude )
  
  
  return(list("m_h_waves"=m_h_waves, "recruitment_curve"=recruitment_curve))
  
  #recruitment_curve
  
}

process_emg_plot <- function(m_h_waves, 
                             recruitment_curve, 
                             relative_waves = FALSE,
                             amp_percent = 1.0,
                             x_lim_factor = 1,
                             main_label = c("a", "b", "c")){
  ################################################################################
  plot(m_h_waves$time,
       m_h_waves$amplitude[[1]], 
       main=main_label[1],
       ylab = "Amplitude (mV)",
       xlab = "Tempo (ms)",
       #col="white",
       pch=".",
       cex=0.05,
       ylim = c(min(unlist(m_h_waves$amplitude)), max(unlist(m_h_waves$amplitude))), 
       xlim=c(0, max(unlist(m_h_waves$time))/x_lim_factor),
       col = rgb(0,0,0, 1))
  
  for(i in 2:length(m_h_waves$amplitude)){
    lines(m_h_waves$time, m_h_waves$amplitude[[i]], col = rgb(0,0,0, 0.125+0.05*(i/8)) )
  }
  
  #Mmax
  m_max = max(recruitment_curve$m_wave_amplitude)
  #m_max 
  #Hmax
  h_max = max(recruitment_curve$h_wave_amplitude)
  #h_max # 4.3915
  #Hmax/Mmax
  h_m_max = h_max/m_max 
  #h_m_max # 0.9246321 | 91,41% Valor alto
  
  ################################################################################
  h_m_max_expression = c(as.expression(bquote('H'['max']/'M'['max']*' = '*.( round(h_m_max, 2)*100 )*.('%')) ))
  i_h_percent_index = 0.0
  if(relative_waves) {
    #Pegamos o indice do Reflexo H mais próximo de Mmax, ou seja, Hmax
    h_max_index = which.min(abs(recruitment_curve$h_wave_amplitude - m_max))
    
    # Com o indice de H max, buscamos o indice que equivale a ~20% de Mmax, 
    # no lado esquerdo da curva
    i_h_percent_index = 
      which.min(abs(recruitment_curve$h_wave_amplitude[1:h_max_index]
                    -m_max*amp_percent))
    
    # Encontramos então a corrente de estimulo
    i_h_percent_stimulus = recruitment_curve$intensity[i_h_percent_index]
    print(i_h_percent_stimulus)
    
    b_q_expresion = bquote('H'[''*.(amp_percent*100)]*' = '*.(round(m_max*amp_percent, 2))*.(' mV') )
    b_q_expresion_2 = bquote('H'['*'*.(amp_percent*100)]*' = '*.(round(recruitment_curve$h_wave_amplitude[i_h_percent_index], 2))*.(' mV') )
    b_q_expresion_3 = bquote('I'['*'*.(amp_percent*100)]*' = '*.(round(i_h_percent_stimulus, 2))*.(' mV') )
    
    
    percent_stimulus_expression = c(as.expression( b_q_expresion ), 
                                    as.expression( b_q_expresion_2 ),
                                    as.expression(b_q_expresion_3) )
    
    
    h_m_max_expression = append(h_m_max_expression, 
                                percent_stimulus_expression)
    
    recruitment_curve$h_wave_amplitude = recruitment_curve$h_wave_amplitude / m_max
    recruitment_curve$m_wave_amplitude = recruitment_curve$m_wave_amplitude / m_max
  }
  
  
  plot(recruitment_curve$intensity,
       recruitment_curve$m_wave_amplitude, 
       type="o", pch=1, lty=1, 
       main=main_label[2],
       xlab="Intensidade do Estímulo (mA)", 
       ylab="Amplitude (mV)")
  
  
  points(recruitment_curve$intensity, 
         recruitment_curve$h_wave_amplitude, 
         pch=16)
  lines(recruitment_curve$intensity, 
        recruitment_curve$h_wave_amplitude)
  
  
  if(relative_waves == TRUE && amp_percent > 0.0 && amp_percent < 1.0) {
    lines(recruitment_curve$intensity, 
          rep(1, length(recruitment_curve$intensity))*amp_percent, 
          col="red")
    
    lines(rep(1, length(recruitment_curve$m_wave_amplitude))*i_h_percent_stimulus, 
          recruitment_curve$m_wave_amplitude, 
          col="blue")
  }
  
  ################################################################################
  
  
  legend("right", 
         legend = c("Onda-M","Reflexo-H", h_m_max_expression ), 
         pch = c(1, 16, NA, NA, NA, NA) )
  
  return(list("m_h_waves"=m_h_waves, "recruitment_curve"=recruitment_curve, "percent_index"=i_h_percent_index))
}


process_emg_mean <- function(folder_files, 
                             folder_list, 
                             pattern, 
                             folder_output="mean") {
  
  emg_files <- list.files(paste(folder_files,folder_list[1], 
                                sep = "/"),
                          pattern = paste0("\\.", pattern, ".txt$"))
  
  folder_mean = paste(folder_files, folder_output, sep = "/")
  
  #Creat dirs
  if(!dir.exists(folder_mean))
    dir.create(folder_mean)
  
  for(file_emg in emg_files){
    #print(file_emg)
    data_sum = rep(0, 2000)
    data_count = 1
    for(folder in folder_list){
      #print(paste(folder_files, folder, file_emg, sep = "/"))
      emg_data <- read.table(paste(folder_files, folder, file_emg, sep="/"))
      
      data_sum = data_sum + emg_data$V2
      data_count = data_count + 1
      #data_by_folder = append(data_by_folder, list(emg_data$V2))
    }
    data_sum = list(emg_data$V1, round(data_sum/data_count, 5))
    write.table(data_sum, 
                paste(folder_mean, file_emg, sep="/"), 
                row.names = FALSE, col.names = FALSE)
  }
}

process_h_reflex <- function(h_wave, 
                             main_label = "a", 
                             y_label = "Amplitude do Reflexo H (mV)", 
                             x_label = "Intervalo entre estímulos (ms)", 
                             time_delay=0.0,
                             time_div=1.0,
                             relative_amplitude = 0.0){
  
  if(relative_amplitude > 0.0){
    h_wave$h_wave_amplitude = h_wave$h_wave_amplitude/relative_amplitude
  }
  max_time_value = max(h_wave$intensity)-time_delay
  
  plot(h_wave$intensity-time_delay, 
       h_wave$h_wave_amplitude, 
       main=main_label,
       ylab=y_label,
       xlab=x_label,
       xlim = c(-time_delay, max_time_value),
       xaxt="n")
  
  axis(1, 
       xaxp=c(-time_delay, 
              max_time_value, 
              (max_time_value+time_delay)*time_div ), 
       las=1)
  
  lines(h_wave$intensity-time_delay, 
        h_wave$h_wave_amplitude)
}

process_spikes <- function(folder_files = "./Tarefa 02/pm1",
                          pattern = "\\.MN.SPIKES.txt$",
                          m_spike_limit = 25.0,
                          intensity_points,
                          intensity_point = 10.1,
                          sample_period = 0.2,
                          main_label = c("a", "b"),
                          interval = c(0.0, 0.0),
                          ia_afferent = FALSE) {
  
  spikes_files <- list.files(folder_files, pattern)
  
  m_spikes = c()
  h_spikes = c()
  axon_spikes = list()
  spike_times = list()
  
  count_intensity_all = 0
  count_intensity_try = 0

  for(spike_file in spikes_files){
    #print(spike_file)
    count_intensity_all = count_intensity_all + 1
    tryCatch({
        spike_data <- read.table(paste(folder_files, spike_file, sep="/"))
        
        count_intensity_try = count_intensity_try + 1
        
        axon_spikes = append(axon_spikes, list(spike_data$V2))
        spike_times = append(spike_times, list(spike_data$V1))
        
        m_spikes_temp = spike_data[spike_data$V1 <= m_spike_limit,]
        h_spikes_temp = spike_data[spike_data$V1 > m_spike_limit,]
        
        m_spikes_length = length(m_spikes_temp$V2)
        h_spikes_length = length(h_spikes_temp$V2)
        
        m_spikes = append(m_spikes, m_spikes_length)
        h_spikes = append(h_spikes, h_spikes_length)  
      },
      error = function(cond) {
        # O código é executado mas parece que os escopos ficam diferentes
        #print("Error")
        #count_intensity_try = count_intensity_try + 1
        #axon_spikes = append(axon_spikes, list(0.0))
        #spike_times = append(spike_times, list(0.0))
        
        #m_spikes = append(m_spikes, 0.0)
        #h_spikes = append(h_spikes, 0.0)
      },
      finally = {
        if(count_intensity_all > count_intensity_try) {
          #print("finally")
          axon_spikes = append(axon_spikes, list(0.0))
          spike_times = append(spike_times, list(0.0))
          
          m_spikes = append(m_spikes, 0.0)
          h_spikes = append(h_spikes, 0.0)
          count_intensity_try = count_intensity_try + 1
        }
      }
    )
  }
  
  axon_spikes = list("spikes"=axon_spikes, "times"=spike_times)
  
  axon_spikes_curve = list("intensity"=intensity_points, 
                           "m_spikes"=m_spikes, 
                           "h_spikes"=h_spikes )
  #mn_spikes_curve
  
  #length(axon_spikes_curve$h_spikes)
  #length(axon_spikes_curve$h_spikes)
  
  plot(axon_spikes_curve$intensity,
       axon_spikes_curve$m_spikes, 
       type="o", pch=1, lty=1, 
       main=main_label[1],
       xlab="Intensidade do Estímulo (mA)", 
       ylab="Contagem")
  
  points(axon_spikes_curve$intensity, 
         axon_spikes_curve$h_spikes, 
         pch=16)
  lines(axon_spikes_curve$intensity, 
        axon_spikes_curve$h_spikes)
  
  plot(y=axon_spikes$spikes[[1]], 
       x=c(1:length(axon_spikes$spikes[[1]]))*0 + 10.0,
       pch='.',
       type="p",
       cex=0.5,
       main=main_label[2],
       xlab="Intensidade do Estímulo (mA)", 
       ylab="Index",
       ylim = c(1, max(unlist(axon_spikes$spikes))),
       xlim = c(10.0, 24.0))
  
  elem_i = 2
  for(mn_len in unlist(lapply(axon_spikes$spikes, length))[-1] ){
    points(y=axon_spikes$spikes[[elem_i]], 
           x = c(1:mn_len)*0 + axon_spikes_curve$intensity[elem_i], 
           pch='.', type="p", cex=0.5,)
    elem_i = elem_i + 1
  }
  
  #x=(round(y,1)*10 - 100)
  
  if(ia_afferent){
    lines(axon_spikes_curve$intensity, 
          axon_spikes_curve$m_spikes)
  }else{
    lines(axon_spikes_curve$intensity, 
          axon_spikes_curve$h_spikes, col="blue")
    
    lines(axon_spikes_curve$intensity, 
          axon_spikes_curve$m_spikes, col="red")
  }
  
  
  
  index_plot = round( ((intensity_point - 10.0 )/sample_period + 1.0) )
  
  print( paste0("Index: ", str(index_plot), " Current: ", str(intensity_point)) )
  if(interval[1] == 0.0 && interval[2] == 0.0){
    interval = c(0.0, 
                 max(axon_spikes$times[[index_plot]]))
  }
  plot(axon_spikes$times[[index_plot]], 
       axon_spikes$spikes[[index_plot]], type="p", 
       main=main_label[3],
       xlim = interval,
       xlab="Tempo (ms)", 
       ylab="Index")
  
  return(list("spikes"=axon_spikes, "spikes_curves"=axon_spikes_curve))
}

################################################################################
#Atividade 01
################################################################################
# Ponto 1
#############Força##############################################################
# Carregando arquivos para análise da força
Force_1 <- read.delim("./Tarefa 01/Force_1.txt", header=FALSE)

start_time = 1000*20 #1000/0.05
end_time = 4000*20 #4000/0.05

plot( c(start_time:end_time)/20000, 
      Force_1$V2[start_time:end_time], 
      type= "b",
      cex=0.35,
      main="a",
      xlab = "Tempo (s)",
      ylab = "Força (N)")


# Para quantificar força foi considerado o intervalo entre 1s e 4s, assim 
# garantimos que a força média não sofra interferência no valor durante as fases 
# inicial e final da força.


max_force_1 = max(Force_1$V2[start_time:end_time])
max_force_1 #167.5631 N
min_force_1 = min(Force_1$V2[start_time:end_time])
min_force_1 #141.166 N
mean_force_1 = mean(Force_1$V2[start_time:end_time])
mean_force_1 #157.215 N
sd_force_1 = sd.sample(Force_1$V2[start_time:end_time])
sd_force_1 #5.097143 N
cv_force_1 = sd_force_1/mean_force_1
cv_force_1 #0.03242148, 3.2%

legend("topright", 
       legend = c(paste0("Média ", round(mean_force_1, 2), " N"), 
                  paste0("            DP ", round(sd_force_1, 2), " N"), 
                  paste0("         CV ", round(cv_force_1*100, 2), " %")) )

################################################################################
# Ponto 2
################################################################################
#carregando arquivos
Spikes_1 <- read.delim("./Tarefa 01/Spikes_1.txt", header=FALSE)

plotSpikes(Spikes_1, 
           index_min=1000.0, 
           index_max=4000.0, 
           rescale = 1000.0,
           main_label="b",
           x_label="Tempo (s)", 
           y_label="Unidade Motora")

#Verificando se há spikes unitários
spikes_teste = getSpikes(Spikes_1, n=0)

spikes_unitarios = sum(unlist(lapply(spikes_teste$spikes_train, length)) == 1)
spikes_unitarios #Há 31 spikes unitários
# Ou seja, não temos intervalo entre spikes

#Verificando se há apenas dois spikes
spikes_binarios = sum(unlist(lapply(spikes_teste$spikes_train, length)) == 2)
spikes_binarios # Há 13 spikes binários,
# Ou seja, temos um único intervalo entre spikes,
# Neste caso, se temos uma população o desvio padrão será zero
# se temos uma amostragem o desvio padrão será indeterminado, 

#Amostragem do IED de 20 motoneuronios 18 deles aleatórios
spikes_sampled = getSpikes(Spikes_1, index_min=1000.0, index_max=4000.0, n=20)
barcode(spikes_sampled$spikes_train, 
        main = "c", 
        xlab = "Tempo (s)")

inter_spikes = getInterSpikes(Spikes_1, 
                              index_min=1000.0, 
                              index_max=4000.0)

#Histograma dos IEDs
print_hist(unlist(inter_spikes), 
           x_lim=2550, 
           y_lim=8000, 
           bins=100, 
           main_label="d", 
           x_label="IED Médio (ms)", 
           y_label="Número de Elementos")

#Variabilidade
inter_spikes_variability = getInterSpikesVariability(inter_spikes)

#Foram considerados apenas as média sem NAs
mean_interval = mean(unlist(inter_spikes_variability$Mean), na.rm = TRUE)
mean_interval # 165.7537

sd_interval = mean(unlist(inter_spikes_variability$Sd_Samp), na.rm = TRUE)
sd_interval # 55.57102 ms /  63.93184

cv_interval = mean(unlist(inter_spikes_variability$Cv_Samp), na.rm = TRUE)
cv_interval # 0.2569582 / 0.2787622

legend("topright", 
       legend = c(paste0("Média ", round(mean_interval, 2), " ms"), 
                  paste0("        DP ", round(sd_interval, 2), " ms"), 
                  paste0("          CV ", round(cv_interval*100, 2), " %")) )

################################################################################
# Ponto 3
#############Força##############################################################
#Carregando arquivos
Force_2 <- read.delim("./Tarefa 01/Force_2.txt", header=FALSE)
# Utilizando as mesmas configurações anteriores
mean_force_2 = mean(Force_2$V2[start_time:end_time])
mean_force_2 # 355.6591 N

plot( c(start_time:end_time)/20000, 
      Force_2$V2[start_time:end_time], 
      type= "b",
      cex=0.35,
      xlab = "Tempo (s)",
      ylab = "Força (N)")

#É necessário mudar o ISI médio, com intuito de igualar à força anterior.
#Carregando arquivos
Force_3 <- read.delim("./Tarefa 01/Force_3.txt", header=FALSE)

plot( c(start_time:end_time)/20000, 
      Force_3$V2[start_time:end_time], 
      type= "b",
      cex=0.35,
      #xlim = c(1, 4.2),
      main = "a",
      xlab = "Tempo (s)",
      ylab = "Força (N)")

# O mesmo foi aumentado para 11.75 ms
mean_force_3 = mean(Force_3$V2[start_time:end_time])
mean_force_3 # 157.2088

# Temos então um valor médio muito próximo ao primeiro experimento
# mean_force_1=157.215 N e mean_force_3=157.2088

max_force_3 = max(Force_3$V2[start_time:end_time])
max_force_3 #176.2056 N
min_force_3 = min(Force_3$V2[start_time:end_time])
min_force_3 #136.4329 N

sd_force_3 = sd.sample(Force_3$V2[start_time:end_time])
sd_force_3 #9.758431
cv_force_3 = sd_force_3/mean_force_3
cv_force_3 #0.06207306, 6.2%

legend("bottomleft", 
       legend = c(paste0("Média ", round(mean_force_3, 2), " N"), 
                  paste0("            DP ", round(sd_force_3, 2), " N"), 
                  paste0("         CV ", round(cv_force_3*100, 2), " %")) )

#############Disparos###########################################################
#carregando arquivos
Spikes_3 <- read.delim("./Tarefa 01/Spikes_3.txt", header=FALSE)

plotSpikes(Spikes_3, 
           index_min=1000.0, 
           index_max=4000.0, 
           rescale = 1000.0,
           main_label = "b",
           x_label="Tempo (s)", 
           y_label="Unidade Motora")

#Verificando se há spikes unitários
spikes_teste = getSpikes(Spikes_3, n=0)

spikes_unitarios = sum(unlist(lapply(spikes_teste$spikes_train, length)) == 1)
spikes_unitarios #Há 29 spikes unitários
# Ou seja, não temos intervalo entre spikes

#Verificando se há apenas dois spikes
spikes_binarios = sum(unlist(lapply(spikes_teste$spikes_train, length)) == 2)
spikes_binarios # Há 16 spikes binários,
# Ou seja, temos um único intervalo entre spikes,
# Neste caso, se temos uma população o desvio padrão será zero
# se temos uma amostragem o desvio padrão será indeterminado, 


#Amostragem do IED de 20 motoneuronios 18 deles aleatórios
spikes_sampled = getSpikes(Spikes_3, index_min=1000.0, index_max=4000.0, n=20)
barcode(spikes_sampled$spikes_train, 
        main = "c", 
        xlab = "Tempo (s)")

inter_spikes = getInterSpikes(Spikes_3, index_min=1000.0, index_max=4000.0)

#Histograma dos IEDs
print_hist(unlist(inter_spikes), 
           x_lim=2800, 
           y_lim=8000, 
           bins=100, 
           main_label="d", 
           x_label="IED Médio (ms)", 
           y_label="Número de Elementos")

#Variabilidade
inter_spikes_variability = getInterSpikesVariability(inter_spikes)

#Foram considerados apenas as média sem NAs
mean_interval = mean(unlist(inter_spikes_variability$Mean), na.rm = TRUE)
mean_interval # 183.5778

sd_interval = mean(unlist(inter_spikes_variability$Sd_Samp), na.rm = TRUE)
sd_interval # 56.24955 / 64.47671

cv_interval = mean(unlist(inter_spikes_variability$Cv_Samp), na.rm = TRUE)
cv_interval # 0.2681345 / 0.2907794

legend("topright", 
       legend = c(paste0("Média ", round(mean_interval, 2), " ms"), 
                  paste0("        DP ", round(sd_interval, 2), " ms"), 
                  paste0("          CV ", round(cv_interval*100, 2), " %")) )


################################################################################
#Atividade 02
################################################################################

emg_data = process_emg(folder_files ="./Tarefa 02/manual",
                       pattern ="EXP.EMG",
                       m_wave_limit =25.0)

process_emg_plot(emg_data$m_h_waves, 
                 emg_data$recruitment_curve, 
                 main_label=c("Curvas de Resposta ao Estímulo",
                              "Curva de Recutamento Reflexo H e Onda M"), 
                 x_lim_factor = 2)

################################################################################
# MNs e Ias
################################################################################
spikes_data_mn = process_spikes(folder_files = "./Tarefa 02/manual",
                                pattern = "\\.MN.SPIKES.txt$",
                                m_spike_limit = 25.0, 
                                intensity_points = emg_data$recruitment_curve$intensity, 
                                intensity_point = 11.8,
                                sample_period = 0.2,
                                main_label = c("Curvas de disparos", 
                                               "b", 
                                               "c"),
                                interval = c(27, 28.4))



spikes_data_ia = process_spikes(folder_files = "./Tarefa 02/manual",
                                pattern = "\\.IA.SPIKES.txt$",
                                m_spike_limit = 25.0,
                                intensity_points = emg_data$recruitment_curve$intensity, 
                                intensity_point = 11.8, 
                                sample_period = 0.2,
                                main_label = c("a", 
                                               "a", 
                                               "d"),
                                interval = c(9, 9.25),
                                ia_afferent = TRUE)

plot(emg_data$m_h_waves$time, 
     emg_data$m_h_waves$amplitude[[10]], 
     type="l",
     main="b",
     xlab = "Tempo (ms)",
     ylab = "Amplitude (mV)")



################################################################################
# Promediação
################################################################################
experiments_folder = c(  "exp1",  "exp2",  "exp3",  "exp4",  "exp5", 
                         "exp6",  "exp7",  "exp8",  "exp9", "exp10", 
                        "exp11", "exp12", "exp13", "exp14", "exp15",
                        "exp16", "exp17", "exp18", "exp19", "exp20")

process_emg_mean("./Tarefa 02", 
                 experiments_folder, 
                 "EXP.EMG", 
                 folder_output="exp_mean")

emg_data = process_emg(folder_files ="./Tarefa 02/exp_mean",
                       pattern ="EXP.EMG",
                       m_wave_limit =25.0)


process_emg_plot(emg_data$m_h_waves, 
                 emg_data$recruitment_curve, 
                 main_label=c("a",
                              "b"), 
                 x_lim_factor = 2)


################################################################################
#Atividade 03
################################################################################
folder_files = "./Tarefa 03"

#utilizando os dados anteriores para levantamento de H20

emg_data = process_emg(folder_files ="./Tarefa 02/exp_mean",
                       pattern ="EXP.EMG",
                       m_wave_limit =25.0)
process_emg_plot(emg_data$m_h_waves, 
                 emg_data$recruitment_curve,
                 main_label =c("Curvas de Resposta ao Estímulo",
                               "a"),
                 relative_waves = TRUE, 
                 amp_percent = 0.20)

# Com o valor de corrente 10.7 mA obtemos uma Curva H com amplitude de 
# aproximadamente 20 %.

################################################################################
#Buscando o sinal de controle
################################################################################
control_folder = c("ctl1", "ctl2", "ctl3", "ctl4", "ctl5", 
                   "ctl6", "ctl7", "ctl8", "ctl9", "ctl10")

process_emg_mean("./Tarefa 03", 
                 control_folder, 
                 "CTL.EMG", 
                 folder_output="ctl_mean")

emg_data_controle = process_emg(folder_files ="./Tarefa 03/ctl_mean",
                                pattern ="CTL.EMG",
                                m_wave_limit =25.0)

emg_data_controle$recruitment_curve$h_wave_amplitude

plot(emg_data_controle$m_h_waves$time, 
     emg_data_controle$m_h_waves$amplitude[[1]], 
     type="l",
     main="b",
     xlab = "Tempo (ms)",
     ylab = "Amplitude (mV)")

h_20 = emg_data_controle$recruitment_curve$h_wave_amplitude

m_max = max(emg_data$recruitment_curve$m_wave_amplitude)

(h_20/m_max)*100

################################################################################
#Controle Tibial
################################################################################
#Ajuste do recrutamento das aferentes Ia do Tibial
tibial_ia_data = read.delim(paste(folder_files, 
                                  "controle/Tibial.IA.SPIKES.txt", sep="/"), 
                            header=FALSE)

# Verificamos que é necessário uma amplitude para recrutar mais de 50% 
# de aferentes, assim a corrente de 15 mA aplicada no CPN foi suficiente para
# recrutar ~69% de aferentes Ia do Musculos Tibial Anterior.
count_index = length(unique(tibial_ia_data$V2))
count_index

ia_tibial_recrutment = (count_index/280)*100
ia_tibial_recrutment

################################################################################
# Em seguida realizamos os estímulos do Nervo Tibial Posterior (PTN) com o 
# condicionamento no Nervo Peroneal Comum (CPN). Para isso, foi estímulo de 
# condicionamento foi aplicado fixadamente no intervalo de 2 a 3 ms, enquanto 
# o estímulo de teste, PTN, foi aplicado no intervalo de 0 a 50 ms para o 
# levantamento da curva de amplitude do reflexo H no musculo Soleus
################################################################################
#


emg_data = process_emg(folder_files ="./Tarefa 03/manual",
                       pattern ="EXP.EMG",
                       m_wave_limit =25.0)


process_h_reflex(emg_data$recruitment_curve, 
                 time_delay=2.0,
                 main_label = "a",
                 y_label = "Amplitude do Reflexo (% Controle)",
                 relative_amplitude=h_20)



################################################################################
#Promediação
################################################################################

experiments_folder = c( "exp1",  "exp2",  "exp3",  "exp4",  "exp5", 
                        "exp6",  "exp7",  "exp8",  "exp9", "exp10", 
                       "exp11", "exp12", "exp13", "exp14", "exp15",
                       "exp16", "exp17", "exp18", "exp19", "exp20")

process_emg_mean("./Tarefa 03", 
                 experiments_folder, 
                 "EXP.EMG", 
                 folder_output="exp_mean")

emg_data = process_emg(folder_files ="./Tarefa 03/exp_mean",
                       pattern ="EXP.EMG",
                       m_wave_limit =25.0)


process_h_reflex(emg_data$recruitment_curve, 
                 time_delay=2.0, 
                 time_div = 1,
                 main_label = "b",
                 y_label = "Amplitude do Reflexo (% Controle)",
                 relative_amplitude=h_20)




