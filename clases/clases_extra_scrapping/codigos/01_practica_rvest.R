# Scraper de Vinos El Salvador 

# Librerias
library(tidyverse)
library(rvest)

#Rutas 
outstub <- 'XNuevo - Vino El Salvador/resultados crudos'

# URL
urls <- read_html('https://vinoelsalvador.com/product-category/espumantes/')
urls1 <- urls %>% 
  html_elements(css='[class="nav-link "]') %>% 
  html_attr('href')
urls2 <- urls %>% 
  html_elements(css='[class="nav-link active"]') %>% 
  html_attr('href')
urls3 <- urls %>% 
  html_elements(css='[class="dropdown-item "]') %>% 
  html_attr('href')
urls <- c(urls1,urls2,urls3)
urls <- tibble(url=urls)
urls <- urls %>% 
  mutate(variedad = str_extract(url,'tintos|blancos|rosado|rosados|naranjos|dulces|espumantes'))
urls <- urls %>% 
  filter(!str_detect(url,'combos|otras-bebidas|accesorios|tienda|importados'))
urls <- urls %>% filter(!is.na(variedad))         
urls <- urls %>% 
  mutate(composicion_varietal = str_remove(url,'https://vinoelsalvador.com/product-category/'),
         composicion_varietal = str_remove(composicion_varietal,variedad),
         composicion_varietal = str_remove_all(composicion_varietal,'\\/'),
         composicion_varietal = str_replace_all(composicion_varietal,'-',' '))
urls <- urls %>% 
  filter(!url %in% c('https://vinoelsalvador.com/product-category/blancos/',
                    'https://vinoelsalvador.com/product-category/tintos/'))
urls <- urls %>% 
  mutate(composicion_varietal = if_else(str_detect(composicion_varietal,'otros'),'',composicion_varietal))

# Levantar URLs 
vinos_urls <- tibble()
i <- 1
for(i in 1:length(urls$url)){
  # Cargar HTML
  tmp <- read_html(urls$url[i])
  # Chequear cantidad de paginas 
  q_pag <- tmp %>% html_elements(css='[class="page-link"]') %>% html_text()
  q_pag <- as.numeric(q_pag[length(q_pag)])
  if(is_empty(q_pag)){
    q_pag <- 1
  }
  for(j in 1:q_pag){
    tmp <- read_html(paste0(urls$url[i],'/page/',j,'/'))
    tmp <- tmp %>% 
      html_elements(css='[class="woocommerce-LoopProduct-link woocommerce-loop-product__link"]') %>% 
      html_attr('href')
    tmp <- unlist(tmp)
    tmp <- tibble(url_prod=tmp)
    tmp <- cross_join(urls[i,],tmp)
    tmp <- tmp %>% distinct()
    vinos_urls <- bind_rows(vinos_urls,tmp)
  }
  print(i)
}
vinos_urls <- vinos_urls %>% 
  rename(comp_var_url = composicion_varietal)
# Scrapear vinos 
vinos_final <- tibble()
i <- 1
for(i in 1:length(vinos_urls$url)){
  tmp <- vinos_urls[i,]
  tmp$url <- NULL 
  tmp_html <- read_html(tmp$url_prod)
  
  # Nombre 
  nombre <- tmp_html %>% 
    html_elements(css='[class="summary entry-summary"]') %>% 
    html_elements(css='[class="product_title entry-title"]') %>% 
    html_text2()
  
  # Precio
  precio <- tmp_html %>% 
    html_elements(css='[class="summary entry-summary"]') %>% 
    html_elements(css='[class="price"]') %>% 
    html_text2()
  
  # Precio con descuento
  precio_con_descuento <- tmp_html %>% 
    html_elements(css='[class="summary entry-summary"]') %>% 
    html_elements(css='[class="woocommerce-Price-amount amount"]') %>% 
    html_text2()
  
  # Bodega
  bodega <- tmp_html %>% 
    html_elements(css='[class="summary entry-summary"]') %>% 
    html_elements(css='[class="Text bodet"]') %>% 
    html_text2()
  if(is_empty(bodega)){
    bodega <- ''
  }
  # Disponibilidad
  stock <- tmp_html %>% 
    html_elements(css='[class="summary entry-summary"]') %>% 
    html_elements(css='[class="stock in-stock"]') %>% 
    html_text2()
  
  # Tabla productos 
  tabla <- tmp_html %>% 
    html_elements(css='[class="woocommerce-product-attributes shop_attributes"]') %>% 
    html_table()
  if(length(tabla) > 0){
    tabla <- tabla[[1]]
    tabla <- tabla %>% 
      rename(caracteristica=X1,descripcion=X2)
    tabla <- tabla %>% 
      group_by(caracteristica) %>% 
      filter(row_number() == 1)
    tabla <- tabla %>% 
      pivot_wider(names_from=caracteristica,values_from=descripcion)
    tabla <- janitor::clean_names(tabla)
  }
  else {
    tabla <- tmp_html %>% 
      html_elements(css='[class="woocommerce-Tabs-panel woocommerce-Tabs-panel--description panel entry-content wc-tab"]') %>% 
      html_text2()
    tabla <- tibble(descripcion_sin_tabla = tabla)
  }
  # Juntar datos 
  tmp <- tmp %>% 
    mutate(nombre = nombre,
           bodega_summary = bodega,
           precio_venta = precio,
           stock = stock) %>% 
    bind_cols(tabla)
  
  vinos_final <- bind_rows(vinos_final,tmp)
  print(i)
}

write_csv(vinos_final,file.path(outstub,paste0('Vino El Salvador ',Sys.Date(),'.csv')))
