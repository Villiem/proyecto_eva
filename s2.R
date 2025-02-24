descarga_enoe <- function(){
  link.base = 'https://www.inegi.org.mx/contenidos/programas/enigh/nc/'
  
  # Construcción del enlace
  url.base = 'https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos//enoe_2024_trim2_sav.zip'
  #print(link)
  temp.enoe = tempfile()
  formato = 'sav'
  zipdir = tempdir()
  utils::download.file(url.base, temp.enoe)
  utils::unzip(temp.enoe, exdir = './data')
  list_dataraw = list.files(zipdir, pattern = paste0(formato,'$'), full.names = T)
  list_names = basename(tools::file_path_sans_ext(list_dataraw))
  # Read all files in the folder
  output = lapply(list_dataraw, rio::import)
  names(output) = list_names
}
