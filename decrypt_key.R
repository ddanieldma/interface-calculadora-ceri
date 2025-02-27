decript_key <- function(chave_codificada, iv_codificado){
  chave <- as.raw(strtoi(substring(chave_codificada, seq(1, nchar(chave_codificada), 2), seq(2, nchar(chave_codificada), 2)), 16L))
  iv <- as.raw(strtoi(substring(iv_codificado, seq(1, nchar(iv_codificado), 2), seq(2, nchar(iv_codificado), 2)), 16L))
  
  # Lendo arquivo criptografado
  path_arquivo_criptografado <- "credentials/encrypted-key.bin"
  conteudo_criptografado <- readBin(path_arquivo_criptografado, what = "raw", n = file.info(path_arquivo_criptografado)$size)
  
  # Descriptografando credenciais
  conteudo_descriptografado <- aes_cbc_decrypt(conteudo_criptografado, chave, iv)
  
  # Formatando para texto
  conteudo_descriptografado_txt <- rawToChar(conteudo_descriptografado)
  
  return (conteudo_descriptografado_txt)
}