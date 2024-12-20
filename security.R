library(jsonlite)

key <- rand_bytes(32)
iv <- rand_bytes(16)

print("chave")
print(key)

key_hex <- paste0(as.character(key), collapse = "")
iv_hex <- paste0(as.character(iv), collapse = "")
print("chave hex")
print(key_hex)
print("iv hex")
print(iv_hex)

# Convertendo chave e iv novamente para raw
key_raw <- as.raw(strtoi(substring(key_hex, seq(1, nchar(key_hex), 2), seq(2, nchar(key_hex), 2)), 16L))
iv_raw <- as.raw(strtoi(substring(iv_hex, seq(1, nchar(iv_hex), 2), seq(2, nchar(iv_hex), 2)), 16L))

# Lendo credenciais
json_credentials <- readLines("secret-key.json")
# Transformando em formato raw
raw_data <- charToRaw(paste(json_credentials, collapse = "\n"))

# Criptografando
encrypted_data <- aes_cbc_encrypt(raw_data, key_raw, iv_raw)
# Salvando credenciais criptografas
raw_encrypted_data <- as.raw(encrypted_data)
writeBin(raw_encrypted_data, "credentials_encrypted.bin")

# Lendo dados criptografados
encrypted_data <- readBin("credentials_encrypted.bin", what = "raw", n = file.info("credentials_encrypted.bin")$size)

# Descriptografando dado cru
decrypted_raw_data <- aes_cbc_decrypt(encrypted_data, key_raw, iv_raw)

# Formatando para texto
decrypted_data <- rawToChar(decrypted_raw_data)

# Salvando no arquivo json
write(decrypted_data, "credentials_decrypted.json")