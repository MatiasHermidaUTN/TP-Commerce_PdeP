module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumaDos num = num + 2

entregaSencilla :: String -> Bool
entregaSencilla fecha = rem (length fecha) 2 == 0

aplicarDescuento :: Num a => a -> a -> a --0<descuento<1
aplicarDescuento precio descuento = precio * descuento
aplicarCostoDeEnvio :: Num a => a -> a -> a
aplicarCostoDeEnvio precio costoEnvio = precio + costoEnvio
precioTotal :: Num a => a -> a -> a -> a -> a
precioTotal precioUnitario cantidad descuento costoEnvio =
 aplicarCostoDeEnvio (aplicarDescuento precioUnitario descuento *  cantidad) costoEnvio

productoDeLujo :: String -> Bool
productoDeLujo nombreProducto = elem 'x' nombreProducto || elem 'z'  nombreProducto
productoCodiciado :: String -> Bool
productoCodiciado nombreProducto = length nombreProducto > 10
letraVocal :: Char -> Bool
letraVocal char = char == 'a' ||  char == 'e' || char == 'i' ||  char == 'o' || char == 'u'|| char == 'A' ||  char == 'E' || char  == 'I' || char == 'O' || char == 'U'
productoCorriente :: String -> Bool
productoCorriente = letraVocal.head
productoDeElite :: String -> Bool
productoDeElite nombreProducto = productoDeLujo nombreProducto &&  productoCodiciado nombreProducto && not (productoCorriente  nombreProducto)

productoXL :: String -> String
productoXL nombreProducto = nombreProducto ++ "XL"

descodiciarProducto :: String -> String
descodiciarProducto nombreProducto = take 10 nombreProducto
versionBarata :: String -> String
versionBarata = reverse.descodiciarProducto
RESOLUCION DE AYUDANTE:

precioTotal :: Float -> Float -> Float -> Float -> Float
precioTotal precioUnitario cantidad descuento costoDeEnvio = aplicarCostoDeEnvio (aplicarDescuento precioUnitario descuento * cantidad) costoDeEnvio

productoDeElite :: String -> Bool
productoDeElite nombreDeProducto = productoDeLujo nombreDeProducto && productoCodiciado nombreDeProducto && (not . productoCorriente) nombreDeProducto

aplicarDescuento :: Float -> Float -> Float
aplicarDescuento unPrecio unDescuento = unPrecio * (1 - unDescuento)

entregaSencilla :: String -> Bool
entregaSencilla unDia = even . length $ unDia

descodiciarProducto :: String -> String
descodiciarProducto nombreDeProducto = take 10 nombreDeProducto

productoDeLujo :: String -> Bool
productoDeLujo nombreDeProducto = elem 'x' nombreDeProducto || elem 'z' nombreDeProducto

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unPrecio unCostoDeEnvio = unPrecio + unCostoDeEnvio

productoCodiciado :: String -> Bool
productoCodiciado nombreDeProducto = length nombreDeProducto > 10

productoCorriente :: String -> Bool
productoCorriente nombreDeProducto = esVocal . head $ nombreDeProducto

esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: String -> String
productoXL nombreDeProducto = nombreDeProducto ++ " XL"

versionBarata :: String -> String
versionBarata nombreDeProducto = reverse . descodiciarProducto $ nombreDeProducto
