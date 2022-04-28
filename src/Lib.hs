module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
---------------------------------------------------------------------------------------------------------------------------------------------------

type TNombreDelProducto = String
type TPrecioDelProducto = Float
type TProducto = (TNombreDelProducto,TPrecioDelProducto)

precioTotal :: TProducto -> Float -> Float -> Float -> Float
precioTotal (_,precioDelProducto) cantidad descuento costoEnvio =
 aplicarCostoDeEnvio (aplicarDescuento precioDelProducto descuento * cantidad) costoEnvio

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unPrecio unCostoDeEnvio = unPrecio + unCostoDeEnvio

aplicarDescuento :: Float -> Float -> Float 
aplicarDescuento unPrecio unDescuento = unPrecio * (1-unDescuento) --0<=descuento<=1

entregaSencilla :: String -> Bool
entregaSencilla unaFecha = (even.length) unaFecha

productoDeElite :: TProducto -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && (not.productoCorriente)  producto

productoDeLujo :: TProducto -> Bool
productoDeLujo (nombreProducto,_) = elem 'x' nombreProducto || elem 'z'  nombreProducto || elem 'X' nombreProducto || elem 'Z'  nombreProducto

productoCodiciado :: TProducto -> Bool
productoCodiciado (nombreProducto,_) = length nombreProducto > 10

productoCorriente :: TProducto -> Bool
productoCorriente (nombreProducto,_) = letraVocal.head $ nombreProducto

letraVocal :: Char -> Bool
letraVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: TProducto -> TProducto
productoXL (nombreProducto,precioDelProducto) = (nombreProducto ++ " XL" , precioDelProducto)

versionBarata :: TProducto -> TProducto
versionBarata producto = (reverse.fst.descodiciarProducto $ producto , snd producto)

descodiciarProducto :: TProducto -> TProducto
descodiciarProducto (nombreProducto,precioDelProducto) = (take 10 nombreProducto,precioDelProducto)