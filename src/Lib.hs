module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
---------------------------------------------------------------------------------------------------------------------------------------------------

type NombreDelProducto = String
type PrecioDelProducto = Float
type Producto = (NombreDelProducto, PrecioDelProducto)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (_, precioDelProducto) cantidad descuento costoEnvio =
 aplicarCostoDeEnvio (aplicarDescuento precioDelProducto descuento * cantidad) costoEnvio

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unPrecio unCostoDeEnvio = unPrecio + unCostoDeEnvio

aplicarDescuento :: Float -> Float -> Float 
aplicarDescuento unPrecio unDescuento = unPrecio * (1-unDescuento) --0<=descuento<=1

entregaSencilla :: String -> Bool
entregaSencilla unaFecha = (even.length) unaFecha

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && (not.productoCorriente)  producto

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto, _) = elem 'x' nombreProducto || elem 'z'  nombreProducto || elem 'X' nombreProducto || elem 'Z'  nombreProducto

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = length nombreProducto > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombreProducto, _) = letraVocal.head $ nombreProducto

letraVocal :: Char -> Bool
letraVocal unaLetra = elem unaLetra "aeiouAEIOU"

productoXL :: Producto -> Producto
productoXL (nombreProducto, precioDelProducto) = (nombreProducto ++ " XL", precioDelProducto)

versionBarata :: Producto -> Producto
versionBarata producto = (reverse.fst.descodiciarProducto $ producto, snd producto)

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombreProducto, precioDelProducto) = (take 10 nombreProducto, precioDelProducto)