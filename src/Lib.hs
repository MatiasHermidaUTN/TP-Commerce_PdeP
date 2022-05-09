module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
---------------------------------------------------------------------------------------------------------------------------------------------------

type Nombre = String
type Precio = Float
type Producto = (Nombre, Precio)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (_, precioDelProducto) cantidad descuento costoEnvio =
 (aplicarCostoDeEnvio costoEnvio).(* cantidad).(aplicarDescuento descuento) $ precioDelProducto

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio unCostoDeEnvio unPrecio = unPrecio + unCostoDeEnvio

aplicarDescuento :: Float -> Float -> Float 
aplicarDescuento unDescuento unPrecio = (*unPrecio).(1-) $ unDescuento --0<=descuento<=1

entregaSencilla :: String -> Bool
entregaSencilla unaFecha = (even.length) unaFecha

productoDeElite :: Producto -> Bool
productoDeElite producto = ((&&).productoDeLujo $ producto).((&&).productoCodiciado $ producto).not.productoCorriente $ producto

productoDeLujo :: Producto -> Bool
productoDeLujo (nombreProducto, _) = elem 'x' nombreProducto || elem 'z'  nombreProducto || elem 'X' nombreProducto || elem 'Z'  nombreProducto

productoCodiciado :: Producto -> Bool
productoCodiciado (nombreProducto, _) = (>10).length $ nombreProducto

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