{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Diagrams.Compare
  ( compareImages
  , sameImageType
  ) where

import Data.Vector.Storable as V
import Codec.Picture
import Codec.Picture.Types (promoteImage, Pixel8)

-- | Calculate the mean sqared error (MSE) and peak signal to noise
--   ratio (PSNR) between two images. Images must be of the same pixel
--   type. If the images are not the same size then it will compare
--   the first n pixels where n is the number of pixels in the smaller
--   image.
compareImages :: DynamicImage -> DynamicImage -> (Double, Double)
compareImages i1 i2
  | sameImageType i1 i2 = (mse, psnr)
  | otherwise           = (1  , 0   )
  where
    mse = V.foldl' (\a x -> a + x*x) 0 xs / n
    psnr = (-10) * logBase 10 mse
    (v1, v2) = (componentValues i1, componentValues i2)
    xs = V.zipWith (-) v1 v2
    n = fromIntegral $ min (V.length v1) (V.length v2)

componentValues :: DynamicImage -> V.Vector Double
componentValues (ImageY8 i)     = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageY16 i)    = V.map ((/65535) . fromIntegral) (imageData i)
componentValues (ImageYA8 i)    = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageYA16 i)   = V.map ((/65535) . fromIntegral) (imageData i)
componentValues (ImageYF i)     = V.map realToFrac                (imageData i)
componentValues (ImageRGB8 i)   = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageRGB16 i)  = V.map ((/65535) . fromIntegral) (imageData i)
componentValues (ImageRGBA8 i)  = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageRGBA16 i) = V.map ((/65535) . fromIntegral) (imageData i)
componentValues (ImageRGBF i)   = V.map realToFrac                (imageData i)
componentValues (ImageYCbCr8 i) = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageCMYK8 i)  = V.map ((/255)   . fromIntegral) (imageData i)
componentValues (ImageCMYK16 i) = V.map ((/65535) . fromIntegral) (imageData i)

sameImageType :: DynamicImage -> DynamicImage -> Bool
sameImageType (ImageY8 _)     (ImageY8 _)     = True
sameImageType (ImageY16 _)    (ImageY16 _)    = True
sameImageType (ImageYA8 _)    (ImageYA8 _)    = True
sameImageType (ImageYA16 _)   (ImageYA16 _)   = True
sameImageType (ImageYF _)     (ImageYF _)     = True
sameImageType (ImageRGB8 _)   (ImageRGB8 _)   = True
sameImageType (ImageRGB16 _)  (ImageRGB16 _)  = True
sameImageType (ImageRGBA8 _)  (ImageRGBA8 _)  = True
sameImageType (ImageRGBA16 _) (ImageRGBA16 _) = True
sameImageType (ImageRGBF _)   (ImageRGBF _)   = True
sameImageType (ImageYCbCr8 _) (ImageYCbCr8 _) = True
sameImageType (ImageCMYK8 _)  (ImageCMYK8 _)  = True
sameImageType (ImageCMYK16 _) (ImageCMYK16 _) = True
sameImageType _               _               = False

