{-# LANGUAGE BangPatterns #-}

import Graphics.Rendering.OpenGL.GL.Tensor
import Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.CUDA as I
import Data.List hiding (intersect)
import Foreign.C.Types
import Foreign.Ptr
import Data.Int
import Prelude as P

import Data.Word
import qualified Graphics.UI.GLUT as G
import Graphics.Rendering.OpenGL.GL.CoordTrans

width :: Int
width = 640

height :: Int
height = 480

fov :: Float
fov = 45.0

maxdepth :: Int
maxdepth = 2

type VectorF = Vertex3 Float
type VectorI = Vertex3 Int
type Vec a = (a, a, a)
type VecF = Vec Float
type SphereIntersect = (Bool, Sphere, Float)
type Index = (Int, Int)
type ArrayPlane a = Array DIM2 a

nullVector :: Exp (VecF)
nullVector = constant (0.0, 0.0, 0.0) 

infixl 6 -.
infixl 6 +.
infixl 7 *.
(-.), (+.), (*.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(-.) = vzipWith (-)
(+.) = vzipWith (+)
(*.) = vzipWith (*)

infixl 6 --.
infixl 6 ++.
infix  7 //.
infixl 7 **.
(--.), (++.), (**.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp a -> Exp (Vec a)
(--.) v f = vmap (flip (-) f) v
(++.) v f = vmap (f+) v
(**.) v f = vmap (f*) v
(//.) :: (Elt a, IsNum a, IsFloating a) => Exp (Vec a) -> Exp a -> Exp (Vec a)
(//.) v f = vmap (flip (/) f) v

cfalse :: Exp Bool
cfalse = constant False

ctrue :: Exp Bool
ctrue = constant True

vmap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (Vec a) -> Exp (Vec b)
vmap f v = let (x1,y1,z1) = unlift v
           in
           lift (f x1, f y1, f z1)

vzipWith :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> Exp (Vec a) -> Exp (Vec b) -> Exp (Vec c)
vzipWith f v1 v2
             = let (x1,y1,z1) = unlift v1
                   (x2,y2,z2) = unlift v2
               in
               lift (f x1 x2, f y1 y2, f z1 z2)

dot :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp a
dot a b = let 
            (x1, y1, z1) = unlift a
            (x2, y2, z2) = unlift b
          in
          x1 * x2 + y1 * y2 + z1 * z2

mag :: (Elt a, IsNum a, A.IsFloating a) => A.Exp (Vec a) -> A.Exp a
mag l = sqrt $ dot l l

normalized :: (Elt a, A.IsNum a, IsFloating a) => A.Exp (Vec a) -> (A.Exp (Vec a))
normalized l = l //. (mag l)

type Ray = (VecF, --start
            VecF) --dir

start :: Exp Ray -> Exp VecF
start r = A.fst r

dir :: Exp Ray -> Exp VecF
dir r = A.snd r

nullRay :: Exp Ray
nullRay = lift (nullVector, nullVector)

type Sphere = (VecF, --center
               Float,        --radius
               VecF, --scolor
               Float,        --reflection
               Float)        --transparency

center :: Exp Sphere -> Exp VecF
center s = let (c, _, _, _, _) = unlift s :: (Exp VecF, Exp Float, Exp VecF, Exp Float, Exp Float)
           in c

radius :: Exp Sphere -> Exp Float
radius s = let (_, r, _, _, _) = unlift s :: (Exp VecF, Exp Float, Exp VecF, Exp Float, Exp Float)

           in lift r

scolor :: Exp Sphere -> Exp VecF
scolor s = let (_, _, c, _, _) = unlift s :: (Exp VecF, Exp Float, Exp VecF, Exp Float, Exp Float)
           in lift c

reflection :: Exp Sphere -> Exp Float
reflection s = let (_, _, _, r, _) = unlift s :: (Exp VecF, Exp Float, Exp VecF, Exp Float, Exp Float)
               in lift r

transparency :: Exp Sphere -> Exp Float
transparency s = let (_, _, _, _, t) = unlift s :: (Exp VecF, Exp Float, Exp VecF, Exp Float, Exp Float)
                 in lift t

type Light = (VecF,  --position
              VecF)  --color

position :: Exp Light -> Exp VecF
position l = A.fst l

color :: Exp Light -> Exp VecF
color l = A.snd l

type Scene = (Vector Sphere, Vector Light)
type MultiBounceFactor = (Float, -- reflection
                          Float) -- refraction

nullMultiBounceFactor :: Exp MultiBounceFactor
nullMultiBounceFactor = constant $ (0.0, 0.0)

type MultiRay = (Ray, Ray, MultiBounceFactor)

nullMultiRay :: Exp MultiRay
nullMultiRay = lift (nullRay, nullRay, nullMultiBounceFactor) :: Exp MultiRay

objects :: Scene -> Vector Sphere
objects (spheres, lights) = spheres

lights :: Scene -> Vector Light
lights (spheres, lights) = lights

intersect :: Exp Sphere -> Exp Ray -> Exp Bool
intersect se re = 
                let 
                    rs = start re
                    cs = center se
                    sr = radius se
                    dr = dir re
                    v = cs -. rs
                    a = dot v dr
                    b2 = dot v v - a * a
                    r2 = sr * sr
                in
                (a <* 0 ||* b2 >* r2) ? (constant False, constant True)

normalizeSphereSurface :: Exp Sphere -> Exp VecF -> Exp VecF
normalizeSphereSurface s v = normalized (v -. (center s))

intersectDist :: Exp Ray -> Exp Sphere -> Exp SphereIntersect
intersectDist r s = 
                let
                    v = (center s) -. (start r)
                    a = dot v (dir r)
                    b2 = dot v v - a * a
                    r2 = (radius s) * (radius s)
                    c = sqrt(r2 - b2)
                    near = a - c
                    far = a + c
                    distance = (near <* 0) ? (far, near)
                in
                (a <* 0 ||* b2 >* r2) ? (
                    lift $ (constant False, s, constant (-1.0)),
                    lift $ (constant True, s, distance)
                )



predComp :: Exp SphereIntersect -> Exp SphereIntersect -> Exp SphereIntersect
predComp a b =  (b1 ==* cfalse  &&* b2 ==* cfalse) ? (a, 
                (b1 ==* cfalse  &&* b2 ==* ctrue)  ? (b,
                (b1 ==* ctrue   &&* b2 ==* cfalse) ? (a,
                (b1 ==* ctrue   &&* b2 ==* ctrue &&* d1 A.<* d2) ? (a, b))))
                where
                    (b1, s1, d1) = A.unlift a :: (Exp Bool, Exp Sphere, Exp Float)
                    (b2, s2, d2) = A.unlift b :: (Exp Bool, Exp Sphere, Exp Float)

minInterSects :: Scene -> Acc (Array DIM2 Ray) -> Acc (Array DIM2 SphereIntersect)
minInterSects s rays = let 
                        objs = objects s
                        usedObjs = use objs
                        dummySphere = ((0.0, 0, 0),
                                        0.0,
                                       (0.0, 0.0, 0.0),
                                        0.0,
                                        0.0)
                        dummyTuple = constant (False, dummySphere, -1.0)
                        k = size usedObjs
                        cols = A.replicate (lift $ Z :. All :. All :. k) rays
                        rows = A.replicate (lift $ Z :. width :. height :. All) usedObjs
                        intersects = A.fold predComp dummyTuple $ A.zipWith intersectDist cols rows
                      in
                        intersects
                        
colorforlight :: Scene -> Exp Sphere -> Exp VecF -> Exp VecF -> Exp Light -> Exp VecF
colorforlight s sph pip norm l = 
                             let 
                                lightpos = position l
                                lightdirection = normalized (lightpos -. pip)
                                blocked = False 
                                clr = ((color l) **. (P.max 0.0 (dot norm lightdirection))) 
                                        *. (scolor sph)  **. (1.0 - reflection sph)
                             in
                                if blocked then constant (0.0, 0.0, 0.0) else clr

traceStep :: Scene -> Acc (ArrayPlane Int) -> Acc (ArrayPlane Ray) -> Acc (ArrayPlane MultiBounceFactor) -> Acc (ArrayPlane (MultiRay, Int, VecF))
traceStep scene depths rays factors = let
                                    minintersects = minInterSects scene rays
                                    intersectsWithRays = A.zip minintersects rays
                                    usedLights = use $ lights scene
                                    l = size usedLights
                                    bounceCols = A.replicate (lift $ Z :. All   :. All    :. l) factors
                                    depthCols  = A.replicate (lift $ Z :. All   :. All    :. l) depths
                                    cols       = A.replicate (lift $ Z :. All   :. All    :. l) intersectsWithRays
                                    rows       = A.replicate (lift $ Z :. width :. height :. All) usedLights
                                in
                                    A.fold1 (\a b -> 
                                                let
                                                    (multiray1, depth1, color1) = unlift a :: (Exp MultiRay, Exp Int, Exp VecF)
                                                    (multiray2, depth2, color2) = unlift a :: (Exp MultiRay, Exp Int, Exp VecF)

                                                in
                                                    lift (multiray2, depth1 + depth2, color1 +. color2)
                                            )
                                            $
                                            A.zipWith4 (trace scene) depthCols cols rows bounceCols
infixl 6 $+.
($+.) :: Acc (ArrayPlane VecF) -> Acc (ArrayPlane VecF) -> Acc (ArrayPlane VecF)
($+.) a b = A.zipWith (+.) a b


infixl 6 $-.
($-.) :: Acc (ArrayPlane VecF) -> Acc (ArrayPlane VecF) -> Acc (ArrayPlane VecF)
($-.) a b = A.zipWith (-.) a b

infixl 7 $*.
($*.) :: Acc (ArrayPlane VecF) -> Acc (ArrayPlane Float) -> Acc (ArrayPlane VecF)
($*.) a b = A.zipWith (**.) a b

traceAll :: Acc (ArrayPlane Ray) -> Scene -> Acc (ArrayPlane VecF)
traceAll rays scene = let
                            color0 = generate (index2 (lift width) (lift height)) (\a -> (nullVector))
                            d0 = generate (index2 (lift width) (lift height)) (\a -> (constant 0))
                            factor0 = generate (index2 (lift width) (lift height)) (\a -> (constant (0.0, 0.0)))
                            ray0 = generate (index2 (lift width) (lift height)) (\a -> nullRay)
                            multiray0 = A.zip3 rays ray0 factor0

                            traceMapStep :: Acc (ArrayPlane VecF) -> 
                                            Acc (ArrayPlane MultiRay) -> 
                                            (Acc (ArrayPlane VecF), [(Acc (ArrayPlane VecF), Acc (ArrayPlane MultiRay))])
                            traceMapStep nc r = let
                                                        (reflectionRays, refractionRays, bounceFactors) = A.unzip3 r
                                                        (reflectionFactors, refractionFactors) = A.unzip bounceFactors

                                                        reflectionContribution = traceStep scene d0 reflectionRays bounceFactors 
                                                        (reflMultiRays, reflDepths, reflImage) = A.unzip3 reflectionContribution
                                                        (reflReflRays, reflRefrRays, reflBounceFactors) = A.unzip3 reflMultiRays

                                                        refractionContribution = traceStep scene d0 refractionRays bounceFactors
                                                        (refrMultiRays, refrDepths, refrImage) = A.unzip3 refractionContribution
                                                        (refrReflRays, refrRefrRays, refrBounceFactors) = A.unzip3 refrMultiRays
                                                        
                                                        --newAccImage = nc $+. reflImage $+. refrImage
                                                        newAccImage = nc -- $+. reflImage
                                                        newAccImageRefl = --newAccImage $+. 
                                                                          (reflImage $*. reflectionFactors)
                                                        newTotalAccImage = newAccImageRefl $+. 
                                                                           (refrImage $*. refractionFactors)
                                                        reflNewMultiRays = A.zip3 reflReflRays reflRefrRays reflBounceFactors
                                                        --refrNewMultiRays = A.zip3 refrReflRays refrRefrRays refrBounceFactors
                                                        newList = [(reflImage, reflNewMultiRays)] 
                                                        --P.++ [(refrImage, refrNewMultiRays)]
                                                   in
                                                        (newTotalAccImage, newList)

                            traceIter :: (Acc (ArrayPlane VecF), [(Acc (ArrayPlane VecF), Acc (ArrayPlane MultiRay))]) -> 
                                         (Acc (ArrayPlane VecF), [(Acc (ArrayPlane VecF), Acc (ArrayPlane MultiRay))])
                            traceIter (c0, accList) = let
                                                            (subImages, multiRays) = P.unzip accList
                                                            (newSubImages, raysAndImages) = P.unzip $ 
                                                                                            P.zipWith traceMapStep subImages multiRays
                                                            accImage = P.foldl ($+.) c0 subImages
                                                            totalAccImage = P.foldl ($+.) accImage newSubImages
                                                            newRayList = P.concat raysAndImages
                                                      in
                                                            (totalAccImage, newRayList)
                            --trace0 = traceMapStep color0 multiray0
                            trace0 = (color0, [(color0, multiray0)])
                            (accImage, _) = foldr ($) trace0 (Data.List.take maxdepth (repeat traceIter))
                            --(accImage, _) = traceIter trace0
                      in
                            accImage

-- Ray -> Sphere -> Normal -> dotNormal -> PointOfHit -> FresnelEffect -> (FresnelEffect, Ray)
reflectionFactorAndRay :: Exp Ray -> Exp Sphere -> Exp VecF -> Exp Float -> Exp VecF -> Exp Float -> Exp (Float, Ray)
reflectionFactorAndRay r s n dn pip f = lift (f, lift (pip, reflectiondirection) :: Exp Ray)
                                where
                                    reflectionRatio = reflection (lift s)
                                    facing = P.max 0.0 (-dn)
                                    reflectiondirection = (dir r) -. (n **. (2.0 * dn))

-- Ray -> Sphere -> Normal -> PointOfImpact -> Inside -> Fresnel -> Depth -> (RefractionEffect, Ray)
refractionFactorAndRay :: Exp Ray -> Exp Sphere -> Exp VecF -> Exp VecF -> Exp Bool -> Exp Float -> Exp Int -> Exp (Float, Ray)
refractionFactorAndRay r s n pip ins f d = let 
                                        nullRay = ((constant (0.0, 0.0, 0.0)), (constant (0.0, 0.0, 0.0)))
                                        nullElement = lift (constant 0, nullRay)
                                     in
                                        (transparency s >* 0.0) ?
                                                (
                                                                let 
                                                                    ce = (dot (dir r) n) * (-1.0)
                                                                    iorconst = constant 1.5 
                                                                    ior = ins ? (1.0 / iorconst, iorconst) :: Exp Float
                                                                    eta = 1.0 / ior
                                                                    gf = (dir r) +. (n **. (ce * eta))
                                                                    sin_t1_2 = 1.0 - ce * ce
                                                                    sin_t2_2 = sin_t1_2 * (eta * eta)
                                                                in
                                                                    (sin_t2_2 <* 1.0) ? (
                                                                        let 
                                                                            gc = n **. (sqrt 1 - sin_t2_2)
                                                                            refraction_direction = gf -. gc 
                                                                            refraction =  (1.0 - f) * (transparency s)
                                                                        in
                                                                        lift (refraction, (pip, refraction_direction)),
                                                                        nullElement
                                                                    ),
                                                nullElement)

trace :: Scene -> Exp Int -> Exp (SphereIntersect, Ray) -> Exp Light -> Exp (Float, Float)-> Exp (MultiRay, Int, VecF)
trace s d i l f = let 
                (reflectionFactor, refractionFactor) = unlift f :: (Exp Float, Exp Float)
                (intersectingSphere, ray) = unlift i :: (Exp SphereIntersect, Exp Ray)
                (hasIntersect, sp, di) = unlift intersectingSphere :: (Exp Bool, Exp Sphere, Exp Float)
                (rstart, rdir) = unlift ray :: (Exp VecF, Exp VecF)
              in
                --((ray /=* nullRay) &&* hasIntersect) ? (
                (hasIntersect) ? (
                                let 
                                    pointofhit = (dir ray) **. (lift di) +. start ray
                                    normal_unrefl = normalizeSphereSurface (lift sp) pointofhit
                                    dotnormalray_unrefl = dot normal_unrefl (dir ray)
                                    isinside = (dotnormalray_unrefl >* 0) ? (ctrue, cfalse)
                                    dotnormalray = (dotnormalray_unrefl >* 0) ? (-dotnormalray_unrefl, dotnormalray_unrefl)
                                    normal = (dotnormalray_unrefl >* 0) ? (normal_unrefl **. (-1.0), normal_unrefl)

                                    transparencyratio = transparency (lift sp)
                                    facing = P.max 0.0 (-dotnormalray)
                                    fresneleffect = reflectionFactor + (1.0 - reflectionFactor) * ((1.0 -facing) ^^ 5)
                                    clr = colorforlight s (lift sp) pointofhit (normal **. 1.0) l
                                    nullFloatRay = lift ((constant 0.0), nullRay) :: Exp (Float, Ray)

                                    (newReflectionFactor, reflectionRay) = unlift (
                                            (d <* constant maxdepth) ? (
                                                    --reflection
                                                    (reflectionFactor ==* 0.0) ? (  
                                                         nullFloatRay,
                                                         --calculate next ray direction and color factor for next trace 
                                                         reflectionFactorAndRay ray 
                                                                                sp 
                                                                                normal 
                                                                                dotnormalray 
                                                                                pointofhit
                                                                                fresneleffect),
                                                    nullFloatRay)) :: (Exp Float, Exp Ray)

                                    (newRefractionFactor, refractionRay) = unlift (
                                            (d <* constant maxdepth) ? (
                                                    --reflection
                                                    (refractionFactor ==* 0.0) ? (  
                                                             nullFloatRay,
                                                             --calculate next ray direction and color factor for next trace 
                                                             refractionFactorAndRay ray 
                                                                                    sp 
                                                                                    normal 
                                                                                    pointofhit
                                                                                    isinside 
                                                                                    fresneleffect 
                                                                                    d), 
                                                    nullFloatRay)) :: (Exp Float, Exp Ray)
                                in  
                                    lift (
                                          lift (
                                                reflectionRay, refractionRay, 
                                                lift (newReflectionFactor, newRefractionFactor) :: Exp MultiBounceFactor
                                               ) :: Exp MultiRay, 
                                            d + 1, clr),
                                    lift (nullMultiRay, d, nullVector)
                                )


updatePixel :: Index -> VecF -> IO ()
updatePixel p@(x, y) c@(r, g, b) = do
                        G.renderPrimitive G.Points $ do
                            G.color $ G.Color3 (CFloat r) (CFloat g) (CFloat b)
                            G.vertex $ Vertex3 (CFloat (P.fromIntegral x)) (CFloat (P.fromIntegral (height - y))) 0


constructRay :: Scene -> Exp VecF -> Exp Index -> Exp Ray
constructRay s eye idx =  let 
                            (x, y) = unlift idx :: (Exp Int, Exp Int)
                            h = constant $ (tan (fov / 360.0 * 2.0 * pi / 2.0)) * 2.0 
                            
                            ww = A.fromIntegral $ constant width :: Exp Float
                            hh = A.fromIntegral $ constant height :: Exp Float
                            w = h * ww / hh
                            rx = ((A.fromIntegral x) - ww/2.0) /ww * w
                            ry = (hh / 2.0 - (A.fromIntegral y)) / hh * h
                            dir = normalized $ lift (rx, ry, constant (-1.0))
                            ray = (lift (eye, dir)) 
                        in
                            ray


calcPixels :: Scene -> Exp VecF -> Acc (Array DIM2 Index) -> Acc (Array DIM2 VecF)
calcPixels s eye idx =  let 
                            rays = A.map (constructRay s eye) idx
                        in
                            traceAll rays s  

updateAllPixels :: Array DIM2 (Index, VecF) -> Int -> Int -> IO ()
updateAllPixels p i j
                | i >= width  = updateAllPixels p 0 (j+1)
                | j >= height = return ()
                | otherwise = 
                    do
                        let (idx, color) = p `indexArray` (Z :. i :. j)
                        updatePixel idx color
                        updateAllPixels p (i+1) (j)

render :: Scene -> IO ()
render s = do
                let 
                    eye = constant (0.0, 0.0, 0.0)
                    indices = A.generate (lift $ Z :. width :. height) unindex2 
                    pixels = calcPixels s eye indices
                    pixelsWithIndices = I.run $ A.zip indices pixels
                putStrLn "calculation done"
                updateAllPixels pixelsWithIndices 0 0
mainDbg :: IO ()
mainDbg = do    
          let scene = (        (fromList (Z :. 5) [
                               ((0.0, -10002.0, -20.0),
                                         10000.0,
                                         (0.8, 0.8, 0.8),
                                         0.0,
                                         0.0),
                               ((0.0, 2.0, -20.0),
                                         4.0,
                                         (0.8, 0.5, 0.5),
                                         0.5,
                                         0.0),
                               ((5.0, 0.0, -15.0),
                                         2.0,
                                         (0.3, 0.8, 0.8),
                                         0.2,
                                         0.0),
                               ((-5.0, 0.0, -15.0),
                                         2.0,
                                         (0.3, 0.5, 0.8),
                                         0.2,
                                         0.0),
                               ((-2.0, -1.0, -10.0),
                                         1.0,
                                         (0.1, 0.1, 0.1),
                                         0.1,
                                         0.8)
                               ]),
                              (fromList (Z :. 1) [
                                ((-10.0, 20.0, 30.0),
                                     (2.0, 2.0, 2.0))
                               ]) 
                        )
              eye = constant (0.0, 0.0, 0.0)
              indices = A.generate (lift $ Z :. width :. height) unindex2 
              pixels = calcPixels scene eye indices
              !pixelsWithIndices = I.run $ A.zip indices pixels
          putStrLn "calculation done"


mainNormal :: IO ()
mainNormal = do    
          (progname, _) <- G.getArgsAndInitialize
          w <- G.createWindow "Haskell raytracer"
          G.windowSize G.$= (G.Size (CInt (P.fromIntegral width)) (CInt (P.fromIntegral height)))
          let scene = (        (fromList (Z :. 5) [
                               ((0.0, -10002.0, -20.0),
                                         10000.0,
                                         (0.8, 0.8, 0.8),
                                         0.0,
                                         0.0),
                               ((0.0, 2.0, -20.0),
                                         4.0,
                                         (0.8, 0.5, 0.5),
                                         0.5,
                                         0.0),
                               ((5.0, 0.0, -15.0),
                                         2.0,
                                         (0.3, 0.8, 0.8),
                                         0.2,
                                         0.0),
                               ((-5.0, 0.0, -15.0),
                                         2.0,
                                         (0.3, 0.5, 0.8),
                                         0.2,
                                         0.0),
                               ((-2.0, -1.0, -10.0),
                                         1.0,
                                         (0.1, 0.1, 0.1),
                                         0.1,
                                         0.8)
                               ]),
                              (fromList (Z :. 1) [
                                ((-10.0, 20.0, 30.0),
                                     (2.0, 2.0, 2.0))
                               ]) 
                        )
          G.reshapeCallback G.$= Just Main.reshape
          G.displayCallback G.$= display scene
          G.mainLoop

main :: IO ()
main = mainDbg

reshape :: Size -> IO ()
reshape size@(Size w h) = do
               G.viewport G.$= (Position 0 0, size)
               G.matrixMode G.$= Projection
               G.loadIdentity
               ortho 0.0 (P.fromIntegral w) 0.0 (P.fromIntegral h) (-1.0) 1.0
               G.matrixMode G.$= Modelview 0

display :: Scene  -> IO ()
display s = do
  G.clear [G.ColorBuffer]
  render s
  G.swapBuffers

