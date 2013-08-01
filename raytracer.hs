{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE BangPatterns          #-}

import Graphics.Rendering.OpenGL.GL.Tensor
import Data.Array.Accelerate as A
import qualified Data.Array.Repa as R
import qualified Data.Array.Accelerate.CUDA as I
import Data.List hiding (intersect)
import Foreign.C.Types
import Foreign.Ptr
import Data.Int

import Data.Word
import qualified Graphics.UI.GLUT as G
import Graphics.Rendering.OpenGL.GL.CoordTrans

width = 640
height = 480
fov = 45.0
maxdepth = 2

type VectorF = Vertex3 Float
type VectorI = Vertex3 Int
type Vec a = (a, a, a)
type VecF = Vec Float
type SphereIntersect = (Bool, Sphere, Float)
type Index = (Int, Int)

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
                                --r = lift $ (pip, lightdirection)
                                --blocked = (I.run $ isblocked (objects s) r) `indexArray` Z
                                blocked = False 
                                clr = ((color l) **. (A.max 0.0 (dot norm lightdirection))) 
                                        *. (scolor sph)  **. (1.0 - reflection sph)
                             in
                                if blocked then constant (0.0, 0.0, 0.0) else clr

traceAll :: Array DIM2 Ray -> Scene -> Acc (Array DIM2 VecF)
traceAll rays scene = let
                            minintersects = minInterSects scene (use rays)
                            intersectsWithRays = A.zip minintersects (use rays)
                            usedLights = use $ lights scene
                            l = size usedLights
                            cols = A.replicate (lift $ Z :. All   :. All    :. l  ) intersectsWithRays
                            rows = A.replicate (lift $ Z :. width :. height :. All) usedLights
                      in
                            A.fold (+.) (constant (0.0, 0.0, 0.0)) $ 
                                A.zipWith (trace scene 0) cols rows
                            
traceBla :: Scene -> Int -> Exp (SphereIntersect, Ray) -> Exp Light -> Exp VecF
traceBla s d r l = let (p, c) = unlift l :: (Exp VecF, Exp VecF) in
                   lift $ c
                       

trace :: Scene -> Int -> Exp (SphereIntersect, Ray) -> Exp Light -> Exp VecF
trace s d r l = let 
                (intersectingSphere, ray) = unlift r :: (Exp SphereIntersect, Exp Ray)
                (hasIntersect, sp, di) = unlift intersectingSphere :: (Exp Bool, Exp Sphere, Exp Float)
                (rstart, rdir) = unlift ray :: (Exp VecF, Exp VecF)
              in
                ((hasIntersect) ? (
                                let 
                                    pointofhit = (dir ray) **. (lift di) +. start ray
                                    normal_unrefl = normalizeSphereSurface (lift sp) pointofhit
                                    dotnormalray_unrefl = dot normal_unrefl (dir ray)
                                    isinside = (dotnormalray_unrefl >* 0) ? (ctrue, cfalse)
                                    dotnormalray = (dotnormalray_unrefl >* 0) ? (-dotnormalray_unrefl, dotnormalray_unrefl)
                                    normal = (dotnormalray_unrefl >* 0) ? (normal_unrefl **. (-1.0), normal_unrefl)
                                    reflectionratio = reflection (lift sp)
                                    transparencyratio = transparency (lift sp)
                                    facing = A.max 0.0 (-dotnormalray)
                                    fresneleffect = reflectionratio + (1.0 - reflectionratio) * ((1.0 -facing) ^^ 5)
                                    clr = colorforlight s (lift sp) pointofhit (normal **. 1.0) l
                                    --clr = lift $ (fresneleffect, fresneleffect, fresneleffect)
                                in 

                                --reflection
                                --if (d < maxdepth) then 
                                --    let reflexclr =  if (reflectionratio > 0) then
                                --                        let reflectiondirection = (dir r) -. (normal *. 2.0 *. dotnormalray) in
                                --                        (trace (Ray pointofhit reflectiondirection) s (d + 1)) *. fresneleffect
                                --                     else
                                --                        fromListStatic [0.0, 0.0, 0.0]
                                --                     in
                                    --let refractclr = if transparencyratio > 0.0 then
                                    --                    let ce = (dot (dir r) normal) * (-1.0) in
                                    --                    let iorconst = 1.5 in
                                    --                    let ior = if isinside then 1.0 / iorconst else iorconst in
                                    --                    let eta = 1.0 / ior in
                                    --                    let gf = (dir r) +. (normal *.ce) *. eta in
                                    --                    let sin_t1_2 = 1.0 - ce * ce in 
                                    --                    let sin_t2_2 = sin_t1_2 * (eta * eta) in
                                    --                    if sin_t2_2 < 1.0 then
                                    --                      let gc = normal *. (sqrt 1 - sin_t2_2) in
                                    --                      let refraction_direction = gf -. gc in
                                    --                      let refraction = trace (Ray pointofhit refraction_direction) s (d + 1) in
                                    --                      refraction *. (1.0 - fresneleffect) *. (transparency sp)
                                    --                    else 
                                    --                      fromListStatic [0.0, 0.0, 0.0]
                                    --                  else
                                    --                    fromListStatic [0.0, 0.0, 0.0]
                                    --                  in
                                 --   let scaleMax = (fromIntegral (maxdepth - d - 1)) in
                                 --   let scaleMin = fromIntegral (maxdepth - 1) in
                                 --   let scale = scaleMax / scaleMin in
                                    --clr +. (reflexclr *. scale)
                                --    clr +. reflexclr
                                --else
                                --    fromListStatic [0.0, 0.0, 0.0]
                                --    clr
                                
                                    --lift (fresneleffect, fresneleffect, fresneleffect),
                                    --lift clr
                                    clr,
                                    constant (0.0, 0.0, 0.0)))


updatePixel :: Index -> VecF -> IO ()
updatePixel p@(x, y) c@(r, g, b) = do
                        G.renderPrimitive G.Points $ do
                            G.color $ G.Color3 (CFloat r) (CFloat g) (CFloat b)
                            G.vertex $ Vertex3 (CFloat (Prelude.fromIntegral x)) (CFloat (Prelude.fromIntegral y)) 0


constructRay :: Scene -> Exp VecF -> Exp Index -> Exp Ray
constructRay s eye idx =  let 
                            (x, y) = unlift idx :: (Exp Int, Exp Int)
                            h = constant $ (tan (fov / 360.0 * 2.0 * pi / 2.0)) * 2.0
                            
                            ww = A.fromIntegral $ constant width
                            hh = A.fromIntegral $ constant height
                            w = h * ww / hh
                            rx = ((A.fromIntegral x) - ww/2.0) /ww * w
                            ry = (hh / 2.0 - (A.fromIntegral y)) / hh * h
                            dir = normalized $ lift (rx, ry, constant (-1.0))
                            ray = (lift (eye, dir)) 
                        in
                            ray


calcPixels :: Scene -> Exp VecF -> Acc (Array DIM2 Index) -> Acc (Array DIM2 VecF)
calcPixels s eye idx =  let 
                            rays = I.run $ A.map (constructRay s eye) idx
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
                    indices = A.fromList (Z :. width :. height) [ (x, y) | x <- [0..width-1], y <- [0..height-1]] :: A.Array A.DIM2 Index
                    pixels = calcPixels s eye (use indices)
                    pixelsWithIndices = I.run $ A.zip (use indices) pixels
                putStrLn "calculation done"
                updateAllPixels pixelsWithIndices 0 0

main :: IO ()
main = do    
          (progname, _) <- G.getArgsAndInitialize
          w <- G.createWindow "Haskell raytracer"
          G.windowSize G.$= (G.Size (CInt (Prelude.fromIntegral width)) (CInt (Prelude.fromIntegral height)))
          let scene = (        (fromList (Z :. 4) [
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

reshape :: Size -> IO ()
reshape size@(Size w h) = do
               G.viewport G.$= (Position 0 0, size)
               G.matrixMode G.$= Projection
               G.loadIdentity
               ortho 0.0 (Prelude.fromIntegral w) 0.0 (Prelude.fromIntegral h) (-1.0) 1.0
               G.matrixMode G.$= Modelview 0

display :: Scene  -> IO ()
display s = do
  G.clear [G.ColorBuffer]
  render s
  G.swapBuffers

