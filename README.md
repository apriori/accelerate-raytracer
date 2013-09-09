accelerate-raytracer
====================

![ScreenShot](https://raw.github.com/apriori/accelerate-raytracer/master/results.png)

A simple raytracer written in haskell accelerate EDSL
based upon 
    https://gist.github.com/AdrianV/5774141, which is based upon 
    http://forum.dlang.org/thread/yzsqwejxqlnzryhrkfuq@forum.dlang.org#post-yzsqwejxqlnzryhrkfuq:40forum.dlang.org

Dependencies are OpenGL, GLUT, accelerate and accelerate-cuda

So far not everything is working properly (shadow tests) and performance is way inferior than even a sequential CPU-only solution. I consider this my playground for haskell and GPU computing. Currently a complete rewrite of the raytracer is still pending.
