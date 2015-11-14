# GeoML

GeoMl is 2D geometry library for Ocaml. It focuses on euclidean geometry
and provides basic types and operations over geometric shapes.
It also implements several cool algorithms :

* Emo Welzl's smallest enclosing disk algorithm

![ws](img/welzl.gif)
* Graham's scan method for finding a convex hull.
* Weiler-Atherton for polygon clipping

You can see some examples of application in the **tests** directory (You'll need **Graphics**)
### Build 
- The library: 
```sh 
make
```
- The documentation 
```sh
$ make doc 
```
- The tests 
```sh
$ make tests
```

### Current
GeoML is currently still in developpement, and have not been tested -nor proved, nor model-checked nor analyzed ... but you know what we say about well typed program ;) -. Thereby, you'll may find some weird behaviors or failures. Feel free to let us know or report an issue about it.

You are also welcome to contact any member of the developpement team if you want to suggest a feature you think it may be cool to have in GeoML.
