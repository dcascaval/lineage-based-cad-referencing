
# CAD DSL Documentation

We provide an overview of the DSL program structure, available operations and their signatures, and any syntactic differences from the presentation of the DSL in the paper text. We encourage the reader to explore the examples, and to treat this document primarily as a reference.

All geometric elements (e.g. points, edges, polygons, faces, solids) are subtypes of `geometry`. DSL programs begin with a parameter block, which declares parameters for the purposes of re-running the program with a slider: 

```scala
parameters { 
  x = 50
  y = 10 < 25 < 100
}
```

creates two sliders for X and Y with default values `50` and `25` respectively, bounding the `min` and `max` values of `y` to `10` and `100`. Sliders always represent real values, and cannot be integers. Integers can be used, but must be declared inline and are considered to be constant. 

## Types

Points expose:
- `.x`, `.y`

Edges expose the following properties: 
- `.start`, `.end`, `.midpoint` 
- `.at : (t: Number) -> Point` (evaluate the edge at a normalized paramter)

Polygons expose the following sets:
- `.points`, `.edges`

Faces expose the following sets: 
- `.points`, `.edges`
- `.polygons`, `.positivePolygons`, `.negativePolygons`. Positive polygons denote outer contours, negative polygons denote holes.

Solids expose the following sets: 
- `.points`, `.basePoints`, `.outerPoints`
- `.edges`, `.baseEdges`, `.outerEdges`, `.verticalEdges`
- `.faces`, `.baseFace`, `.outerFace`, `.verticalFaces`

## Primitives

`pt : Number, Number -> Point`
`Line : Point, Point -> Edge`
`Square(base: Point, sideLength: Number) -> Rectangle`
`Rectangle: (base: Point, width: Number, height: Number) -> Rectangle`. Width and height can both be negative. `Rectangle` is a subtype of `Polygon`, and exposes the following additional properties: 
 - `top`, `bottom`, `left`, `right` for its 4 edges
 - `topLeft`, `bottomLeft`, `topRight`, `bottomRight` for its 4 vertices
 - `center` to return the center point (no lineage)

`Circle : (center: Point, radius: Number) -> Circle`. Radius cannot be negative. `Circle` is a subtype of `Polygon` and exposes an additional `center` property.

`Polygon : (center: Point, numSides: Int, radius: Number) -> Polygon` creates a regular polygon. Each element of this polygon _can_ be safely indexed since the number of sides is polygon, allowing us to do `hex.points(0)`. Attempting to do this kind of indexing on non-constant sets will raise a type error.

## CSG and CAD operations

- `Union : (Polygon|Face, Polygon|Face) -> Face`
- `Difference : (Polygon|Face, Polygon|Face) -> Face`
- `Intersection : (Polygon|Face, Polygon|Face) -> Face`

In all cases, these operations produce `Face`, because any of them may end up producing a polygon "with a hole" or with two disconnected positive elements. For the common case that we want to assert that a face has only a single polygon in `polygons` (and to work with that polygon), the `.single()` method is offered as a postfix to provide this assertion, e.g. 

```scala
g = Union(a, b).single() // Assert single polygon 
Fillet(g, g.points,...) // Use the polygon
```

We also provide the following operations found in modern CAD systems: 
- `Chamfer : (p: Polygon, pts: Set[Point], distance: Number) -> Polygon`. Trims off a corner and errors out if the trim is too large to be applied or would split the polygon. Will do nothing if given points not in `p`.
- `Fillet : (p: Polygon, pts: Set[Point], radius: Number) -> Polygon`. Rounds the polygon using a circular arc of `radius`, similar to Chamfer. 
- `ExtrudeCurve : (e: Edge, length: Number) -> ExtrudedCurve`. Returns the positive created geometry from an extrude (just the extruded rectangle, which may have arc segments if `e` is an arc). Exposes `.top`, `.bottom`, `.front`, `.back` for edges.
- `ExtrudeEdge : (p: Polygon, e: Edge, length: Number) -> ExtrudedPolygon`. Creates the extruded geometry (vis. `ExtrudeCurve`) and unions it with `p`. Returns a subtype of Face that additionally exposes `.positive` field that contains the extruded geometry prior to union. Does not error if the extrusion causes the polygon to now have holes, etc.
- `ExtrudeEdge : (p: Polygon, e: Edge, length: Number, startParameter: Number, endParameter: Number) -> ExtrudedPolygon`. Extrudes only the segment of the edge from `e.at(startParameter)` to `e.at(endParameter)`, allowing us to extrude sub-segments - e.g. to extrude the first half of an edge, `startParameter = 0.0` and `endParameter = 0.5`.
- `InsetEdge : (p: Polygon, e: Edge, length: Number) -> InsetPolygon`. Extrudes inwards - differences instead of unions after creating extruded geometry. Exposes a `.negative` field that contains the geometry prior to difference.
- `InsetEdge : (p: Polygon, e: Edge, length: Number, startParameter: Number, endParameter: Number) -> InsetPolygon`

## 3D Elements

Solids can be created by
- `Extrude3D : (area: Polygon|Face, length: Number) -> Solid`
- `Revolve : (area: Polygon|Face, axis: Edge [, startAngle: Number, endAngle: Number]) -> Solid`. Start and end angles are given in degrees and default to 0 and 360 respectively.

## Transforms

Every 2D element of type `T` subtype of  `geometry` can be transformed via methods: 
- `.translate : Point => T`
- `.rotate : (center: Point, angle: Number) => T` (in radians)
- `.rotateDeg : (center: Point, angle: Number) => T` (in degrees)
- `.scale : (center: Point, factor: Number) => T`
- `.mirror : (axis: Edge) => T` 
- `.mirror : (axisStart: Point, axisEnd: Point) => T`

3D elements (solids and their constituents), have transforms with slightly different signatures, to enable 3D transformation:
- `.translate : (x: Number, y: Number, z: Number) -> T`
- `.translateX : Number -> T` (analogously, `translateY`, `translateZ`)
- `.move : (start: Point3, end: Point3) -> T` (move by the vector `end-start`)
- `.rotate: (axisStart: Point, axisEnd: Point, angle: Number) -> T` rotate around an axis
- `.rotateX: (angle: Number) -> T` rotates around X axis in world space. (analogously, `rotateY`, `rotateZ`)

## Queries 

`query: (s: Set[T], q: Query) -> Set[T]` resolves a constructed object of type `Query` on a set, filtering it. The result set may be empty, but will never contain any elements not in `s`. For example, the basic structure of query usage:

```scala
result = query(b.points, from(a))  // Construct query object and execute query
point = single(result) // Assert set is of size 1, and extract the value
print(point.x) // Use the specific value.
```

Other usages:
```scala
query(b.points, fromAny(a.points, c)) // A point that is from any of a.points, or from c
query(a.polygons, from(f) and not(from(g))) // infix syntax, select a polygon from a face
query(e.points, derivedFromAll(f1, f2)) // Refer to derived lineage edges
```

Query objects can be constructed using the base query operators described in the paper:
- `from: geo -> Query`
- `fromAny: (geometry | Set[geometry])* -> Query`
- `fromAll: (geometry | Set[geometry])* -> Query`
- `derivedFromAny: (geometry | Set[geometry])* -> Query`
- `derivedFromAll: (geometry | Set[geometry])* -> Query`
- `contains: query -> Query` 
- `and: Query* -> Query` 
- `or: Query* -> Query` 
- `not: Query -> Query` 

Query combinators additionally allow for an infix syntax to be used; e.g. `from(l1) and from(l2)`, equivalent to `and(from(l1), from(l2))`.

## Assertions

We offer two assertions: 
- `single : (set: Set[T]) -> T` asserts that `set` has one element and returns it.
- `empty: (set: Set[T]) -> ()` asserts that `set` has no elements.

## Sequence Operations

It is occasionally useful to construct _indexable_ sequences (of constant length and fixed order), such as the points created by `Polygon`. We provide two constructs: 

- `Tabulate : (n: Int, f: Int => T) -> Seq[T]`. Returns an indexable sequence of size `n`, which can be accessed by calling it with an integer, e.g. `seq(2)`.
- `Map : (s: Set[T], f: T => Q) -> Set[Q]`. Applies `f` to each element in `s`. If `s` is indexable, the result of map will also be indexable.

## Drawing & Printing 

`draw : (geometry | style)* -> ()`. `draw` can be called on any geometry or set of geometry to visualize it in the viewport. Objects are drawn as black by default, which can be modified by including styles in the argument list. The results of `draw` are completely independent of argument order. For example: 

```scala
draw(U.points, color(red)) // Draw a set of points in red
draw(U.top, style(dotted)) // Draw a single edge, dashed
draw(A.points, A.edges, B.points, color(orange)) // Draw several sets in orange.
```

"Styles" can be created via specific calls, namely:
  - `style(translucent(<opacity>))`, where `opacity` is a float from 0.0 to 1.0, draws the objects with the given opacity 
  - `style(dotted)` elides drawing faces, and instead draws all edges, using a dashed stroke.
  - `color(<color>)`, where color is an identifier that corresponds to any CSS color string 


`print : (any*) -> ()` can be called to inspect any object in the language by printing its runtime value to the browser console. Additionally, the method `typ` can be used to extract a string of any runtime value's type, which can be then printed, e.g. `print(u.points.typ)` will print `Set(Point)`.


## Lambdas and Operations 
To aid in code re-use we also provide basic functions. One can create a first class function, which captures any variables at the time it is defined:

```scala
c = 3
foo = (a, b) => 
  a + b + c
c = 14 // Shadows, not captured
bar = foo(1, 2) // bar = 6
```

Additionally, if there are multiple variables in a scope that one wants to capture, one can create an operation: 
```scala
c = 3
operation foo(a,b) { 
  x = a + b + c
  r = x * x
}
bar = foo(1,2)
print(bar.x) // prints 6
print(bar.r) // prints 36
```


