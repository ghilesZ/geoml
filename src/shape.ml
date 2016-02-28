

type t =
  | Rectangle of Rectangle.t
  | RegPolygon of Polygon.Regular.t
  | Polygon of Polygon.t
  | Circle of Circle.t
  | Segment of Segment.t
  | Point of Point.t
  | Line of Line.t
  | Shapes of t list


let rec translate x y = function
  | Rectangle r -> Rectangle (Rectangle.translate x y r)
  | RegPolygon rp -> RegPolygon (Polygon.Regular.translate x y rp)
  | Polygon p -> Polygon (Polygon.translate x y p)
  | Circle c -> Circle (Circle.translate x y c)
  | Segment s -> Segment (Segment.translate x y s)
  | Point pt -> Point (Point.translate x y pt)
  | Line ln -> Line (Line.translate x y ln)
  | Shapes shs -> Shapes (List.map (translate x y) shs)




