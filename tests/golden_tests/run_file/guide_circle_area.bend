type Shape:
  Circle { radius }
  Rectangle { width, height }

def area(shape):
  match shape:
    case Shape/Circle:
      return 3.14 * shape.radius ** 2.0
    case Shape/Rectangle:
      return shape.width * shape.height

def main:
  return area(Shape/Circle { radius: 10.0 })