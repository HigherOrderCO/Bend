object V2 { x, y }

def distance(a, b):
  open V2: a
  open V2: b
  dx = b.x - a.x
  dy = b.y - a.y
  return (dx * dx + dy * dy) ** 0.5

def main():
  return distance(V2 { x: 10.0, y: 10.0 }, V2 { x: 20.0, y: 20.0 })