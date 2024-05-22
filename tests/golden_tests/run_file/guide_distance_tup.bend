def distance(a, b):
  (ax, ay) = a
  (bx, by) = b
  dx = bx - ax
  dy = by - ay
  return (dx * dx + dy * dy) ** 0.5

def main():
  return distance((10.0, 10.0), (20.0, 20.0))