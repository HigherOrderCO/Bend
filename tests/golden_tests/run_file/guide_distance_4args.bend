def distance(ax, ay, bx, by):
  dx = bx - ax
  dy = by - ay
  return (dx * dx + dy * dy) ** 0.5

def main():
  return distance(10.0, 10.0, 20.0, 20.0)