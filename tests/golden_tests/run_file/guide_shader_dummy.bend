# given a shader, returns a square image
def render(depth, shader):
  bend d = 0, i = 0:
    when d < depth:
      color = (fork(d+1, i*2+0), fork(d+1, i*2+1))
    else:
      width = depth / 2
      color = shader(i % width, i / width)
  return color

# given a position, returns a color
# for this demo, it just busy loops
def demo_shader(x, y):
  bend i = 0:
    when i < 10:
      color = fork(i + 1)
    else:
      color = 0x000001
  return color

# renders a 256x256 image using demo_shader
def main:
  return render(5, demo_shader)