def random//constant():
  return 3.14

def //thisshouldfail():
  return random//constant()

def main:
  return //thisshouldfail()
