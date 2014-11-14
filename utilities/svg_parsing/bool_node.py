class BoolNode:

	def __init__(self, d, cx, cy, rx, ry):
		self.d = d
		self.cx = cx
		self.cy = cy
		self.rx = rx
		self.ry = ry

	def output_haskell(self):
		print("S.ellipse ! A.d \"" +
		      self.d +
		      '" ! A.cx "' +
		      self.cx +
		      '" ! A.cy "' +
		      self.cy +
		      '" ! A.rx "' +
		      self.rx +
		      '" ! A.ry "' +
		      self.ry +
		      '"')