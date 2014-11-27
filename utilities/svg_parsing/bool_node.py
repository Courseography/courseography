class BoolNode:

	def __init__(self, d, cx, cy, rx, ry, id_):
		self.d = d
		self.cx = cx
		self.cy = cy
		self.rx = rx
		self.ry = ry
		self.id = id_

	def output_haskell(self):
		print("S.ellipse ! A.class_ \"bool\" ! A.id_ \"" + 
			  self.id + 
			  "\" ! A.d \"" +
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