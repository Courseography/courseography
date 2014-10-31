class Path:

	def __init__(self, d):
		self.d = d

	def output_haskell(self):
		print("S.path ! A.style \"fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" ! A.d \"" + self.d + "\"")