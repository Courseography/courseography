class Path:

	def __init__(self, d, id_):
		self.d = d
		self.id = id_

	def output_haskell(self):
		print("S.path ! A.id_ \"" + 
			  self.id +
			  "\" ! A.class_ \"path\"" +
			  " ! A.style \"fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" ! A.d \"" + self.d + "\"")
