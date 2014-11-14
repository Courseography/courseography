class Region:

	def __init__(self, d):
		self.d = d

	def output_haskell(self):
		print("S.path ! A.style \"fill:#222222;fill-opacity:1;stroke:none;display:inline\" ! A.d \"" + self.d + "\"")