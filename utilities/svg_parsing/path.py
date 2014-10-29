class Path:

	def __init__(self, d):
		self.d = d

	def output_haskell(self):
		print("S.path ! A.d \"" + self.d + "\"")