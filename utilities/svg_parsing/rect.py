class Rect:

	def __init__(self, width, height, x, y):
		self.width = width
		self.height = height
		self.x = x
		self.y = y

	def output_haskell(self):
		print("S.g $ do \n             S.rect" +
		      " ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.x \"" + self.x +
		      "\" ! A.y \"" + self.y +
		      "\" ! A.fill \"#000\"")