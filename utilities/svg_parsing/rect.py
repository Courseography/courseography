class Rect:

	def __init__(self, width, height, x, y, transform):
		self.width = width
		self.height = height
		self.x = x
		self.y = y
		self.parent_transform = transform

	def output_haskell(self):
		print("S.g ! A.transform \"" + self.parent_transform + "\" $ do \n             S.rect" +
		      " ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.x \"" + self.x +
		      "\" ! A.y \"" + self.y +
		      "\" ! A.fill \"#000\"")