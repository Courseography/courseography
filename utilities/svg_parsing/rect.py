class Rect:

	def __init__(self, width, height, x, y, fill, transform, text):
		self.width = width
		self.height = height
		self.x = x
		self.y = y
		self.fill = fill
		self.text_x = float(x) + (float(width)/2)
		self.text_y = (float(y)) + (float(height)/2)
		self.text = text.replace('\n', '')
		self.parent_transform = transform

	def output_haskell(self):
		print("S.g ! A.transform \"" + self.parent_transform + "\" $ do \n"  + 
			  "             S.rect" +
		      " ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.x \"" + self.x +
		      "\" ! A.y \"" + self.y +
		      "\" ! A.stroke \"#ccc\" ! A.fill \"" + self.fill +
		      "\" \n" +
		      "             S.text_ " +
		      " ! A.x \"" + str(self.text_x) + 
		      "\" ! A.y \"" + str(self.text_y) +
		      "\" $ \"" + str(self.text) + "\"")