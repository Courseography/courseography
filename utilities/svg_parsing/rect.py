class Rect:

	def __init__(self, width, height, x, y, transform, text, style):
		self.width =  width
		self.height =  height
		self.x =  x
		self.y =  y
		self.text_x = float(x) + (float(width)/2)
		self.text_y =  float(y) + (float(height)/2)
		self.parent_transform =  transform
		self.text = text
		self.style = style

	def output_haskell(self):
		print("S.g ! A.transform \"" + self.parent_transform + "\" ! A.style \"" + self.style + "\" $ do \n"  + 
			  "             S.rect" +
		      " ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.x \"" + self.x +
		      "\" ! A.y \"" + self.y +
		      "\" ! A.stroke \"#ccc\" ! A.fill \"#fff\" \n" + 
		      "             S.text_ " +
		      " ! A.x \"" + str(self.text_x) + 
		      "\" ! A.y \"" + str(self.text_y) +
		      '" $ "'+ self.text + '"')