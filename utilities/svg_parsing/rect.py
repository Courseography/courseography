class Rect:

	def __init__(self, width, height, x, y, transform, style):
		self.width =  width
		self.height =  height
		self.x =  x
		self.y =  y
		self.text_x = float(x) + (float(width)/2)
		self.text_y =  float(y) + (float(height)/2)
		self.parent_transform =  transform
		self.text = ""
		self.style = style

	def output_haskell(self):
		print("S.g ! A.transform \"" + self.parent_transform + "\" ! A.style \"" + self.style + "\" $ do \n"  + 
			  "             S.rect" +
			  " ! A.class_ \"node\"" +
			  " ! A.id_ \"" + 
			  self.text + 
			  "\"" +
		      " ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.x \"" + self.x +
		      "\" ! A.y \"" + self.y +
		      "\" ! A.stroke \"#ccc\" ! A.fill \"#fff\" \n" + 
		      "             S.text_ " +
		      " ! A.x \"" + str(self.text_x) + 
		      "\" ! A.y \"" + str(self.text_y) +
		      '" $ "'+ self.text + '"')

	def __contains__(self, text):
		dx = float(text.get("x")) - float(self.x)
		dy = float(text.get("y")) - float(self.y)
		offset = 9
		return dx >= -1 * offset and \
		       dx <= float(self.width) + offset and \
		       dy >= -1 * offset and \
		       dy <= float(self.height) + offset
