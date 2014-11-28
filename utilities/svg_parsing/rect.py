class Rect:

	def __init__(self, width, height, x, y, transform, style, hybrid):
		self.width =  width
		self.height =  height
		self.x = float(x) + float(transform[transform.find("(") + 1 : transform.find(",")])
		self.y = float(y) + float(transform[transform.find(",") + 1 : transform.find(")")])
		self.text_x = float(self.x) + (float(width)/2)
		self.text_y =  float(self.y) + (float(height)/2)
		self.parent_transform_x = float(transform[transform.find("(") + 1 : transform.find(",")])
		self.parent_transform_y = float(transform[transform.find(",") + 1 : transform.find(")")])
		self.text = ""
		self.style = style
		self.hybrid = hybrid
		self.colour = "#fff"
		self.class_ = "hybrid" if self.hybrid else "node"

	def output_haskell(self):
		if self.hybrid:
			self.colour = "#bbb"
		prefix = ""
		if not self.text[0].isalpha():
			prefix = "CSC"
		if self.hybrid:
			prefix = "h"
		print("S.g ! A.class_ \"" + self.class_ + "\" " +
			  " ! A.id_ \"" + prefix + self.text + 
			  "\" ! A.style \"" + "\" $ do \n"  + 
			  "             S.rect ! A.width \"" + self.width +
		      "\" ! A.height \"" + self.height + 
		      "\" ! A.rx \"4" + 
		      "\" ! A.ry \"4" +
		      "\" ! A.x \"" + str(self.x) +
		      "\" ! A.y \"" + str(self.y) +
		      "\" ! A.fill \"" + self.colour + "\" \n" + 
		      "             S.text_ " +
		      " ! A.x \"" + str(self.text_x) + 
		      "\" ! A.y \"" + str(self.text_y) +
		      '" $ "' +
		      self.text +
		      '"')

	def __contains__(self, coords):
		dx = float(coords[0]) - float(self.x) + (float(self.parent_transform_x) * coords[2])
		dy = float(coords[1]) - float(self.y) + (float(self.parent_transform_y) * coords[2])
		offset = 9
		return dx >= -1 * offset and \
		       dx <= float(self.width) + offset and \
		       dy >= -1 * offset and \
		       dy <= float(self.height) + offset
