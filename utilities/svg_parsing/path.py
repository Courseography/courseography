class Path:

	def __init__(self, d, id_):
		self.d = convert_path_to_absolute(d)
		self.id = id_

		coords = self.d.split(" ")
		if coords[-1] == "z":
			self.isPath = False
		else:
			self.isPath = True
		# coords = list(filter(lambda l: l != "m" and l != "l" and l != "M" and l != "L"), coords)
		# self.xStart = float()

	def output_haskell(self):
		print("S.path ! A.id_ \"" + 
			  self.id +
			  "\" ! A.class_ \"path\"" +
			  " ! A.style \"fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1\" ! A.d \"" + self.d + "\"")

def convert_path_to_absolute(d):
	path = d.split(" ")
	chars = ['m','l','z', "M"]
	for i in range(len(path)):
		if not path[i][0].isalpha():
			path[i] = path[i].split(",")
			path[i][0] = float(path[i][0])
			path[i][1] = float(path[i][1])

		if not path[i] in chars:
			if not path[i-1] in chars:
				path[i][0] = path[i][0] + path[i-1][0]
				path[i][1] = path[i][1] + path[i-1][1]

	for i in range(len(path)):
		if path[i] in chars:
			path[i] = path[i].upper()

	string = ""
	for i in path:
		if type(i) == list:
			string = string + str(i[0]) + "," + str(i[1]) + " "
		else:
			string = string + str(i) + " "
	return string
