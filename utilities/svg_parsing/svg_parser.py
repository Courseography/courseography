from bs4 import BeautifulSoup
import sys
from path import *
from rect import *
from region import *
from bool_node import *
paths = []
rects = []
regions = []
bools = []
final_rects = []

path_id_counter = 0;
bool_id_counter = 0;

def read_svg():
	global rects
	global paths
	global regions
	global bools

	with open("./graph_regions.svg", "r") as svg_file:
		content = svg_file.read()
		soup = BeautifulSoup(content)

	find_all_and_process(soup, 'path', process_path)
	find_all_and_process(soup, 'rect', process_rect)
	find_all_and_process(soup, 'ellipse', process_bool)
	find_all_and_process(soup, 'text', process_text)

	for rect_1 in rects:
		for rect_2 in rects:

			if (rect_2.x, rect_2.y, 0) in rect_1:
				# rect_1.hohoho = rect_1.hohoho or rect_2.hohoho
				# rect_2.hohoho = rect_1.hohoho or rect_2.hohoho
				if rect_1.text == "":
					rects.remove(rect_1)
					final_rects.append(rect_2)
				else:
					rects.remove(rect_2)
					final_rects.append(rect_1)

def find_all_and_process(soup, tag, fn):
	for elem in soup.find_all(tag):
		fn(elem)

def output_svg():
	print_header()
	print("svgDoc :: S.Svg")
	print("svgDoc = S.docTypeSvg ! A.id_ \"graphRootSVG\" ! A.version \"1.1\" ! S.customAttribute \"viewBox\" \"0 0 1050 700\"$ do")

	print("    S.g $ do")
	for i in regions:
		print("        ", end="")
		i.output_haskell()

	print("    S.g ! A.transform \" translate(0,-308.2677)\" $ do")
	print("        S.g ! A.transform \"translate(29.540919,340.70929)\" ! A.class_ \"nodes\"$ do")
	for i in final_rects:
		print("            ", end="")
		i.output_haskell()

	print("            S.g $ do")
	for i in paths:
		if not i.isPath:
			continue
		print("                ", end="")
		i.output_haskell()

	print("            S.g $ do")
	for i in bools:
		print("                ", end="")
		i.output_haskell()


def print_header():
	print("{-# LANGUAGE OverloadedStrings #-}")
	print("module SVGGen where")
	print("import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)")
	print("import qualified Text.Blaze.Svg11 as S")
	print("import qualified Text.Blaze.Svg11.Attributes as A")
	print("import Text.Blaze.Svg.Renderer.String (renderSvg)")


def process_path(elem):
	global path_id_counter
	if elem.parent.get("id") == "layer3":
		regions.append(Region(elem.get("d"),
			                  elem.get("style"),
			                  "p" + str(path_id_counter)))
		path_id_counter += 1

	elif elem.parent.get("id") == "layer2":
		pass

	elif elem.parent.get("id") == "clipPath1":
		pass

	elif elem.parent.get("id") == "clipPath2":
		pass
	else:
		paths.append(Path(elem.get("d"),
		                  "p" + str(path_id_counter), -146, 288))
		path_id_counter += 1

def process_rect(elem):
	rect = elem
	if rect == None:
		return
	width = rect.get("width")
	height = rect.get("height")
	x = rect.get("x")
	y = rect.get("y")
	transform = elem.parent.get("transform")
	parent_style = elem.parent.get("style")
	style = elem.parent.get("style")
	new_rect = Rect(width, height, x, y, transform, style, "a14c3a" in parent_style)
	rects.append(new_rect)

def process_text(elem):
	if elem == None or elem.get("x") == None or elem.get("y") == None:
		return

	for rect in rects:
		if (elem.get("x"), elem.get("y"), 1) in rect:
			text = elem.text
			if "/" in text:
				text = text[:text.index("/")]
			if "," in text:
				text = text[:text.index(",")]
			rect.text = text
			
	for boolean in bools:
		if (elem.get("x"), elem.get("y"), 0) in boolean:
			boolean.text = elem.text

def process_bool(elem):
	global bool_id_counter
	bools.append(BoolNode(elem.get("d"),
	                      elem.get("cx"),
	                      elem.get("cy"),
	                      elem.get("rx"),
	                      elem.get("ry"),
	                      "bool" + str(bool_id_counter)))
	bool_id_counter += 1

if __name__ == "__main__":
	read_svg()
	output_svg()
