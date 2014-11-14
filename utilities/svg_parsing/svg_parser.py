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

path_id_counter = 0;
bool_id_counter = 0;

def read_svg():
	with open("../../graph_regions.svg", "r") as svg_file:
		content = svg_file.read()
		soup = BeautifulSoup(content)

	find__all_and_process(soup, 'path', process_path)
	find__all_and_process(soup, 'text', process_rect)
	find__all_and_process(soup, 'ellipse', process_bool)

def find__all_and_process(soup, tag, fn):
	for elem in soup.find_all(tag):
		fn(elem)

def output_svg():
	print_header()
	print("svgDoc :: S.Svg")
	print("svgDoc = S.docTypeSvg ! A.version \"1.1\" ! A.width \"1052.3622\" ! A.height \"744.09448\" $ do")
	
	print("    S.g $ do")
	for i in regions:
		print("        ", end="")
		i.output_haskell()

	print("    S.g ! A.transform \" translate(0,-308.2677)\" $ do")
	print("        S.g ! A.transform \"translate(29.540919,340.70929)\" ! A.class_ \"nodes\"$ do")
	for i in rects:
		print("            ", end="")
		i.output_haskell()
	
	print("            S.g ! A.transform \"translate(-146,288)\" $ do")
	for i in paths:
		print("                ", end="")
		i.output_haskell()

	print("            S.g ! A.transform \"translate(-146,288)\" $ do")
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
		                  "p" + str(path_id_counter)))
		path_id_counter += 1

def process_rect(elem):
	rect = elem.parent.find_previous_sibling().find("rect")
	if rect == None:
		return
	width = rect.get("width")
	height = rect.get("height")
	x = rect.get("x")
	y = rect.get("y")
	transform = elem.parent.get("transform")
	style = elem.parent.get("style")
	text = elem.text
	rects.append(Rect(width, height, x, y, transform, text, style))

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