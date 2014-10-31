from bs4 import BeautifulSoup

from path import *
from rect import *
paths = []
rects = []

def read_svg():
	with open("../../graph_regions.svg", "r") as svg_file:
		content = svg_file.read()

		soup = BeautifulSoup(content)

	for elem in soup.find_all('path'):
		process_path(elem)

	for elem in soup.find_all('rect'):
		process_rect(elem)

def output_svg():
	print_header()
	print("svgDoc :: S.Svg")
	print("svgDoc = S.docTypeSvg ! A.version \"1.1\" ! A.width \"1052.3622\" ! A.height \"744.09448\" $ do")


	print("    S.g $ do")
	for i in rects:
		print("        ", end="")
		i.output_haskell()
	
	print("    S.g ! A.transform \"translate(-146,288)\" $ do")
	for i in paths:
		print("        ", end="")
		i.output_haskell()

def print_header():
	print("{-# LANGUAGE OverloadedStrings #-}")
	print("module SVGGen where")
	print("import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)")
	print("import qualified Text.Blaze.Svg11 as S")
	print("import qualified Text.Blaze.Svg11.Attributes as A")
	print("import Text.Blaze.Svg.Renderer.String (renderSvg)")


def process_path(elem):
			paths.append(Path(elem.get("d")))

def process_rect(elem):
			width = elem.get("width")
			height = elem.get("height")
			x = elem.get("x")
			y = elem.get("y")
			transform = elem.parent.get("transform")
			rects.append(Rect(width, height, x, y, transform))

if __name__ == "__main__":
	read_svg()
	output_svg()