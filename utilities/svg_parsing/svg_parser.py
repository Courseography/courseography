from bs4 import BeautifulSoup

from path import *
from rect import *
from region import *
from bool_node import *
from label import *

paths = []
rects = []
regions = []
region_labels = []
bools = []
final_rects = []

path_id_counter = 0
bool_id_counter = 0


def read_svg():
    global rects
    global paths
    global regions
    global bools

    with open('graph_regions.svg') as svg_file:
        content = svg_file.read()
        soup = BeautifulSoup(content)

    find_all_and_process(soup, 'path', process_path)
    find_all_and_process(soup, 'rect', process_rect)
    find_all_and_process(soup, 'ellipse', process_bool)
    find_all_and_process(soup, 'text', process_text)

    for rect_1 in rects:
        for rect_2 in rects:
            if (rect_2.x, rect_2.y, 0) in rect_1:
                if rect_1.text == '':
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
    print('svgDoc :: S.Svg')
    print('svgDoc = S.docTypeSvg ! A.id_ "graphRootSVG" ! A.version "1.1" ! S.customAttribute "viewBox" "0 0 1050 700"$ do')

    # Arrowhead def
    print('    S.defs $ do')
    print('        S.marker ! A.id_ "arrow" ! ' +
          'A.viewbox "0 0 10 10" ! '
          'A.refx "1" ! A.refy "5" ! A.markerunits "strokewidth" ! ' +
          'A.orient "auto" ! A.markerwidth "4.5" ! A.markerheight "4.5" $ do'
    )
    print('            S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"')

    print('    S.g $ do')
    list(map(output_region, regions))
    print('    S.g ! A.transform " translate(0,-308.2677)" $ do')
    print('        S.g ! A.transform "translate(29.540919,340.70929)" ! A.class_ "nodes"$ do')
    list(map(output_rect, final_rects))
    print('            S.g $ do')
    list(map(output_path, paths))
    print('            S.g $ do')
    list(map(output_bool, bools))
    print('    S.g ! A.transform "translate(-120,313.70929)" $ do')
    list(map(output_region_label, region_labels))


def output_region(region):
    print('        ', end='')
    region.output_haskell()


def output_rect(rect):
    print('            ', end='')
    rect.output_haskell()


def output_path(path):
    if not path.isPath:
        return
    print('                ', end='')
    path.output_haskell()


def output_bool(boolean):
    print('                ', end='')
    boolean.output_haskell()


def output_region_label(label):
    print('        ', end='')
    label.output_haskell()


def print_header():
    print('{-# LANGUAGE OverloadedStrings #-}')
    print('module SVGGen where')
    print('import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)')
    print('import qualified Text.Blaze.Svg11 as S')
    print('import qualified Text.Blaze.Svg11.Attributes as A')
    print('import Text.Blaze.Svg.Renderer.String (renderSvg)')


def process_path(elem):
    global path_id_counter
    if elem.parent.get('id') == 'layer3':
        regions.append(Region(elem.get('d'),
                              elem.get('style'),
                              'p' + str(path_id_counter)))
        path_id_counter += 1
    elif elem.parent.get('id') not in ['layer2', 'clipPath1', 'clipPath2']:
        paths.append(Path(elem.get('d'),
                          'p' + str(path_id_counter), -146, 288))
        path_id_counter += 1


def process_rect(elem):
    if elem is not None:
        width = elem.get('width')
        height = elem.get('height')
        x = elem.get('x')
        y = elem.get('y')
        transform = elem.parent.get('transform')
        parent_style = elem.parent.get('style') # Determine if hybrid
        new_rect = Rect(width, height, x, y, transform, 'a14c3a' in parent_style)
        rects.append(new_rect)


def process_text(elem):
    if elem is None or elem.get('x') is None or elem.get('y') is None:
        return

    found = False
    for rect in rects:
        if (elem.get('x'), elem.get('y'), 1) in rect:
            rect.text[elem.get('y')] = (elem.text)
            found = True

    for boolean in bools:
        if (elem.get('x'), elem.get('y'), 0) in boolean:
            boolean.text = elem.text
            found = True

    if not found:
        region_labels.append(Label(elem.text, elem.get('x'), elem.get('y'), elem.get('transform')))


def process_bool(elem):
    global bool_id_counter
    bools.append(BoolNode(elem.get('d'),
                          elem.get('cx'),
                          elem.get('cy'),
                          elem.get('rx'),
                          elem.get('ry'),
                          'bool' + str(bool_id_counter)))
    bool_id_counter += 1


if __name__ == '__main__':
    read_svg()
    output_svg()
