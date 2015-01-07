class BoolNode:

    def __init__(self, d, cx, cy, rx, ry, id_):
        self.d = d
        self.cx = float(cx) + -146 # translation should be a param,
        self.cy = float(cy) + 288 # translation should be a param,
        self.rx = rx
        self.ry = ry
        self.id = id_
        self.text = ''

    def output_haskell(self):
        print('S.g ! A.class_ "bool" ! A.id_ "' +
              self.id +
              '" $do\n                    S.ellipse ! A.d "' +
              self.d +
              '" ! A.cx "' +
              str(self.cx) +
              '" ! A.cy "' +
              str(self.cy) +
              '" ! A.rx "' +
              self.rx +
              '" ! A.ry "' +
              self.ry +
              '"\n                    S.text_ ' +
              ' ! A.x "' +
              str(self.cx) +
              '" ! A.y "' +
              str(self.cy) + '" $ do "' +
              self.text +
              '"')

    def __contains__(self, coords):
        dx = float(coords[0]) - float(self.cx) + - 146 # There should be no translation.
        dy = float(coords[1]) - float(self.cy	) + 288 # There should be no translation.
        offset = 9
        return dx >= -1 * offset and \
               dx <= float(2) + offset and \
               dy >= -1 * offset and \
               dy <= float(2) + offset
