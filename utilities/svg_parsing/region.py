class Region:

    def __init__(self, d, style, id_):
        self.id = id_
        self.d = d
        self.style = style

    def output_haskell(self):
        print('S.path ! A.class_ "region"' +
              ' ! A.style "' +
              self.style +
              '" ! A.d "' +
              self.d +
              '"')
