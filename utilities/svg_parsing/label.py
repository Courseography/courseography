class Label:

    def __init__(self, text, x, y, transform):
        if transform is None:
            transform = "translate(0,0)"
        self.x = float(x) + float(transform[transform.find("(") + 1: transform.find(",")])
        self.y = float(y) + float(transform[transform.find(",") + 1: transform.find(")")])
        self.text = text

    def output_haskell(self):
        print("S.text_ ! A.class_ \"region-label\"" +
              " ! A.x \"" + str(self.x) + "\"" +
              " ! A.y \"" + str(self.y) + "\"" +
              " $ \"" + self.text + "\"")
