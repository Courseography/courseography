AREAS = {
    'theory': ['CSC165', 'CSC236', 'CSC240', 'CSC263', 'CSC265',
               'CSC310', 'CSC324', 'CSC373', 'CSC438', 'CSC448',
               'CSC463'],
    'core': ['CSC108', 'CSC148', 'CSC104', 'CSC120', 'CSC490',
             'CSC491', 'CSC494', 'CSC495'],
    'se': ['CSC207', 'CSC301', 'CSC302', 'CSC410'],
    'systems': ['CSC209', 'CSC258', 'CSC358', 'CSC369', 'CSC372',
                'CSC458', 'CSC469', 'CSC488', 'ECE385', 'ECE489'],
    'hci': ['CSC200', 'CSC300',  'CSC318', 'CSC404', 'CSC428',
            'CSC454'],
    'graphics': ['CSC320', 'CSC418', 'CSC420'],
    'num': ['CSC336', 'CSC436', 'CSC446', 'CSC456'],
    'ai': ['CSC321', 'CSC384', 'CSC401', 'CSC411', 'CSC412',
           'CSC485', 'CSC486'],
    'dbweb': ['CSC309', 'CSC343', 'CSC443']
}

class Rect:

    def __init__(self, width, height, x, y, transform, hybrid):
        self.width = width
        self.height = height
        self.x = float(x) + float(transform[transform.find('(') + 1: transform.find(',')])
        self.y = float(y) + float(transform[transform.find(',') + 1: transform.find(')')])
        self.text_x = float(self.x) + (float(width)/2)
        self.text_y = float(self.y) + (float(height)/2)
        self.parent_transform_x = float(transform[transform.find('(') + 1: transform.find(',')])
        self.parent_transform_y = float(transform[transform.find(',') + 1: transform.find(')')])
        self.text = '' # Text is set later.
        self.extra_text = '' # Some hybrids require two lines of text, and SVG text elements do not
                             # wrap around, nor do they respect the newline character.
        self.hybrid = hybrid
        self.colour = '#fff'
        self.class_ = 'hybrid' if self.hybrid else 'node'

    def output_haskell(self):
        if self.hybrid:
            self.colour = "#bbb"
        prefix = ""
        if self.text == '385' or self.text == '489':
            prefix = 'ECE'
        elif not self.text[0].isalpha():
            prefix = "CSC"
        if self.hybrid:
            prefix = "hCSC"

        #Figure out the research area
        code = (prefix + self.text + self.extra_text)[:6]
        self.area = 'core'
        for area, courses in AREAS.items():
            if code in courses:
                self.area = area
                break

        

        if len(self.extra_text) == 0:
            text = ("             S.text_ " +
                   " ! A.x \"" + str(self.text_x) +
                   "\" ! A.y \"" + str(self.text_y) +
                   '" $ "' +
                   self.text +
                   '"')
        else:
            text = ("             S.text_ " +
                   " ! A.x \"" + str(self.text_x) +
                   "\" ! A.y \"" + str(self.text_y - float(self.height)/4) +
                   '" $ "' +
                   self.text +
                   '"\n'
                   "             S.text_ " +
                   " ! A.x \"" + str(self.text_x) +
                   "\" ! A.y \"" + str(self.text_y + float(self.height)/4) +
                   '" $ "' +
                   self.extra_text +
                   '"')

        print("S.g ! A.class_ \"" + self.class_ + "\" " +
              " ! A.id_ \"" + prefix + self.text + self.extra_text + "\""
              " ! S.dataAttribute \"group\" \"" + self.area + "\""
              " ! A.style \"" + "\" $ do \n"  +
              "             S.rect ! A.width \"" + self.width +
              "\" ! A.height \"" + self.height +
              "\" ! A.rx \"4" +
              "\" ! A.ry \"4" +
              "\" ! A.x \"" + str(self.x) +
              "\" ! A.y \"" + str(self.y) +
              "\" ! A.fill \"" + self.colour + "\" \n" +
              text
              )

    def __contains__(self, coords):
        dx = float(coords[0]) - float(self.x) + (float(self.parent_transform_x) * coords[2]) # Coords[2] is 0 if translation should not be applied, 1 if it should. Very hacky.
        dy = float(coords[1]) - float(self.y) + (float(self.parent_transform_y) * coords[2]) # Coords[2] is 0 if translation should not be applied, 1 if it should. Very hacky. Text elements are the only elements that require this.
        offset = 9
        return (-1 * offset <= dx <= float(self.width) + offset and
			    -1 * offset <= dy <= float(self.height) + offset)
