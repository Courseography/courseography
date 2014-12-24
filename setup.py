import os
os.system("python3 ./utilities/svg_parsing/svg_parser.py > ./hs/SVGGen.hs")
os.system("runhaskell ./hs/cssGen.hs")
