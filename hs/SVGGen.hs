{-# LANGUAGE OverloadedStrings #-}
module SVGGen where
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.id_ "graphRootSVG" ! A.version "1.1" ! S.customAttribute "viewBox" "0 0 1050 700"$ do
    S.defs $ do
        S.marker ! A.id_ "arrow" ! A.viewbox "0 0 10 10" ! A.refx "1" ! A.refy "5" ! A.markerunits "strokewidth" ! A.orient "auto" ! A.markerwidth "4.5" ! A.markerheight "4.5" $ do
            S.polyline ! A.points "0,1 10,5 0,9" ! A.fill "black"
    S.g $ do
        S.path ! A.style "fill:#f298ff;fill-opacity:0.54497358;stroke:none;display:inline" ! A.d "m 872.9544,541.69048 0,-117.849 -27.28329,0 -23.37349,0 -31.23745,0 -50.21917,0 0,-186.49687 0,-77.23323 104.46025,0 85.47853,0 107.79952,0.24101 -0.8962,185.02361 0,157.36769 0,190.70079 -84.31924,0 -80.40946,0 0,-151.75398 z"
        S.path ! A.style "fill:#ff008f;fill-opacity:0.56613758;stroke:none;display:inline" ! A.d "m 740.84103,551.62569 -3e-5,-127.78412 59.21055,0 72.25845,-9e-5 0,127.78433 0,141.81867 -72.25845,0 -59.21055,0 z"
        S.path ! A.style "fill:#1c00a2;fill-opacity:0.54497358;stroke:none;display:inline" ! A.d "m 476.806,553.53262 0,-104.82456 131.57889,0 132.45611,4.2e-4 0,104.824 0,139.912 -132.45611,0 -131.57889,0 z"
        S.path ! A.style "fill:#c60000;fill-opacity:0.58730164;stroke:none;display:inline" ! A.d "m 476.806,389.71338 0,-59.16632 0,-85.02982 0,-85.02984 210.15111,-0.23584 53.88389,-0.23608 0,144.43221 0,144.43199 -108.57168,0 -155.46332,0 z"
        S.path ! A.style "fill:#c6e1ff;fill-opacity:0.58730164;stroke:none;display:inline" ! A.d "m 298.945,563.90813 0,-93.57186 0,-28.24179 25.229,0 0,-149.1723 0,-141.58254 c -15.49708,0 15.49708,0 0,0 0,-28.94737 0,-60.297788 0,-89.245158 l 57.01791,0 95.61403,0 0,298.894278 6e-5,332.45572 -82.0176,0 -95.8434,0 z"
        S.path ! A.style "fill:#c6ff20;fill-opacity:0.58730164;stroke:none;display:inline" ! A.d "m 199.61296,675.02385 4e-5,-18.42106 18.42101,0 13.28699,-3.1e-4 0,-136.84179 0,-136.84211 48.99371,0 43.85965,0 0,22.87851 -3.6e-4,36.29739 -25.229,0 0,52.8286 0,99.26091 0,99.26091 -54.1566,-4.2e-4 -45.17544,4.2e-4 z"
        S.path ! A.style "fill:#2fff2b;fill-opacity:0.58730164;stroke:none;display:inline" ! A.d "m 25.051555,465.37472 0,-205.26315 93.859645,0 112.4098,0 0,25.43859 0,72.80732 -58.46243,0 -31.09257,0 0,132.45584 0,202.63116 -62.767077,0 -53.947368,0 z"
        S.path ! A.style "fill:#2f692b;fill-opacity:0.58730164;stroke:none;display:inline" ! A.d "m 141.766,504.40981 0,-146.05263 46.88205,0 42.67295,3e-4 0,146.05233 0,152.19267 -31.708,0 0,36.842 -57.847,0 0,-36.842 z"
    S.g ! A.transform " translate(0,-308.2677)" $ do
        S.g ! A.transform "translate(29.540919,340.70929)" ! A.class_ "nodes"$ do
            S.g ! A.class_ "node"  ! A.id_ "CSC438" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "399.59551999999996" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "419.59551999999996" ! A.y "594.60641" $ "438"
            S.g ! A.class_ "node"  ! A.id_ "CSC463" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "289.40411" ! A.y "424.2453" ! A.fill "#fff" 
             S.text_  ! A.x "309.40411" ! A.y "440.2453" $ "463"
            S.g ! A.class_ "node"  ! A.id_ "CSC448" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "289.40411" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "309.40411" ! A.y "594.60641" $ "448"
            S.g ! A.class_ "node"  ! A.id_ "CSC165" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "65.207497" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "319.56799" ! A.y "37.80279999999999" ! A.fill "#fff" 
             S.text_  ! A.x "352.1717385" ! A.y "53.80279999999999" $ "165"
            S.g ! A.class_ "node"  ! A.id_ "CSC236" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "65.207497" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "319.56799" ! A.y "139.5925" ! A.fill "#fff" 
             S.text_  ! A.x "352.1717385" ! A.y "155.5925" $ "236"
            S.g ! A.class_ "node"  ! A.id_ "CSC373" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "332.17169" ! A.y "362.2453" ! A.fill "#fff" 
             S.text_  ! A.x "352.17169" ! A.y "378.2453" $ "373"
            S.g ! A.class_ "node"  ! A.id_ "CSC108" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "461.17589999999996" ! A.y "37.80279999999999" ! A.fill "#fff" 
             S.text_  ! A.x "481.17589999999996" ! A.y "53.80279999999999" $ "108"
            S.g ! A.class_ "node"  ! A.id_ "CSC148" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "461.17589999999996" ! A.y "88.5777" ! A.fill "#fff" 
             S.text_  ! A.x "481.17589999999996" ! A.y "104.5777" $ "148"
            S.g ! A.class_ "node"  ! A.id_ "CSC207" ! S.dataAttribute "group" "se" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "633.82123" ! A.y "139.5925" ! A.fill "#fff" 
             S.text_  ! A.x "653.82123" ! A.y "155.5925" $ "207"
            S.g ! A.class_ "node"  ! A.id_ "CSC209" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "753.82123" ! A.y "228.222" ! A.fill "#fff" 
             S.text_  ! A.x "773.82123" ! A.y "244.222" $ "209"
            S.g ! A.class_ "node"  ! A.id_ "CSC258" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "822.76929" ! A.y "139.5925" ! A.fill "#fff" 
             S.text_  ! A.x "842.76929" ! A.y "155.5925" $ "258"
            S.g ! A.class_ "node"  ! A.id_ "CSC369" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "783.82123" ! A.y "311.688101" ! A.fill "#fff" 
             S.text_  ! A.x "803.82123" ! A.y "327.688101" $ "369"
            S.g ! A.class_ "node"  ! A.id_ "CSC318" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "362.2453" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "378.2453" $ "318"
            S.g ! A.class_ "node"  ! A.id_ "CSC320" ! S.dataAttribute "group" "graphics" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "145.04160000000002" ! A.y "363.363297" ! A.fill "#fff" 
             S.text_  ! A.x "165.04160000000002" ! A.y "379.363297" $ "320"
            S.g ! A.class_ "node"  ! A.id_ "CSC418" ! S.dataAttribute "group" "graphics" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "145.04160000000002" ! A.y "424.2453" ! A.fill "#fff" 
             S.text_  ! A.x "165.04160000000002" ! A.y "440.2453" $ "418"
            S.g ! A.class_ "node"  ! A.id_ "CSC454" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "594.60641" $ "454"
            S.g ! A.class_ "node"  ! A.id_ "CSC300" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "298.9904" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "314.9904" $ "300"
            S.g ! A.class_ "node"  ! A.id_ "CSC200" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "236.990398" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "252.990398" $ "200"
            S.g ! A.class_ "node"  ! A.id_ "CSC404" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "512.3459" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "528.3459" $ "404"
            S.g ! A.class_ "node"  ! A.id_ "CSC428" ! S.dataAttribute "group" "hci" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "48.36250000000001" ! A.y "424.2453" ! A.fill "#fff" 
             S.text_  ! A.x "68.36250000000001" ! A.y "440.2453" $ "428"
            S.g ! A.class_ "node"  ! A.id_ "CSC336" ! S.dataAttribute "group" "num" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "249.40411" ! A.y "363.363297" ! A.fill "#fff" 
             S.text_  ! A.x "269.40411" ! A.y "379.363297" $ "336"
            S.g ! A.class_ "node"  ! A.id_ "CSC446" ! S.dataAttribute "group" "num" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "214.57599" ! A.y "510.2894" ! A.fill "#fff" 
             S.text_  ! A.x "234.57599" ! A.y "526.2894" $ "446"
            S.g ! A.class_ "node"  ! A.id_ "CSC456" ! S.dataAttribute "group" "num" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "214.57599" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "234.57599" ! A.y "594.60641" $ "456"
            S.g ! A.class_ "node"  ! A.id_ "CSC436" ! S.dataAttribute "group" "num" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "214.57599" ! A.y "422.47321" ! A.fill "#fff" 
             S.text_  ! A.x "234.57599" ! A.y "438.47321" $ "436"
            S.g ! A.class_ "node"  ! A.id_ "CSC384" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "519.1554" ! A.y "480.47321" ! A.fill "#fff" 
             S.text_  ! A.x "539.1554" ! A.y "496.47321" $ "384"
            S.g ! A.class_ "node"  ! A.id_ "CSC486" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "519.1554" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "539.1554" ! A.y "594.60641" $ "486"
            S.g ! A.class_ "node"  ! A.id_ "CSC401" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "591.08002" ! A.y "480.47321" ! A.fill "#fff" 
             S.text_  ! A.x "611.08002" ! A.y "496.47321" $ "401"
            S.g ! A.class_ "node"  ! A.id_ "CSC485" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "663.00458" ! A.y "480.47321" ! A.fill "#fff" 
             S.text_  ! A.x "683.00458" ! A.y "496.47321" $ "485"
            S.g ! A.class_ "node"  ! A.id_ "CSC321" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "454.80798000000004" ! A.y "424.2453" ! A.fill "#fff" 
             S.text_  ! A.x "474.80798000000004" ! A.y "440.2453" $ "321"
            S.g ! A.class_ "node"  ! A.id_ "CSC411" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "459.37549" ! A.y "480.47321" ! A.fill "#fff" 
             S.text_  ! A.x "479.37549" ! A.y "496.47321" $ "411"
            S.g ! A.class_ "node"  ! A.id_ "CSC301" ! S.dataAttribute "group" "se" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "633.82123" ! A.y "311.688101" ! A.fill "#fff" 
             S.text_  ! A.x "653.82123" ! A.y "327.688101" $ "301"
            S.g ! A.class_ "node"  ! A.id_ "CSC302" ! S.dataAttribute "group" "se" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "633.82123" ! A.y "373.688103" ! A.fill "#fff" 
             S.text_  ! A.x "653.82123" ! A.y "389.688103" $ "302"
            S.g ! A.class_ "node"  ! A.id_ "CSC410" ! S.dataAttribute "group" "se" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "559.83911" ! A.y "373.688103" ! A.fill "#fff" 
             S.text_  ! A.x "579.83911" ! A.y "389.688103" $ "410"
            S.g ! A.class_ "node"  ! A.id_ "CSC324" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "402.17169" ! A.y "373.688103" ! A.fill "#fff" 
             S.text_  ! A.x "422.17169" ! A.y "389.688103" $ "324"
            S.g ! A.class_ "node"  ! A.id_ "CSC488" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "956.9658999999999" ! A.y "424.52161" ! A.fill "#fff" 
             S.text_  ! A.x "976.9658999999999" ! A.y "440.52161" $ "488"
            S.g ! A.class_ "node"  ! A.id_ "ECE489" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "956.9658999999999" ! A.y "479.3551" ! A.fill "#fff" 
             S.text_  ! A.x "976.9658999999999" ! A.y "495.3551" $ "489"
            S.g ! A.class_ "node"  ! A.id_ "CSC469" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "862.8861" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "882.8861" ! A.y "594.60641" $ "469"
            S.g ! A.class_ "node"  ! A.id_ "CSC358" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "862.8861" ! A.y "311.688101" ! A.fill "#fff" 
             S.text_  ! A.x "882.8861" ! A.y "327.688101" $ "358"
            S.g ! A.class_ "node"  ! A.id_ "CSC458" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "862.8861" ! A.y "368.104897" ! A.fill "#fff" 
             S.text_  ! A.x "882.8861" ! A.y "384.104897" $ "458"
            S.g ! A.class_ "node"  ! A.id_ "ECE385" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "862.8861" ! A.y "480.47321" ! A.fill "#fff" 
             S.text_  ! A.x "882.8861" ! A.y "496.47321" $ "385"
            S.g ! A.class_ "node"  ! A.id_ "CSC309" ! S.dataAttribute "group" "dbweb" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "753.82123" ! A.y "510.2894" ! A.fill "#fff" 
             S.text_  ! A.x "773.82123" ! A.y "526.2894" $ "309"
            S.g ! A.class_ "node"  ! A.id_ "CSC343" ! S.dataAttribute "group" "dbweb" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "725.48462" ! A.y "424.52161" ! A.fill "#fff" 
             S.text_  ! A.x "745.48462" ! A.y "440.52161" $ "343"
            S.g ! A.class_ "node"  ! A.id_ "CSC443" ! S.dataAttribute "group" "dbweb" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "715.45349" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "735.45349" ! A.y "594.60641" $ "443"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC301" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "50" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "15.380709999999993" ! A.y "472.29559" ! A.fill "#bbb" 
             S.text_  ! A.x "40.38070999999999" ! A.y "484.29559" $ "301"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC263" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "916.4259999999999" ! A.y "232.222" ! A.fill "#bbb" 
             S.text_  ! A.x "929.9259999999999" ! A.y "244.222" $ "263"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSCCalc1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "64.826897" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "125.19790999999998" ! A.y "478.4258" ! A.fill "#bbb" 
             S.text_  ! A.x "157.6113585" ! A.y "490.4258" $ "Calc1"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC258" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "956.9658999999999" ! A.y "347.688099" ! A.fill "#bbb" 
             S.text_  ! A.x "976.9658999999999" ! A.y "359.688099" $ "258"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC373" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "663.00458" ! A.y "541.3551" ! A.fill "#bbb" 
             S.text_  ! A.x "676.50458" ! A.y "553.3551" $ "373"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSCSta1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "420.63" ! A.y "204.583099" ! A.fill "#bbb" 
             S.text_  ! A.x "434.13" ! A.y "216.583099" $ "Sta1"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSCLin1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "482.72748" ! A.y "204.583099" ! A.fill "#bbb" 
             S.text_  ! A.x "496.22748" ! A.y "216.583099" $ "Lin1"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSCSta1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "597.58002" ! A.y "549.60641" ! A.fill "#bbb" 
             S.text_  ! A.x "611.08002" ! A.y "561.60641" $ "Sta1"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSCSta1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "55.5783" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "380.78949" ! A.y "533.53979" ! A.fill "#bbb" 
             S.text_  ! A.x "408.57864" ! A.y "545.53979" $ "Sta1"
            S.g ! A.class_ "node"  ! A.id_ "Calc1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "61.5" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "171.02999999999997" ! A.y "37.80279999999999" ! A.fill "#fff" 
             S.text_  ! A.x "201.77999999999997" ! A.y "53.80279999999999" $ "Calc1"
            S.g ! A.class_ "node"  ! A.id_ "Sta1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "50" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "46.34779" ! A.y "88.5777" ! A.fill "#fff" 
             S.text_  ! A.x "71.34779" ! A.y "104.5777" $ "Sta1"
            S.g ! A.class_ "node"  ! A.id_ "Sta2" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "50" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "46.34779" ! A.y "150.5777" ! A.fill "#fff" 
             S.text_  ! A.x "71.34779" ! A.y "166.5777" $ "Sta2"
            S.g ! A.class_ "node"  ! A.id_ "CSC412" ! S.dataAttribute "group" "ai" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "459.37549" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "479.37549" ! A.y "594.60641" $ "412"
            S.g ! A.class_ "node"  ! A.id_ "CSC420" ! S.dataAttribute "group" "graphics" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "145.97791" ! A.y "578.60641" ! A.fill "#fff" 
             S.text_  ! A.x "165.97791" ! A.y "594.60641" $ "420"
            S.g ! A.class_ "node"  ! A.id_ "CSC104" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "547.49847" ! A.y "37.80279999999999" ! A.fill "#fff" 
             S.text_  ! A.x "567.49847" ! A.y "53.80279999999999" $ "104"
            S.g ! A.class_ "node"  ! A.id_ "CSC120" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "633.82123" ! A.y "37.80279999999999" ! A.fill "#fff" 
             S.text_  ! A.x "653.82123" ! A.y "53.80279999999999" $ "120"
            S.g ! A.class_ "node"  ! A.id_ "Lin1" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "50" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "126.34780999999998" ! A.y "150.5777" ! A.fill "#fff" 
             S.text_  ! A.x "151.34780999999998" ! A.y "166.5777" $ "Lin1"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC263" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "640.32123" ! A.y "232.222" ! A.fill "#bbb" 
             S.text_  ! A.x "653.82123" ! A.y "244.222" $ "263"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC236" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "916.4259999999999" ! A.y "524.6064" ! A.fill "#bbb" 
             S.text_  ! A.x "929.9259999999999" ! A.y "536.6064" $ "236"
            S.g ! A.class_ "node"  ! A.id_ "CSC372" ! S.dataAttribute "group" "systems" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "862.8861" ! A.y "424.52161" ! A.fill "#fff" 
             S.text_  ! A.x "882.8861" ! A.y "440.52161" $ "372"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC209" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "98.7251" ! A.y "331.271599" ! A.fill "#bbb" 
             S.text_  ! A.x "112.2251" ! A.y "343.271599" $ "209"
            S.g ! A.class_ "node"  ! A.id_ "CSC310" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "402.17169" ! A.y "311.688101" ! A.fill "#fff" 
             S.text_  ! A.x "422.17169" ! A.y "327.688101" $ "310"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC148" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "408.67169" ! A.y "270.508101" ! A.fill "#bbb" 
             S.text_  ! A.x "422.17169" ! A.y "282.508101" $ "148"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC207" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "741.43658" ! A.y "358.664902" ! A.fill "#bbb" 
             S.text_  ! A.x "754.93658" ! A.y "370.664902" $ "207"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC263" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "133.56439" ! A.y "524.6064" ! A.fill "#bbb" 
             S.text_  ! A.x "147.06439" ! A.y "536.6064" $ "263"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC165" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "50" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "703.82123" ! A.y "319.688101" ! A.fill "#bbb" 
             S.text_  ! A.x "728.82123" ! A.y "331.688101" $ "165"
            S.g ! A.class_ "node"  ! A.id_ "CSC263" ! S.dataAttribute "group" "theory" ! A.style "" $ do 
             S.rect ! A.width "65.207497" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "319.56799" ! A.y "228.222" ! A.fill "#fff" 
             S.text_  ! A.x "352.1717385" ! A.y "244.222" $ "263"
            S.g ! A.class_ "node"  ! A.id_ "CSC465" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "40" ! A.height "32" ! A.rx "4" ! A.ry "4" ! A.x "507.44426999999996" ! A.y "373.688103" ! A.fill "#fff" 
             S.text_  ! A.x "527.44427" ! A.y "389.688103" $ "465"
            S.g ! A.class_ "hybrid"  ! A.id_ "hCSC236" ! S.dataAttribute "group" "core" ! A.style "" $ do 
             S.rect ! A.width "27" ! A.height "24" ! A.rx "4" ! A.ry "4" ! A.x "487.62622" ! A.y "315.688101" ! A.fill "#bbb" 
             S.text_  ! A.x "501.12622" ! A.y "327.688101" $ "236"
            S.g $ do
                S.path ! A.id_ "p8" ! A.class_ "path" ! A.d "M 481.17589999999996,69.8187 481.17589999999996,80.59 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p10" ! A.class_ "path" ! A.d "M 461.14959999999996,104.5777 365.80609999999996,104.5777 365.80609999999996,131.6109 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p12" ! A.class_ "path" ! A.d "M 501.197,104.5777 653.8212,104.5777 653.8212,131.6006 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p14" ! A.class_ "path" ! A.d "M 673.8309,160.7942 773.8212000000001,186.7876 773.8212000000001,220.2088 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p16" ! A.class_ "path" ! A.d "M 501.164,104.5777 691.9126,104.5777 691.9126,155.5925 814.7595,155.5925 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p18" ! A.class_ "path" ! A.d "M 352.1717,37.83080000000001 352.1717,15.5 711.30008,15.5 711.30008,142.10289 815.0569,142.1027 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p20" ! A.class_ "path" ! A.d "M 68.36250000000001,394.2599 68.36250000000001,416.21320000000003 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p22" ! A.class_ "path" ! A.d "M 461.19309999999996,104.5777 269.40409999999997,104.5777 269.40409999999997,355.3633 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p24" ! A.class_ "path" ! A.d "M 269.4041,395.3975 269.4041,526.2894 262.60900000000004,526.2894 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p26" ! A.class_ "path" ! A.d "M 269.4041,395.36469999999997 269.4041,594.6064 262.60900000000004,594.6064 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p28" ! A.class_ "path" ! A.d "M 269.40408,395.3819 269.4041,438.4732 262.60900000000004,438.4732 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p30" ! A.class_ "path" ! A.d "M 539.1554,512.4294 539.1554,570.5964 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p32" ! A.class_ "path" ! A.d "M 633.828,155.5925 611.0799999999999,155.5925 611.0799999999999,472.4528 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p34" ! A.class_ "path" ! A.d "M 753.8247,244.222 695.816,244.222 695.816,472.4536 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p36" ! A.class_ "path" ! A.d "M 753.8388,244.222 695.5728,244.222 695.5728,327.6881 681.8408000000001,327.6881 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p38" ! A.class_ "path" ! A.d "M 653.8212,343.7028 653.8212,365.65610000000004 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p40" ! A.class_ "path" ! A.d "M 803.8212,343.6863 803.8212,594.6064 854.8497,594.6064 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p42" ! A.class_ "path" ! A.d "M 773.8212,260.2329 773.8212,502.2779 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p44" ! A.class_ "path" ! A.d "M 750.7838,456.5612 766.0358,502.72520000000003 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p46" ! A.class_ "path" ! A.d "M 735.4535,456.5546 735.4535,570.597 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p48" ! A.class_ "path" ! A.d "M 803.8212,343.6863 803.8212,594.6064 763.4497,594.6064 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p50" ! A.class_ "path" ! A.d "M 48.0034,496.29560000000004 53.91329999999999,505.59929999999997 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p52" ! A.class_ "path" ! A.d "M 929.9259999999999,256.222 929.9259999999999,440.52160000000003 948.9831999999999,440.52160000000003 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p54" ! A.class_ "path" ! A.d "M 929.9259999999999,256.222 929.9259999999999,327.6881 910.8688999999999,327.6881 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p56" ! A.class_ "path" ! A.d "M 929.9259999999999,256.222 929.9259999999999,384.10490000000004 910.8688999999999,384.10490000000004 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p58" ! A.class_ "path" ! A.d "M 157.6114,477.9258 160.33749999999998,464.101 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p60" ! A.class_ "path" ! A.d "M 384.79179999999997,155.5925 451.1436,155.5925 469.37969999999996,173.196 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p62" ! A.class_ "path" ! A.d "M 633.8051,155.5925 517.8581,155.5925 494.2333,174.2636 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p64" ! A.class_ "path" ! A.d "M 491.05589999999995,184.5831 523.3506,184.5831 523.3506,328.8723 559.5278,367.82079999999996 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p66" ! A.class_ "path" ! A.d "M 976.9658,371.6881 976.9658,416.4896 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p68" ! A.class_ "path" ! A.d "M 690.0046,562.8022000000001 709.3035,576.3072 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p70" ! A.class_ "path" ! A.d "M 793.8525,244.222 824.8892999999999,244.222 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p72" ! A.class_ "path" ! A.d "M 842.7693,171.5614 842.7693,228.8535 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p74" ! A.class_ "path" ! A.d "M 842.7693,251.5904 842.7693,327.6881 831.7897,327.6881 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p76" ! A.class_ "path" ! A.d "M 842.7693,251.5904 842.7693,495.3551 854.87,495.6924 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p78" ! A.class_ "path" ! A.d "M 842.7693,251.5904 842.7693,327.6881 854.8669,327.6881 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p80" ! A.class_ "path" ! A.d "M 842.7693,251.5904 842.7693,384.1049 854.8669,384.1049 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p82" ! A.class_ "path" ! A.d "M 309.4041,456.2525 309.4041,526.2894 334.29170000000005,526.2894 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p84" ! A.class_ "path" ! A.d "M 352.1717,394.2158 352.1717,499.161 352.1717,510.921 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p86" ! A.class_ "path" ! A.d "M 352.1717,533.6579 352.1717,594.6064 391.61199999999997,594.6064 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p88" ! A.class_ "path" ! A.d "M 352.1717,533.6579 352.1717,630.6064 539.1554,630.6064 539.1554,618.6025 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p90" ! A.class_ "path" ! A.d "M 597.8287,549.6064 562.4753,517.5912 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p92" ! A.class_ "path" ! A.d "M 611.08,549.6064 611.08,520.4806 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p94" ! A.class_ "path" ! A.d "M 624.3312,549.6064 659.6847,517.5912000000001 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p96" ! A.class_ "path" ! A.d "M 611.08,549.6064 611.08,520.4806 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p98" ! A.class_ "path" ! A.d "M 425.8931,533.5398 453.13599999999997,514.6587 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p100" ! A.class_ "path" ! A.d "M 201.7799,69.8187 201.7799,104.5777 104.34519999999998,104.5777 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p102" ! A.class_ "path" ! A.d "M 71.3478,120.5924 71.3478,142.5457 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p104" ! A.class_ "path" ! A.d "M 46.342899999999986,166.5777 26.347799999999978,166.5777 26.347799999999978,440.5216 40.36439999999999,440.5216 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p106" ! A.class_ "path" ! A.d "M 479.3755,512.4294 479.3755,570.5964 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p108" ! A.class_ "path" ! A.d "M 653.8212,256.222 653.8212,303.6711 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p110" ! A.class_ "path" ! A.d "M 943.4259999999999,524.7677 953.1658,516.2265 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p112" ! A.class_ "path" ! A.d "M 842.76908,251.590892 842.7693,440.52160000000003 854.8669,440.52160000000003 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p114" ! A.class_ "path" ! A.d "M 112.2251,355.27160000000003 112.2251,630.6064 234.57600000000002,630.6064 234.57600000000002,618.6025 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p116" ! A.class_ "path" ! A.d "M 112.2251,355.27160000000003 112.2251,440.2453 96.3663,440.2453 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p118" ! A.class_ "path" ! A.d "M 112.2251,355.27160000000003 112.2251,379.3633 137.05489999999998,379.3633 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p120" ! A.class_ "path" ! A.d "M 112.22507999999999,355.27160000000003 112.2251,440.2453 137.05489999999998,440.2453 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p122" ! A.class_ "path" ! A.d "M 492.32860000000005,228.5831 479.6068,267.7384 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p124" ! A.class_ "path" ! A.d "M 441.5344,228.5831 466.4769,269.0062 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p126" ! A.class_ "path" ! A.d "M 474.808,289.8766 474.808,416.2759 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p128" ! A.class_ "path" ! A.d "M 474.808,289.8766 474.808,327.6881 450.19309999999996,327.6881 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p130" ! A.class_ "path" ! A.d "M 422.1717,294.5081 422.1717,303.7138 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p132" ! A.class_ "path" ! A.d "M 753.3129,382.6649 748.7245,416.5759 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p134" ! A.class_ "path" ! A.d "M 150.9776,548.6064 158.2681,570.9635000000001 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p136" ! A.class_ "path" ! A.d "M 715.5655,343.7048 732.4424,416.716 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p138" ! A.class_ "path" ! A.d "M 163.04469999999998,182.5841 192.3318,222.6613 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p140" ! A.class_ "path" ! A.d "M 201.7799,69.8246 201.7799,220.222 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p142" ! A.class_ "path" ! A.d "M 201.7799,242.9589 201.7799,379.3633 241.42719999999997,379.3633 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p144" ! A.class_ "path" ! A.d "M 201.7799,242.9589 201.7799,379.3633 193.02519999999998,379.3633 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p146" ! A.class_ "path" ! A.d "M 201.78008,242.9589 201.7799,594.6064 194.01170000000002,594.6064 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p148" ! A.class_ "path" ! A.d "M 434.13,228.5831 434.13,244.222 392.78689999999995,244.222 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p150" ! A.class_ "path" ! A.d "M 471.29600000000005,184.5831 352.17170000000004,184.5831 352.17170000000004,220.2348 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p152" ! A.class_ "path" ! A.d "M 352.1717,260.2223 352.1717,354.2534 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p154" ! A.class_ "path" ! A.d "M 382.14869999999996,259.9907 382.14869999999996,496.4732 451.3982,496.4732 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p156" ! A.class_ "path" ! A.d "M 381.89390000000003,260.0908 381.89390000000003,389.68809999999996 394.1508,389.68809999999996 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p158" ! A.class_ "path" ! A.d "M 382.33040000000005,259.9347 382.33040000000005,464.0774 539.1554000000001,464.0774 539.1554000000001,472.4967 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p160" ! A.class_ "path" ! A.d "M 506.22,339.6881 517.5304,366.3329 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p162" ! A.class_ "path" ! A.d "M 309.4041,456.2617 309.4041,570.6306 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p164" ! A.class_ "path" ! A.d "M 340.4949,69.80779999999999 340.4949,131.5942 "  ! A.markerEnd "url(#arrow)"
                S.path ! A.id_ "p166" ! A.class_ "path" ! A.d "M 319.555,155.5925 309.4041,155.5925 309.4041,416.22659999999996 "  ! A.markerEnd "url(#arrow)"
            S.g $ do
                S.g ! A.class_ "bool" ! A.id_ "bool0" $do
                    S.ellipse ! A.d "m 637.0559,-103.4169 c 0,4.069454 -4.42342,7.368399 -9.88,7.368399 -5.45657,0 -9.88,-3.298945 -9.88,-7.368399 0,-4.06946 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.29894 9.88,7.3684 z" ! A.cx "481.17589999999996" ! A.cy "184.5831" ! A.rx "9.8800001" ! A.ry "7.3684001"
                    S.text_  ! A.x "481.17589999999996" ! A.y "184.5831" $ do "and"
                S.g ! A.class_ "bool" ! A.id_ "bool1" $do
                    S.ellipse ! A.d "m 998.64929,-43.778 c 0,4.069455 -4.42343,7.3684 -9.88,7.3684 -5.45658,0 -9.88,-3.298945 -9.88,-7.3684 0,-4.069455 4.42342,-7.3684 9.88,-7.3684 5.45657,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "842.76929" ! A.cy "244.222" ! A.rx "9.8800001" ! A.ry "7.3684001"
                    S.text_  ! A.x "842.76929" ! A.y "244.222" $ do "and"
                S.g ! A.class_ "bool" ! A.id_ "bool2" $do
                    S.ellipse ! A.d "m 508.05169,238.2894 c 0,4.06945 -4.42342,7.3684 -9.88,7.3684 -5.45657,0 -9.88,-3.29895 -9.88,-7.3684 0,-4.06946 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.29894 9.88,7.3684 z" ! A.cx "352.17169" ! A.cy "526.2894" ! A.rx "9.8800001" ! A.ry "7.3684001"
                    S.text_  ! A.x "352.17169" ! A.y "526.2894" $ do "or"
                S.g ! A.class_ "bool" ! A.id_ "bool3" $do
                    S.ellipse ! A.d "m 630.68798,-5.4919 c 0,4.069455 -4.42342,7.3684001 -9.88,7.3684001 -5.45657,0 -9.88,-3.2989451 -9.88,-7.3684001 0,-4.069455 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "474.80798000000004" ! A.cy "282.5081" ! A.rx "9.8800001" ! A.ry "7.3684001"
                    S.text_  ! A.x "474.80798000000004" ! A.y "282.5081" $ do "and"
                S.g ! A.class_ "bool" ! A.id_ "bool4" $do
                    S.ellipse ! A.d "m 357.66,-52.409599 c 0,4.069455 -4.42343,7.3684 -9.88,7.3684 -5.45657,0 -9.88,-3.298945 -9.88,-7.3684 0,-4.069455 4.42343,-7.3684 9.88,-7.3684 5.45657,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "201.77999999999997" ! A.cy "235.59040099999999" ! A.rx "9.8800001" ! A.ry "7.3684001"
                    S.text_  ! A.x "201.77999999999997" ! A.y "235.59040099999999" $ do "and"
    S.g ! A.transform "translate(-120,313.70929)" $ do
        S.text_ ! A.class_ "region-label" ! A.x "322.88751" ! A.y "357.7804" $ "Numerical"
        S.text_ ! A.class_ "region-label" ! A.x "322.88751" ! A.y "370.91171299999996" $ "Computing"
        S.text_ ! A.class_ "region-label" ! A.x "256.146212" ! A.y "-13.036373999999999" $ "Humans and"
        S.text_ ! A.class_ "region-label" ! A.x "261.150112" ! A.y "1.6647260000000017" $ "Computing"
        S.text_ ! A.class_ "region-label" ! A.x "287.03281" ! A.y "57.3218" $ "Graphics"
        S.text_ ! A.class_ "region-label" ! A.x "539.17513" ! A.y "-200.8463565" $ "Theory"
        S.text_ ! A.class_ "region-label" ! A.x "755.20093" ! A.y "320.30141000000003" $ "Artificial"
        S.text_ ! A.class_ "region-label" ! A.x "745.8493" ! A.y "335.00259" $ "Intelligence"
        S.text_ ! A.class_ "region-label" ! A.x "690.63831" ! A.y "9.5697002" $ "Software"
        S.text_ ! A.class_ "region-label" ! A.x "682.29163" ! A.y "24.270901" $ "Engineering"
        S.text_ ! A.class_ "region-label" ! A.x "1054.1652" ! A.y "-97.782898" $ "Systems"
        S.text_ ! A.class_ "region-label" ! A.x "909.37219" ! A.y "339.72839" $ "Web and"
        S.text_ ! A.class_ "region-label" ! A.x "904.70227" ! A.y "354.4296" $ "Databases"
