{-# LANGUAGE OverloadedStrings #-}
module SVGGen where
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.id_ "graphRootSVG" ! A.version "1.1" ! S.customAttribute "viewBox" "0 0 1050 700"$ do
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
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "438" ! A.width "40" ! A.height "32" ! A.x "399.59551999999996" ! A.y "578.60641"
             S.text_  ! A.x "419.59551999999996" ! A.y "594.60641" $ "438"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "438" ! A.width "40" ! A.height "32" ! A.x "399.59551999999996" ! A.y "578.60641"
             S.text_  ! A.x "419.59551999999996" ! A.y "594.60641" $ "438"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "463" ! A.width "40" ! A.height "32" ! A.x "289.40411" ! A.y "424.2453"
             S.text_  ! A.x "309.40411" ! A.y "440.2453" $ "463"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "463" ! A.width "40" ! A.height "32" ! A.x "289.40411" ! A.y "424.2453"
             S.text_  ! A.x "309.40411" ! A.y "440.2453" $ "463"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "448" ! A.width "40" ! A.height "32" ! A.x "289.40411" ! A.y "578.60641"
             S.text_  ! A.x "309.40411" ! A.y "594.60641" $ "448"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "448" ! A.width "40" ! A.height "32" ! A.x "289.40411" ! A.y "578.60641"
             S.text_  ! A.x "309.40411" ! A.y "594.60641" $ "448"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "165/240" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "37.80279999999999"
             S.text_  ! A.x "352.1717385" ! A.y "53.80279999999999" $ "165/240"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "165/240" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "37.80279999999999"
             S.text_  ! A.x "352.1717385" ! A.y "53.80279999999999" $ "165/240"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236/240" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "139.5925"
             S.text_  ! A.x "352.1717385" ! A.y "155.5925" $ "236/240"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236/240" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "139.5925"
             S.text_  ! A.x "352.1717385" ! A.y "155.5925" $ "236/240"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "373" ! A.width "40" ! A.height "32" ! A.x "332.17169" ! A.y "362.2453"
             S.text_  ! A.x "352.17169" ! A.y "378.2453" $ "373"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "373" ! A.width "40" ! A.height "32" ! A.x "332.17169" ! A.y "362.2453"
             S.text_  ! A.x "352.17169" ! A.y "378.2453" $ "373"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "108" ! A.width "40" ! A.height "32" ! A.x "461.17589999999996" ! A.y "37.80279999999999"
             S.text_  ! A.x "481.17589999999996" ! A.y "53.80279999999999" $ "108"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "108" ! A.width "40" ! A.height "32" ! A.x "461.17589999999996" ! A.y "37.80279999999999"
             S.text_  ! A.x "481.17589999999996" ! A.y "53.80279999999999" $ "108"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "148" ! A.width "40" ! A.height "32" ! A.x "461.17589999999996" ! A.y "88.5777"
             S.text_  ! A.x "481.17589999999996" ! A.y "104.5777" $ "148"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "148" ! A.width "40" ! A.height "32" ! A.x "461.17589999999996" ! A.y "88.5777"
             S.text_  ! A.x "481.17589999999996" ! A.y "104.5777" $ "148"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "207" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "139.5925"
             S.text_  ! A.x "653.82123" ! A.y "155.5925" $ "207"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "207" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "139.5925"
             S.text_  ! A.x "653.82123" ! A.y "155.5925" $ "207"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "209" ! A.width "40" ! A.height "32" ! A.x "753.82123" ! A.y "228.222"
             S.text_  ! A.x "773.82123" ! A.y "244.222" $ "209"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "209" ! A.width "40" ! A.height "32" ! A.x "753.82123" ! A.y "228.222"
             S.text_  ! A.x "773.82123" ! A.y "244.222" $ "209"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "258" ! A.width "40" ! A.height "32" ! A.x "822.76929" ! A.y "139.5925"
             S.text_  ! A.x "842.76929" ! A.y "155.5925" $ "258"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "258" ! A.width "40" ! A.height "32" ! A.x "822.76929" ! A.y "139.5925"
             S.text_  ! A.x "842.76929" ! A.y "155.5925" $ "258"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "369" ! A.width "40" ! A.height "32" ! A.x "783.82123" ! A.y "311.688101"
             S.text_  ! A.x "803.82123" ! A.y "327.688101" $ "369"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "369" ! A.width "40" ! A.height "32" ! A.x "783.82123" ! A.y "311.688101"
             S.text_  ! A.x "803.82123" ! A.y "327.688101" $ "369"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "318" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "362.2453"
             S.text_  ! A.x "68.36250000000001" ! A.y "378.2453" $ "318"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "318" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "362.2453"
             S.text_  ! A.x "68.36250000000001" ! A.y "378.2453" $ "318"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "320" ! A.width "40" ! A.height "32" ! A.x "145.04160000000002" ! A.y "363.363297"
             S.text_  ! A.x "165.04160000000002" ! A.y "379.363297" $ "320"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "320" ! A.width "40" ! A.height "32" ! A.x "145.04160000000002" ! A.y "363.363297"
             S.text_  ! A.x "165.04160000000002" ! A.y "379.363297" $ "320"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "418" ! A.width "40" ! A.height "32" ! A.x "145.04160000000002" ! A.y "424.2453"
             S.text_  ! A.x "165.04160000000002" ! A.y "440.2453" $ "418"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "418" ! A.width "40" ! A.height "32" ! A.x "145.04160000000002" ! A.y "424.2453"
             S.text_  ! A.x "165.04160000000002" ! A.y "440.2453" $ "418"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "454" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "578.60641"
             S.text_  ! A.x "68.36250000000001" ! A.y "594.60641" $ "454"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "454" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "578.60641"
             S.text_  ! A.x "68.36250000000001" ! A.y "594.60641" $ "454"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "300" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "298.9904"
             S.text_  ! A.x "68.36250000000001" ! A.y "314.9904" $ "300"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "300" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "298.9904"
             S.text_  ! A.x "68.36250000000001" ! A.y "314.9904" $ "300"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "200" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "236.990398"
             S.text_  ! A.x "68.36250000000001" ! A.y "252.990398" $ "200"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "200" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "236.990398"
             S.text_  ! A.x "68.36250000000001" ! A.y "252.990398" $ "200"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "404" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "512.3459"
             S.text_  ! A.x "68.36250000000001" ! A.y "528.3459" $ "404"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "404" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "512.3459"
             S.text_  ! A.x "68.36250000000001" ! A.y "528.3459" $ "404"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "428" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "424.2453"
             S.text_  ! A.x "68.36250000000001" ! A.y "440.2453" $ "428"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "428" ! A.width "40" ! A.height "32" ! A.x "48.36250000000001" ! A.y "424.2453"
             S.text_  ! A.x "68.36250000000001" ! A.y "440.2453" $ "428"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "336" ! A.width "40" ! A.height "32" ! A.x "249.40411" ! A.y "363.363297"
             S.text_  ! A.x "269.40411" ! A.y "379.363297" $ "336"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "336" ! A.width "40" ! A.height "32" ! A.x "249.40411" ! A.y "363.363297"
             S.text_  ! A.x "269.40411" ! A.y "379.363297" $ "336"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "446" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "510.2894"
             S.text_  ! A.x "234.57599" ! A.y "526.2894" $ "446"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "446" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "510.2894"
             S.text_  ! A.x "234.57599" ! A.y "526.2894" $ "446"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "456" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "578.60641"
             S.text_  ! A.x "234.57599" ! A.y "594.60641" $ "456"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "456" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "578.60641"
             S.text_  ! A.x "234.57599" ! A.y "594.60641" $ "456"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "436" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "422.47321"
             S.text_  ! A.x "234.57599" ! A.y "438.47321" $ "436"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "436" ! A.width "40" ! A.height "32" ! A.x "214.57599" ! A.y "422.47321"
             S.text_  ! A.x "234.57599" ! A.y "438.47321" $ "436"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "384" ! A.width "40" ! A.height "32" ! A.x "519.1554" ! A.y "480.47321"
             S.text_  ! A.x "539.1554" ! A.y "496.47321" $ "384"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "384" ! A.width "40" ! A.height "32" ! A.x "519.1554" ! A.y "480.47321"
             S.text_  ! A.x "539.1554" ! A.y "496.47321" $ "384"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "486" ! A.width "40" ! A.height "32" ! A.x "519.1554" ! A.y "578.60641"
             S.text_  ! A.x "539.1554" ! A.y "594.60641" $ "486"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "486" ! A.width "40" ! A.height "32" ! A.x "519.1554" ! A.y "578.60641"
             S.text_  ! A.x "539.1554" ! A.y "594.60641" $ "486"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "401" ! A.width "40" ! A.height "32" ! A.x "591.08002" ! A.y "480.47321"
             S.text_  ! A.x "611.08002" ! A.y "496.47321" $ "401"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "401" ! A.width "40" ! A.height "32" ! A.x "591.08002" ! A.y "480.47321"
             S.text_  ! A.x "611.08002" ! A.y "496.47321" $ "401"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "485" ! A.width "40" ! A.height "32" ! A.x "663.00458" ! A.y "480.47321"
             S.text_  ! A.x "683.00458" ! A.y "496.47321" $ "485"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "485" ! A.width "40" ! A.height "32" ! A.x "663.00458" ! A.y "480.47321"
             S.text_  ! A.x "683.00458" ! A.y "496.47321" $ "485"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "321" ! A.width "40" ! A.height "32" ! A.x "454.80798000000004" ! A.y "424.2453"
             S.text_  ! A.x "474.80798000000004" ! A.y "440.2453" $ "321"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "321" ! A.width "40" ! A.height "32" ! A.x "454.80798000000004" ! A.y "424.2453"
             S.text_  ! A.x "474.80798000000004" ! A.y "440.2453" $ "321"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "411" ! A.width "40" ! A.height "32" ! A.x "459.37549" ! A.y "480.47321"
             S.text_  ! A.x "479.37549" ! A.y "496.47321" $ "411"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "411" ! A.width "40" ! A.height "32" ! A.x "459.37549" ! A.y "480.47321"
             S.text_  ! A.x "479.37549" ! A.y "496.47321" $ "411"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "301" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "311.688101"
             S.text_  ! A.x "653.82123" ! A.y "327.688101" $ "301"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "301" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "311.688101"
             S.text_  ! A.x "653.82123" ! A.y "327.688101" $ "301"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "302" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "373.688103"
             S.text_  ! A.x "653.82123" ! A.y "389.688103" $ "302"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "302" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "373.688103"
             S.text_  ! A.x "653.82123" ! A.y "389.688103" $ "302"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "410" ! A.width "40" ! A.height "32" ! A.x "559.83911" ! A.y "373.688103"
             S.text_  ! A.x "579.83911" ! A.y "389.688103" $ "410"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "410" ! A.width "40" ! A.height "32" ! A.x "559.83911" ! A.y "373.688103"
             S.text_  ! A.x "579.83911" ! A.y "389.688103" $ "410"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "324" ! A.width "40" ! A.height "32" ! A.x "402.17169" ! A.y "373.688103"
             S.text_  ! A.x "422.17169" ! A.y "389.688103" $ "324"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "324" ! A.width "40" ! A.height "32" ! A.x "402.17169" ! A.y "373.688103"
             S.text_  ! A.x "422.17169" ! A.y "389.688103" $ "324"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "488" ! A.width "40" ! A.height "32" ! A.x "956.9658999999999" ! A.y "424.52161"
             S.text_  ! A.x "976.9658999999999" ! A.y "440.52161" $ "488"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "488" ! A.width "40" ! A.height "32" ! A.x "956.9658999999999" ! A.y "424.52161"
             S.text_  ! A.x "976.9658999999999" ! A.y "440.52161" $ "488"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "489" ! A.width "40" ! A.height "32" ! A.x "956.9658999999999" ! A.y "479.3551"
             S.text_  ! A.x "976.9658999999999" ! A.y "495.3551" $ "489"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "489" ! A.width "40" ! A.height "32" ! A.x "956.9658999999999" ! A.y "479.3551"
             S.text_  ! A.x "976.9658999999999" ! A.y "495.3551" $ "489"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "469" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "578.60641"
             S.text_  ! A.x "882.8861" ! A.y "594.60641" $ "469"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "469" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "578.60641"
             S.text_  ! A.x "882.8861" ! A.y "594.60641" $ "469"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "358" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "311.688101"
             S.text_  ! A.x "882.8861" ! A.y "327.688101" $ "358"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "358" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "311.688101"
             S.text_  ! A.x "882.8861" ! A.y "327.688101" $ "358"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "458" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "368.104897"
             S.text_  ! A.x "882.8861" ! A.y "384.104897" $ "458"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "458" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "368.104897"
             S.text_  ! A.x "882.8861" ! A.y "384.104897" $ "458"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "385" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "480.47321"
             S.text_  ! A.x "882.8861" ! A.y "496.47321" $ "385"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "385" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "480.47321"
             S.text_  ! A.x "882.8861" ! A.y "496.47321" $ "385"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "309" ! A.width "40" ! A.height "32" ! A.x "753.82123" ! A.y "510.2894"
             S.text_  ! A.x "773.82123" ! A.y "526.2894" $ "309"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "309" ! A.width "40" ! A.height "32" ! A.x "753.82123" ! A.y "510.2894"
             S.text_  ! A.x "773.82123" ! A.y "526.2894" $ "309"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "343" ! A.width "40" ! A.height "32" ! A.x "725.48462" ! A.y "424.52161"
             S.text_  ! A.x "745.48462" ! A.y "440.52161" $ "343"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "343" ! A.width "40" ! A.height "32" ! A.x "725.48462" ! A.y "424.52161"
             S.text_  ! A.x "745.48462" ! A.y "440.52161" $ "343"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "443" ! A.width "40" ! A.height "32" ! A.x "715.45349" ! A.y "578.60641"
             S.text_  ! A.x "735.45349" ! A.y "594.60641" $ "443"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "443" ! A.width "40" ! A.height "32" ! A.x "715.45349" ! A.y "578.60641"
             S.text_  ! A.x "735.45349" ! A.y "594.60641" $ "443"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "301/384" ! A.width "50" ! A.height "24" ! A.x "15.380709999999993" ! A.y "472.29559"
             S.text_  ! A.x "40.38070999999999" ! A.y "484.29559" $ "301/384"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "301/384" ! A.width "50" ! A.height "24" ! A.x "15.380709999999993" ! A.y "472.29559"
             S.text_  ! A.x "40.38070999999999" ! A.y "484.29559" $ "301/384"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "916.4259999999999" ! A.y "232.222"
             S.text_  ! A.x "929.9259999999999" ! A.y "244.222" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "916.4259999999999" ! A.y "232.222"
             S.text_  ! A.x "929.9259999999999" ! A.y "244.222" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Calc1" ! A.width "64.826897" ! A.height "24" ! A.x "125.19790999999998" ! A.y "478.4258"
             S.text_  ! A.x "157.6113585" ! A.y "490.4258" $ "Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Calc1" ! A.width "64.826897" ! A.height "24" ! A.x "125.19790999999998" ! A.y "478.4258"
             S.text_  ! A.x "157.6113585" ! A.y "490.4258" $ "Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "258,324" ! A.width "40" ! A.height "24" ! A.x "956.9658999999999" ! A.y "347.688099"
             S.text_  ! A.x "976.9658999999999" ! A.y "359.688099" $ "258,324"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "258,324" ! A.width "40" ! A.height "24" ! A.x "956.9658999999999" ! A.y "347.688099"
             S.text_  ! A.x "976.9658999999999" ! A.y "359.688099" $ "258,324"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "373" ! A.width "27" ! A.height "24" ! A.x "663.00458" ! A.y "541.3551"
             S.text_  ! A.x "676.50458" ! A.y "553.3551" $ "373"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "373" ! A.width "27" ! A.height "24" ! A.x "663.00458" ! A.y "541.3551"
             S.text_  ! A.x "676.50458" ! A.y "553.3551" $ "373"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "27" ! A.height "24" ! A.x "420.63" ! A.y "204.583099"
             S.text_  ! A.x "434.13" ! A.y "216.583099" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "27" ! A.height "24" ! A.x "420.63" ! A.y "204.583099"
             S.text_  ! A.x "434.13" ! A.y "216.583099" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Lin1" ! A.width "27" ! A.height "24" ! A.x "482.72748" ! A.y "204.583099"
             S.text_  ! A.x "496.22748" ! A.y "216.583099" $ "Lin1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Lin1" ! A.width "27" ! A.height "24" ! A.x "482.72748" ! A.y "204.583099"
             S.text_  ! A.x "496.22748" ! A.y "216.583099" $ "Lin1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "27" ! A.height "24" ! A.x "597.58002" ! A.y "549.60641"
             S.text_  ! A.x "611.08002" ! A.y "561.60641" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "27" ! A.height "24" ! A.x "597.58002" ! A.y "549.60641"
             S.text_  ! A.x "611.08002" ! A.y "561.60641" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1,Sta2" ! A.width "55.5783" ! A.height "24" ! A.x "380.78949" ! A.y "533.53979"
             S.text_  ! A.x "408.57864" ! A.y "545.53979" $ "Sta1,Sta2"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1,Sta2" ! A.width "55.5783" ! A.height "24" ! A.x "380.78949" ! A.y "533.53979"
             S.text_  ! A.x "408.57864" ! A.y "545.53979" $ "Sta1,Sta2"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Calc1" ! A.width "61.5" ! A.height "32" ! A.x "171.02999999999997" ! A.y "37.80279999999999"
             S.text_  ! A.x "201.77999999999997" ! A.y "53.80279999999999" $ "Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Calc1" ! A.width "61.5" ! A.height "32" ! A.x "171.02999999999997" ! A.y "37.80279999999999"
             S.text_  ! A.x "201.77999999999997" ! A.y "53.80279999999999" $ "Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "50" ! A.height "32" ! A.x "46.34779" ! A.y "88.5777"
             S.text_  ! A.x "71.34779" ! A.y "104.5777" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta1" ! A.width "50" ! A.height "32" ! A.x "46.34779" ! A.y "88.5777"
             S.text_  ! A.x "71.34779" ! A.y "104.5777" $ "Sta1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta2" ! A.width "50" ! A.height "32" ! A.x "46.34779" ! A.y "150.5777"
             S.text_  ! A.x "71.34779" ! A.y "166.5777" $ "Sta2"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Sta2" ! A.width "50" ! A.height "32" ! A.x "46.34779" ! A.y "150.5777"
             S.text_  ! A.x "71.34779" ! A.y "166.5777" $ "Sta2"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "412" ! A.width "40" ! A.height "32" ! A.x "459.37549" ! A.y "578.60641"
             S.text_  ! A.x "479.37549" ! A.y "594.60641" $ "412"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "412" ! A.width "40" ! A.height "32" ! A.x "459.37549" ! A.y "578.60641"
             S.text_  ! A.x "479.37549" ! A.y "594.60641" $ "412"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "420" ! A.width "40" ! A.height "32" ! A.x "145.97791" ! A.y "578.60641"
             S.text_  ! A.x "165.97791" ! A.y "594.60641" $ "420"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "420" ! A.width "40" ! A.height "32" ! A.x "145.97791" ! A.y "578.60641"
             S.text_  ! A.x "165.97791" ! A.y "594.60641" $ "420"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "104" ! A.width "40" ! A.height "32" ! A.x "547.49847" ! A.y "37.80279999999999"
             S.text_  ! A.x "567.49847" ! A.y "53.80279999999999" $ "104"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "104" ! A.width "40" ! A.height "32" ! A.x "547.49847" ! A.y "37.80279999999999"
             S.text_  ! A.x "567.49847" ! A.y "53.80279999999999" $ "104"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "120" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "37.80279999999999"
             S.text_  ! A.x "653.82123" ! A.y "53.80279999999999" $ "120"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "120" ! A.width "40" ! A.height "32" ! A.x "633.82123" ! A.y "37.80279999999999"
             S.text_  ! A.x "653.82123" ! A.y "53.80279999999999" $ "120"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Lin1" ! A.width "50" ! A.height "32" ! A.x "126.34780999999998" ! A.y "150.5777"
             S.text_  ! A.x "151.34780999999998" ! A.y "166.5777" $ "Lin1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "Lin1" ! A.width "50" ! A.height "32" ! A.x "126.34780999999998" ! A.y "150.5777"
             S.text_  ! A.x "151.34780999999998" ! A.y "166.5777" $ "Lin1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "640.32123" ! A.y "232.222"
             S.text_  ! A.x "653.82123" ! A.y "244.222" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "640.32123" ! A.y "232.222"
             S.text_  ! A.x "653.82123" ! A.y "244.222" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236" ! A.width "27" ! A.height "24" ! A.x "916.4259999999999" ! A.y "524.6064"
             S.text_  ! A.x "929.9259999999999" ! A.y "536.6064" $ "236"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236" ! A.width "27" ! A.height "24" ! A.x "916.4259999999999" ! A.y "524.6064"
             S.text_  ! A.x "929.9259999999999" ! A.y "536.6064" $ "236"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "372" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "424.52161"
             S.text_  ! A.x "882.8861" ! A.y "440.52161" $ "372"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "372" ! A.width "40" ! A.height "32" ! A.x "862.8861" ! A.y "424.52161"
             S.text_  ! A.x "882.8861" ! A.y "440.52161" $ "372"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "209" ! A.width "27" ! A.height "24" ! A.x "98.7251" ! A.y "331.271599"
             S.text_  ! A.x "112.2251" ! A.y "343.271599" $ "209"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "209" ! A.width "27" ! A.height "24" ! A.x "98.7251" ! A.y "331.271599"
             S.text_  ! A.x "112.2251" ! A.y "343.271599" $ "209"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "310" ! A.width "40" ! A.height "32" ! A.x "402.17169" ! A.y "311.688101"
             S.text_  ! A.x "422.17169" ! A.y "327.688101" $ "310"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "310" ! A.width "40" ! A.height "32" ! A.x "402.17169" ! A.y "311.688101"
             S.text_  ! A.x "422.17169" ! A.y "327.688101" $ "310"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "148" ! A.width "27" ! A.height "24" ! A.x "408.67169" ! A.y "270.508101"
             S.text_  ! A.x "422.17169" ! A.y "282.508101" $ "148"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "148" ! A.width "27" ! A.height "24" ! A.x "408.67169" ! A.y "270.508101"
             S.text_  ! A.x "422.17169" ! A.y "282.508101" $ "148"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "207" ! A.width "27" ! A.height "24" ! A.x "741.43658" ! A.y "358.664902"
             S.text_  ! A.x "754.93658" ! A.y "370.664902" $ "207"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "207" ! A.width "27" ! A.height "24" ! A.x "741.43658" ! A.y "358.664902"
             S.text_  ! A.x "754.93658" ! A.y "370.664902" $ "207"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "133.56439" ! A.y "524.6064"
             S.text_  ! A.x "147.06439" ! A.y "536.6064" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263" ! A.width "27" ! A.height "24" ! A.x "133.56439" ! A.y "524.6064"
             S.text_  ! A.x "147.06439" ! A.y "536.6064" $ "263"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "165/Calc1" ! A.width "50" ! A.height "24" ! A.x "703.82123" ! A.y "319.688101"
             S.text_  ! A.x "728.82123" ! A.y "331.688101" $ "165/Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "165/Calc1" ! A.width "50" ! A.height "24" ! A.x "703.82123" ! A.y "319.688101"
             S.text_  ! A.x "728.82123" ! A.y "331.688101" $ "165/Calc1"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263/265" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "228.222"
             S.text_  ! A.x "352.1717385" ! A.y "244.222" $ "263/265"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "263/265" ! A.width "65.207497" ! A.height "32" ! A.x "319.56799" ! A.y "228.222"
             S.text_  ! A.x "352.1717385" ! A.y "244.222" $ "263/265"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "465" ! A.width "40" ! A.height "32" ! A.x "507.44426999999996" ! A.y "373.688103"
             S.text_  ! A.x "527.44427" ! A.y "389.688103" $ "465"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "465" ! A.width "40" ! A.height "32" ! A.x "507.44426999999996" ! A.y "373.688103"
             S.text_  ! A.x "527.44427" ! A.y "389.688103" $ "465"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236" ! A.width "27" ! A.height "24" ! A.x "487.62622" ! A.y "315.688101"
             S.text_  ! A.x "501.12622" ! A.y "327.688101" $ "236"
            S.g ! A.class_ "node" $ do 
             S.rect ! A.id_ "236" ! A.width "27" ! A.height "24" ! A.x "487.62622" ! A.y "315.688101"
             S.text_  ! A.x "501.12622" ! A.y "327.688101" $ "236"
            S.g ! A.transform "translate(-146,288)" $ do
                S.path ! A.id_ "p8" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 627.1759,-218.1813 0,10.7713"
                S.path ! A.id_ "p9" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 627.1759,-199.41 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p10" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 607.1496,-183.4223 -95.3435,0 0,27.0332"
                S.path ! A.id_ "p11" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 511.8061,-148.3891 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p12" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 647.197,-183.4223 152.6242,0 0,27.0229"
                S.path ! A.id_ "p13" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 799.8212,-148.3994 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p14" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 819.8309,-127.2058 99.9903,25.9934 0,33.4212"
                S.path ! A.id_ "p15" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 919.8212,-59.7912 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p16" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 647.164,-183.4223 190.7486,0 0,51.0148 122.8469,0"
                S.path ! A.id_ "p17" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 968.7595,-132.4075 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p18" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,-250.1692 0,-22.3308 359.12838,0 0,126.60289 103.75682,-1.9e-4"
                S.path ! A.id_ "p19" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 969.0569,-145.8973 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p20" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 214.3625,106.2599 0,21.9533"
                S.path ! A.id_ "p21" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 214.3625,136.2132 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p22" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 607.1931,-183.4223 -191.789,0 0,250.7856"
                S.path ! A.id_ "p23" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 415.4041,75.3633 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p24" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 415.4041,107.3975 0,130.8919 -6.7951,0"
                S.path ! A.id_ "p25" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 400.609,238.2894 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p26" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 415.4041,107.3647 0,199.2417 -6.7951,0"
                S.path ! A.id_ "p27" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 400.609,306.6064 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p28" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 415.40408,107.3819 2e-5,43.0913 -6.7951,0"
                S.path ! A.id_ "p29" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 400.609,150.4732 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p30" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 685.1554,224.4294 0,58.167"
                S.path ! A.id_ "p31" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 685.1554,290.5964 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p32" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 779.828,-132.4075 -22.748,0 0,316.8603"
                S.path ! A.id_ "p33" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 757.08,192.4528 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p34" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 899.8247,-43.778 -58.0087,0 0,228.2316"
                S.path ! A.id_ "p35" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 841.816,192.4536 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p36" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 899.8388,-43.778 -58.266,0 0,83.4661 -13.732,0"
                S.path ! A.id_ "p37" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 819.8408,39.6881 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p38" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 799.8212,55.7028 0,21.9533"
                S.path ! A.id_ "p39" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 799.8212,85.6561 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p40" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 949.8212,55.6863 0,250.9201 51.0285,0"
                S.path ! A.id_ "p41" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1008.8497,306.6064 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p42" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 919.8212,-27.7671 0,242.045"
                S.path ! A.id_ "p43" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 919.8212,222.2779 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p44" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 896.7838,168.5612 15.252,46.164"
                S.path ! A.id_ "p45" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 914.5455,222.3214 0.9831,-12.9628 -3.8065,4.4171 -5.6887,-1.28 z"
                S.path ! A.id_ "p46" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 881.4535,168.5546 0,114.0424"
                S.path ! A.id_ "p47" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 881.4535,290.597 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p48" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 949.8212,55.6863 0,250.9201 -40.3715,0"
                S.path ! A.id_ "p49" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 901.4497,306.6064 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p50" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 194.0034,208.2956 5.9099,9.3037"
                S.path ! A.id_ "p51" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 204.2029,224.3521 -2.2139,-12.8102 -2.6119,5.2133 -5.829,0.1487 z"
                S.path ! A.id_ "p52" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1075.926,-31.778 0,184.2996 19.0572,0"
                S.path ! A.id_ "p53" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1102.9832,152.5216 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p54" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1075.926,-31.778 0,71.4661 -19.0571,0"
                S.path ! A.id_ "p55" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1048.8689,39.6881 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p56" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1075.926,-31.778 0,127.8829 -19.0571,0"
                S.path ! A.id_ "p57" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1048.8689,96.1049 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p58" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 303.6114,189.9258 2.7261,-13.8248"
                S.path ! A.id_ "p59" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 307.8852,168.2521 -7.2271,10.806 5.4859,-1.976 4.3252,3.9106 z"
                S.path ! A.id_ "p60" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 530.7918,-132.4075 66.3518,0 18.2361,17.6035"
                S.path ! A.id_ "p61" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 621.1355,-109.2478 -5.1611,-11.9316 -1.3142,5.6809 -5.631,1.5138 z"
                S.path ! A.id_ "p62" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 779.8051,-132.4075 -115.947,0 -23.6248,18.6711"
                S.path ! A.id_ "p63" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 633.9568,-108.776 12.515,-3.5178 -5.454,-2.0627 -0.7466,-5.7829 z"
                S.path ! A.id_ "p64" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 637.0559,-103.4169 32.2947,0 0,144.2892 36.1772,38.9485"
                S.path ! A.id_ "p65" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 710.9723,85.6824 -4.5032,-12.1951 -1.6218,5.6009 -5.7051,1.2047 z"
                S.path ! A.id_ "p66" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1122.9658,83.6881 0,44.8015"
                S.path ! A.id_ "p67" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1122.9658,136.4896 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p68" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 836.0046,274.8022 19.2989,13.505"
                S.path ! A.id_ "p69" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 861.8581,292.8939 -6.9651,-10.9766 -0.4087,5.8166 -5.3247,2.3765 z"
                S.path ! A.id_ "p70" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 939.8525,-43.778 31.0368,0"
                S.path ! A.id_ "p71" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 978.8893,-43.778 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p72" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-116.4386 0,57.2921"
                S.path ! A.id_ "p73" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-51.1465 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p74" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-36.4096 0,76.0977 -10.9796,0"
                S.path ! A.id_ "p75" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 969.7897,39.6881 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p76" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-36.4096 0,243.7647 12.1007,0.3373"
                S.path ! A.id_ "p77" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1008.8669,207.9153 -11.8561,-5.3324 2.8596,5.0816 -3.1382,4.9145 z"
                S.path ! A.id_ "p78" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-36.4096 0,76.0977 12.0976,0"
                S.path ! A.id_ "p79" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1008.8669,39.6881 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p80" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.7693,-36.4096 0,132.5145 12.0976,0"
                S.path ! A.id_ "p81" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1008.8669,96.1049 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p82" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 455.4041,168.2525 0,70.0369 24.8876,0"
                S.path ! A.id_ "p83" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 488.2917,238.2894 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p84" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,106.2158 0,104.9452 0,11.76"
                S.path ! A.id_ "p85" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,230.921 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p86" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,245.6579 0,60.9485 39.4403,0"
                S.path ! A.id_ "p87" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 545.612,306.6064 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p88" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,245.6579 0,96.9485 186.9837,0 0,-12.0039"
                S.path ! A.id_ "p89" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 685.1554,322.6025 -5,12 5,-3 5,3 z"
                S.path ! A.id_ "p90" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "M 743.8287,261.6064 708.4753,229.5912"
                S.path ! A.id_ "p91" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 702.5454,224.2212 5.5386,11.7611 1.1325,-5.7199 5.5799,-1.6924 z"
                S.path ! A.id_ "p92" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 757.08,261.6064 0,-29.1258"
                S.path ! A.id_ "p93" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 757.08,224.4806 -5,12 5,-3 5,3 z"
                S.path ! A.id_ "p94" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 770.3312,261.6064 35.3535,-32.0152"
                S.path ! A.id_ "p95" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 811.6146,224.2212 -12.2511,4.3488 5.58,1.6924 1.1325,5.7199 z"
                S.path ! A.id_ "p96" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 757.08,261.6064 0,-29.1258"
                S.path ! A.id_ "p97" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 757.08,224.4806 -5,12 5,-3 5,3 z"
                S.path ! A.id_ "p98" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "M 571.8931,245.5398 599.136,226.6587"
                S.path ! A.id_ "p99" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 605.7113,222.1017 -12.711,2.726 5.3138,2.4007 0.3825,5.8184 z"
                S.path ! A.id_ "p100" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.7799,-218.1813 0,34.759 -97.4347,0"
                S.path ! A.id_ "p101" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 242.3452,-183.4223 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p102" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 217.3478,-167.4076 0,21.9533"
                S.path ! A.id_ "p103" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 217.3478,-137.4543 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p104" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 192.3429,-121.4223 -19.9951,0 0,273.9439 14.0166,0"
                S.path ! A.id_ "p105" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 194.3644,152.5216 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p106" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 625.3755,224.4294 0,58.167"
                S.path ! A.id_ "p107" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 625.3755,290.5964 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p108" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 799.8212,-31.778 0,47.4491"
                S.path ! A.id_ "p109" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 799.8212,23.6711 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p110" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1089.426,236.7677 9.7398,-8.5412"
                S.path ! A.id_ "p111" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1105.1805,222.9518 -12.3188,4.1528 5.5522,1.7812 1.0412,5.7373 z"
                S.path ! A.id_ "p112" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 988.76908,-36.409108 2.2e-4,188.930708 12.0976,0"
                S.path ! A.id_ "p113" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 1008.8669,152.5216 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p114" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 258.2251,67.2716 0,275.3348 122.3509,0 0,-12.0039"
                S.path ! A.id_ "p115" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 380.576,322.6025 -5,12 5,-3 5,3 z"
                S.path ! A.id_ "p116" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 258.2251,67.2716 0,84.9737 -15.8588,0"
                S.path ! A.id_ "p117" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 234.3663,152.2453 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p118" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 258.2251,67.2716 0,24.0917 24.8298,0"
                S.path ! A.id_ "p119" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 291.0549,91.3633 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p120" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 258.22508,67.2716 2e-5,84.9737 24.8298,0"
                S.path ! A.id_ "p121" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 291.0549,152.2453 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p122" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 638.3286,-59.4169 -12.7218,39.1553"
                S.path ! A.id_ "p123" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 623.1348,-12.6531 8.4633,-9.8677 -5.6823,1.3082 -3.8283,-4.3982 z"
                S.path ! A.id_ "p124" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 587.5344,-59.4169 24.9425,40.4231"
                S.path ! A.id_ "p125" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 616.6778,-12.1856 -2.0463,-12.8379 -2.6797,5.1787 -5.8305,0.0724 z"
                S.path ! A.id_ "p126" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 620.808,1.8766 0,126.3993"
                S.path ! A.id_ "p127" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 620.808,136.2759 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p128" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 620.808,1.8766 0,37.8115 -24.6149,0"
                S.path ! A.id_ "p129" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 588.1931,39.6881 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p130" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 568.1717,6.5081 0,9.2057"
                S.path ! A.id_ "p131" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 568.1717,23.7138 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p132" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 899.3129,94.6649 -4.5884,33.911"
                S.path ! A.id_ "p133" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 893.6519,136.5037 6.5639,-11.2212 -5.3572,2.3024 -4.5526,-3.6433 z"
                S.path ! A.id_ "p134" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 296.9776,260.6064 7.2905,22.3571"
                S.path ! A.id_ "p135" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 306.7483,290.5693 1.0333,-12.9588 -3.8236,4.4023 -5.6837,-1.302 z"
                S.path ! A.id_ "p136" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 861.5655,55.7048 16.8769,73.0112"
                S.path ! A.id_ "p137" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 880.2441,136.5105 2.169,-12.8178 -4.1959,4.049 -5.5472,-1.7969 z"
                S.path ! A.id_ "p138" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 309.0447,-105.4159 29.2871,40.0772"
                S.path ! A.id_ "p139" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 343.0519,-58.8795 -3.0432,-12.6388 -2.2669,5.3722 -5.807,0.5279 z"
                S.path ! A.id_ "p140" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.7799,-218.1754 0,150.3974"
                S.path ! A.id_ "p141" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.7799,-59.778 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p142" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.7799,-45.0411 0,136.4044 39.6473,0"
                S.path ! A.id_ "p143" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 395.4272,91.3633 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p144" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.7799,-45.0411 0,136.4044 -8.7547,0"
                S.path ! A.id_ "p145" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 331.0252,91.3633 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p146" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 347.78008,-45.0411 -1.8e-4,351.6475 -7.7682,0"
                S.path ! A.id_ "p147" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 332.0117,306.6064 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p148" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 580.13,-59.4169 0,15.6389 -41.3431,0"
                S.path ! A.id_ "p149" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 530.7869,-43.778 12,5 -3,-5 3,-5 z"
                S.path ! A.id_ "p150" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 617.296,-103.4169 -119.1243,0 0,35.6517"
                S.path ! A.id_ "p151" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,-59.7652 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p152" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,-27.7777 0,94.0311"
                S.path ! A.id_ "p153" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 498.1717,74.2534 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p154" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 528.1487,-28.0093 0,236.4825 69.2495,0"
                S.path ! A.id_ "p155" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 605.3982,208.4732 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p156" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 527.8939,-27.9092 0,129.5973 12.2569,0"
                S.path ! A.id_ "p157" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 548.1508,101.6881 -12,-5 3,5 -3,5 z"
                S.path ! A.id_ "p158" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 528.3304,-28.0653 0,204.1427 156.825,0 0,8.4193"
                S.path ! A.id_ "p159" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 685.1554,192.4967 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p160" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 652.22,51.6881 11.3104,26.6448"
                S.path ! A.id_ "p161" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 666.6563,85.6969 -0.0864,-12.9997 -3.4303,4.7152 -5.7747,-0.8078 z"
                S.path ! A.id_ "p162" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 455.4041,168.2617 0,114.3689"
                S.path ! A.id_ "p163" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 455.4041,290.6306 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p164" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 486.4949,-218.1922 0,61.7864"
                S.path ! A.id_ "p165" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 486.4949,-148.4058 5,-12 -5,3 -5,-3 z"
                S.path ! A.id_ "p166" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 465.555,-132.4075 -10.1509,0 0,260.6341"
                S.path ! A.id_ "p167" ! A.class_ "path" ! A.style "fill:none;stroke:#000000;stroke-width:1px;stroke-linecap:butt;stroke-linejoin:miter;stroke-opacity:1" ! A.d "m 455.4041,136.2266 5,-12 -5,3 -5,-3 z"
            S.g ! A.transform "translate(-146,288)" $ do
                S.ellipse ! A.class_ "bool" ! A.id_ "bool0" ! A.d "m 637.0559,-103.4169 c 0,4.069454 -4.42342,7.368399 -9.88,7.368399 -5.45657,0 -9.88,-3.298945 -9.88,-7.368399 0,-4.06946 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.29894 9.88,7.3684 z" ! A.cx "627.1759" ! A.cy "-103.4169" ! A.rx "9.8800001" ! A.ry "7.3684001"
                S.ellipse ! A.class_ "bool" ! A.id_ "bool1" ! A.d "m 998.64929,-43.778 c 0,4.069455 -4.42343,7.3684 -9.88,7.3684 -5.45658,0 -9.88,-3.298945 -9.88,-7.3684 0,-4.069455 4.42342,-7.3684 9.88,-7.3684 5.45657,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "988.76929" ! A.cy "-43.778" ! A.rx "9.8800001" ! A.ry "7.3684001"
                S.ellipse ! A.class_ "bool" ! A.id_ "bool2" ! A.d "m 508.05169,238.2894 c 0,4.06945 -4.42342,7.3684 -9.88,7.3684 -5.45657,0 -9.88,-3.29895 -9.88,-7.3684 0,-4.06946 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.29894 9.88,7.3684 z" ! A.cx "498.17169" ! A.cy "238.2894" ! A.rx "9.8800001" ! A.ry "7.3684001"
                S.ellipse ! A.class_ "bool" ! A.id_ "bool3" ! A.d "m 630.68798,-5.4919 c 0,4.069455 -4.42342,7.3684001 -9.88,7.3684001 -5.45657,0 -9.88,-3.2989451 -9.88,-7.3684001 0,-4.069455 4.42343,-7.3684 9.88,-7.3684 5.45658,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "620.80798" ! A.cy "-5.4919" ! A.rx "9.8800001" ! A.ry "7.3684001"
                S.ellipse ! A.class_ "bool" ! A.id_ "bool4" ! A.d "m 357.66,-52.409599 c 0,4.069455 -4.42343,7.3684 -9.88,7.3684 -5.45657,0 -9.88,-3.298945 -9.88,-7.3684 0,-4.069455 4.42343,-7.3684 9.88,-7.3684 5.45657,0 9.88,3.298945 9.88,7.3684 z" ! A.cx "347.78" ! A.cy "-52.409599" ! A.rx "9.8800001" ! A.ry "7.3684001"
