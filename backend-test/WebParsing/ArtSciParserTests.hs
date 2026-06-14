{-|
Description: ArtSciParser module tests.

Module that contains the tests for the functions in the ArtSciParser module.

-}

module WebParsing.ArtSciParserTests
( test_artSciParser
) where

import qualified Data.Text as T
import WebParsing.ArtSciParser (parseDepartmentList)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parsedDepts :: [(T.Text, T.Text)]
parsedDepts =
  [ ("/section/Academic-Bridging-Program", "Academic Bridging Program"),
    ("/section/Actuarial-Science", "Actuarial Science"),
    ("/section/African-Studies", "African Studies Centre"),
    ("/section/American-Studies", "American Studies"),
    ("/section/Anatomy", "Anatomy"),
    ("/section/Anthropology", "Anthropology"),
    ("/section/Archaeology", "Archaeology"),
    ("/section/Architecture-and-Visual-Studies", "Architecture and Visual Studies"),
    ("/section/Art-History", "Art History"),
    ("/section/Astronomy-and-Astrophysics", "Astronomy and Astrophysics"),
    ("/section/Biochemistry", "Biochemistry"),
    ("/section/Business-Fundamentals", "Business Fundamentals"),
    ("/section/Centre-for-Caribbean-Studies", "Caribbean Studies, Centre for"),
    ("/section/Cell-and-Systems-Biology", "Cell and Systems Biology"),
    ("/section/Chemistry", "Chemistry"),
    ("/section/Cinema-Studies-Institute", "Cinema Studies (Cinema Studies Institute)"),
    ("/section/Classics", "Classics"),
    ("/section/Computer-Science", "Computer Science"),
    ("/section/Contemporary-Asian-Studies", "Contemporary Asian Studies, Dr. David Chu Program in"),
    ("/section/Criminology-and-Sociolegal-Studies", "Criminology and Sociolegal Studies, Centre for"),
    ("/section/Diaspora-and-Transnational-Studies", "Diaspora and Transnational Studies"),
    ("/section/Drama,-Theatre-and-Performance-Studies", "Drama, Theatre and Performance Studies, Centre for"),
    ("/section/Earth-Sciences", "Earth Sciences"),
    ("/section/East-Asian-Studies", "East Asian Studies"),
    ("/section/Ecology-and-Evolutionary-Biology", "Ecology and Evolutionary Biology"),
    ("/section/Economics", "Economics"),
    ("/section/English", "English"),
    ("/section/Centre-for-Entrepreneurship", "Entrepreneurship, Centre for"),
    ("/section/School-of-the-Environment", "Environment (School of the Environment)"),
    ("/section/Slavic-and-East-European-Languages-and-Cultures", "Estonian"),
    ("/section/Centre-for-Ethics", "Ethics, Centre for"),
    ("/section/European-Affairs", "European Affairs"),
    ("/section/Slavic-and-East-European-Languages-and-Cultures", "Finnish"),
    ("/section/First-Year-Foundations", "First-Year Foundations"),
    ("/section/Forest-Conservation-and-Forest-Biomaterials-Science", "Forest Conservation and Forest Biomaterials Science"),
    ("/section/French", "French"),
    ("/section/Geography-and-Planning", "Geography and Planning"),
    ("/section/German", "German"),
    ("/section/History", "History"),
    ("/section/History-and-Philosophy-of-Science-and-Technology", "History and Philosophy of Science and Technology"),
    ("/section/Human-Biology", "Human Biology"),
    ("/section/Hungarian", "Hungarian"),
    ("/section/Immunology", "Immunology"),
    ("/section/Indigenous-Studies", "Indigenous Studies"),
    ("/section/Industrial-Relations-and-Human-Resources", "Industrial Relations and Human Resources, Centre for"),
    ("/section/Innis-College", "Innis College"),
    ("/section/Italian", "Italian"),
    ("/section/Centre-for-Jewish-Studies", "Jewish Studies, Centre for"),
    ("/section/Laboratory-Medicine-and-Pathobiology", "Laboratory Medicine and Pathobiology"),
    ("/section/Latin-American-Studies", "Latin American Studies"),
    ("/section/Linguistics", "Linguistics"),
    ("/section/Materials-Science", "Materials Science"),
    ("/section/Mathematics", "Mathematics"),
    ("/section/Centre-for-Medieval-Studies", "Medieval Studies, Centre for"),
    ("/section/Molecular-Genetics-and-Microbiology", "Molecular Genetics and Microbiology"),
    ("/section/Munk-School-of-Global-Affairs-and-Public-Policy", "Munk School of Global Affairs and Public Policy"),
    ("/section/Music", "Music"),
    ("/section/Near-and-Middle-Eastern-Civilizations", "Near and Middle Eastern Civilizations"),
    ("/section/New-College", "New College"),
    ("/section/Nutritional-Sciences", "Nutritional Sciences"),
    ("/section/Munk-School-of-Global-Affairs-and-Public-Policy", "Peace, Conflict and Justice"),
    ("/section/Pharmacology-and-Toxicology", "Pharmacology and Toxicology"),
    ("/section/Philosophy", "Philosophy"),
    ("/section/Physics", "Physics"),
    ("/section/Physiology", "Physiology"),
    ("/section/Planetary-Science", "Planetary Science"),
    ("/section/Political-Science", "Political Science"),
    ("/section/Portuguese", "Portuguese"),
    ("/section/Psychology", "Psychology"),
    ("/section/Munk-School-of-Global-Affairs-and-Public-Policy", "Public Policy"),
    ("/section/Religion", "Religion"),
    ("/section/Rotman-Commerce", "Rotman Commerce"),
    ("/section/St.-Michael's-College", "St. Michael's College"),
    ("/section/Sexual-Diversity-Studies", "Sexual Diversity Studies, Mark S. Bonham Centre for"),
    ("/section/Slavic-and-East-European-Languages-and-Cultures", "Slavic and East European Languages and Cultures"),
    ("/section/Sociology", "Sociology"),
    ("/section/South-Asian-Studies", "South Asian Studies"),
    ("/section/Spanish", "Spanish"),
    ("/section/Statistical-Sciences", "Statistical Sciences"),
    ("/section/Canadian-Institute-for-Theoretical-Astrophysics", "Theoretical Astrophysics (Canadian Institute for Theoretical Astrophysics)"),
    ("/section/Trinity-College", "Trinity College"),
    ("/section/University-College", "University College"),
    ("/section/Geography-and-Planning", "Urban Studies"),
    ("/section/Victoria-College", "Victoria College"),
    ("/section/Women-and-Gender-Studies", "Women and Gender Studies"),
    ("/section/Woodsworth-College", "Woodsworth College"),
    ("/writing-faculty-arts-science", "Writing in the Faculty of Arts & Science"),
    ("/section/Yiddish-Studies", "Yiddish Studies")
  ]

-- | List of test cases as (label, input URL, expected output)
parseDeptListTestCases :: [(String, String, [(T.Text, T.Text)])]
parseDeptListTestCases =
    [ ("Program/subject areas page", "https://artsci.calendar.utoronto.ca/listing-program-subject-areas", parsedDepts) ]

-- | Run a test case (label, input URL, expected output) on the parseDepartmentList function.
runParseDeptListTest :: (String, String, [(T.Text, T.Text)]) -> TestTree
runParseDeptListTest (label, input, expected) =
    testCase label $ do
        actual <- parseDepartmentList input
        assertEqual ("Unexpected parsing result for " ++ label) expected actual

-- | Run all the parseDeptList test cases
runParseDeptListTests :: [TestTree]
runParseDeptListTests = map runParseDeptListTest parseDeptListTestCases

-- | Test suite for ArtSciParser module
test_artSciParser :: TestTree
test_artSciParser =
    testGroup "ArtSciParser tests" runParseDeptListTests
