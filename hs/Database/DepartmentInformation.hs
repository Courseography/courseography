{-# LANGUAGE OverloadedStrings #-}

module Database.DepartmentName
    (department, getDepartments) where

import Data.Text (Text)
import Database.Tables hiding (Text)
import Database.Persist.Sqlite (runSqlite, updateWhere, (=.), (==.))
import Config (databasePath)

department :: [(Text, Text)]
department = [
	("ABS", "Aboriginal Studies"),
    ("USA", "American Studies"),
    ("ANA", "Anatomy"),
    ("ANT", "Anthropology"),
    ("FAH", "Art"),
    ("ARC", "Architecture"),
    ("ARH", "Archaeology"),
    ("VIS", "Visual Studies"),
    ("JAV", "Architecture"),
    ("AST", "Astronomy & Astrophysics"),
    ("PLN", "Astronomy & Astrophysics"),
    ("BCH", "Biochemistry"),
    ("BIO", "Biology"),
    ("CTA", "Canadian Institute for Theoretical Astrophysics"),
    ("CHM", "Chemistry"),
    ("JSC", "Chemistry"),
    ("CSB", "Cell and Systems Biology"),
    ("CIN", "Cinema Studies"),
    ("CLA", "Classics"),
    ("COL", "Collaborative Literature"),
    ("EDU", "Concurrent Teacher Education Program"),
    ("CSC", "Computer Science"),
    ("CAS", "Contemporary Asian Studies"),
    ("CRI", "Centre for Criminology and Sociolegal Studies"),
    ("DTS", "Diaspora and Transnational Studies"),
    ("DRM", "Centre for Drama, Theatre and Performance"),
    ("EAS", "East Asian Studies"),
    ("ESS", "Earth Sciences"),
    ("EEB", "Ecology & Evolutionary Biology"),
    ("ECO", "Economics"),
    ("IRE", "Employment Relations, Centre for Industrial Relations and Human Resources"),
    ("ENG", "English"),
    ("ELL", "English Language Learning"),
    ("ENV", "School of Environment"),
    ("ETH", "Centre for Ethics"),
    ("EUR", "European Studies"),
    ("FOR", "Forestry"),
    ("FRE", "French"),
    ("GGR", "Geography"),
    ("GER", "German"),
    ("HIS", "History"),
    ("HPS", "History and Philosophy of Science and Technology"),
    ("HMB", "Human Biology"),
    ("IMM", "Immunology"),
    ("IMC", "Impact Center"),
    ("INI", "Innis College Courses"),
    ("ITA", "Italian"),
    ("CJS", "Center for Jewish Studies"),
    ("LMP", "Laboratory Medicine and Pathology"),
    ("LAS", "Latin American Studies"),
    ("LIN", "Linguistics"),
    ("MSE", "Material Science"),
    ("MAT", "Mathematics"),
    ("MST", "Centre for Medieval Studies"),
    ("MGY", "Molecular Genetics and Microbiology"),
    ("MUS", "Music"),
    ("NMC", "Near & Middle Eastern Civilizations"),
    ("NEW", "New College Courses"),
    ("NFS", "Nutritional Science"),
    ("PCJ", "Peace, Conflict, and Justice Studies"),
    ("PHC", "Pharmaceutical Chemistry"),
    ("PCL", "Pharmacology"),
    ("PHL", "Philosophy"),
    ("PHY", "Physics"),
    ("PSL", "Physiology"),
    ("POL", "Political Science"),
    ("PSY", "Psychology"),
    ("PPG", "School of Public Policy"),
    ("RLG", "Religion"),
    ("RSM", "Rotman Commerce"),
    ("SAS", "South Asian Studies"),
    ("SDS", "Sexual Diversity Studies, Mark S. Bonham Centre"),
    ("SLA", "Slavic Languages and Literature"),
    ("SOC", "Sociology"),
    ("SPA", "Spanish"),
    ("SMC", "St. Michael's College Courses"),
    ("STA", "Statistical Sciences"),
    ("TRN", "Trinity College Courses"),
    ("UNI", "University College Courses"),
    ("VIC", "Victoria College Courses"),
    ("WGS", "Women and Gender Studies"),
    ("WDW", "Woodsworth College Courses")]
    
getDepartment (code, name) =
    insert [DepartmentCode ==. code] [DepartmentName ==. name] 


getDepartments :: IO ()
getDepartments = runSqlite databasePath $
	mapM_ getDepartment department
