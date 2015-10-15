{-# LANGUAGE OverloadedStrings #-}

module Database.DepartmentInformation
    (department, getDepartments) where

import Data.Text (Text)
import Database.Tables (Department(Department))
import Database.Persist.Sqlite (runSqlite, insert)
import Config (databasePath)

department :: [([Text], Text)]
department = [
    (["ABS"], "Aboriginal Studies"),
    (["ACT"], "Acturial Science"),
    (["USA"], "American Studies"),
    (["ANA"], "Anatomy"),
    (["ANT"], "Anthropology"),
    (["FAH"], "Art"),
    (["ARC", "VIS", "JAV"], "Architecture"),
    (["ARH"], "Archaeology"),
    (["JAV"], "Architecture"),
    (["AST", "PLN"], "Astronomy & Astrophysics"),
    (["BCH", "BCB"], "Biochemistry"),
    (["BIO"], "Biology"),
    (["CTA"], "Canadian Institute for Theoretical Astrophysics"),
    (["CHM", "JSC"], "Chemistry"),
    (["CSB"], "Cell and Systems Biology"),
    (["CIN"], "Cinema Studies"),
    (["CLA", "GRK", "LAT"], "Classics"),
    (["COL"], "Collaborative Literature"),
    (["EDU"], "Concurrent Teacher Education Program"),
    (["CSC", "ECE"], "Computer Science"),
    (["CAS"], "Contemporary Asian Studies"),
    (["CRI"], "Centre for Criminology and Sociolegal Studies"),
    (["DTS"], "Diaspora and Transnational Studies"),
    (["DRM"], "Centre for Drama, Theatre and Performance"),
    (["EAS"], "East Asian Studies"),
    (["ESS", "JGA"], "Earth Sciences"),
    (["EEB", "EHJ", "JMB"], "Ecology & Evolutionary Biology"),
    (["ECO"], "Economics"),
    (["IRE"], "Employment Relations, Centre for Industrial Relations and Human Resources"),
    (["ENG", "JEI"], "English"),
    (["ELL"], "English Language Learning"),
    (["ENV", "JEE"], "School of Environment"),
    (["ETH"], "Centre for Ethics"),
    (["EUR", "HUN", "JRA", "MGR"], "European Studies"),
    (["FOR"], "Forestry"),
    (["FRE", "FCS", "FSL", "JFL"], "French"),
    (["GGR", "JGE", "JGI"],  "Geography"),
    (["GER"], "German"),
    (["HIS", "JHA"], "History"),
    (["HPS", "JHE"], "History and Philosophy of Science and Technology"),
    (["HMB", "HAJ", "JEH"], "Human Biology"),
    (["IMM"], "Immunology"),
    (["IMC"], "Impact Center"),
    (["INI"], "Innis College Courses"),
    (["ITA"], "Italian"),
    (["CJS"], "Center for Jewish Studies"),
    (["LMP"], "Laboratory Medicine and Pathology"),
    (["LAS"], "Latin American Studies"),
    (["LIN", "JAL", "JLP", "JLS"], "Linguistics"),
    (["MSE"], "Material Science"),
    (["MAT", "APM"], "Mathematics"),
    (["MST"], "Centre for Medieval Studies"),
    (["MGY", "MIJ"], "Molecular Genetics and Microbiology"),
    (["MUS"], "Music"),
    (["NMC", "NML"], "Near & Middle Eastern Civilizations"),
    (["NEW", "JLN", "JNH", "JQR"], "New College Courses"),
    (["NFS"], "Nutritional Science"),
    (["PCJ", "MUN"], "Peace, Conflict, and Justice Studies"),
    (["PHC"], "Pharmaceutical Chemistry"),
    (["PCL"], "Pharmacology"),
    (["PHL"], "Philosophy"),
    (["PHY", "JPE", "JPH"], "Physics"),
    (["PSL"], "Physiology"),
    (["POL", "JHP", "JPD", "JPF", "JPP", "JPR", "JPU"], "Political Science"),
    (["PSY"], "Psychology"),
    (["PPG"], "School of Public Policy"),
    (["RLG", "MHB"], "Religion"),
    (["RSM", "MGT"], "Rotman Commerce"),
    (["SAS"], "South Asian Studies"),
    (["SDS", "JSU"], "Sexual Diversity Studies, Mark S. Bonham Centre"),
    (["SLA", "EST", "FIN"], "Slavic Languages and Literature"),
    (["SOC"], "Sociology"),
    (["SPA", "PRT"], "Spanish"),
    (["SMC"], "St. Michael's College Courses"),
    (["STA"], "Statistical Sciences"),
    (["TRN"], "Trinity College Courses"),
    (["UNI", "CDN", "COG", "HST", "JUM", "PHS"], "University College Courses"),
    (["VIC", "IVP", "JSV"], "Victoria College Courses"),
    (["WGS"], "Women and Gender Studies"),
    (["WDW"], "Woodsworth College Courses")]
    
getDepartment (code, name) = 
    insert (Department code name)

getDepartments :: IO ()
getDepartments = runSqlite databasePath $
	mapM_ getDepartment department
