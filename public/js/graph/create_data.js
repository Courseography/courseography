// Math courses
var math = [
    'MAT135136137157Calc1', 'MAT221223240Lin1', 'STA247255257Sta1', 'Sta248261'
];

// All CSC Courses
var allCourses = ['CSC108', 'CSC148', 'CSC165240', 'CSC207', 'CSC236240',
                  'CSC209', 'CSC258', 'CSC236265265', 'CSC300', 'CSC301',
                  'CSC302', 'CSC309', 'CSC310', 'CSC318', 'CSC320',
                  'CSC321', 'CSC324', 'CSC336', 'CSC343', 'CSC358',
                  'CSC369', 'CSC372', 'CSC348', 'ECE385', 'CSC401',
                  'CSC404', 'CSC410', 'CSC411', 'CSC412', 'CSC418',
                  'CSC420', 'CSC428', 'CSC386', 'CSC438', 'CSC443',
                  'CSC446', 'CSC448', 'CSC454', 'CSC456', 'CSC458',
                  'CSC463', 'CSC465', 'CSC469', 'CSC486', 'CSC488',
                  'ECE489', 'CSC373', 'CSC466'];

specialistCourses = ['CSC373', 'CSC369', 'MAT221223240Lin1'];

var areaNames = ['theory', 'core', 'se', 'systems', 'hci',
                 'graphics', 'num', 'ai'];


// Required for specialist
var reqs = [
    'CSC108', 'CSC148', 'CSC165240', 'CSC207',
    'CSC209', 'CSC236240', 'CSC258', 'CSC236265',
    'CSC369', 'CSC373', 'MAT135136137157Calc1', 'MAT221223240Lin1', 'STA247255257Sta1'
];

// 'Inquiry' courses
var CSCinq = [
    'CSC301', 'CSC318', 'CSC404', 'CSC411',
    'CSC418', 'CSC420', 'CSC428', 'CSC454',
    'CSC485'
];

// Focus-related courses
var sciFocusList = [
    'CSC336', 'CSC446', 'CSC456', 'CSC320',
    'CSC418', 'CSC321', 'CSC411', 'CSC343',
    'CSC384', 'CSC358', 'CSC458'
];

var AIFocusList = [
    'CSC310', 'CSC438', 'CSC448',
    'CSC463', 'CSC401', 'CSC485', 'CSC320',
    'CSC420', 'CSC321', 'CSC411', 'CSC412',
    'CSC384', 'CSC486'
];

var NLPFocusList = [
    'CSC318', 'CSC401', 'CSC485', 'CSC309',
    'CSC321', 'CSC411', 'CSC428', 'CSC486'
];

var visionFocusList = [
    'CSC320', 'CSC336', 'CSC411', 'CSC420',
    'CSC418', 'CSC412'
];

var systemsFocusList = [
    'CSC324', 'CSC343', 'CSC443', 'CSC469',
    'CSC488', 'CSC372', 'ECE385', 'CSC358',
    'CSC458', 'CSC301', 'CSC309', 'CSC410', 'ECE489'
];

var gameFocusList = [
    'CSC300', 'CSC301', 'CSC318', 'CSC324',
    'CSC384', 'CSC418', 'CSC404'
];

var HCIFocusList = [
    'CSC300', 'CSC301', 'CSC318', 'CSC428',
    'CSC309', 'CSC320', 'CSC321', 'CSC343',
    'CSC384', 'CSC401', 'CSC404', 'CSC418',
    'CSC485'
];

var theoryFocusList = [
    'CSC336', 'CSC463', 'CSC310', 'CSC438',
    'CSC448', 'Sta248261'
];

var webFocusList = [
    'Sta248261', 'CSC309', 'CSC343', 'CSC358',
    'CSC458', 'CSC411', 'CSC310', 'CSC443', 'CSC469'
];

// Courses that can be taken with no other prerequisites
var initiallyTakeable = [
    'csc104', 'csc120', 'csc108', 'CSC165240',
    'MAT135136137157Calc1', 'MAT221223240Lin1', 'csc200', 'csc300'
];